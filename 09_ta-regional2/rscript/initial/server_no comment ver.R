server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    landuse_table = NULL,
    land_req = NULL,
    projected_land_use = NULL,
    sciendo_db = NULL,
    GDP_scen_df = NULL,
    GDP_totals_graph = NULL,
    output_total_graph = NULL,
    income_total_graph = NULL,
    labour_total_graph = NULL,
    land_req_path = NULL,
    sciendo_db_path = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  #### Set Working Directory ####
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  observeEvent(input$wd, {
    rv$wd <- parseDirPath(volumes, input$wd)
  })
  
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  output$user_guide <- renderUI({
    guide_path <- "../helpfile/help.md"
    if (file.exists(guide_path)) {
      html_content <- rmarkdown::render(guide_path, output_format = "html_fragment", quiet = TRUE,
                                        output_options = list(metadata = list(title = "Trade-Off Analysis (Regional 2)")))
      HTML(readLines(html_content))
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
  #### Load and process data ####
  observeEvent(input$land_req_file, {
    land_req <- input$land_req_file$datapath
    rv$land_req <- land_req
  })
  
  observeEvent(input$sciendo_db, {
    sciendo_db <- input$sciendo_db$datapath
    rv$sciendo_db <- read.csv(sciendo_db, header = TRUE, stringsAsFactors = FALSE)
  })
  
  #### Perform GDP and Labour Analysis ####
  observeEvent(input$processTAReg2, {
    # Projected Land Use
    load(rv$land_req)
    
    withProgress(message = 'Running TA Regional 2 Analysis', value = 0, {
      tryCatch({
        nodata_val<-0
        land.requirement.db<-land.requirement_table
        names(landuse_lut) <- as.character(landuse_lut[1,])
        landuse_lut <- landuse_lut[-1,]
        lc.list<-subset(landuse_lut, select=c(ID, LC))
        
        landuse_area0_freq<-freq(landuse_area0)
        landuse_area0_table<-as.data.frame(na.omit(landuse_area0_freq))
        colnames(landuse_area0_table)<-c("ID", "COUNT")
        landuse_area0<-as.matrix(landuse_area0_table$COUNT)
        
        next_data_luc <- rv$sciendo_db
        landuse_area_table <- as.data.frame(na.omit(next_data_luc))
        landuse_area_table$ID_LC <- NULL
        landuse_area_table$LC <- NULL
        landuse_area <- landuse_area_table
        
        landuse_table<-merge(lc.list, landuse_area0_table, by="ID")
        landuse_table<-cbind(landuse_table, landuse_area)
        landuse_table$LC<-NULL
        colnames(landuse_table)[1] <- "LAND_USE"
        n_periods <- ncol(landuse_table) - 1
        sim_periods <- ncol(landuse_table) - 2
        for (i in 1:n_periods) {
          colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
        }
        for (i in 2:n_periods) {
          change_col_name <- paste0("CHANGE_T", i-1, "_T", i)
          landuse_table[[change_col_name]] <- landuse_table[[paste0("T", i, "_HA")]] - landuse_table[[paste0("T", i-1, "_HA")]]
        }
        rv$landuse_table <- landuse_table
        
        # Initialize results storage
        GDP_scen.list <- list()
        GDP_totals_scen.list <- list()
        
        # Loop through each period and perform the calculations
        for (t in 1:sim_periods) {
          
          # Create the diagonal matrix for the current period
          landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[, t])))
          
          # MODEL FINAL DEMAND for each period
          land.distribution.scen <- land.distribution.prop %*% landuse_area_diag
          land.requirement.scen <- rowSums(land.distribution.scen)
          
          fin_dem.rtot <- rowSums(fin_dem[, t, drop = FALSE])
          int_con.rtot <- rowSums(int_con[, t, drop = FALSE])
          
          demand <- fin_dem.rtot + int_con.rtot
          
          land.requirement.coeff <- land.requirement.db$LRC
          land.productivity.coeff <- land.requirement.db$LPC
          
          fin_dem.scen <- land.requirement.scen / land.productivity.coeff
          fin_dem.scen[is.infinite(fin_dem.scen)] <- 0
          fin_dem.scen[is.na(fin_dem.scen)] <- 0
          
          # CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
          fin.output.scen <- Leontief %*% fin_dem.scen
          fin.output.scen <- round(fin.output.scen, digits = 1)
          fin.output.scen <- as.data.frame(fin.output.scen)
          fin.output.scen <- sapply(fin.output.scen, as.numeric)
          if (is.list(fin.output.scen)) {
            stop("fin.output.scen is still a list. Ensure that it's numeric.")
          }
          colnames(fin.output.scen)[1] <- "OUTPUT_Scen"
          
          GDP.prop.from.output <- GDP.val / demand
          GDP.prop.from.output[is.na(GDP.prop.from.output)] <- 0
          
          GDP.scen <- GDP.prop.from.output * as.numeric(fin.output.scen)
          GDP.scen[sapply(GDP.scen, is.infinite)] <- NA
          GDP.scen[is.na(GDP.scen)] <- 0
          GDP.scen <- round(GDP.scen, digits = 1)
          GDP.scen <- as.data.frame(GDP.scen)
          colnames(GDP.scen)[1] <- "GDP_scen"
          
          # Combine GDP and GDP_scen columns
          GDP_summary <- cbind(GDP, GDP.scen)
          GDP_summary$P_OUTPUT <- NULL
          GDP_summary$P_GDP <- NULL
          
          # CALCULATE TOTAL GDP
          GDP_tot_scen <- sum(GDP_summary$GDP_scen, na.rm = TRUE)
          
          # Store the GDP summary, overall results, and the graphs for this period
          GDP_scen.list[[t]] <- GDP.scen
          GDP_totals_scen.list[[t]] <- GDP_tot_scen
        }
        
        #Tabel perbandingan GDP setiap sektor untuk semua timeseries
        GDP_scen <- data.frame(
          Sector = GDP$SECTOR, 
          Category = GDP$CATEGORY,
          GDP_bau = GDP$GDP,                 
          purrr::map_dfc(GDP_scen.list, ~.x)
        )
        period_names <- paste0("Period_", seq_len(length(GDP_scen.list)))
        colnames(GDP_scen)[4:(3+length(period_names))] <- period_names
        GDP_scen_df <- as.data.frame(GDP_scen)
        rownames(GDP_scen_df)<-NULL
        rv$GDP_scen_df <- GDP_scen_df
        
        # Barchart total GDP untuk semua time series(t)
        GDP_totals_df <- data.frame(
          Period = paste("Period", seq_len(length(GDP_totals_scen.list))),
          GDP_totals = unlist(GDP_totals_scen.list)
        )
        GDP_totals_df <- rbind(data.frame(Period = "BAU", GDP_totals = GDP_tot), GDP_totals_df)
        GDP_totals_graph <- ggplot(data = GDP_totals_df, aes(x = Period, y = GDP_totals)) +
          geom_bar(stat = "identity", position = "dodge", colour = "black") +
          labs(x = "Period", y = "GDP Total") +
          ggtitle(paste("BAU vs Scenario GDP Total")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        rv$GDP_totals_graph <- GDP_totals_graph
        
        # Remove unwanted columns from GDP_scen_df
        db_GDP <- GDP_scen_df[, !(colnames(GDP_scen_df) %in% c("Sector", "Category"))]
        rownames(db_GDP) <- NULL
        
        # Create totals for Output, Income, and Labour
        db_output_total <- create_totals_df(GDP, db_GDP, 1, "Period BAU")
        output_total_graph <- create_bar_plot(db_output_total, "BAU vs Scenario Output Total")
        
        db_income_total <- create_totals_df(GDP, db_GDP, Inc.multiplier$Inc.multiplier, "Period BAU")
        income_total_graph <- create_bar_plot(db_income_total, "BAU vs Scenario Income Total")
        
        db_labour_total <- create_totals_df(GDP, db_GDP, Lab.multiplier$Lab.multiplier, "Period BAU")
        labour_total_graph <- create_bar_plot(db_labour_total, "BAU vs Scenario Labour Total")
        
        rv$output_total_graph <- output_total_graph
        rv$income_total_graph <- income_total_graph
        rv$labour_total_graph <- labour_total_graph
        rv$land_req_path <- rename_uploaded_file(input$land_req_file)
        rv$sciendo_db_path <- rename_uploaded_file(input$sciendo_db)
        
        # Return Results
        list(
          landuse_table = landuse_table,
          GDP_totals_graph = GDP_totals_graph,
          GDP_scen_df = GDP_scen_df,
          output_total_graph = output_total_graph,
          income_total_graph = income_total_graph,
          labour_total_graph = labour_total_graph,
          land_req_path = rv$land_req_path,
          sciendo_db_path = rv$sciendo_db_path
        )
        
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("viewReport")
        
      }, error = function(e) {
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  #### Report Generation ####
  report_content <- reactive({
    params <- list(
      session_log = format_session_info_table(),
      landuse_table = rv$landuse_table,
      GDP_totals_graph = rv$GDP_totals_graph,
      GDP_scen_df = rv$GDP_scen_df,
      output_total_graph = rv$output_total_graph,
      income_total_graph = rv$income_total_graph,
      labour_total_graph = rv$labour_total_graph,
      land_req_path = rv$land_req_path,
      sciendo_db_path = rv$sciendo_db_path,
      output_dir = rv$wd
    )
    output_file <- paste0("ta_regional2_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    output_dir <- rv$wd
    render(
      "../report_template/ta-regional2_report.Rmd",
      output_file = output_file,
      output_dir = output_dir,
      params = params,
      envir = new.env(parent = globalenv())
    )
  })
  
  observeEvent(input$viewReport, {
    showNotification("Opening report...", type = "message")
    file.show(report_content())
  })
}
