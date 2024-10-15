server <- function(input, output, session) {
  
  #### Initialize all required reactive values ####
  #' This section initializes reactive values that will store important data 
  #' such as land use tables, GDP analysis results, file paths, and various graphs.
  
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
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  #### Set Working Directory ####
  #' This section allows the user to choose a working directory from their system.
  #' The chosen directory will be used for storing the analysis results.
  
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
  
  #### Display User Guide ####
  #' This section renders the user guide from a markdown file located in the helpfile directory.
  #' If the help file is missing, it displays an error message.
  
  output$user_guide <- renderUI({
    guide_paths <- c(
      "09_ta-regional2/helpfile/ta-reg2_quick_user_guide.Rmd",
      "../helpfile/ta-reg2_quick_user_guide.Rmd"
    )
    
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    
    HTML("<p>User guide file not found.</p>")
  })
  
  #### Load and process data ####
  #' Load land use requirement and database files provided by the user.
  #' The loaded data will be used in the GDP and labor analysis later in the script.
  
  observeEvent(input$land_req_file, {
    land_req <- input$land_req_file$datapath
    rv$land_req <- land_req
  })
  
  observeEvent(input$sciendo_db, {
    sciendo_db <- input$sciendo_db$datapath
    rv$sciendo_db <- read.csv(sciendo_db, header = TRUE, stringsAsFactors = FALSE)
  })
  
  #### Perform GDP and Labour Analysis ####
  #' This section runs the Trade-Off Analysis for Regional 2, using input data 
  #' to project land use changes, calculate GDP, and generate graphs comparing
  #' the results under different scenarios.
  
  observeEvent(input$processTAReg2, {
    
    # Initialize an empty list to track missing inputs
    missing_inputs <- c()
    
    # Validate input files to ensure all required data is uploaded before proceeding
    if (is.null(rv$land_req)) {
      missing_inputs <- c(missing_inputs, "Land Requirement Database")
    }
    if (is.null(rv$sciendo_db)) {
      missing_inputs <- c(missing_inputs, "SCIENDO Database")
    }
    if (is.null(rv$wd) || length(rv$wd) == 0 || is.na(rv$wd) || rv$wd == "") {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    
    # If there are missing inputs, show a notification and stop
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Please upload the following inputs:", paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    withProgress(message = 'Running TA Regional 2 Analysis', value = 0, {
      tryCatch({
        
        land_req_path <- rename_uploaded_file(input$land_req_file)
        sciendo_db_path <- rename_uploaded_file(input$sciendo_db)
        output_dir <- rv$wd
        
        load(land_req_path)
        
        # Capture the start time at the beginning of the process
        start_time <- Sys.time()
        
        # Load land use requirement data
        land.requirement.db <- land.requirement_table
        # names(landuse_lut) <- as.character(landuse_lut[1,])
        # landuse_lut <- landuse_lut[-1,]
        lc.list <- subset(landuse_lut, select = c(ID, LC))

        # Prepare data for land use change analysis
        next_data_luc <- read.csv(sciendo_db_path)
        landuse_area_table <- as.data.frame(na.omit(next_data_luc))
        landuse_area_table$ID_LC <- NULL
        landuse_area_table$LC <- NULL
        landuse_area <- landuse_area_table
        
        # Merge land use data for all periods
        landuse_table <- merge(lc.list, landuse_area0_table, by = "ID")
        landuse_table <- cbind(landuse_table, landuse_area)
        landuse_table$LC <- NULL
        colnames(landuse_table)[1] <- "LAND_USE"
        
        # Generate new columns for each time period and calculate land use change
        n_periods <- ncol(landuse_table) - 1
        for (i in 1:n_periods) {
          colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
        }
        for (i in 2:n_periods) {
          change_col_name <- paste0("CHANGE_T", i - 1, "_T", i)
          landuse_table[[change_col_name]] <- landuse_table[[paste0("T", i, "_HA")]] - landuse_table[[paste0("T", i - 1, "_HA")]]
        }
        rv$landuse_table <- landuse_table
        
        # Perform GDP calculations for each scenario period
        # Initialize lists to store results for each time period
        GDP_scen.list <- list()
        GDP_totals_scen.list <- list()
        
        # Loop through each simulation period to calculate GDP
        for (t in 1:(n_periods - 1)) {
          
          #' Create a diagonal matrix for the current period based on land use area
          #' 
          #' This matrix represents the land use distribution for the given simulation 
          #' period, which is necessary to calculate the land requirement for each scenario.
          landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[, t])))
          
          #' Model final demand based on land use changes
          #' 
          #' The land distribution proportion is multiplied by the diagonal matrix to 
          #' determine the land requirement for each scenario.
          land.distribution.scen <- land.distribution.prop %*% landuse_area_diag
          land.requirement.scen <- rowSums(land.distribution.scen)
          
          #' Calculate the total demand for the current period

          #' Final demand (fin_dem.rtot) and intermediate consumption (int_con.rtot) 
          #' are summed to determine the total demand for the given simulation period.
          fin_dem.rtot <- rowSums(fin_dem[, t, drop = FALSE])
          int_con.rtot <- rowSums(int_con[, t, drop = FALSE])
          demand <- fin_dem.rtot + int_con.rtot
          
          #' Adjust land requirement by productivity coefficient

          #' The land requirement is adjusted based on the land productivity coefficient 
          #' from the database. Infinite and missing values are handled accordingly.
          land.requirement.coeff <- land.requirement.db$LRC
          land.productivity.coeff <- land.requirement.db$LPC
          fin_dem.scen <- land.requirement.scen / land.productivity.coeff
          fin_dem.scen[is.infinite(fin_dem.scen)] <- 0
          fin_dem.scen[is.na(fin_dem.scen)] <- 0
          
          #' Calculate the final output for the scenario

          #' The Leontief matrix is used to calculate the final output for each scenario 
          #' of land use change. This matrix is rounded and converted to a numeric format.
          fin.output.scen <- Leontief %*% fin_dem.scen
          fin.output.scen <- round(fin.output.scen, digits = 1)
          fin.output.scen <- as.data.frame(fin.output.scen)
          fin.output.scen <- sapply(fin.output.scen, as.numeric)
          
          #' Ensure the output is numeric
          if (is.list(fin.output.scen)) {
            stop("fin.output.scen is still a list. Ensure that it's numeric.")
          }
          colnames(fin.output.scen)[1] <- "OUTPUT_Scen"
          
          #' Calculate GDP from the output

          #' The GDP for the scenario is calculated by multiplying the output by the 
          #' GDP proportion from the output. Missing and infinite values are set to zero.
          GDP.prop.from.output <- GDP.val / demand
          GDP.prop.from.output[is.na(GDP.prop.from.output)] <- 0
          GDP.scen <- GDP.prop.from.output * as.numeric(fin.output.scen)
          GDP.scen[sapply(GDP.scen, is.infinite)] <- NA
          GDP.scen[is.na(GDP.scen)] <- 0
          GDP.scen <- round(GDP.scen, digits = 1)
          GDP.scen <- as.data.frame(GDP.scen)
          colnames(GDP.scen)[1] <- "GDP_scen"
          
          #' Combine base and scenario GDP

          #' The base GDP (GDP) and scenario GDP (GDP_scen) are combined into a summary 
          #' table for comparison.
          GDP <- subset(GDP, SECTOR!="Total")
          GDP_summary <- cbind(GDP, GDP.scen)
          GDP_summary$P_OUTPUT <- NULL
          GDP_summary$P_GDP <- NULL
          
          #' Calculate total GDP for the current period

          #' The total GDP for the scenario is calculated by summing the values from 
          #' the GDP summary table, ignoring missing values.
          GDP_tot_scen <- sum(GDP_summary$GDP_scen, na.rm = TRUE)
          
          #' Store the GDP results
 
          #' The GDP for each simulation period is stored in reactive lists for 
          #' further analysis and plotting.
          GDP_scen.list[[t]] <- GDP.scen
          GDP_totals_scen.list[[t]] <- GDP_tot_scen
        }
        
        #' Combine results into data frames for plotting and comparison
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
        
        #' Create bar charts to visualize total GDP
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
        
        #' Remove unwanted columns from GDP_scen_df
        db_GDP <- GDP_scen_df[, !(colnames(GDP_scen_df) %in% c("Sector", "Category"))]
        rownames(db_GDP) <- NULL
        
        #' Create bar charts to visualize total Output, Income, and Labour
        db_output_total <- create_totals_df(GDP, db_GDP, 1, "Period BAU")
        output_total_graph <- create_bar_plot(db_output_total, "BAU vs Scenario Output Total")
        
        db_income_total <- create_totals_df(GDP, db_GDP, Inc.multiplier$Inc.multiplier, "Period BAU")
        income_total_graph <- create_bar_plot(db_income_total, "BAU vs Scenario Income Total")
        
        db_labour_total <- create_totals_df(GDP, db_GDP, Lab.multiplier$Lab.multiplier, "Period BAU")
        labour_total_graph <- create_bar_plot(db_labour_total, "BAU vs Scenario Labour Total")
        
        #' Store file paths for reference in the final report
        rv$output_total_graph <- output_total_graph
        rv$income_total_graph <- income_total_graph
        rv$labour_total_graph <- labour_total_graph
        
        # Capture the end time at the end of the process
        end_time <- Sys.time()
        
        #### Report Generation ####
        params <- list(
          session_log = format_session_info_table(),
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          landuse_table = landuse_table,
          GDP_totals_graph = GDP_totals_graph,
          GDP_scen_df = GDP_scen_df,
          output_total_graph = output_total_graph,
          income_total_graph = income_total_graph,
          labour_total_graph = labour_total_graph,
          land_req_path = land_req_path,
          sciendo_db_path = sciendo_db_path,
          output_dir = output_dir
        )
        
        output_file <- paste0("ta_regional2_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        rv$report_file <- file.path(output_dir, output_file)
        
        # Corrected render function
        rmarkdown::render(
          "../report_template/ta-regional2_report.Rmd",
          output_file = output_file,
          output_dir = output_dir,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        # Notify user of successful analysis
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("open_report")
        shinyjs::show("open_output_folder")
        
      }, error = function(e) {
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  # Open Output Folder button observer
  observeEvent(input$open_output_folder, {
    if (!is.null(rv$wd) && dir.exists(rv$wd)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$wd)
      } else {
        system2("open", args = rv$wd)
      }
    } else {
      showNotification("Output directory not found", type = "error")
    }
  })
  
  # Open Report button observer
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$report_file)
      } else {
        system2("open", args = rv$report_file)
      }
    } else {
      showNotification("Report file not found", type = "error")
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # shinyjs::delay(1000, stopApp())
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # shinyjs::delay(1000, stopApp())
  })
}