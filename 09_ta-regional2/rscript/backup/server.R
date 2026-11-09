server <- function(input, output, session) {
  
  #### Initialize all required reactive values ####
  #' This section initializes reactive values that will store important data 
  #' such as land use tables, GDP analysis results, file paths, and various graphs.
  
  rv <- reactiveValues(
    landuse_table = NULL,
    land_req = NULL,
    projected_land_use = NULL,
    sciendo_db = NULL,
    GDP_scen = NULL,
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
        landuse_area_table$LC <- NULL
        landuse_area <- landuse_area_table
        
        # Merge land use data for all periods
        landuse_bau_table <- merge(lc.list, landuse_area0_table, by = "ID")
        # landuse_table <- left_join(landuse_bau_table, landuse_area, by = "ID")
        # landuse_table <- cbind(landuse_bau_table, landuse_area)
        if ("ID" %in% colnames(landuse_bau_table) && "ID" %in% colnames(landuse_area)) {
          # Run this if both tables have an "ID" column
          landuse_table <- left_join(landuse_bau_table, landuse_area, by = "ID")
        } else {
          # Run this if one or both tables do not have an "ID" column
          landuse_table <- cbind(landuse_bau_table, landuse_area)
        }
        landuse_table$LC <- NULL
        colnames(landuse_table)[1] <- "LAND_USE"
        
        # Generate new columns for each time period and calculate land use change
        n_periods <- ncol(landuse_table) - 1
        for (i in 1:n_periods) {
          colnames(landuse_table)[i + 1] <- paste0("T", i, "_HA")
        }
        # Initialize storage for summary outputs per period if needed
        GDP_summary_list <- list()
        GDP_overall_list <- list()
        
        for (i in 2:ncol(landuse_area)) {
          # Create a diagonal matrix for the current period's land use area
          landuse_area_diag <- diag(as.numeric(as.matrix(landuse_area[i])))
          
          # MODEL FINAL DEMAND
          land.distribution.scen <- land.distribution.prop %*% landuse_area_diag
          land.requirement.scen <- rowSums(land.distribution.scen)
          fin_dem.rtot <- rowSums(fin_dem)
          int_con.rtot <- rowSums(int_con)
          demand <- fin_dem.rtot + int_con.rtot
          land.requirement.coeff <- land.requirement.db$LRC
          land.productivity.coeff <- land.requirement.db$LPC
          # fin_dem.scen <- land.requirement.scen / land.productivity.coeff
          fin_dem.scen <- land.requirement.scen * land.productivity.coeff
          fin_dem.scen[is.infinite(fin_dem.scen)] <- 0
          fin_dem.scen[is.na(fin_dem.scen)] <- 0
          
          # CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
          fin.output.scen <- Leontief %*% fin_dem.scen
          fin.output.scen <- round(fin.output.scen, digits = 1)
          colnames(fin.output.scen)[1] <- "OUTPUT_Scen"
          GDP.prop.from.output <- GDP.val / demand
          GDP.prop.from.output[is.na(GDP.prop.from.output)] <- 0
          GDP.scen <- GDP.prop.from.output * fin.output.scen
          GDP.scen <- round(GDP.scen, digits = 1)
          GDP.scen[is.na(GDP.scen)] <- 0
          colnames(GDP.scen)[1] <- "GDP_scen"
          GDP.diff <- GDP.scen - GDP$GDP
          GDP.diff <- round(GDP.diff, digits = 1)
          colnames(GDP.diff)[1] <- "GDP_diff"
          GDP.rate <- GDP.diff / GDP.val
          GDP.rate[is.na(GDP.rate)] <- 0
          GDP.rate <- round(GDP.rate, digits = 2)
          colnames(GDP.rate)[1] <- "GDP_rate"
          GDP_summary <- cbind(GDP, GDP.scen, fin.output.scen, GDP.diff, GDP.rate)
          GDP_summary$P_OUTPUT <- NULL
          GDP_summary$P_GDP <- NULL
          
          # Calculate total GDP
          GDP_tot_scen <- as.matrix(GDP_summary$GDP_scen)
          GDP_tot_scen <- colSums(GDP_tot_scen)
          GDP_tot_diff <- GDP_tot_scen - GDP_tot
          GDP_tot_rate <- GDP_tot_diff / GDP_tot
          text1 <- "Total GDP"
          text2 <- "Scenario GDP"
          text3 <- "GDP difference"
          text4 <- "Rate of difference"
          GDP_overall1 <- rbind(text1, text2, text3, text4)
          GDP_overall2 <- rbind(GDP_tot, GDP_tot_scen, GDP_tot_diff, GDP_tot_rate)
          GDP_overall <- cbind(GDP_overall1, GDP_overall2)
          
          # Store results for each period if needed
          GDP_summary_list[[i]] <- GDP_summary
          GDP_overall_list[[i]] <- GDP_overall
        }
        
        # Extract GDP.scen column from each GDP_summary in GDP_summary_list
        GDP_scen_list <- lapply(GDP_summary_list, function(x) x$GDP_scen)
        GDP_scen_df <- data.frame(do.call(cbind, GDP_scen_list))
        colnames(GDP_scen_df) <- paste0("GDP_scen_Period_", seq_along(GDP_scen_df))
        
        #' Combine results into data frames for plotting and comparison
        GDP_scen <- data.frame(
          Sector = GDP$SECTOR, 
          Category = GDP$CATEGORY,
          GDP_bau = GDP$GDP,                 
          GDP_scen_df
        )
        
        rv$GDP_scen <- GDP_scen
        
        #' Create bar charts to visualize total GDP
        GDP_totals_df <- data.frame(
          Period = paste("Period", seq_len(length(GDP_scen_df))),
          GDP_totals = colSums(GDP_scen_df)
        )
        GDP_totals_df <- rbind(data.frame(Period = "Base Year", GDP_totals = GDP_tot), GDP_totals_df)
        GDP_totals_graph <- ggplot(data = GDP_totals_df, aes(x = Period, y = GDP_totals)) +
          geom_bar(stat = "identity", position = "dodge", colour = "black") +
          labs(x = "Period", y = "GDP Total") +
          ggtitle(paste("Base Year vs Scenario")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
        rv$GDP_totals_graph <- GDP_totals_graph
        
        #' Remove unwanted columns from GDP_scen_df
        db_GDP <- GDP_scen[, !(colnames(GDP_scen) %in% c("Sector", "Category"))]
        rownames(db_GDP) <- NULL
        
        #' Create bar charts to visualize total Output, Income, and Labour
        db_output_total <- create_totals_df(GDP, db_GDP, 1, "Base Year")
        output_total_graph <- create_bar_plot(db_output_total, "Base Year vs Scenario")
        
        db_income_total <- create_totals_df(GDP, db_GDP, Inc.multiplier$Inc.multiplier, "Base Year")
        income_total_graph <- create_bar_plot(db_income_total, "Base Year vs Scenario")
        
        db_labour_total <- create_totals_df(GDP, db_GDP, Lab.multiplier$Lab.multiplier, "Base Year")
        labour_total_graph <- create_bar_plot(db_labour_total, "Base Year vs Scenario")
        
        #' Remove unwanted columns from GDP_scen_df
        db_GDP <- GDP_scen_df[, !(colnames(GDP_scen_df) %in% c("Sector", "Category"))]
        rownames(db_GDP) <- NULL
        
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
          GDP_scen = GDP_scen,
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
    shinyalert(
      title = "Confirmation",
      text =  "Do you want to return to main menu?",
      showCancelButton = TRUE,
      size = "xs",
      type = "warning",
      inputId = "alert"
    )
  })
  
  observeEvent(input$alert, {
    if(input$alert) {
      js$closeWindow()
      message("Return to main menu!")  
      # shinyjs::delay(1000, stopApp())
    }
  })
}