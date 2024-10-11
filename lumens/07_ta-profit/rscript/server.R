#' Shiny Server Function for Trade-Off Analysis (Profit)
#'
#' This server function handles the backend processing for the Trade-Off Analysis Shiny application.
#' It manages file uploads, reactive values, data processing, and report generation for land-use change analysis.
#'

server <- function(input, output, session) {
  
  #### Initialize reactive values ####
  #' Reactive values are used to store data that is updated based on user inputs. 
  #' These values will hold file paths, raster data, calculated results, and maps.
  rv <- reactiveValues(
    wd = "",  # Working directory for saving outputs
    report_file = NULL,  # Path to the generated report
    map1_file = NULL,  # First map file input
    map2_file = NULL,  # Second map file input
    year1 = NULL,  # Start year of the analysis
    year2 = NULL,  # End year of the analysis
    carbon_file = NULL,  # File containing carbon data
    npv_file = NULL,  # File containing NPV data
    map1_rast = NULL,  # Raster object for the first map
    map2_rast = NULL,  # Raster object for the second map
    tbl_npv = NULL,  # NPV data table
    quesc_tbl = NULL,  # Table combining carbon and NPV data
    dt_quesc_npv = NULL,
    all_tbl_carbon = NULL,  # All carbon data
    tbl_carbon = NULL,  # Processed carbon data table
    period = NULL,  # Time period for the analysis
    tot_area = NULL,  # Total area for the land use change
    opcost_table = NULL,  # Opportunity cost table
    npv_chg_map = NULL,  # NPV change map
    opcost_map = NULL,  # Opportunity cost map
    emission_map = NULL,  # Emission map
    raster_nodata = NULL,  # Value for raster cells with no data
    map_npv1 = NULL,  # Raster for NPV at year1
    map_npv2 = NULL,  # Raster for NPV at year2
    opcost_curve = NULL  # Opportunity cost curve
  )
  
  #' Available volumes for file selection, used to set the working directory for saving outputs
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  # Set working directory ---------------------------------------------------
  #' Allow users to choose the output directory where all generated files and reports will be saved.
  shinyDirChoose(
    input, 
    'wd',
    roots = volumes,
    session = session
  )
  
  #' Display the selected output directory path
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  #' Render the user guide to help users understand how to use the application
  output$user_guide <- renderUI({
    guide_path <- "../helpfile/help.md"
    if (file.exists(guide_path)) {
      html_content <- rmarkdown::render(guide_path, output_format = "html_fragment", quiet = TRUE,
                                        output_options = list(metadata = list(title = "Trade-Off Analysis (Profit)")))
      HTML(paste(readLines(html_content), collapse = "\n"))  # Added collapse to ensure proper HTML rendering
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
  # Helper Functions --------------------------------------------------------
  #' Check if a string is numeric, used to validate inputs before processing
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  # File Inputs -------------------------------------------------------------
  #' Load and process the first map file, converting it to a raster format
  observeEvent(input$map1_file, {
    map1_file <- input$map1_file
    rv$map1_file <- map1_file
    rv$map1_rast <- rast(rv$map1_file$datapath)
  })
  
  #' Load and process the second map file, converting it to a raster format
  observeEvent(input$map2_file, {
    map2_file <- input$map2_file
    rv$map2_file <- map2_file
    rv$map2_rast <- rast(rv$map2_file$datapath)
  })
  
  #' Load the carbon data file and process it into a table format for analysis
  observeEvent(input$carbon_file, {
    rv$carbon_file <- input$carbon_file
    rv$carbon_file_path <- input$carbon_file$datapath
    df_c <- read.csv(rv$carbon_file_path)
    rv$quesc_tbl <- df_c
    rv$tbl_carbon <- df_c %>% dplyr::select(ID = 1, Carbon = 3)
  })
  
  #' Load the NPV data file and extract relevant columns for analysis
  observeEvent(input$npv_file, {
    rv$npv_file <- input$npv_file
    rv$npv_file_path <- input$npv_file$datapath
    df_n <- read.csv(rv$npv_file_path)
    rv$tbl_npv <- df_n %>% dplyr::select(ID_LC = 1, NPV = 3)
  })
  
  # Calculate Period --------------------------------------------------------
  #' Calculate the time period based on user input for start and end years
  observeEvent(c(input$year1, input$year2), {
    if (is_numeric_str(input$year1)) {
      rv$year1 <- as.numeric(input$year1)
    }
    if (is_numeric_str(input$year2)) {
      rv$year2 <- as.numeric(input$year2)
    }
    if (!is.null(rv$year1) && !is.null(rv$year2)) {
      rv$period <- rv$year2 - rv$year1
    }
  })
  
  #' Update the raster_nodata value based on user input
  observeEvent(input$raster_nodata, {
    if (is_numeric_str(input$raster_nodata)) {
      rv$raster_nodata <- as.numeric(input$raster_nodata)
    }
  })
  
  # Data Processing ---------------------------------------------------------
  #' Main processing logic to calculate NPV, opportunity costs, and carbon emissions
  observeEvent(input$process, {
    
    # Initialize an empty list to track missing inputs
    missing_inputs <- c()
    
    # Validate input files to ensure all required data is uploaded before proceeding
    if (is.null(rv$map1_file)) {
      missing_inputs <- c(missing_inputs, "First Map File")
    }
    if (is.null(rv$year1)) {
      missing_inputs <- c(missing_inputs, "First Year Value")
    }
    if (is.null(rv$map2_file)) {
      missing_inputs <- c(missing_inputs, "Second Map File")
    }
    if (is.null(rv$year2)) {
      missing_inputs <- c(missing_inputs, "Second Year Value")
    }
    if (is.null(rv$npv_file)) {
      missing_inputs <- c(missing_inputs, "NPV Data File")
    }
    if (is.null(rv$carbon_file)) {
      missing_inputs <- c(missing_inputs, "Carbon Data File")
    }
    if (is.null(rv$raster_nodata)) {
      missing_inputs <- c(missing_inputs, "No Data Value")
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
    
    #' Progress bar to show users that the analysis is running
    withProgress(message = "Running TA Profit Analysis", value = 0, {
      tryCatch({
        
        # Increment progress
        incProgress(0.1, detail = "Renaming uploaded files")
        
        # Capture the start time at the beginning of the process
        start_time <- Sys.time()
        
        map1_file_path <- rename_uploaded_file(rv$map1_file)
        map2_file_path <- rename_uploaded_file(rv$map2_file)
        npv_file_path <- rename_uploaded_file(rv$npv_file)
        carbon_file_path <- rename_uploaded_file(rv$carbon_file)
        year1 <- rv$year1
        year2 <- rv$year2
        raster_nodata <- rv$raster_nodata
        output_dir <- rv$wd
        
        #' Prepare NPV lookup table by combining carbon and NPV data, and calculate total area
        incProgress(0.2, detail = "Preparing NPV lookup table")
        npv_result <- prepare_npv_lookup(rv$tbl_npv, rv$quesc_tbl)
        rv$dt_quesc_npv <- npv_result$dt_quesc_npv
        rv$tot_area <- npv_result$tot_area
        
        #' Build the opportunity cost table based on the land use change period and total area
        incProgress(0.3, detail = "Building opportunity cost table")
        opcost_result <- build_opcost_table(rv$dt_quesc_npv, rv$period, rv$tot_area)
        rv$opcost_table <- opcost_result$opcost_all
        rv$opcost_table$order <- c(1:nrow(rv$opcost_table))
        
        #' Perform carbon accounting and calculate carbon emissions based on land use change
        incProgress(0.4, detail = "Performing carbon accounting")
        map1_rast <- terra::rast(map1_file_path)
        map2_rast <- terra::rast(map2_file_path)
        carbon_result <- carbon_accounting(map1_rast, map2_rast, rv$tbl_npv, rv$tbl_carbon, raster_nodata)
        rv$map_carbon1 <- carbon_result$map_carbon1
        rv$map_carbon2 <- carbon_result$map_carbon2
        rv$emission_map <- carbon_result$emission_map
        
        #' Perform NPV accounting to calculate changes in NPV between the two time periods
        incProgress(0.5, detail = "Performing NPV accounting")
        npv_change <- npv_accounting(map1_rast, map2_rast, rv$tbl_npv)
        rv$map_npv1 <- npv_change$map_npv1
        rv$map_npv2 <- npv_change$map_npv2
        rv$npv_chg_map <- npv_change$npv_chg_map
        
        #' Calculate the opportunity cost map by combining NPV changes with carbon emissions
        incProgress(0.6, detail = "Calculating opportunity cost map")
        rv$opcost_map <- calculate_opcost_map(rv$npv_chg_map, rv$emission_map)
        
        #' Generate output maps and save them in the selected directory
        incProgress(0.7, detail = "Generating output maps")
        generate_output_maps(rv$map_carbon1, rv$map_carbon2, rv$emission_map, rv$opcost_map, output_dir)
        
        #' Generate the opportunity cost curve for visualization
        incProgress(0.8, detail = "Generating opportunity cost curve")
        rv$opcost_curve <- generate_opportunity_cost_curve(rv$opcost_table)
        
        # Capture the end time at the end of the process
        end_time <- Sys.time()
        
        #' Compile report parameters
        report_params <- list(
          session_log = format_session_info_table(),
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          map_carbon1 = rv$map_carbon1,
          map_carbon2 = rv$map_carbon2,
          emission_map = rv$emission_map,
          opcost_map = rv$opcost_map,
          opcost_table = rv$opcost_table,
          opcost_curve = rv$opcost_curve,
          npv1_map = rv$map_npv1,
          npv2_map = rv$map_npv2,  # Fixed: Changed from map_npv1 to map_npv2
          delta_npv = rv$npv_chg_map,
          map1_file_path = map1_file_path,
          map2_file_path = map2_file_path,
          npv_file_path = npv_file_path,
          carbon_file_path = carbon_file_path,
          year1 = year1,
          year2 = year2,
          raster_nodata = raster_nodata,
          output_dir = output_dir
        )
        
        #' Render the report
        incProgress(0.9, detail = "Rendering report")
        output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        rv$report_file <- file.path(output_dir, output_file)
        
        rmarkdown::render(
          input = "../report_template/ta-profit_report.Rmd",
          output_file = output_file,
          output_dir = output_dir,
          params = report_params,
          envir = new.env(parent = globalenv())
        )
        
        #' Indicate that the process has completed successfully
        incProgress(1, detail = "Finalizing")
        showNotification("All outputs have been generated", type = "message")
        
        #' Update the status messages in the UI
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("open_report")
        shinyjs::show("open_output_folder")
        
      }, error = function(e) {
        #' Handle errors and display an error message in the UI
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })  # Closing the observeEvent for input$process
  
  # Open Report button observer (moved outside the process observer)
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
  
  # Open Output Folder button observer (assuming you have logic for this)
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
  
  # Handle session end -------------------------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Return to Main Menu button observer -------------------------------------
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # Uncomment the following line if you want to stop the app after a delay
    # shinyjs::delay(1000, stopApp())
  })
  
}  # Closing the server function
