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
  volumes <- c(
    getVolumes()()
  )
  
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
      HTML(readLines(html_content))
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
    rv$map1_file <- input$map1_file$datapath
    rv$map1_rast <- raster(rv$map1_file)
  })
  
  #' Load and process the second map file, converting it to a raster format
  observeEvent(input$map2_file, {
    rv$map2_file <- input$map2_file$datapath
    rv$map2_rast <- raster(rv$map2_file)
  })
  
  #' Load the carbon data file and process it into a table format for analysis
  observeEvent(input$carbon_file, {
    rv$carbon_file <- input$carbon_file$datapath
    df_c <- read.csv(rv$carbon_file)
    rv$quesc_tbl <- df_c
    rv$tbl_carbon <- df_c %>% dplyr::select(ID = 1, Carbon = 3)
  })
  
  #' Load the NPV data file and extract relevant columns for analysis
  observeEvent(input$npv_file, {
    rv$npv_file <- input$npv_file$datapath
    df_n <- read.csv(rv$npv_file)
    rv$tbl_npv <- df_n %>% dplyr::select(ID_LC = 1, NPV = 3)
  })
  
  # Calculate Period --------------------------------------------------------
  #' Calculate the time period based on user input for start and end years
  observeEvent(c(input$year1, input$year2), {
    rv$period <- as.numeric(input$year2) - as.numeric(input$year1)
  })
  
  # Data Processing ---------------------------------------------------------
  #' Main processing logic to calculate NPV, opportunity costs, and carbon emissions
  observeEvent(input$process, {
    
    #' Validate input files to ensure all required data is uploaded before proceeding
    if (is.null(rv$map1_rast) || is.null(rv$map2_rast) || is.null(rv$tbl_carbon) || is.null(rv$tbl_npv)) {
      showNotification("Please upload all required files", type = "error")
      return()
    }
    
    #' Progress bar to show users that the analysis is running
    withProgress(message = "Running TA Profit Analysis", value = 0, {
      tryCatch({
        #' Prepare NPV lookup table by combining carbon and NPV data, and calculate total area
        npv_result <- prepare_npv_lookup(rv$tbl_npv, rv$quesc_tbl)
        rv$quesc_tbl <- npv_result$quesc_tbl
        tot_area <- npv_result$tot_area
        
        #' Build the opportunity cost table based on the land use change period and total area
        opcost_result <- build_opcost_table(rv$quesc_tbl, rv$period, tot_area)
        rv$opcost_table <- opcost_result$opcost_all
        rv$opcost_table$order <- c(1:nrow(rv$opcost_table))
        
        #' Perform carbon accounting and calculate carbon emissions based on land use change
        carbon_result <- carbon_accounting(rv$map1_rast, rv$map2_rast, rv$tbl_npv, rv$tbl_carbon, input$raster_nodata)
        rv$map_carbon1 <- carbon_result$map_carbon1
        rv$map_carbon2 <- carbon_result$map_carbon2
        rv$emission_map <- carbon_result$emission_map
        
        #' Perform NPV accounting to calculate changes in NPV between the two time periods
        npv_result <- npv_accounting(rv$map1_rast, rv$map2_rast, rv$tbl_npv)
        rv$map_npv1 <- npv_result$map_npv1
        rv$map_npv2 <- npv_result$map_npv2
        rv$npv_chg_map <- npv_result$npv_chg_map
        
        #' Calculate the opportunity cost map by combining NPV changes with carbon emissions
        rv$opcost_map <- calculate_opcost_map(rv$npv_chg_map, rv$emission_map)
        
        #' Generate output maps and save them in the selected directory
        generate_output_maps(rv$map_carbon1, rv$map_carbon2, rv$emission_map, rv$opcost_map, rv$wd)
        
        #' Generate the opportunity cost curve for visualization
        rv$opcost_curve <- generate_opportunity_cost_curve(rv$opcost_table)
        
        #' Indicate that the process has completed successfully
        setProgress(1, message = "Processing Complete")
        showNotification("All outputs have been generated", type = "message")
        
        #' Update the status messages in the UI
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("viewReport")
        
      }, error = function(e) {
        #' Handle errors and display an error message in the UI
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  # Generate Report ---------------------------------------------------------
  #' Create a reactive function to generate the final report after the analysis is complete
  report_content <- reactive({
    params <- list(
      map_carbon1 = rv$map_carbon1,
      map_carbon2 = rv$map_carbon2,
      emission_map = rv$emission_map,
      opcost_map = rv$opcost_map,
      opcost_table = rv$opcost_table,
      opcost_curve = rv$opcost_curve,
      npv1_map = rv$map_npv1,
      npv2_map = rv$map_npv1,
      delta_npv = rv$npv_chg_map
    )
    
    output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    output_dir <- rv$wd
    rv$report_file <- paste(output_dir, output_file, sep = "/")
    
    render(
      "../report_template/ta-profit_report.Rmd",
      output_file = output_file,
      output_dir = output_dir,
      params = params,
      envir = new.env(parent = globalenv())
    )
  })
  
  #' Event handler to open the generated report
  observeEvent(input$viewReport, {
    showNotification("Opening report...", type = "message")
    file.show(report_content())
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
