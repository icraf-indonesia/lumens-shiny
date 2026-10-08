source('../../helper.R')

# Define a list of required packages
required_packages <- c(
  "terra", "dplyr", "tidyterra", "ggplot2", "forcats", "stringr",
  "cowplot", "networkD3", "scales", "purrr", "rmarkdown",
  "kableExtra", "htmlTable", "knitr", "magrittr", "tidyr",
  "rlang", "stats", "utils", "methods", "sf", "ggrepel", "readr",
  "viridis", "textclean", "shiny", "shinydashboard", "shinyjs",
  "shinyFiles", "bslib", "shinyalert", "writexl", "mapview", "leaflet"
)


if (file.exists("functions_ques_pre.R")){
  source("functions_ques_pre.R")
} else {
  source("03_preques/rscript/functions_ques_pre.R")
}

check_and_install_packages(required_packages)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  titlePanel("Pre-QuES Module"),
  sidebarLayout(
    sidebarPanel(
      # Mode selection
      radioButtons("analysis_mode", "Analysis Mode",
                   choices = c("Single Period" = "single", "Batch Processing" = "batch"),
                   selected = "single"),
      
      # Single mode inputs 
      conditionalPanel(
        condition = "input.analysis_mode == 'single'",
        fileInput("lc_t1", "Land Use/Cover T1", accept = c(".tif", ".tiff")),
        numericInput("t1_year", "T1 Year", value = 1990),
        fileInput("lc_t2", "Land Use/Cover T2", accept = c(".tif", ".tiff")),
        numericInput("t2_year", "T2 Year", value = 2010)
      ),
      
      # Batch mode inputs
      conditionalPanel(
        condition = "input.analysis_mode == 'batch'",
        
        div(class = "mb-3",
            shinyDirButton("batch_base_dir", "Select Folder Containing All Landcover Rasters", 
                           "Please select the folder containing all raster files",
                           style = "width: 100%;")
        ),
        verbatimTextOutput("print_batch_base_dir", placeholder = TRUE),
        
        textInput("batch_pattern", "File pattern",
                  value = "landscape_{year}.tif"),
        div(class = "mb-3",
            helpText("Use {year} as a placeholder for the year. For example, if your files are named landscape_2020.tif, landscape_2025.tif, etc., set the pattern to landscape_{year}.tif.")
        ),
        
        textInput("batch_years", "Years", 
                  value = "2020, 2025, 2030, 2045, 2050"),
        div(class = "mb-3",
            helpText("Periods will be created between consecutive years: e.g., 2020-2025, 2025-2030, ...")
        )
      ),
      
      # Shared inputs 
      fileInput("lookup_lc", "Land Use/Cover Lookup Table (CSV)", accept = c(".csv")),
      fileInput("lookup_trajectory", "Trajectory Lookup Table (CSV)", accept = c(".csv")),
      fileInput("zone_shapefile", "Planning Units (Shapefile)",
                accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE, 
                placeholder = "Upload the .shp, .dbf, .prj, and .shx files."),
      
      # Output directory and actions 
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("run_analysis", "Run Pre-QuES Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
            actionButton("open_output_folder", "Open Output Folder",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          actionButton("returnButton", "Return to Main Menu", 
                       style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", uiOutput("user_guide")),
        tabPanel("Log",
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^4)
  
  # Directory selection 
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  selected_output_dir <- reactiveVal(value = NULL)
  observe({
    if (!is.null(input$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  output$selected_dir <- renderText({
    if (!is.null(selected_output_dir())) {
      paste("Selected output directory:", selected_output_dir())
    } else {
      "No output directory selected"
    }
  })
  
  output$print_output_dir <- renderPrint({
    if (!is.null(selected_output_dir())) {
      cat(paste(selected_output_dir()))
    } else {
      cat("No output directory selected")
    }
  })
  
  # Batch mode: Base folder selection 
  shinyDirChoose(input, "batch_base_dir", roots = volumes, session = session)
  batch_base_dir <- reactiveVal(NULL)
  observe({
    if (!is.null(input$batch_base_dir)) {
      batch_base_dir(parseDirPath(volumes, input$batch_base_dir))
    }
  })
  output$print_batch_base_dir <- renderPrint({
    if (!is.null(batch_base_dir())) {
      cat(batch_base_dir())
    } else {
      cat("No folder selected")
    }
  })
  
  # Helper to parse years
  parse_years <- function(years_text) {
    y <- trimws(unlist(strsplit(years_text, ",")))
    y <- as.numeric(y)
    y <- y[!is.na(y)]
    if (length(y) < 2) {
      return(NULL)
    }
    y <- sort(y)
    return(y)
  }
  
  # User guide
  output$user_guide <- renderUI({
    guide_paths <- c(
      "03_preques/helpfile/preques_quick_user_guide_ID.Rmd",
      "../helpfile/preques_quick_user_guide_ID.Rmd"
    )
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    HTML("<p>User guide file not found.</p>")
  })
  
  # Reactive values for inputs
  rv <- reactiveValues(
    lc_t1 = NULL,
    lc_t2 = NULL,
    lookup_lc = NULL,
    zone_input = NULL,
    lookup_zone = NULL,
    lookup_trajectory = NULL
  )
  
  observe({
    rv$lc_t1 <- input$lc_t1
    rv$lc_t2 <- input$lc_t2
    rv$lookup_lc <- input$lookup_lc
    rv$lookup_trajectory <- input$lookup_trajectory
    rv$zone_input <- input$zone_shapefile
    rv$lookup_zone <- NULL
  })
  
  # Input validation 
  validate_single_inputs <- reactive({
    validate(
      need(rv$lc_t1, "Please upload Land Use/Cover T1 file"),
      need(rv$lc_t2, "Please upload Land Use/Cover T2 file"),
      need(rv$lookup_lc, "Please upload Land Use/Cover Lookup Table (CSV) file"),
      need(rv$zone_input, "Please upload Planning Units file"),
      need(rv$lookup_trajectory, "Please upload Land Use/Cover Trajectory Lookup Table (CSV) file"),
      need(selected_output_dir(), "Please select an output directory")
    )
    return(TRUE)
  })
  
  validate_batch_inputs <- reactive({
    years <- parse_years(input$batch_years)
    validate(
      need(batch_base_dir(), "Please select a base folder with rasters"),
      need(input$batch_pattern != "", "Please provide a file pattern"),
      need(!is.null(years) && length(years) >= 2, 
           "Please enter at least two years separated by commas (e.g., 2024,2030,2036)"),
      need(rv$lookup_lc, "Please upload Land Use/Cover Lookup Table (CSV) file"),
      need(rv$zone_input, "Please upload Planning Units file"),
      need(rv$lookup_trajectory, "Please upload Land Use/Cover Trajectory Lookup Table (CSV) file"),
      need(selected_output_dir(), "Please select an output directory")
    )
    return(TRUE)
  })
  
  # Function to generate file path
  generate_path <- function(base_dir, pattern, year) {
    filename <- gsub("\\{year\\}", year, pattern)
    file.path(base_dir, filename)
  }
  
  # Run analysis 
  observeEvent(input$run_analysis, {
    mode <- input$analysis_mode
    if (mode == "single") {
      req(validate_single_inputs())
      run_single_analysis()
    } else {
      req(validate_batch_inputs())
      run_batch_analysis()
    }
  })
  
  # Single analysis function 
  run_single_analysis <- function() {
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    withProgress(message = 'Running Pre-QuES Analysis', value = 0.1, {
      tryCatch({
        lc_t1_raster <- terra::rast(rv$lc_t1$datapath)
        zone_data <- process_planning_unit(
          zone_type = "shapefile",
          zone_input = rv$zone_input,
          lc_t1_raster = lc_t1_raster
        )
        results <- run_preques_analysis(
          lc_t1_input = rv$lc_t1,
          lc_t2_input = rv$lc_t2,
          admin_z_input = zone_data$zone_raster,
          lc_lookup_input = rv$lookup_lc,
          zone_lookup_input = zone_data$lookup_zone,
          trajectory_lookup_input = rv$lookup_trajectory,
          time_points = list(t1 = input$t1_year, t2 = input$t2_year),
          output_dir = selected_output_dir(),
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          }
        )
        output$status_messages <- renderText("Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder or view the report.")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification("running_notification")
        showNotification("Analysis completed successfully!", type = "message")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        output$success_message <- renderText(NULL)
        removeNotification("running_notification")
        showNotification("Error in analysis. Please check the error messages.", type = "error")
      })
    })
  }
  
  # Batch analysis function
  run_batch_analysis <- function() {
    years <- parse_years(input$batch_years)
    if (is.null(years) || length(years) < 2) {
      showNotification("Invalid years input.", type = "error")
      return()
    }
    
    n_periods <- length(years) - 1
    period_labels <- paste0(years[1:n_periods], "-", years[2:(n_periods+1)])
    
    base_dir <- batch_base_dir()
    pattern <- input$batch_pattern
    
    all_paths <- sapply(years, function(y) generate_path(base_dir, pattern, y))
    missing <- !file.exists(all_paths)
    if (any(missing)) {
      missing_years <- years[missing]
      showNotification(
        paste("Warning: The following year files are missing:",
              paste(missing_years, collapse = ", ")),
        type = "warning", duration = 10
      )
      valid_years <- years[!missing]
      if (length(valid_years) < 2) {
        showNotification("Not enough valid years to form any period.", type = "error")
        return()
      }
      years <- valid_years
      n_periods <- length(years) - 1
      period_labels <- paste0(years[1:n_periods], "-", years[2:(n_periods+1)])
    }
    
    main_out <- selected_output_dir()
    
    total <- n_periods
    progress <- Progress$new(session, min = 0, max = total)
    on.exit(progress$close())
    progress$set(message = "Batch processing periods", value = 0)
    
    log_messages <- character()
    error_messages <- character()
    
    for (i in seq_len(n_periods)) {
      t1_year <- years[i]
      t2_year <- years[i+1]
      period_label <- period_labels[i]
      
      progress$set(value = i - 1, detail = paste("Starting:", period_label))
      
      period_out <- file.path(main_out, paste0("preques_", period_label))
      dir.create(period_out, showWarnings = FALSE, recursive = TRUE)
      
      t1_path <- generate_path(base_dir, pattern, t1_year)
      t2_path <- generate_path(base_dir, pattern, t2_year)
      
      lc_t1_input <- list(
        datapath = t1_path,
        name = paste0("landcover", t1_year)
      )
      lc_t2_input <- list(
        datapath = t2_path,
        name = paste0("landcover", t2_year)
      )
      
      tryCatch({
        lc_t1_raster <- terra::rast(t1_path)
        zone_data <- process_planning_unit(
          zone_type = "shapefile",
          zone_input = rv$zone_input,
          lc_t1_raster = lc_t1_raster
        )
        
        results <- run_preques_analysis(
          lc_t1_input = lc_t1_input,
          lc_t2_input = lc_t2_input,
          admin_z_input = zone_data$zone_raster,
          lc_lookup_input = rv$lookup_lc,
          zone_lookup_input = zone_data$lookup_zone,
          trajectory_lookup_input = rv$lookup_trajectory,
          time_points = list(t1 = t1_year, t2 = t2_year),
          output_dir = period_out,
          progress_callback = NULL
        )
        
        log_messages <- c(log_messages, paste(period_label, "completed successfully."))
      }, error = function(e) {
        err_msg <- paste(period_label, "failed:", e$message)
        log_messages <- c(log_messages, err_msg)
        error_messages <- c(error_messages, err_msg)
      })
      
      progress$set(value = i, detail = paste("Completed:", period_label))
    }
    
    output$status_messages <- renderText(paste(log_messages, collapse = "\n"))
    if (length(error_messages) > 0) {
      output$error_messages <- renderText(paste("Errors encountered:\n", paste(error_messages, collapse = "\n")))
    } else {
      output$error_messages <- renderText(NULL)
    }
    output$success_message <- renderText("Batch processing finished. Check log for details.")
    shinyjs::show("open_output_folder")
    removeNotification("running_notification")
    showNotification("Batch processing completed.", type = "message")
  }
  
  # Open output folder
  observeEvent(input$open_output_folder, {
    if (!is.null(selected_output_dir())) {
      if (.Platform$OS.type == "windows") {
        shell.exec(selected_output_dir())
      } else {
        system2("open", selected_output_dir())
      }
    }
  })
  
  # Open report 
  observeEvent(input$open_report, {
    if (input$analysis_mode == "single") {
      report_path <- file.path(selected_output_dir(), "PreQUES_report.html")
      if (file.exists(report_path)) {
        showNotification("Opening report...", type = "message")
        utils::browseURL(report_path)
      } else {
        showNotification("Report file not found.", type = "error")
      }
    } else {
      showNotification("In batch mode, reports are inside each period's subfolder. Please open the output folder.", type = "info")
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
    }
  })
}

shinyApp(ui, server)