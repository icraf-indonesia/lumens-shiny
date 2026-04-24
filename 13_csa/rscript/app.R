source('../../helper.R')

library(shiny)
library(shinyjs)
library(shinyalert)
library(rmarkdown)
library(bslib)
library(terra)
library(dplyr)
library(readr)
library(plotly)
library(stringr)
library(RColorBrewer)
library(kableExtra)
library(shinyFiles)
library(pkgdown)
library(sf)

# Source the functions
source("functions.R")

# JavaScript code for closing window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  titlePanel("QUES-C Paddy Field Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lulc", "Land cover map", accept = c("image/tiff", ".tif")),
      textInput("year", "Year of map", value = "1990"),
      radioButtons("zone_type", "Planning Units Input Type",
                   choices = c("Raster" = "raster", "Shapefile" = "shapefile"), selected = "shapefile"),
      conditionalPanel(
        condition = "input.zone_type == 'raster'",
        fileInput("zone_raster", "Planning Units (Raster)", accept = c(".tif", ".tiff")),
        fileInput("lookup_zone", "Planning Units Lookup (CSV)", accept = c(".csv"))
      ),
      conditionalPanel(
        condition = "input.zone_type == 'shapefile'",
        fileInput("zone_shapefile", "Planning Units (Shapefile)",
                  accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
      ),
      fileInput("lc_table", "Landcover lookup table (CSV)", accept = c(".csv")),
      fileInput("conversion_table", "Conversion lookup table (CSV)", accept = c(".csv")),
      # fileInput("sf_table", "Scaling Factor lookup table (CSV)", accept = c(".csv")),
      fileInput("pupuk_table", "Fertilizer lookup table (CSV)", accept = c(".csv")),
      # fileInput("n2o_table", "N2O Conversion lookup table (CSV)", accept = c(".csv")),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          textOutput("selected_directory"),
          actionButton("process", "Run Analysis",
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
        tabPanel("User Guide", 
                 uiOutput("user_guide")),
        tabPanel("Log",
                 br(),
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  #### Initialize reactive values ####
  rv <- reactiveValues(
    wd = "",  # Working directory for saving outputs
    report_file = NULL,  
    
    # Filepath
    lulc_file_path = NULL,
    pu_file_path = NULL,
    lookup_pu_file_path = NULL, 
    lookup_lc_file_path = NULL, 
    lookup_conversion_file_path = NULL, 
    lookup_pupuk_file_path = NULL, 
    
    # Main inputs
    lulc = NULL,
    year = NULL,
    zone_type = NULL,
    
    # Planning unit input
    zone_input = NULL,            # raster OR shapefile
    lookup_zone = NULL,           # only for raster
    
    # Lookup tables
    lc_table = NULL,
    conversion_table = NULL,
    pupuk_table = NULL
  )
  
  #' Directory selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Update reactive value when output directory is selected
  observe({
    if (!is.null(input$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  #' Display the selected output directory path
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  output$user_guide <- renderUI({
    guide_paths <- c(
      "13_csa/helpfile/csa_quick_user_guide_ID.Rmd",
      "../helpfile/csa_quick_user_guide_ID.Rmd"
    )
    
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    
    HTML("<p>User guide file not found.</p>")
  })
  
  # Update reactive values when inputs change
  observe({
    rv$lulc <- input$lulc
    rv$year <- input$year
    if (input$zone_type == "raster") {
      rv$zone_input <- input$zone_raster
      rv$lookup_zone <- input$lookup_zone
    } else {
      rv$zone_input <- input$zone_shapefile
      rv$lookup_zone <- NULL  # Will be created from shapefile
    }
    rv$pu_table <- input$pu_table
    rv$lc_table <- input$lc_table
    rv$conversion_table <- input$conversion_table
    rv$pupuk_table <- input$pupuk_table
  })
  
  # Input validation
  validate_inputs <- reactive({
    validate(
      need(rv$lulc, "Please upload Land Use/Cover T1 file"),
      need(rv$year, "Please upload Land Use/Cover T2 file"),
      need(input$zone_type, "Please select Planning Units Input Type"),
      need(rv$zone_input, "Please upload Planning Units file"),
      need(if(input$zone_type == "raster") rv$lookup_zone else TRUE, "Please upload Planning Units Lookup (CSV) file for raster input"),
      need(rv$lc_table, "Please upload Landcover Lookup Table (CSV) file"),
      need(rv$conversion_table, "Please upload Conversion Lookup Table (CSV) file"),
      need(rv$pupuk_table, "Please upload Fertilizer Lookup Table (CSV) file"),
      need(rv$wd != "", "Please select an output directory")
    )
    TRUE
  })
  
  observeEvent(input$process, {
    rv$wd <- parseDirPath(volumes, input$wd)
    req(validate_inputs(), rv$wd)
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Running QUES-C Paddy Analysis', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Starting analysis...")
        
        start_time <- Sys.time()
        
        result <- preprocess_data(
          pathLULCT = rv$lulc$datapath,
          zone_type = input$zone_type,
          pathPU = if (input$zone_type == "raster") rv$zone_input$datapath else rv$zone_input,
          pathLookupPU = if (input$zone_type == "raster") rv$lookup_zone$datapath else NULL,
          pathLookupLC = rv$lc_table$datapath,
          pathLookupConversion = rv$conversion_table$datapath,
          pathLookupPupuk = rv$pupuk_table$datapath,
          year = rv$year
        )
        
        incProgress(0.5, detail = "Generating report...")
        
        end_time <- Sys.time()
        
        paths <- list(
          pathLULCT = rv$lulc$datapath,
          pathPU = rv$zone_input$datapath,
          pathLookupPU = if (input$zone_type == "raster") rv$lookup_zone$datapath else NULL,
          pathLookupLC = rv$lc_table$datapath,
          pathLookupConversion = rv$conversion_table$datapath,
          pathLookupPupuk = rv$pupuk_table$datapath
        )
        
        output_file <- paste0("quesc-paddy_report_log", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        report_path <- file.path(rv$wd, output_file)
        
        rmarkdown::render(
          input = "../report_template/quesc-paddy_ID.Rmd",
          output_file = report_path,
          params = list(
            result = result, 
            start_time = format(start_time, "%Y-%m-%d %H:%M:%S"), 
            end_time = format(end_time, "%Y-%m-%d %H:%M:%S"), 
            paths = paths,
            output_dir = rv$wd, 
            year = input$year
            ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        
        rv$report_file <- report_path
        
        incProgress(0.9, detail = "Finished.")
        
        output$status_messages <- renderText("Analysis completed successfully!")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        showNotification("Analysis completed successfully!", type = "message")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification("Error in analysis. Please check the error messages.", type = "error")
      })
    })
    
    removeNotification("running_notification")
  })
  
  
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
      shinyjs::delay(1000, stopApp())
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)