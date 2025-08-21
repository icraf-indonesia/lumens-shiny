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

# Source the functions
source("functions_ta_profit.R")

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
  titlePanel("Profitability Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lulc_t1", "Land cover map at T1", accept = c("image/tiff", ".tif")),
      textInput("year1", "Year of T1", value = "1990"),
      fileInput("lulc_t2", "Land cover map at T2", accept = c("image/tiff", ".tif")),
      textInput("year2", "Year of T2", value = "2000"),
      fileInput("pu_raster", "Planning Unit Raster", accept = c("image/tiff", ".tif")),
      fileInput("pu_table", "Planning Unit lookup table", accept = c(".csv")),
      fileInput("npv_table", "NPV lookup table", accept = c(".csv")),
      selectInput(
        inputId = "currency",
        label = "Select a Currency:",
        choices = c("IDR", "USD", "EUR", "JPY"),
        selected = "IDR"
      ),
      # fileInput("cstock_table", "Carbon Stock lookup table", accept = c(".csv")),
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
    report_file = NULL,  # Path to the generated report
    total_table = NULL,
    npv1_table = NULL,
    npv2_table = NULL,
    deltaNPV_table = NULL,
    npv1_chart = NULL,
    npv2_chart = NULL,
    deltaNPV_chart = NULL,
    map1_file_path = NULL,
    map2_file_path = NULL,
    npv_file_path = NULL,
    # carbon_file_path = NULL,
    pu_table_path = NULL,
    npv1_map = NULL,
    npv2_map = NULL,
    deltaNPV_map = NULL,
    year1 = NULL,
    year2 = NULL,
    currency = NULL
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
      "07_ta-profit/helpfile/ta-profit_quick_user_guide.Rmd",
      "../helpfile/ta-profit_quick_user_guide.Rmd"
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
    rv$lulc_t1 <- input$lulc_t1
    rv$lulc_t2 <- input$lulc_t2
    # rv$cstock_table <- input$cstock_table
    rv$npv_table <- input$npv_table
    rv$pu_raster <- input$pu_raster
    rv$pu_table <- input$pu_table
    rv$currency <- input$currency
  })
  
  # Input validation
  validate_inputs <- reactive({
    validate(
      need(rv$lulc_t1, "Please upload Land Use/Cover T1 file"),
      need(rv$lulc_t2, "Please upload Land Use/Cover T2 file"),
      # need(rv$cstock_table, "Please upload Carbon Stock Lookup Table (CSV) file"),
      need(rv$npv_table, "Please upload NPV Lookup Table (CSV) file"),
      need(rv$pu_raster, "Please upload Planning Units Raster"),
      need(rv$pu_table, "Please upload Planning Units Lookup Table (CSV) file"),
      need(rv$currency, "Please select your currency"),
      need(rv$wd != "", "Please select an output directory")
    )
    TRUE
  })
  
  # Run analysis
  observeEvent(input$process, {
    rv$wd <- parseDirPath(volumes, input$wd)
    req(validate_inputs(), rv$wd)
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Running TA Profitability Analysis', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Starting analysis...")
        
        start_time <- Sys.time()
        
        result <- preprocess_data(
          pathLULCT1 = input$lulc_t1$datapath,
          pathLULCT2 = input$lulc_t2$datapath,
          pathPU = input$pu_raster$datapath,
          # pathLookupCstock = input$cstock_table$datapath,
          pathLookupPU = input$pu_table$datapath,
          pathLookupNPV = input$npv_table$datapath,
          valueT1 = input$year1,
          valueT2 = input$year2
        )
        
        incProgress(0.5, detail = "Generating report...")
        
        end_time <- Sys.time()
        paths <- list(
          pathLULCT1 = input$lulc_t1$datapath,
          pathLULCT2 = input$lulc_t2$datapath,
          # pathLookupCstock = input$cstock_table$datapath,
          pathLookupNPV = input$npv_table$datapath,
          pathLookupPU = input$pu_table$datapath
        )
        times <- list(start_time = start_time, end_time = end_time, valueT1 = input$year1, valueT2 = input$year2)
        
        pu_list <- unique(result$combinedRasterTable$PU)
        pu_outputs <- list()
        for (pu_name in pu_list) {
          pu_data <- result$combinedRasterTable %>% filter(PU == pu_name)
          pu_outputs[[pu_name]] <- process_pu_data(pu_data, pu_name, input$currency)
        }
        
        # And update the main chart generation:
        params <- generate_report_params(
          data = result,
          maps = result,
          paths = paths,
          times = times,
          pu_outputs = pu_outputs,
          output_dir = rv$wd,
          currency = input$currency 
        )
        
        output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        report_path <- file.path(rv$wd, output_file)
        
        rmarkdown::render(
          input = "../report_template/ta-profit-ID.Rmd",
          output_file = report_path,
          params = params,
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