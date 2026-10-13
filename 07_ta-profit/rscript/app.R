#' Profitability Analysis Shiny Application
#'
#' @description
#' This Shiny web application performs **Profitability Analysis** for land-use and land-cover (LULC)
#' changes between two time periods (T1 and T2). It integrates spatial raster processing,
#' economic valuation (NPV), and carbon stock analysis to produce a comprehensive report
#' summarizing land-use profitability and environmental implications.
#'
#' @details
#' The app reads spatial raster inputs for two LULC periods (T1 and T2),
#' planning unit raster, and lookup tables for NPV and carbon stock values.
#' It processes these datasets to compute opportunity costs, emissions,
#' and profitability per land-use transition, and exports the results
#' in a structured report format (HTML or PDF).
#'
#' @seealso
#' \code{\link{functions.R}} for modular processing functions
#'
#' @import shiny shinyjs shinyalert rmarkdown bslib terra dplyr readr plotly stringr
#' RColorBrewer kableExtra shinyFiles pkgdown purrr tidyverse
#'
#' @export

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
library(purrr)
library(tidyverse)
library(openxlsx)
library(readxl)
library(tools)

#' Source Function Definitions
#'
#' @description
#' Loads all helper functions used in the profitability analysis
#' (data preprocessing, lookup operations, report generation, etc.).
source("functions.R")

#' JavaScript Extension for Shiny
#'
#' @description
#' Provides a small JS function (`closeWindow()`) for allowing
#' the Shiny app to close its browser tab or window.
#'
#' @examples
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

#' User Interface Definition
#'
#' @description
#' Defines the layout, input controls, and display panels of the
#' Profitability Analysis Shiny application. Users can upload LULC,
#' planning unit, and lookup tables, select analysis parameters, and
#' trigger report generation.
#'
#' @section UI Structure:
#' - **Sidebar Panel**: input controls for file uploads and options  
#' - **Main Panel**: log messages and user guide tabs  
#'
#' @seealso
#' \code{\link{server}} for server-side processing logic
#'
#' @return
#' A Shiny UI object to be passed into `shinyApp(ui, server)`
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
      fileInput("npv_table", "EAE lookup table", accept = c(".csv")),
      selectInput(
        inputId = "npv_table_year",
        label = "Which year does your EAE table use?",
        choices = c("T1", "T2", "Others"),
        selected = "T1"
      ),
      conditionalPanel(
        condition = "input.npv_table_year == 'Others'",
        numericInput(
          "npv_table_year_custom",
          "Custom Year",
          value = as.numeric(format(Sys.Date(), "%Y")),
          min = 1900
        )
      ),
      fileInput("cstock_table", "Carbon lookup table", accept = c(".csv")),
      selectInput(
        inputId = "currency",
        label = "Select a Currency:",
        choices = c("IDR", "USD", "EUR", "JPY"),
        selected = "IDR"
      ),
      # Directory selector and control buttons
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

#' @title Shiny Server Logic for Profitability Analysis App
#'
#' @description
#' Defines all server-side logic for the **TA Profitability Analysis** Shiny application.
#' Handles input validation, reactive state management, analysis execution, report generation,
#' and user interaction (notifications, report/folder opening, and navigation).
#'
#' @param input Shiny input object, containing UI input values.
#' @param output Shiny output object, defining reactive UI outputs.
#' @param session Shiny session object for user session handling.
#'
#' @return No return value. Called internally by \code{shinyApp()} to initialize the app.
#'
#' @seealso \code{\link{ui}}, \code{\link{preprocess_data}}, \code{\link{process_pu_data}}, \code{\link{generate_report_params}}
#'
#' @keywords server reactive shiny-app
#'
#' @export
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  #' @section Reactive Values:
  #' Initializes all key reactive values used to store analysis inputs, results, and file paths.
  #' @keywords reactive-values
  rv <- reactiveValues(
    wd = "",
    report_file = NULL, 
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
    carbon_file_path = NULL,
    pu_table_path = NULL,
    npv1_map = NULL,
    npv2_map = NULL,
    deltaNPV_map = NULL,
    year1 = NULL,
    year2 = NULL,
    currency = NULL
  )
  
  #' @section Directory Selection:
  #' Enables the user to choose an output directory for storing analysis results and reports.
  #' Uses \pkg{shinyFiles} to interface with the filesystem.
  #' @seealso \code{\link[shinyFiles]{shinyDirChoose}}, \code{\link[shinyFiles]{parseDirPath}}
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  
  #' @section Reactive Output Directory:
  #' Tracks and stores the user-selected output directory path as a reactive value.
  selected_output_dir <- reactiveVal(value = NULL)
  
  #' @section Observe Output Directory:
  #' Updates the reactive output directory when a new folder is selected by the user.
  observe({
    if (!is.null(input$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  #' @section Render Selected Directory:
  #' Displays the selected output directory path in the UI.
  #' Updates the reactive value \code{rv$wd}.
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  #' @section Render User Guide:
  #' Dynamically loads and renders the user guide R Markdown file into an HTML fragment.
  #' Displays a fallback message if the guide file is not found.
  #' @importFrom rmarkdown render
  output$user_guide <- renderUI({
    guide_paths <- c(
      "07_ta-profit/helpfile/ta-profit_quick_user_guide_ID.Rmd",
      "../helpfile/ta-profit_quick_user_guide_ID.Rmd"
    )
    
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    
    HTML("<p>User guide file not found.</p>")
  })
  
  #' @section Observe Input Changes:
  #' Keeps reactive values synchronized with file and text inputs from the UI.
  observe({
    rv$lulc_t1 <- input$lulc_t1
    rv$lulc_t2 <- input$lulc_t2
    rv$cstock_table <- input$cstock_table
    rv$npv_table <- input$npv_table
    rv$pu_raster <- input$pu_raster
    rv$pu_table <- input$pu_table
    rv$currency <- input$currency
  })
  
  # EAE table year selected by user
  selected_eae_year <- reactive({
    
    switch(
      input$npv_table_year,
      "T1" = as.numeric(input$year1),
      "T2" = as.numeric(input$year2),
      "Others" = as.numeric(input$npv_table_year_custom)
    )
    
  })
  
  #' @section Input Validation:
  #' Validates all user inputs before running the analysis.
  #' Prevents execution if any required file or field is missing.
  #' @return TRUE if all inputs are valid, otherwise displays validation messages.
  validate_inputs <- reactive({
    validate(
      need(rv$lulc_t1, "Please upload Land Use/Cover T1 file"),
      need(rv$lulc_t2, "Please upload Land Use/Cover T2 file"),
      need(rv$cstock_table, "Please upload Carbon Stock Lookup Table (CSV) file"),
      need(rv$npv_table, "Please upload NPV Lookup Table (CSV) file"),
      need(rv$pu_raster, "Please upload Planning Units Raster"),
      need(rv$pu_table, "Please upload Planning Units Lookup Table (CSV) file"),
      need(rv$currency, "Please select your currency"),
      need(rv$wd != "", "Please select an output directory")
    )
    TRUE
  })
  
  #' @section Run Analysis:
  #' Main observer triggered by the **Run Analysis** button.
  #' Executes the data preprocessing, profitability analysis, and report generation.
  #' @details
  #' - Calls \code{preprocess_data()} to clean and merge raster and lookup data.
  #' - Processes each planning unit using \code{process_pu_data()}.
  #' - Generates report parameters and renders an HTML report.
  #' - Displays progress and completion notifications.
  #' @importFrom shiny withProgress incProgress showNotification
  observeEvent(input$process, {
    rv$wd <- parseDirPath(volumes, input$wd)
    req(validate_inputs(), rv$wd)
    
    eae_year <- selected_eae_year()
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Running TA Profitability Analysis', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Starting analysis...")
        
        start_time <- Sys.time()
        
        result <- preprocess_data(
          pathLULCT1 = input$lulc_t1$datapath,
          pathLULCT2 = input$lulc_t2$datapath,
          pathPU = input$pu_raster$datapath,
          pathLookupCstock = input$cstock_table$datapath,
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
          pathPU = input$pu_raster$datapath,
          pathLookupNPV = input$npv_table$datapath,
          pathLookupPU = input$pu_table$datapath,
          pathLookupCstock = input$cstock_table$datapath
        )
        
        times <- list(
          start_time = start_time, 
          end_time = end_time, 
          valueT1 = input$year1, 
          valueT2 = input$year2,
          eae_year = selected_eae_year()
          )
        
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
  
  #' @section Open Report:
  #' Opens the generated HTML report after analysis completion.
  #' Displays an error notification if the report file cannot be found.
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
  
  #' @section Open Output Folder:
  #' Opens the output folder in the system's file explorer.
  #' Handles Windows and macOS/Linux environments separately.
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
  
  #' @section Session Termination:
  #' Stops the app gracefully when the user session ends.
  session$onSessionEnded(function() {
    stopApp()
  })
  
  #' @section Return to Main Menu:
  #' Shows a confirmation dialog for returning to the main menu.
  #' Closes the app window upon confirmation.
  #' @importFrom shinyalert shinyalert
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
  
  #' @section Alert Response:
  #' Handles user confirmation from the return-to-menu alert dialog.
  #' Closes the Shiny session and window if confirmed.
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