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

# Source the functions

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
      # fileInput("lulc_t1", "Land cover map at T1", accept = c("image/tiff", ".tif")),
      # textInput("year1", "Year of T1", value = "1990"),
      # fileInput("lulc_t2", "Land cover map at T2", accept = c("image/tiff", ".tif")),
      # textInput("year2", "Year of T2", value = "2000"),
      # fileInput("pu_raster", "Planning Unit Raster", accept = c("image/tiff", ".tif")),
      # fileInput("pu_table", "Planning Unit lookup table", accept = c(".csv")),
      fileInput("agric_table", "Agricultural lookup table", accept = c(".csv")),
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
    total_table = NULL
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
    rv$agric_table <- input$agric_table
  })
  
  # Input validation
  validate_inputs <- reactive({
    validate(
      need(rv$agric_table, "Please upload Planning Units Lookup Table (CSV) file"),
      need(rv$wd != "", "Please select an output directory")
    )
    TRUE
  })
  
  # Run analysis
  observeEvent(input$process, {
    rv$wd <- parseDirPath(volumes, input$wd)
    req(validate_inputs(), rv$wd)
    req(input$agric_table)
    
    # === INSERTED YOUR AGRICULTURAL FORMULA HERE ===
    areaOfAgricTable <- read.csv(input$agric_table$datapath)
    
    # Uniform column names
    colnames(areaOfAgricTable) <- c('TAHUN', 'SAWAH_IRIGASI', 'TADAH_HUJAN', 'LUAS_PANEN')
    
    # TOTAL LUAS SAWAH (ha/yr)
    areaOfAgricTable$LUAS_TOTAL <- areaOfAgricTable$SAWAH_IRIGASI + areaOfAgricTable$TADAH_HUJAN
    
    # MASA TANAM DALAM SETAHUN (Index Penanaman Irigasi)
    areaOfAgricTable$IP_IRIGASI1 <- (areaOfAgricTable$LUAS_PANEN - areaOfAgricTable$TADAH_HUJAN) / areaOfAgricTable$SAWAH_IRIGASI
    areaOfAgricTable$IP_IRIGASI2 <- areaOfAgricTable$LUAS_PANEN / areaOfAgricTable$SAWAH_IRIGASI
    
    # SFw = Faktor skala lahan sawah irigasi intermitten
    # 0.49 * Tadah Hujan + (Luas Panen - Tadah Hujan) * 1   /  15
    areaOfAgricTable$SFW <- (0.49 * areaOfAgricTable$TADAH_HUJAN + 
                               (areaOfAgricTable$LUAS_PANEN - areaOfAgricTable$TADAH_HUJAN)) / 15
    # === END FORMULA BLOCK ===
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Running TA Profitability Analysis', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Starting analysis...")
        
        start_time <- Sys.time()
        
        incProgress(0.5, detail = "Generating report...")
        
        end_time <- Sys.time()
        
        output_file <- paste0("quesc-paddy_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        report_path <- file.path(rv$wd, output_file)
        
        rmarkdown::render(
          input = "../report_template/quesc-paddy_ID.Rmd",
          output_file = report_path,
          params = list(areaOfAgricTable = areaOfAgricTable, start_time = start_time, end_time = end_time),
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