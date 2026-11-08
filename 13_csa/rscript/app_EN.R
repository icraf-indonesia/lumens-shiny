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
  titlePanel("Modul Emisi Karbon - Lahan Pertanian"),
  sidebarLayout(
    sidebarPanel(
      
      h4("Fertilizer Data Setup"),
      
      radioButtons("has_fertilizer", 
                   "Do you already have fertilizer dosage data?",
                   choices = c("Yes" = "yes", "No" = "no")),
      
      # =============================
      # CASE: USER DOES NOT HAVE DATA
      # =============================
      conditionalPanel(
        condition = "input.has_fertilizer == 'no'",
        
        h5("Step 1: Upload Planning Unit"),
        
        radioButtons("zone_type", "Planning Units Input Type",
                     choices = c("Raster" = "raster", "Shapefile" = "shapefile"), selected = "shapefile"),
        conditionalPanel(
          condition = "input.zone_type == 'raster'",
          fileInput("zone_raster", "Planning Units (Raster)", accept = c(".tif", ".tiff")),
          fileInput("lookup_zone", "Planning Units Lookup (CSV)", accept = c(".csv", ".xlsx"))
        ),
        conditionalPanel(
          condition = "input.zone_type == 'shapefile'",
          fileInput("zone_shapefile", "Planning Units (Shapefile)",
                    accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
        ),
        h5("Step 2: Fertilizer Configuration"),
        
        radioButtons("use_single", "Use Single Fertilizer?",
                     choices = c("Yes" = "yes", "No" = "no")),
        
        conditionalPanel(
          condition = "input.use_single == 'yes'",
          numericInput("n_single", "Number of Single Fertilizers", 1, min = 1),
          uiOutput("single_fertilizer_names")
        ),
        
        radioButtons("use_compound", "Use Compound Fertilizer?",
                     choices = c("Ya" = "yes", "Tidak" = "no")),
        
        conditionalPanel(
          condition = "input.use_compound == 'yes'",
          numericInput("n_compound", "Number of Compound Fertilizers", 1, min = 1),
          uiOutput("compound_fertilizer_names")
        ),
        
        actionButton("generate_template", "Generate Template",
                     style = "background-color:#FFA500; color:white; font-size: 18px; padding: 10px 15px; margin-bottom: 15px;"),
        downloadButton("download_template", "Download Template",
                       style = "font-size: 18px; padding: 10px 15px; margin-bottom: 15px;"),
        br()
      ),
      
      # =============================
      # CASE: FINAL INPUT (AFTER TEMPLATE OR DIRECT)
      # =============================
      conditionalPanel(
        condition = "input.has_fertilizer == 'yes' | input.generate_template > 0",
        h4("Upload Analysis Inputs"),
        conditionalPanel(
          condition = "input.has_fertilizer == 'yes'",
          radioButtons("zone_type", "Planning Units Input Type",
                       choices = c("Raster" = "raster", "Shapefile" = "shapefile"), selected = "shapefile"),
          conditionalPanel(
            condition = "input.zone_type == 'raster'",
            fileInput("zone_raster", "Planning Units (Raster)", accept = c(".tif", ".tiff")),
            fileInput("lookup_zone", "Planning Units Lookup (CSV)", accept = c(".csv", ".xlsx"))
          ),
          conditionalPanel(
            condition = "input.zone_type == 'shapefile'",
            fileInput("zone_shapefile", "Planning Units (Shapefile)",
                      accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
          )
        ),
        fileInput("lulc", "Land Cover Map", accept = c(".tif", ".tiff")),
        textInput("year", "Year of map", value = "1990"),
        fileInput("lc_table", "Landcover lookup table (CSV)", accept = c(".csv", ".xlsx")),
        fileInput("conversion_table", "Conversion lookup table (CSV)", accept = c(".csv", ".xlsx")),
        fileInput("pupuk_table", "Fertilizer lookup table (CSV)", accept = c(".csv", ".xlsx")),
        
        verbatimTextOutput("validation_message")
      ),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Pilih Direktori Keluaran", "Please select a directory", 
                         style = "font-size: 18px; padding: 10px 15px; "),
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
    
    single = list(),
    compound = list(),
    
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
  
  get_pu_ids <- function(input, LULCT = NULL) {
    
    # ======================
    # SHAPEFILE
    # ======================
    if (!is.null(input$zone_shapefile)) {
      
      sf_object <- read_shapefile(input$zone_shapefile)
      
      if (is.null(sf_object)) {
        stop("Failed to read shapefile. Please check your input.")
      }
      
      # Rename kolom (safe)
      cols <- names(sf_object)
      
      if (length(cols) < 2) {
        stop("Shapefile must have at least 2 columns")
      }
      
      sf_object <- sf_object %>%
        dplyr::rename(
          Value = all_of(cols[1]),
          planning_unit = all_of(cols[2])
        )
      
      # Lookup table
      LookupPU <- sf::st_drop_geometry(sf_object)
      
      # Kalau hanya butuh PU ID → return di sini
      if (is.null(LULCT)) {
        return(unique(sf_object$Value))
      }
      
      # Kalau mau langsung rasterize (full pipeline)
      lc_res <- terra::res(LULCT)
      
      PU <- rasterise_multipolygon(
        sf_object,
        raster_res = lc_res,
        field = "Value"
      )
      
      levels(PU) <- LookupPU
      
      return(list(
        PU = PU,
        LookupPU = LookupPU,
        PU_ID = unique(sf_object$Value)
      ))
    }
    
    # ======================
    # RASTER
    # ======================
    if (!is.null(input$zone_raster)) {
      
      PU <- terra::rast(input$zone_raster$datapath)
      
      PU_ID <- unique(terra::values(PU))
      
      return(list(
        PU = PU,
        LookupPU = NULL,
        PU_ID = PU_ID
      ))
    }
    
    return(NULL)
  }
  
  observe({
    req(input$use_single == "yes", input$n_single)
    
    for (i in 1:input$n_single) {
      rv$single[[i]] <- input[[paste0("single_name_", i)]]
    }
  })
  
  
  observe({
    req(input$use_compound == "yes", input$n_compound)
    
    for (i in 1:input$n_compound) {
      rv$compound[[i]] <- input[[paste0("compound_name_", i)]]
    }
  })
  
  output$single_fertilizer_names <- renderUI({
    
    req(input$use_single == "yes")
    req(input$n_single)
    
    lapply(1:input$n_single, function(i) {
      
      value_old <- if (length(rv$single) >= i) rv$single[[i]] else ""
      
      textInput(
        inputId = paste0("single_name_", i),
        label = paste("Single Fertilizer", i, "Name"),
        value = value_old,
        placeholder = "e.g. UREA"
      )
    })
  })
  
  output$compound_fertilizer_names <- renderUI({
    
    req(input$use_compound == "yes")
    req(input$n_compound)
    
    lapply(1:input$n_compound, function(i) {
      
      value_old <- if (length(rv$compound) >= i) rv$compound[[i]] else ""
      
      textInput(
        inputId = paste0("compound_name_", i),
        label = paste("Compound Fertilizer", i, "Name"),
        value = value_old,
        placeholder = "e.g. NPK 15-10-12"
      )
    })
  })
  
  template_data <- reactive({
    
    req(input$generate_template)
    
    pu_ids <- get_pu_ids(input)
    
    validate(
      need(!is.null(pu_ids) && length(pu_ids) > 0, 
           "PU not detected. Please upload valid data.")
    )
    
    # ======================
    # BASE COLUMNS
    # ======================
    cols <- c("ID", "UNIT_PERENCANAAN", "SATUAN")
    
    # ======================
    # SINGLE FERTILIZER
    # ======================
    if (!is.null(input$use_single) && input$use_single == "yes") {
      
      single_names <- sapply(1:input$n_single, function(i) {
        input[[paste0("single_name_", i)]]
      })
      
      # bersihin NA / kosong
      single_names <- single_names[!is.na(single_names) & single_names != ""]
      
      cols <- c(cols, paste0("Tunggal_", single_names))
    }
    
    # ======================
    # COMPOUND FERTILIZER
    # ======================
    if (!is.null(input$use_compound) && input$use_compound == "yes") {
      
      compound_names <- sapply(1:input$n_compound, function(i) {
        input[[paste0("compound_name_", i)]]
      })
      
      compound_names <- compound_names[!is.na(compound_names) & compound_names != ""]
      
      cols <- c(cols, paste0("Majemuk_", compound_names))
    }
    
    # ======================
    # BUILD DATAFRAME
    # ======================
    df <- data.frame(matrix(NA, nrow = length(pu_ids), ncol = length(cols)))
    colnames(df) <- cols
    
    df$ID <- pu_ids
    df$SATUAN <- "Kg/Ha"
    
    df
  })
  
  output$download_template <- downloadHandler(
    filename = function() {
      "fertilizer_template.csv"
    },
    content = function(file) {
      write.csv(template_data(), file, row.names = FALSE)
    }
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
      need(rv$year, "Please input year"),
      need(input$zone_type, "Please select Planning Units Input Type"),
      need(rv$zone_input, "Please upload Planning Units file"),
      need(if(input$zone_type == "raster") rv$lookup_zone else TRUE, "Please upload Planning Units Lookup file for raster input"),
      need(rv$lc_table, "Please upload Landcover Lookup Table file"),
      need(rv$conversion_table, "Please upload Conversion Lookup Table file"),
      need(rv$pupuk_table, "Please upload Fertilizer Lookup Table file"),
      # need(
      #   !(input$use_single == "yes" && any(single_names= "")),
      #   "Please fill all Single Fertilizer names"
      # ),
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