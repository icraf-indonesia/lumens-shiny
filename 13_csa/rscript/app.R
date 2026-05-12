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
library(openxlsx)

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
      
      h4("Pengaturan Data Pupuk"),
      
      radioButtons("has_fertilizer", 
                   "Apakah sudah punya data dosis pupuk?",
                   choices = c("Ya" = "yes", "Tidak" = "no")),
      
      # =============================
      # CASE: USER DOES NOT HAVE DATA
      # =============================
      conditionalPanel(
        condition = "input.has_fertilizer == 'no'",
        
        h5("Langkah 1: Unggah Unit Perencanaan"),
        
        radioButtons("zone_type", "Tipe Input Unit Perencanaan",
                     choices = c("Raster" = "raster", "Shapefile" = "shapefile"), selected = "shapefile"),
        conditionalPanel(
          condition = "input.zone_type == 'raster'",
          fileInput("zone_raster", "Unit Perencanaan (Raster)", accept = c(".tif", ".tiff")),
          fileInput("lookup_zone", "Tabel Referensi Unit Perencanaan", accept = c(".csv", ".xlsx"))
        ),
        conditionalPanel(
          condition = "input.zone_type == 'shapefile'",
          fileInput("zone_shapefile", "Unit Perencanaan (Shapefile)",
                    accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
        ),
        h5("Langkah 2: Konfigurasi Pupuk"),
        
        radioButtons("use_single", "Apakah menggunakan pupuk tunggal?",
                     choices = c("Ya" = "yes", "Tidak" = "no")),
        
        conditionalPanel(
          condition = "input.use_single == 'yes'",
          numericInput("n_single", "Jumlah Pupuk Tunggal", 1, min = 1),
          uiOutput("single_fertilizer_names")
        ),
        
        radioButtons("use_compound", "Apakah menggunakan pupuk majemuk?",
                     choices = c("Ya" = "yes", "Tidak" = "no")),
        
        conditionalPanel(
          condition = "input.use_compound == 'yes'",
          numericInput("n_compound", "Jumlah Pupuk Majemuk", 1, min = 1),
          uiOutput("compound_fertilizer_names")
        ),
        
        actionButton("generate_template", "Buat Template",
                     style = "background-color:#FFA500; color:white; font-size: 18px; padding: 10px 15px; margin-bottom: 15px;"),
        hidden(
          downloadButton(
            "download_template",
            "Unduh Template",
            style = "font-size: 18px; padding: 10px 15px; margin-bottom: 15px;"
          )
        ),
        br()
      ),
      
      # =============================
      # CASE: FINAL INPUT (AFTER TEMPLATE OR DIRECT)
      # =============================
      conditionalPanel(
        condition = "input.has_fertilizer == 'yes' | input.generate_template > 0",
        h4("Unggah Data Input"),
        conditionalPanel(
          condition = "input.has_fertilizer == 'yes'",
          radioButtons("zone_type", "Tipe Input Unit Perencanaan",
                       choices = c("Raster" = "raster", "Shapefile" = "shapefile"), selected = "shapefile"),
          conditionalPanel(
            condition = "input.zone_type == 'raster'",
            fileInput("zone_raster", "Unit Perencanaan (Raster)", accept = c(".tif", ".tiff")),
            fileInput("lookup_zone", "Tabel Referensi Unit Perencanaan", accept = c(".csv", ".xlsx"))
          ),
          conditionalPanel(
            condition = "input.zone_type == 'shapefile'",
            fileInput("zone_shapefile", "Unit Perencanaan (Shapefile)",
                      accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
          )
        ),
        fileInput("lulc", "Peta Tutupan Lahan", accept = c(".tif", ".tiff")),
        textInput("year", "Tahun Peta", value = "1990"),
        fileInput("lc_table", "Tabel Referensi Tutupan Lahan dengan Klasifikasi Padi", accept = c(".csv", ".xlsx")),
        fileInput("conversion_table", "Tabel Parameter Emisi dan Konversi", accept = c(".csv", ".xlsx")),
        fileInput("pupuk_table", "Tabel Dosis Pupuk", accept = c(".csv", ".xlsx")),
        
        verbatimTextOutput("validation_message")
      ),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Pilih Direktori Keluaran", "Pilih Direktori", 
                         style = "font-size: 18px; padding: 10px 15px; "),
          textOutput("selected_directory"),
          actionButton("process", "Jalankan Analisis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Buka Laporan",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
            actionButton("open_output_folder", "Buka Folder Keluaran",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          actionButton("returnButton", "Kembali ke Menu Utama", 
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
  
  get_pu_ids <- function(input) {
    
    # ======================
    # SHAPEFILE
    # ======================
    if (!is.null(input$zone_shapefile)) {
      
      sf_object <- read_shapefile(input$zone_shapefile)
      
      if (is.null(sf_object)) {
        stop("Gagal membaca shapefile.")
      }
      
      cols <- names(sf_object)
      
      if (length(cols) < 2) {
        stop("Shapefile minimal harus memiliki 2 kolom.")
      }
      
      sf_object <- sf_object %>%
        dplyr::rename(
          Value = all_of(cols[1]),
          planning_unit = all_of(cols[2])
        )
      
      lookup_df <- sf_object %>%
        sf::st_drop_geometry() %>%
        dplyr::select(Value, planning_unit) %>%
        distinct()
      
      return(lookup_df)
    }
    
    # ======================
    # RASTER + LOOKUP TABLE
    # ======================
    if (!is.null(input$zone_raster) &&
        !is.null(input$lookup_zone)) {
      
      ext <- tools::file_ext(input$lookup_zone$name)
      
      lookup_df <- switch(
        tolower(ext),
        
        csv = readr::read_csv(
          input$lookup_zone$datapath,
          show_col_types = FALSE
        ),
        
        xlsx = openxlsx::read.xlsx(
          input$lookup_zone$datapath
        ),
        
        stop("Format lookup table tidak didukung.")
      )
      
      colnames(lookup_df)[1:2] <- c(
        "Value",
        "planning_unit"
      )
      
      return(
        lookup_df %>%
          dplyr::select(Value, planning_unit) %>%
          distinct()
      )
    }
    
    # ======================
    # RASTER ONLY
    # ======================
    if (!is.null(input$zone_raster)) {
      
      PU <- terra::rast(input$zone_raster$datapath)
      
      PU_ID <- unique(terra::values(PU))
      
      return(
        data.frame(
          Value = PU_ID,
          planning_unit = paste0("PU_", PU_ID)
        )
      )
    }
    
    return(NULL)
  }
  
  observeEvent(
    list(
      input$zone_type,
      input$zone_raster,
      input$zone_shapefile,
      input$lookup_zone
    ),
    {
      
      shinyjs::hide("download_template")
      
    }
  )
  
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
  
  template_data <- eventReactive(input$generate_template, {
    
    pu_table <- get_pu_ids(input)
    
    validate(
      need(
        !is.null(pu_table) && nrow(pu_table) > 0,
        "Unit Perencanaan tidak terdeteksi."
      )
    )
    
    # ======================
    # BASE COLUMNS
    # ======================
    cols <- c(
      "ID",
      "UNIT_PERENCANAAN",
      "SATUAN"
    )
    
    # ======================
    # SINGLE FERTILIZER
    # ======================
    if (input$use_single == "yes") {
      
      single_names <- sapply(
        1:input$n_single,
        function(i) input[[paste0("single_name_", i)]]
      )
      
      single_names <- single_names[
        single_names != ""
      ]
      
      cols <- c(
        cols,
        paste0("Tunggal_", single_names)
      )
    }
    
    # ======================
    # COMPOUND FERTILIZER
    # ======================
    if (input$use_compound == "yes") {
      
      compound_names <- sapply(
        1:input$n_compound,
        function(i) input[[paste0("compound_name_", i)]]
      )
      
      compound_names <- compound_names[
        compound_names != ""
      ]
      
      cols <- c(
        cols,
        paste0("Majemuk_", compound_names)
      )
    }
    
    # ======================
    # BUILD TEMPLATE
    # ======================
    df <- data.frame(
      matrix(
        NA,
        nrow = nrow(pu_table),
        ncol = length(cols)
      )
    )
    
    colnames(df) <- cols
    
    df$ID <- pu_table$Value
    df$UNIT_PERENCANAAN <- pu_table$planning_unit
    df$SATUAN <- "Kg/Ha"
    
    df
    
  })
  
  observeEvent(input$generate_template, {
    
    req(template_data())
    
    shinyjs::show("download_template")
    
    showNotification(
      "Template berhasil dibuat.",
      type = "message"
    )
    
  })
  
  output$download_template <- downloadHandler(
    
    filename = function() {
      paste0(
        "fertilizer_template_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      req(input$generate_template > 0)
      
      template <- template_data()
      
      req(nrow(template) > 0)
      
      wb <- openxlsx::createWorkbook()
      
      openxlsx::addWorksheet(
        wb,
        "Template_Pupuk"
      )
      
      openxlsx::writeData(
        wb,
        sheet = "Template_Pupuk",
        x = template
      )
      
      openxlsx::saveWorkbook(
        wb,
        file,
        overwrite = TRUE
      )
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
      paste0("Direktori keluaran terpilih: ",  rv$wd)
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
      need(rv$lulc, "Silakan unggah peta tutupan/penggunaan lahan (LULC)"),
      need(rv$year, "Silakan isi tahun peta"),
      need(input$zone_type, "Silakan pilih tipe input unit perencanaan"),
      need(rv$zone_input, "Silakan unggah data unit perencanaan"),
      need(
        if(input$zone_type == "raster") rv$lookup_zone else TRUE,
        "Silakan unggah tabel referensi unit perencanaan (khusus raster)"
      ),
      need(rv$lc_table, "Silakan unggah tabel referensi tutupan lahan"),
      need(rv$conversion_table, "Silakan unggah tabel parameter emisi dan konversi"),
      need(rv$pupuk_table, "Silakan unggah tabel dosis pupuk"),
      need(rv$wd != "", "Silakan pilih direktori keluaran"))
    TRUE
  })
  
  observeEvent(input$process, {
    print("BUTTON CLICKED")
    
    print("STEP 1: before validation")
    validate_inputs()
    print("STEP 2: after validation")
    
    rv$wd <- parseDirPath(volumes, input$wd)
    req(rv$wd)
    
    print("STEP 3: before preprocess")
    
    rv$wd <- parseDirPath(volumes, input$wd)
    req(validate_inputs(), rv$wd)
    showNotification("Analisis sedang berjalan, mohon tunggu...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Menjalankan Analisis', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Memulai analisis...")
        
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
        
        incProgress(0.5, detail = "Menyusun laporan...")
        
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
        
        incProgress(0.9, detail = "Selesai.")
        
        output$status_messages <- renderText("Analisis berhasil diselesaikan!")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        showNotification("Analisis berhasil diselesaikan!", type = "message")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Terjadi kesalahan dalam analisis:", e$message))
        output$error_messages <- renderText(paste("Terjadi kesalahan dalam analisis:", e$message))
        showNotification("Terjadi kesalahan dalam analisis. Silakan periksa pesan error.", type = "error")
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
      showNotification("File laporan tidak ditemukan", type = "error")
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
      showNotification("Direktori keluaran tidak ditemukan", type = "error")
    }
  })
  
  # Handle session end -------------------------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Return to Main Menu button observer -------------------------------------
  observeEvent(input$returnButton, {
    shinyalert(
      title = "Konfirmasi",
      text =  "Apakah Anda ingin kembali ke menu utama?",
      showCancelButton = TRUE,
      size = "xs",
      type = "warning",
      inputId = "alert"
    )
  })
  
  observeEvent(input$alert, {
    if(input$alert) {
      js$closeWindow()
      message("Kembali ke menu utama")  
      shinyjs::delay(1000, stopApp())
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)