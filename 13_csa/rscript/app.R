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
  
  extendShinyjs(
    text = jscode,
    functions = c("closeWindow")
  ),
  
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "favicon.ico"
    )
  ),
  
  titlePanel("Modul Emisi Karbon - Lahan Pertanian"),
  
  sidebarLayout(
    
    ###########################################################
    ## SIDEBAR
    ###########################################################
    
    sidebarPanel(
      
      ###########################################################
      ## STEP 1
      ###########################################################
      
      h4("1. Unit Perencanaan"),
      
      radioButtons(
        "zone_type",
        "Tipe Input Unit Perencanaan",
        choices = c(
          "Raster" = "raster",
          "Shapefile" = "shapefile"
        ),
        selected = "shapefile"
      ),
      
      conditionalPanel(
        
        condition = "input.zone_type == 'raster'",
        
        fileInput(
          "zone_raster",
          "Peta Unit Perencanaan (Raster)",
          accept = c(".tif", ".tiff")
        ),
        
        fileInput(
          "lookup_zone",
          "Tabel Referensi Unit Perencanaan",
          accept = c(".csv", ".xlsx")
        )
        
      ),
      
      conditionalPanel(
        
        condition = "input.zone_type == 'shapefile'",
        
        fileInput(
          "zone_shapefile",
          "Peta Unit Perencanaan (Shapefile)",
          multiple = TRUE,
          accept = c(
            ".shp",
            ".dbf",
            ".prj",
            ".shx"
          )
        )
        
      ),
      
      hr(),
      
      ###########################################################
      ## STEP 2
      ###########################################################
      
      conditionalPanel(
        
        condition = "
          (input.zone_type == 'raster' && input.zone_raster != '') ||
          (input.zone_type == 'shapefile' && input.zone_shapefile.length > 0)
        ",
        
        h4("2. Generate Template Parameter Emisi & Konversi (Opsional)"),
        
        tags$p(
          "Workbook akan dibuat dengan:",
          tags$br(),
          "\u2713 Satu sheet untuk setiap Unit Perencanaan",
          tags$br(),
          "\u2713 Seluruh parameter emisi dan konversi telah terisi nilai default, silahkan mengubah nilainya jika diperlukan"
        ),
        
        actionButton(
          "generate_template",
          "Generate Template",
          style="
            background-color:#FFA500;
            color:white;
            font-size:18px;
            padding:10px 15px;
            width:100%;
          "
        ),
        
        br(),
        br(),
        
        hidden(
          
          downloadButton(
            "download_template",
            "Download Template",
            style="
              font-size:18px;
              padding:10px 15px;
              width:100%;
            "
          )
          
        )
        
      ),
      
      hr(),
      
      ###########################################################
      ## STEP 3
      ###########################################################
      
      conditionalPanel(
        
        condition = "input.generate_template > 0",
        
        h4("3. Data Analisis"),
        
        fileInput(
          "lulc",
          "Peta Tutupan Lahan",
          accept = c(".tif", ".tiff")
        ),
        
        textInput(
          "year",
          "Tahun Peta",
          value = "2020"
        ),
        
        fileInput(
          "lc_table",
          "Tabel Referensi Tutupan Lahan",
          accept = c(".csv", ".xlsx")
        ),
        
        fileInput(
          "parameter_table",
          "Template Parameter Emisi & Konversi",
          accept = c(".xlsx")
        ),
        
        verbatimTextOutput(
          "validation_message"
        )
        
      ),
      
      hr(),
      
      ###########################################################
      ## STEP 4
      ###########################################################
      
      h4("4. Output"),
      
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        
        shinyDirButton(
          "wd",
          "Pilih Direktori Keluaran",
          "Pilih Direktori",
          style = "font-size:18px; padding:10px 15px;"
        ),
        
        textOutput("selected_directory"),
        
        actionButton(
          "process",
          "Jalankan Analisis",
          style = "font-size:18px; padding:10px 15px; background-color:#4CAF50; color:white;"
        ),
        
        hidden(
          actionButton(
            "open_report",
            "Buka Laporan",
            style = "font-size:18px; padding:10px 15px; background-color:#008CBA; color:white;"
          )
        ),
        
        hidden(
          actionButton(
            "open_output_folder",
            "Buka Folder Keluaran",
            style = "font-size:18px; padding:10px 15px; background-color:#008CBA; color:white;"
          )
        ),
        
        actionButton(
          "returnButton",
          "Kembali ke Menu Utama",
          style = "font-size:18px; padding:10px 15px; background-color:#FA8072; color:white;"
        )
      )
      
    ),
    
    ###########################################################
    ## MAIN PANEL
    ###########################################################
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          
          "User Guide",
          
          uiOutput(
            "user_guide"
          )
          
        ),
        
        tabPanel(
          
          "Log",
          
          br(),
          
          textOutput(
            "selected_dir"
          ),
          
          verbatimTextOutput(
            "status_messages"
          ),
          
          verbatimTextOutput(
            "error_messages"
          )
          
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
    
    # Main inputs
    lulc = NULL,
    year = NULL,
    zone_type = NULL,
    
    # Planning unit input
    zone_input = NULL,            # raster OR shapefile
    lookup_zone = NULL,           # only for raster
    
    # Lookup tables
    lc_table = NULL,
    parameter_table = NULL
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
  
  
  parameter_template <- eventReactive(input$generate_template, {
    
    pu_table <- get_pu_ids(input)
    
    validate(
      need(
        !is.null(pu_table) && nrow(pu_table) > 0,
        "Unit Perencanaan tidak terdeteksi."
      )
    )
    
    template <- data.frame(
      
      ID = 1:19,
      
      Name = c(
        "0% dosis pupuk",
        "50% dosis pupuk",
        "100% dosis pupuk",
        "Tidak ditanami padi",
        "Rotasi padi 1 kali",
        "Rotasi padi 2-3 kali",
        "N UREA",
        "N NPK 15-10-12",
        "GWP CO2",
        "GWP CH4",
        "GWP N2O",
        "Faktor Emisi N2O Lahan Sawah",
        "Faktor Emisi CO2",
        "Faktor Emisi CH4 per luas panen",
        "Scaling Factor Rejim Air",
        "Scaling Factor Jenis Tanah",
        "Scaling Factor Varietas Padi",
        "Cultivation Period of Rice",
        "Dosis Pupuk Tunggal Urea"
      ),
      
      Variable = c(
        "dosis0",
        "dosis50",
        "dosis100",
        "rotasi0",
        "rotasi1",
        "rotasi2",
        "N_UREA",
        "N_NPK",
        "GWP_CO2",
        "GWP_CH4",
        "GWP_N2O",
        "EF_N2O",
        "EF_CO2",
        "EF",
        "SFw",
        "SFs",
        "SFr",
        "t",
        "UREA"
      ),
      
      Value = c(
        0.0639,
        0.9025,
        0.0336,
        15635,
        173243,
        403316,
        0.46,
        0.15,
        1,
        27.2,
        273,
        0.003,
        0.2,
        1.61,
        1,
        1,
        1,
        240,
        300
      ),
      
      Satuan = c(
        rep("unitless",3),
        rep("Ha",3),
        rep("unitless",2),
        rep("unitless",3),
        "unitless",
        "unitless",
        "kg/Ha/hari",
        rep("unitless",3),
        "hari",
        "kg/Ha/season"
      ),
      
      Keterangan = c(
        "Tidak menerima bantuan pupuk",
        "Menerima bantuan pupuk dengan subsidi harga",
        "Menerima bantuan pupuk gratis",
        "Luas lahan yang tidak ditanami padi",
        "Luas lahan yang ditanami padi 1 kali",
        "Luas lahan yang ditanami padi 2-3 kali",
        "",
        "",
        "Berdasarkan IPCC 2021",
        "Berdasarkan IPCC 2021",
        "Berdasarkan IPCC 2021",
        "",
        "",
        "Baseline EF untuk Sawah tergenang tanpa BO",
        "Scaling Faktor untuk Perbedaan Rejim Air",
        "Scaling Faktor Jenis Tanah",
        "Scaling Faktor Varietas Padi",
        "Lama budidaya padi dalam setahun",
        "Default dosis pupuk tunggal urea"
      ),
      
      stringsAsFactors = FALSE
    )
    
    list(
      pu_table = pu_table,
      template = template
    )
    
  })
  
  observeEvent(input$generate_template, {
    
    req(parameter_template())
    
    shinyjs::show("download_template")
    
    showNotification(
      "Template Parameter Emisi & Konversi berhasil dibuat.",
      type = "message"
    )
    
  })
  
  output$download_template <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Parameter_Emisi_dan_Konversi_",
        Sys.Date(),
        ".xlsx"
      )
      
    },
    
    content = function(file) {
      
      req(parameter_template())
      
      data <- parameter_template()
      
      wb <- openxlsx::createWorkbook()
      
      for(i in seq_len(nrow(data$pu_table))){
        
        sheet_name <- data$pu_table$planning_unit[i]
        
        ## maksimal 31 karakter (aturan Excel)
        sheet_name <- substr(sheet_name,1,31)
        
        openxlsx::addWorksheet(
          wb,
          sheet_name
        )
        
        openxlsx::writeData(
          wb,
          sheet = sheet_name,
          x = data$template
        )
        
      }
      
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
    rv$parameter_table <- input$parameter_table
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
      need(
        rv$parameter_table,
        "Silakan unggah Template Parameter Emisi & Konversi"
      ),
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
          pathPU = if (input$zone_type == "raster") {
            rv$zone_input$datapath
          } else {
            rv$zone_input
          },
          pathLookupPU = if (input$zone_type == "raster") {
            rv$lookup_zone$datapath
          } else {
            NULL
          },
          pathLookupLC = rv$lc_table$datapath,
          pathParameter = rv$parameter_table$datapath,
          year = rv$year
        )
        
        incProgress(0.5, detail = "Menyusun laporan...")
        
        end_time <- Sys.time()
        
        paths <- list(
          pathLULCT = rv$lulc$datapath,
          pathPU = if (input$zone_type == "raster") {
            rv$zone_input$datapath
          } else {
            paste(sapply(rv$zone_input$datapath, basename), collapse = ", ")
          },
          pathLookupPU = if (input$zone_type == "raster") {
            rv$lookup_zone$datapath
          } else {
            NULL
          },
          pathLookupLC = rv$lc_table$datapath,
          pathParameter = rv$parameter_table$datapath
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