# ==============================================================================
# app.R
# Regional Economic Descriptive Analysis - Shiny App
# ==============================================================================

source("../../helper.R")

# --- Load functions_ta_regeco1.R ---
if (file.exists("functions_ta_regeco1.R")) {
  source("functions_ta_regeco1.R")
} else {
  source("08_ta-regional1/rscript/functions_ta_regeco1.R")
}

# --- Required packages ---
required_packages <- c(
  "terra", "dplyr", "tidyterra", "ggplot2", "forcats", "stringr",
  "cowplot", "networkD3", "scales", "purrr", "rmarkdown",
  "kableExtra", "htmlTable", "knitr", "magrittr", "tidyr",
  "rlang", "stats", "utils", "methods", "sf", "ggrepel",
  "viridis", "textclean", "shiny", "shinydashboard", "shinyjs",
  "shinyFiles", "bslib", "shinyalert"
)

check_and_install_packages(required_packages)

# --- Load TARegEco package ---
if (!requireNamespace("TARegEco", quietly = TRUE)) {
  if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
  devtools::install_github("icraf-indonesia/TARegEco")
}
library(TARegEco)

# --- JS untuk close window ---
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  
  titlePanel("Modul Analisis Deskriptif Ekonomi Regional"),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
      .alloc-label {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 4px;
        width: 100%;
      }
      .alloc-info-row {
        display: flex;
        align-items: center;
        gap: 6px;
        margin-bottom: 6px;
      }
      .alloc-info-row .form-group {
        margin-bottom: 0;
        flex: 1;
      }
      .alloc-help-icon {
        display: inline-flex;
        align-items: center;
        color: #0d6efd;
        cursor: pointer;
        flex-shrink: 0;
        margin-top: 2px;
        line-height: 1;
      }
      .alloc-help-icon a,
      .alloc-help-icon .action-button {
        color: inherit !important;
        text-decoration: none !important;
        background: none !important;
        border: none !important;
        padding: 0 !important;
        cursor: pointer !important;
      }
      .alloc-help-icon:hover,
      .alloc-help-icon:hover a {
        color: #0a58ca !important;
      }
      .dir-btn-wrapper {
        position: relative;
        width: 100%;
      }
      .dir-btn-wrapper .btn {
        width: 100%;
        text-align: center;
      }
      .dir-help-overlay {
        position: absolute;
        right: 12px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 10;
      }
        "))
      ),
      # --- Opsi: Load dari RDS preprocessing (opsional) ---
      checkboxInput(
        "load_rds",
        tagList(
          "Muat dari RDS praproses (lewati pembacaan Excel)",
          tooltip(
            tags$span(
              class = "alloc-help-icon",
              icon("circle-question")
            ),
            HTML(
              "<b>Apa itu RDS?</b><br>",
              "RDS adalah format file dari R (R Data Serialized) yang menyimpan ",
              "objek hasil olahan data dalam bentuk terkompresi. File ini mempercepat ",
              "proses karena semua input (tabel IO, tenaga kerja, lahan, dll.) ",
              "sudah dibaca dan divalidasi sebelumnya.<br><br>",
              "<b>Darimana mendapatkannya?</b><br>",
              "File <code style='background:#2c3e50; color:#ffffff; padding:2px 6px; border-radius:4px;'>",
              "descriptive_input_preprocessing.rds</code> dihasilkan ",
              "otomatis setiap kali Anda menjalankan analisis deskriptif ",
              "dengan input Excel. File ini tersimpan di folder output yang Anda pilih.<br><br>",
              "Gunakan RDS jika Anda ingin mengulang analisis atau melanjutkan ke modul proyeksi tanpa harus mengunggah ulang file Excel."
            ),
            placement = "right"
          )
        ),
        value = FALSE
      ),
      tags$div(
        class = "alert alert-info",
        style = "font-size: 12px; margin-top: 5px; padding: 8px;",
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600;",
            "ℹ️ Apa itu RDS?"
          ),
          tags$ul(
            style = "margin-top: 6px; padding-left: 18px;",
            tags$li(
              tags$br(),
              tags$b("Apa itu RDS?"),
              tags$br(),
              "RDS (", tags$code(".rds"), ") adalah format file dari R (R Data Serialized) yang menyimpan ",
              "objek hasil olahan data dalam bentuk terkompresi. File ini mempercepat ",
              "proses karena semua input (tabel IO, tenaga kerja, lahan, dll.) ",
              "sudah dibaca dan divalidasi sebelumnya."
            ),
            tags$li(
              tags$b("Darimana?"),
              tags$br(),
              " File ", tags$code("descriptive_input_preprocessing.rds"),
              " dihasilkan otomatis setiap kali Anda menjalankan analisis deskriptif ",
              "dengan input Excel. Cek folder output yang Anda pilih pada langkah sebelumnya."
            ),
            tags$li(
              tags$b("Kapan dipakai?"),
              tags$br(),
              " Saat mengulang analisis atau melanjutkan ke modul proyeksi ",
              "tanpa ingin mengunggah ulang file Excel."
            )
          )
        )
      ),
      
      # ==========================================
      # KONDISI 1: Jika load_rds == TRUE
      # ==========================================
      conditionalPanel(
        condition = "input.load_rds == true",
        fileInput(
          "preprocessing_rds_file",
          label = label_with_help("Preprocessing RDS", "preprocessing_rds_file"),
          accept = c(".rds"),
          placeholder = "Unggah berkas preprocessing (.rds)"
        ),
        tags$div(
          class = "alloc-info-row",
          checkboxInput(
            "new_lc_area",
            "Pembaharuan Luas Tutupan Lahan",
            value = FALSE
          ),
          regeco_help_icon("new_lc_area")
        ),
        # Land Use Lookup Table hanya muncul jika new_lc_area dicentang
        conditionalPanel(
          condition = "input.new_lc_area == true",
          fileInput(
            "landuse_table_file",
            label = label_with_help("Tabel Luas Tutupan Lahan (ha)", "landuse_table_file_rds"),
            accept = c(".xlsx", ".xls"),
            placeholder = "Unggah berkas tabel (.xlsx)"
          )
        )
      ),
      
      # ==========================================
      # KONDISI 2: Jika load_rds == FALSE
      # ==========================================
      conditionalPanel(
        condition = "input.load_rds == false",
        # Input IO Excel
        fileInput(
          "io_file",
          label = label_with_help("Tabel Input/Output", "io_file"),
          accept = c(".xlsx", ".xls"),
          placeholder = "Unggah berkas tabel (.xlsx)"
        ),
        # Input Land Use Lookup Table
        fileInput(
          "landuse_table_file",
          label = label_with_help("Tabel Luas Tutupan Lahan (ha)", "landuse_table_file"),
          accept = c(".xlsx", ".xls"),
          placeholder = "Unggah berkas tabel (.xlsx)"
        )
      ),
      
      # --- Metadata (read-only, dari profil) ---
      tags$hr(),
      tags$b("Metadata (auto-detected)"),
      verbatimTextOutput("metadata_preview"),
      
      tags$hr(),
      
      # --- Output directory ---
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          dir_button_with_help("wd", "Pilih Folder Output", "Silakan pilih direktori output", "wd"),
          textOutput("selected_directory"),
          actionButton("processTAReg1", "Run Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(actionButton("open_report", "Open Report",
                              style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")),
          hidden(actionButton("open_output_folder", "Open Output Folder",
                              style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")),
          actionButton("returnButton", "Return to Main Menu",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", uiOutput("user_guide")),
        tabPanel("Log",
                 br(),
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 plotOutput("result_plot"))
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    report_file  = NULL,
    load_rds     = FALSE,
    new_lc_area  = FALSE
  )
  
  # --- Output directory ---
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "wd", roots = volumes, session = session)
  
  selected_output_dir <- reactive({
    selection <- input$wd
    if (is.null(selection)) return(NULL)

    path <- parseDirPath(volumes, selection)
    if (length(path) != 1L || is.na(path) || !dir.exists(path)) return(NULL)
    path
  })
  
  output$selected_directory <- renderText({
    path <- selected_output_dir()
    if (is.null(path)) return("")
    paste0("Folder output terpilih: ", path)
  })
  
  #### Click-to-open help modals ####
  lapply(names(regeco_help_texts), function(id) {
    observeEvent(input[[paste0("regeco_help_", id)]], {
      info <- regeco_help_texts[[id]]
      showModal(modalDialog(
        title = tagList(icon("circle-info"), " ", info$title),
        HTML(info$body),
        easyClose = TRUE,
        footer = modalButton("Tutup"),
        size = "m"
      ))
    })
  })
  
  # --- User guide ---
  output$user_guide <- renderUI({
    guide_paths <- c(
      "helpfile/ta-reg1_quick_user_guide_ID.Rmd",
      "../../08_ta-regional1/helpfile/ta-reg1_quick_user_guide_ID.Rmd"
    )
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    HTML("<p>User guide file not found.</p>")
  })
  
  # --- Sync checkbox ke reactive values ---
  observeEvent(input$load_rds,    { rv$load_rds    <- isTRUE(input$load_rds) })
  observeEvent(input$new_lc_area, { rv$new_lc_area <- isTRUE(input$new_lc_area) })

  uploaded_landuse_metadata <- reactive({
    file <- input$landuse_table_file
    if (is.null(file)) return(NULL)
    read_landuse_metadata(file$datapath)
  })
  
  # --- Preview metadata ---
  output$metadata_preview <- renderText({
    if (rv$load_rds) {
      if (is.null(input$preprocessing_rds_file)) {
        return("RDS: menunggu file...")
      }
      obj <- tryCatch(
        readRDS(input$preprocessing_rds_file$datapath),
        error = function(e) NULL
      )
      if (is.null(obj$io$profile)) return("RDS: gagal dibaca.")
      profile <- obj$io$profile
      data_source <- "RDS"
    } else {
      if (is.null(input$io_file)) return("Belum ada input.")
      profil_io <- tryCatch(
        read_input_file(input$io_file$datapath,
                        sheet = "0. Profil Data",
                        skip = 2, header = TRUE),
        error = function(e) NULL
      )
      if (is.null(profil_io) || nrow(profil_io) < 3) {
        return("Excel: gagal membaca sheet '0. Profil Data'.")
      }
      profile <- list(
        area_name = trimws(profil_io$Deskripsi[1]),
        year      = as.numeric(profil_io$Deskripsi[2]),
        currency  = trimws(profil_io$Deskripsi[3]),
        io_source = trimws(profil_io$Deskripsi[4])
      )
      data_source <- "Excel"
    }

    landuse <- profile$landuse
    if (!rv$load_rds || rv$new_lc_area) {
      landuse <- tryCatch(uploaded_landuse_metadata(), error = function(e) NULL)
    }
    paste(
      c(
        paste0("Sumber input: ", data_source),
        paste0("Wilayah: ", profile$area_name),
        paste0("Tahun IO: ", profile$year),
        paste0("Mata uang: ", profile$currency),
        paste0("Sumber IO: ", profile$io_source %||% "-"),
        paste0("Tahun luas tutupan lahan: ", landuse$year %||% "Belum tercatat"),
        paste0("Sumber luas tutupan lahan: ", landuse$source %||% "Belum tercatat"),
        landuse_year_warning(profile$year, landuse$year)
      ),
      collapse = "\n"
    )
  })
  
  # --- Process Analysis ---
  observeEvent(input$processTAReg1, {
    
    # --- Validasi input ---
    missing_inputs <- c()
    
    if (!rv$load_rds) {
      if (is.null(input$io_file))            missing_inputs <- c(missing_inputs, "Tabel Input/Output")
      if (is.null(input$landuse_table_file)) missing_inputs <- c(missing_inputs, "Tabel Luas Tutupan Lahan (ha)")
    } else {
      if (is.null(input$preprocessing_rds_file)) {
        missing_inputs <- c(missing_inputs, "Preprocessing RDS")
      }
      if (rv$new_lc_area && is.null(input$landuse_table_file)) {
        missing_inputs <- c(missing_inputs, "Tabel Luas Tutupan Lahan (ha)")
      }
    }
    
    output_dir <- selected_output_dir()
    if (is.null(output_dir)) {
      missing_inputs <- c(missing_inputs, "Folder Output")
    }
    
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Silakan lengkapi input berikut:",
              paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    withProgress(message = "Running TA Regional Economy Descriptive Analysis", value = 0, {
      tryCatch({
        
        start_time <- Sys.time()
        
        # =====================================================================
        # PREPROCESSING & VALIDATION (mengikuti alur notebook)
        # =====================================================================
        incProgress(0.1, detail = "Reading inputs")
        
        load_rds    <- isTRUE(rv$load_rds)
        new_lc_area <- isTRUE(rv$new_lc_area)
        
        if (!load_rds) {
          io_file_path            <- input$io_file$datapath
          landuse_table_file_path <- input$landuse_table_file$datapath
          
          # --- Read inputs (persis notebook) ---
          profil_io         <- read_input_file(io_file_path, sheet = "0. Profil Data", skip = 2, header = TRUE)
          sektor_ekonomi    <- read_input_file(io_file_path, sheet = "1. Sektor Ekonomi", skip = 2, header = TRUE)
          permintaan_antara <- read_input_file(io_file_path, sheet = "2. Permintaan Antara", skip = 8, as_matrix = TRUE, header = TRUE)
          permintaan_akhir  <- read_input_file(io_file_path, sheet = "3. Permintaan Akhir", skip = 3, header = TRUE)
          input_primer      <- read_input_file(io_file_path, sheet = "4. Input Primer", skip = 2, header = TRUE)
          tenaga_kerja      <- read_input_file(io_file_path, sheet = "5. Tenaga Kerja", skip = 2, header = TRUE)
          proposi_lahan     <- read_input_file(io_file_path, sheet = "6. Proposi Lahan", skip = 9, as_matrix = TRUE, header = TRUE)
          laju_pertumbuhan  <- read_input_file(io_file_path, sheet = "7. Laju Pertumbuhan PDRB", skip = 2, header = TRUE)
          luas_area         <- read_input_file(landuse_table_file_path, skip = 2, header = TRUE)
        } else {
          # --- Reload from RDS ---
          input_preprocessing <- readRDS(input$preprocessing_rds_file$datapath)
          message(paste0("Using loaded preprocessing RDS file, generated on ",
                         input_preprocessing$metadata$timestamp))
          
          if (new_lc_area) {
            landuse_table_file_path <- input$landuse_table_file$datapath
            luas_area <- read_input_file(landuse_table_file_path, skip = 2, header = TRUE)
          }
        }
        
        # =====================================================================
        # PREPARE DATA (persis notebook)
        # =====================================================================
        incProgress(0.1, detail = "Preparing data")
        
        if (!load_rds) {
          # --- Prepare profile ---
          profile <- list(
            area_name = trimws(profil_io$Deskripsi[1]),
            year      = as.numeric(profil_io$Deskripsi[2]),
            currency  = trimws(profil_io$Deskripsi[3]),
            io_source = trimws(profil_io$Deskripsi[4])
          )
          
          # --- Prepare sectors ---
          sector <- sektor_ekonomi %>%
            dplyr::select(2:4) %>%
            stats::setNames(c("Nama_Sektor", "Kategori", "Berbasis_Lahan")) %>%
            dplyr::filter(complete.cases(.))
          
          # --- Prepare intermediate consumption ---
          permintaan_antara <- permintaan_antara[
            rowSums(is.na(permintaan_antara)) != ncol(permintaan_antara),
            colSums(is.na(permintaan_antara)) != nrow(permintaan_antara)
          ]
          
          intermediate_consumption.m <- matrix(
            as.numeric(permintaan_antara[, -1]),
            nrow = nrow(permintaan_antara),
            ncol = ncol(permintaan_antara) - 1
          )
          
          # --- Prepare final demand ---
          final_demand.m <- permintaan_akhir %>%
            dplyr::filter(rowSums(is.na(dplyr::select(., -`Sektor Ekonomi`))) <
                            ncol(dplyr::select(., -`Sektor Ekonomi`))) %>%
            tibble::column_to_rownames(var = "Sektor Ekonomi") %>%
            as.matrix()
          
          final_demand_component <- tibble::tibble(
            ID = seq_along(colnames(permintaan_akhir)[-1]) + 300,
            Nama_Komponen = colnames(permintaan_akhir)[-1]
          )
          
          # --- Prepare added value ---
          added_value.m <- input_primer %>%
            dplyr::select(-ID) %>%
            dplyr::select(where(~ !all(is.na(.)))) %>%
            dplyr::filter(rowSums(is.na(dplyr::select(., -`Sektor Ekonomi`))) <
                            ncol(dplyr::select(., -`Sektor Ekonomi`))) %>%
            tibble::column_to_rownames(var = "Sektor Ekonomi") %>%
            as.matrix()
          
          added_value_lookup <- input_primer %>% dplyr::select(1:2)
          
          # --- Prepare labour ---
          labour <- tenaga_kerja$`Tenaga Kerja (Jiwa)`
          labour <- labour[!is.na(labour)]
          
          # --- Prepare land proportion ---
          land_proportion <- apply(proposi_lahan[, -1], 2, as.numeric)
          land_proportion <- land_proportion[!apply(is.na(land_proportion), 1, all), , drop = FALSE]
          land_proportion <- land_proportion[, !apply(is.na(land_proportion), 2, all), drop = FALSE]
          dimnames(land_proportion) <- NULL
          
          # --- Prepare landuse table ---
          landuse_table <- luas_area
          colnames(landuse_table)[1:3] <- c("ID", "LC", "area")
          
          # --- Prepare growth rate ---
          growth_rate <- laju_pertumbuhan %>% dplyr::pull(`Laju Pertumbuhan`)
          growth_rate <- growth_rate[!is.na(growth_rate)]
          
          # --- Build Full IO Table & Code Registry ---
          full_io_result <- build_full_io_table(
            sector = sector,
            intermediate_consumption_m = intermediate_consumption.m,
            final_demand_m = final_demand.m,
            final_demand_component = final_demand_component,
            added_value_m = added_value.m,
            added_value_lookup = added_value_lookup
          )
          
          full_io_table <- full_io_result$full_io_table
          code_registry <- full_io_result$code_registry
        } else {
          if (new_lc_area) {
            landuse_table <- luas_area
            colnames(landuse_table)[1:3] <- c("ID", "LC", "area")
            input_preprocessing$landuse_table <- landuse_table
          }
          
          profile                    <- input_preprocessing$io$profile
          sector                     <- input_preprocessing$io$sector
          intermediate_consumption.m <- input_preprocessing$io$intermediate_consumption
          final_demand.m             <- input_preprocessing$io$final_demand
          final_demand_component     <- input_preprocessing$io$final_demand_component
          added_value.m              <- input_preprocessing$io$added_value
          added_value_lookup         <- input_preprocessing$io$added_value_lookup
          labour                     <- input_preprocessing$labour
          land_proportion            <- input_preprocessing$land_proportion
          landuse_table              <- input_preprocessing$landuse_table
          growth_rate                <- input_preprocessing$growth_rate
          full_io_table              <- input_preprocessing$io$full_io_table
          code_registry              <- input_preprocessing$io$code_registry
        }

        if (!load_rds || new_lc_area) {
          profile$landuse <- uploaded_landuse_metadata()
        }
        year_warning <- landuse_year_warning(profile$year, profile$landuse$year)
        if (!is.null(year_warning)) {
          showNotification(year_warning, type = "warning", duration = 10)
        }
        
        # =====================================================================
        # DERIVE SECTOR INDEX
        # =====================================================================
        incProgress(0.05, detail = "Deriving sector index")
        
        berbasis_lahan <- tolower(trimws(as.character(sector$Berbasis_Lahan)))
        constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
        unconstrained  <- 1 - constrained
        
        sector$Constrained   <- constrained
        sector$Unconstrained <- unconstrained
        
        # =====================================================================
        # VALIDATE INPUT
        # =====================================================================
        incProgress(0.05, detail = "Validating input")
        
        validation_status <- validate_io_data(
          sector,
          intermediate_consumption.m,
          final_demand.m,
          added_value.m,
          labour,
          land_proportion,
          landuse_table,
          growth_rate,
          tolerance_io = 0.01,
          stop_on_fail = TRUE
        )
        
        # =====================================================================
        # GET METADATA
        # =====================================================================
        incProgress(0.05, detail = "Getting metadata")
        
        io_metadata <- get_io_metadata(
          sector = sector,
          intermediate_consumption = intermediate_consumption.m,
          final_demand = final_demand.m,
          added_value = added_value.m,
          labour = labour,
          land_proportion = land_proportion,
          landuse_table = landuse_table,
          growth_rate = growth_rate,
          sector_units = "Unitless",
          ic_units = profile$currency,
          fd_units = profile$currency,
          av_units = profile$currency,
          labour_units = "Jiwa",
          lp_units = "Unitless",
          lu_units = "Hektar",
          gr_units = "Persentase"
        )
        io_metadata$timestamp <- Sys.time()
        
        metadata_txt_path <- file.path(output_dir, "metadata_input_data.txt")
      
        export_metadata_txt(io_metadata = io_metadata, output_path = metadata_txt_path)
        
        # =====================================================================
        # SAVE INPUT PREPROCESSING (RDS)
        # =====================================================================
        incProgress(0.05, detail = "Saving preprocessing RDS")
        
        io <- list(
          profile                  = profile,
          sector                   = sector,
          intermediate_consumption = intermediate_consumption.m,
          final_demand             = final_demand.m,
          final_demand_component   = final_demand_component,
          added_value              = added_value.m,
          added_value_lookup       = added_value_lookup,
          full_io_table            = full_io_table,
          code_registry            = code_registry
        )
        
        input_preprocessing <- list(
          io              = io,
          labour          = labour,
          land_proportion = land_proportion,
          landuse_table   = landuse_table,
          growth_rate     = growth_rate,
          status          = validation_status,
          metadata        = io_metadata
        )
        
        save_preprocessing_data(input_preprocessing, output_dir)
        
        # =====================================================================
        # CORE ANALYSIS (persis notebook)
        # =====================================================================
        incProgress(0.2, detail = "Running core analysis")
        
        balance_result <- check_io_balance(
          intermediate_consumption.m,
          final_demand.m,
          added_value.m,
          sector_names = sector$Nama_Sektor,
          tolerance_percent = 1
        )
        
        leontief_result <- compute_leontief(intermediate_consumption.m, added_value.m)
        linkage_result  <- compute_linkage(intermediate_consumption.m, added_value.m)
        linkages_table  <- create_linkages_table(sector, linkage_result)
        linkages_table  <- classify_quadrant(linkages_table)
        
        gdp_baseline <- calculate_gdp(
          added_value.m,
          intermediate_consumption.m,
          final_demand.m,
          sector,
          land_proportion,
          landuse_table,
          leontief_result$Leontief
        )
        
        OM <- calc_output_multiplier(leontief_result$Leontief, sector)
        IM <- calc_income_multiplier(
          leontief_result$Leontief,
          gdp_baseline$Total_OUTPUT,
          added_value.m, sector
        )
        LM <- calc_labour_multiplier(
          leontief_result$Leontief,
          labour,
          gdp_baseline$Total_OUTPUT, sector
        )
        SUBM <- calc_sub_multiplier(
          leontief_result$Leontief,
          gdp_baseline$OUTPUT,
          added_value.m,
          intermediate_consumption.m, sector
        )
        multiplier_table <- combine_multipliers(OM, IM, LM, SUBM)
        
        # =====================================================================
        # PREPARE REPORT DATA
        # =====================================================================
        core_analysis_results <- list(
          sector                   = sector,
          intermediate_consumption = intermediate_consumption.m,
          final_demand             = final_demand.m,
          added_value              = added_value.m,
          added_value_components   = get_added_value_components(intermediate_consumption.m, added_value.m),
          io_balance               = balance_result,
          leontief_result          = leontief_result,
          A                        = leontief_result$A,
          Leontief                 = leontief_result$Leontief,
          linkage_result           = linkage_result,
          linkages_table           = linkages_table,
          gdp_baseline             = gdp_baseline,
          output_multiplier        = OM,
          income_multiplier        = IM,
          labour_multiplier        = LM,
          sub_multiplier           = SUBM,
          multiplier_table         = multiplier_table,
          full_io_table            = full_io_table,
          code_registry            = code_registry,
          status                   = validation_status,
          metadata                 = io_metadata          
        )
        
        # =====================================================================
        # EXPORT EXCEL & RDS
        # =====================================================================
        incProgress(0.05, detail = "Exporting Excel & RDS")
        
        export_io_analysis(
          sector          = sector,
          profile         = profile,
          gdp_baseline    = gdp_baseline,
          linkage_result  = linkage_result,
          growth_rate     = growth_rate,
          labour          = labour,
          OM = OM, IM = IM, LM = LM, SUBM = SUBM,
          land_proportion = land_proportion,
          landuse_table   = landuse_table,
          linkage_table   = linkages_table,
          full_io_table   = full_io_table,
          code_registry   = code_registry,
          balance_detail  = balance_result$balance_detail,
          output_path     = file.path(output_dir, "Hasil_Analisis_Modul_Deskriptif.xlsx")
        )
        
        save_analysis_data(
          profile = profile,
          sector = sector,
          intermediate_consumption_m = intermediate_consumption.m,
          final_demand_m = final_demand.m,
          added_value_m = added_value.m,
          labour = labour,
          land_proportion = land_proportion,
          growth_rate = growth_rate,
          landuse_table = landuse_table,
          gdp_baseline = gdp_baseline,
          linkage_result = linkage_result,
          OM = OM, IM = IM, LM = LM, SUBM = SUBM,
          full_io_table = full_io_table,
          code_registry = code_registry,
          output_dir = output_dir
        )
        
        # =====================================================================
        # RENDER HTML REPORT
        # =====================================================================
        incProgress(0.05, detail = "Rendering HTML report")
        
        output_file <- paste0("ta_regional1_report_",
                              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        rv$report_file <- file.path(output_dir, output_file)
        
        report_template <- if (file.exists("report_template/ta-regeco1.Rmd")) {
          "report_template/ta-regeco1.Rmd"
        } else {
          "../../08_ta-regional1/report_template/ta-regeco1.Rmd"
        }
        
        rmarkdown::render(
          report_template,
          output_file = output_file,
          output_dir  = output_dir,
          params = list(
            start_time  = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
            end_time    = as.character(format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            results     = core_analysis_results,
            multiplier  = multiplier_table,
            balance     = balance_result,
            linkages    = linkages_table,
            gdp         = gdp_baseline,
            unit        = profile$currency,
            location    = profile$area_name,
            io_period   = profile$year,
            io_source   = profile$io_source,
            landuse_year = profile$landuse$year %||% "Belum tercatat",
            landuse_source = profile$landuse$source %||% "Belum tercatat",
            landuse_year_warning = year_warning %||% "",
            data_source = if (load_rds) "RDS" else "Excel",
            output_dir  = output_dir
          ),
          envir = new.env(parent = globalenv())
        )
        
        setProgress(1, message = "Processing Complete")
        showNotification("All outputs have been generated", type = "message")
        output$status_messages <- renderText("Analysis completed successfully!")
        
        shinyjs::show("open_report")
        shinyjs::show("open_output_folder")
        
      }, error = function(e) {
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  # --- Open folder ---
  observeEvent(input$open_output_folder, {
    path <- selected_output_dir()
    if (!is.null(path)) {
      if (.Platform$OS.type == "windows") shell.exec(path) else system2("open", args = shQuote(path))
    } else {
      showNotification("Output directory not found", type = "error")
    }
  })
  
  # --- Open report ---
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") shell.exec(rv$report_file) else system2("open", args = shQuote(rv$report_file))
    } else {
      showNotification("Report file not found", type = "error")
    }
  })
  
  # --- Return button ---
  observeEvent(input$returnButton, {
    shinyalert(
      title = "Confirmation",
      text = "Do you want to return to main menu?",
      showCancelButton = TRUE,
      size = "xs",
      type = "warning",
      inputId = "alert"
    )
  })
  
  observeEvent(input$alert, {
    if (input$alert) js$closeWindow()
  })
  
  session$onSessionEnded(function() stopApp())
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui, server)
