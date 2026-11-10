source('../../helper.R')

required_packages <- c(
  "terra", "dplyr", "tidyterra", "ggplot2", "forcats", "stringr",
  "cowplot", "networkD3", "scales", "purrr", "rmarkdown",
  "kableExtra", "htmlTable", "knitr", "magrittr", "tidyr",
  "rlang", "sf", "ggrepel", "readr", "viridis", "textclean",
  "shiny", "shinydashboard", "shinyjs", "shinyFiles", "bslib",
  "shinyalert", "writexl", "mapview", "leaflet"
)

if (file.exists("functions.R")){
  source("functions.R")
} else {
  source("09_ta-regional2/rscript/functions.R")
}

check_and_install_packages(required_packages)

# Cek fungsi kritis sudah ter-load
required_fns <- c(
  "load_projection_baseline",
  "load_scenario_inputs",
  "run_projection_scenario",
  "create_area_matrix",
  "read_input_file"
)
missing_fns <- required_fns[!vapply(required_fns, exists, logical(1))]
if (length(missing_fns) > 0) {
  stop("Fungsi belum ter-load: ", paste(missing_fns, collapse = ", "))
}
cat("âœ“ Semua fungsi kritis ter-load\n\n")

# =========================================================
# HELPER FUNCTIONS
# =========================================================
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

app_css <- "
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
  .scenario-box {
    border: 1px solid #dee2e6;
    border-radius: 6px;
    padding: 12px 14px;
    margin-top: 10px;
    background-color: #f8f9fa;
  }
  .section-title { font-weight: 600; margin: 6px 0; }
  .note { font-size: 12px; color: #6c757d; }
  .period-preview { font-size: 12px; color: #6c757d; margin: -6px 0 10px 0; }
  
  /* Mencegah klik pada downloadButton saat disabled */
  a.disabled, a[disabled] {
    pointer-events: none !important;
    opacity: 0.5 !important;
    cursor: not-allowed !important;
  }
  
  /* Custom CSS Notifikasi Toast */
  .shiny-notification-message { 
    background-color: #28a745 !important;
    color: white !important;
    border: 1px solid #1e7e34 !important;
    font-weight: 600;
  }
  .shiny-notification-error {
    background-color: #dc3545 !important;
    color: white !important;
    border: 1px solid #bd2130 !important;
    font-weight: 600;
  }
"

btn_style <- function(bg) {
  sprintf("font-size: 18px; padding: 10px 15px; background-color: %s; color: white;", bg)
}

xlsx_types <- c(".xlsx", ".xls")
rds_types  <- c(".rds")

# ---- Deteksi lokasi template Rmd ----
find_report_template <- function(filename = "ta-regeco-projection.Rmd") {
  candidates <- c(
    filename,                                        # WD = shiny/
    file.path("shiny", filename),                    # WD = TARegEco/
    file.path("..", "shiny", filename),
    file.path(dirname(getwd()), filename),
    file.path(getwd(), filename)
  )
  for (cand in candidates) {
    if (file.exists(cand)) {
      return(normalizePath(cand))
    }
  }
  for (search_root in c(".", "..", "../..")) {
    hits <- list.files(
      search_root,
      pattern    = paste0("^", gsub("\\.", "\\\\.", filename), "$"),
      recursive  = TRUE,
      full.names = TRUE
    )
    if (length(hits) > 0) {
      return(normalizePath(hits[1]))
    }
  }
  stop(
    "Template '", filename, "' tidak ditemukan.\n",
    "  Working dir : ", getwd()
  )
}

# ---- Indicator metadata ----
indicator_meta <- list(
  PDRB_Total_Juta          = list(label = "PDRB Total (Rp Juta)",                       col = "GDP"),
  Serapan_TK_Orang         = list(label = "Serapan Tenaga Kerja (Orang)",               col = "LABOUR"),
  Pendapatan_TK_Juta       = list(label = "Pendapatan Tenaga Kerja (Rp Juta)",          col = "INCOME"),
  Surplus_Usaha_Bruto_Juta = list(label = "Surplus Usaha Bruto (Rp Juta)",              col = "SURPLUS"),
  Output_Total_Juta        = list(label = "Output Total (Rp Juta)",                     col = "OUTPUT"),
  # ---- per-kapita ----
  PDRB_per_Kapita_Juta     = list(label = "PDRB per Kapita (Rp Juta/Orang)",            col = "GDP_PER_CAPITA"),
  Pendapatan_per_Kapita_Juta = list(label = "Pendapatan TK per Kapita (Rp Juta/Orang)", col = "INCOME_PER_CAPITA")
)

indicator_choices <- setNames(
  names(indicator_meta),
  vapply(indicator_meta, function(m) m$label, character(1))
)

# ---- Helper baca baseline GDP (untuk template download) ----
read_baseline_gdp <- function(use_rds, rds_file, xlsx_file) {
  if (use_rds) {
    if (is.null(rds_file)) return(NULL)
    obj <- tryCatch(readRDS(rds_file$datapath), error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    if (is.data.frame(obj) && "SECTOR" %in% names(obj)) return(as.data.frame(obj))
    if (is.list(obj) && !is.null(obj$gdp_baseline)) return(as.data.frame(obj$gdp_baseline))
    return(NULL)
  }
  if (is.null(xlsx_file)) return(NULL)
  
  # Deteksi nama sheet GDP secara fleksibel
  sheets <- tryCatch(readxl::excel_sheets(xlsx_file$datapath),
                     error = function(e) character(0))
  
  gdp_sheet_candidates <- c("GDP", "Analisis Deskriptif", "Analisis_Deskriptif",
                            "Gdp", "gdp")
  gdp_sheet <- intersect(gdp_sheet_candidates, sheets)[1]
  
  if (is.na(gdp_sheet) || length(gdp_sheet) == 0) {
    warning("[read_baseline_gdp] Sheet 'GDP' tidak ditemukan. ",
            "Sheet tersedia: ", paste(sheets, collapse = ", "))
    return(NULL)
  }
  
  as.data.frame(readxl::read_excel(xlsx_file$datapath, sheet = gdp_sheet))
}

# ---- Helper baca P_default (untuk template download) ----
read_baseline_p_default <- function(use_rds, rds_file, xlsx_file) {
  if (use_rds) {
    if (is.null(rds_file)) return(NULL)
    obj <- tryCatch(readRDS(rds_file$datapath), error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    if (is.list(obj)) {
      if (!is.null(obj$P_default)) return(as.matrix(obj$P_default))
      if (!is.null(obj$land_proportion)) return(as.matrix(obj$land_proportion))
      if (!is.null(obj$baseline$P_default)) return(as.matrix(obj$baseline$P_default))
      if (!is.null(obj$baseline$land_proportion)) return(as.matrix(obj$baseline$land_proportion))
    }
    return(NULL)
  }
  if (is.null(xlsx_file)) return(NULL)
  tryCatch({
    as.data.frame(readxl::read_excel(xlsx_file$datapath, sheet = "Land_Proportion"))
  }, error = function(e) NULL)
}

# ---- Helper baca nama LC (untuk template download) ----
read_baseline_lc_names <- function(use_rds, rds_file, xlsx_file) {
  if (use_rds) {
    if (is.null(rds_file)) return(NULL)
    obj <- tryCatch(readRDS(rds_file$datapath), error = function(e) NULL)
    if (is.null(obj)) return(NULL)
    
    # Cari di berbagai lokasi
    lt <- NULL
    if (is.list(obj) && !is.null(obj$landuse_table)) {
      lt <- obj$landuse_table
    } else if (is.list(obj) && !is.null(obj$baseline$landuse_table)) {
      lt <- obj$baseline$landuse_table
    }
    
    if (is.null(lt)) return(NULL)
    
    # landuse_table bisa berupa data.frame atau list
    if (is.data.frame(lt) && "LC" %in% names(lt)) {
      return(as.character(lt$LC))
    }
    if (is.list(lt) && "LC" %in% names(lt)) {
      return(as.character(lt$LC))
    }
    return(NULL)
  }
  
  if (is.null(xlsx_file)) return(NULL)
  
  tryCatch({
    lc_tbl <- readxl::read_excel(xlsx_file$datapath, sheet = "Land_Cover")
    if (!"LC" %in% names(lc_tbl)) return(NULL)
    as.character(lc_tbl$LC)
  }, error = function(e) NULL)
}

# =========================================================
# UI
# =========================================================

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$style(HTML(app_css))
  ),
  
  titlePanel("Regional Economic Projection"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      # ---------------------------------------------------
      # 1. BASELINE
      # ---------------------------------------------------
      div(class = "section-title", "1. Baseline dari Analisis Deskriptif"),
      
      checkboxInput(
        "load_rds",
        tagList(
          "Muat dari RDS Analisis Deskriptif (lewati pembacaan Excel)",
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
              "descriptive_analysis_module_output.rds</code> dihasilkan ",
              "otomatis setiap kali Anda menjalankan analisis deskriptif. ",
              "Cek folder output yang Anda pilih pada langkah sebelumnya.<br><br>",
              "Gunakan RDS untuk melanjutkan ke modul proyeksi tanpa harus mengunggah ulang file Excel."
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
              tags$b("ℹ️ Apa itu RDS?"),
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
      
      conditionalPanel(
        condition = "input.load_rds == false",
        fileInput(
          "io_output_file",
          label = label_with_help("Hasil Analisis Input-Output Deskriptif", "io_output_file"),
          accept = xlsx_types,
          placeholder = "Unggah IO Analysis Output (.xlsx)"
        )
      ),
      
      conditionalPanel(
        condition = "input.load_rds == true",
        fileInput(
          "descriptive_rds_file",
          label = label_with_help("RDS Analisis Deskriptif", "descriptive_rds_file"),
          accept = rds_types,
          placeholder = "Unggah berkas preprocessing (.rds)"
        )
      ),
      
      fileInput(
        "projection_area_file",
        label = label_with_help("Proyeksi Luas Lahan BAU", "projection_area_file"),
        accept = xlsx_types,
        placeholder = "Unggah berkas tabel (.xlsx)"
      ),
      
      fileInput(
        "population_file",
        label = label_with_help("Proyeksi Jumlah Penduduk", "population_file"),
        accept = c(".xlsx", ".xls", ".csv"),
        placeholder = "Unggah berkas tabel (.xlsx, .csv)"
      ),
      div(class = "note", textOutput("pop_file_check")),
      tags$hr(),
      
      # ---------------------------------------------------
      # 2. PERIODE PROYEKSI
      # ---------------------------------------------------
      div(class = "section-title", "2. Periode proyeksi"),
      p("Tahun awal, tahun akhir, dan interval dideteksi otomatis dari kolom tahun di file Proyeksi Lahan BAU.",
        class = "note"),
      
      div(class = "period-preview", textOutput("detected_years")),
      div(class = "period-preview", textOutput("period_preview")),
      
      # ---------------------------------------------------
      # 3. SKENARIO
      # ---------------------------------------------------
      tags$hr(),
      div(class = "section-title", "3. Skenario Intervensi"),
      
      radioButtons(
        "has_scenario",
        label = label_with_help("Apakah ada Skenario Intervensi?", "has_scenario"),
        choices = c("Ya", "Tidak"),
        selected = "Tidak",
        inline = TRUE
      ),
      
      conditionalPanel(
        condition = "input.has_scenario == 'Ya'",
        
        textInput("scenario_name", label = label_with_help("Nama Skenario Intervensi", "scenario_name"),
                  placeholder = "mis. Skenario Ekonomi Hijau"),
        p(
          "Satu skenario dapat menggabungkan Asumsi 1, 2, dan 3. Kosongkan file pada asumsi yang tidak dipakai.",
          class = "note"
        ),
        
        tabsetPanel(
          id = "assumption_tabs",
          type = "pills",
          
          tabPanel(
            "A1: Lahan",
            div(
              class = "scenario-box",
              tags$b("Asumsi 1: Perubahan Lahan"),
              p("Simulasi dampak perubahan luas dan distribusi penggunaan lahan.", class = "note"),
              
              downloadButton("download_template_a1", "Unduh Template Asumsi 1 (Lahan)",
                             style = "margin-bottom: 12px; font-size: 13px;"),
              
              fileInput("a1_land_file", label = label_with_help("Perubahan Lahan (Asumsi 1)", "a1_land_file"),
                        accept = xlsx_types, placeholder = "Unggah berkas tabel (.xlsx)"),
              div(class = "note", textOutput("a1_sheet_check")),
              
              div(
                style = "margin-top: 8px;",
                actionButton("reset_a1", "Hapus Asumsi 1",
                             icon = icon("trash"),
                             style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
              )
            )
          ),
          
          tabPanel(
            "A2: Produktivitas",
            div(
              class = "scenario-box",
              tags$b("Asumsi 2: Peningkatan Produktivitas"),
              p("Simulasi peningkatan produktivitas lahan (LPC) menuju target.", class = "note"),
              
              downloadButton("download_template_a2", "Unduh Template Asumsi 2 (Produktivitas)",
                             style = "margin-bottom: 12px; font-size: 13px;"),
              
              fileInput("a2_lpc_file", label = label_with_help("Target Produktivitas LPC (Asumsi 2)", "a2_lpc_file"),
                        accept = xlsx_types, placeholder = "Unggah berkas tabel (.xlsx)"),
              
              div(
                style = "margin-top: 8px;",
                actionButton("reset_a2", "Hapus Asumsi 2",
                             icon = icon("trash"),
                             style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
              )
            )
          ),
          
          tabPanel(
            "A3: Hilirisasi",
            div(
              class = "scenario-box",
              tags$b("Asumsi 3: Hilirisasi"),
              p("Perubahan struktur I-O via RAS balancing.", class = "note"),
              
              downloadButton("download_template_a3", "Unduh Template Asumsi 3 (Hilirisasi)",
                             style = "margin-bottom: 12px; font-size: 13px;"),
              
              fileInput("a3_io_changes_file", label = label_with_help("Perubahan Struktur Input-Output (Asumsi 3)", "a3_io_changes_file"),
                        accept = xlsx_types, placeholder = "Unggah berkas tabel (.xlsx)"),
              
              div(
                style = "margin-top: 8px;",
                actionButton("reset_a3", "Hapus Asumsi 3",
                             icon = icon("trash"),
                             style = "background-color: #dc3545; color: white; font-size: 12px; padding: 4px 10px;")
              )
            )
          )
        )
      ),
      
      # ---------------------------------------------------
      # METADATA & OUTPUT
      # ---------------------------------------------------
      tags$hr(),
      tags$b("Metadata (auto-detected)"),
      verbatimTextOutput("metadata_preview"),
      
      tags$hr(),
      
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        dir_button_with_help("wd", "Pilih Folder Output", "Silakan pilih direktori output", "wd"),
        textOutput("selected_directory"),
        actionButton("processTAReg2", "Run Analysis", style = btn_style("#4CAF50")),
        hidden(downloadButton("download_results", "Download Results (.xlsx)",
                              style = btn_style("#008CBA"))),
        hidden(actionButton("open_report", "Open Report", style = btn_style("#008CBA"))),
        hidden(actionButton("open_output_folder", "Open Output Folder", style = btn_style("#008CBA"))),
        actionButton("returnButton", "Return to Main Menu", style = btn_style("#FA8072"))
      )
    ),
    
    mainPanel(
      width = 7,
      
      uiOutput("status_alert"),
      
      tabsetPanel(
        
        tabPanel("User Guide", uiOutput("user_guide")),
        
        tabPanel(
          "Scenario Summary",
          br(),
          div(tags$b("Nama skenario: "), textOutput("scenario_name_text", inline = TRUE)),
          h5("Komponen skenario", style = "margin-top: 14px;"),
          tableOutput("scenario_summary"),
          
          hr(),
          h5("Perbandingan Proyeksi"),
          selectInput("chart_indicator", "Indikator",
                      choices = indicator_choices,
                      selected = "PDRB_Total_Juta"),
          plotOutput("projection_plot", height = "420px"),
          h5("Rekapitulasi Proyeksi", style = "margin-top: 14px;"),
          tableOutput("recap_table"),
          
          hr(),
          uiOutput("linkage_section")
        ),
        
        tabPanel(
          "Log",
          br(),
          textOutput("selected_dir"),
          verbatimTextOutput("status_messages"),
          verbatimTextOutput("error_messages")
        )
      )
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  # Kontrol tombol download template & reset berdasarkan ketersediaan data
  observe({
    has_baseline <- !is.null(baseline_sectors())
    
    shinyjs::toggleState("download_template_a1", condition = has_baseline)
    shinyjs::toggleState("download_template_a2", condition = has_baseline)
    shinyjs::toggleState("download_template_a3", condition = has_baseline)
    
    shinyjs::toggleState("reset_a1", condition = !is.null(a1_file()))
    shinyjs::toggleState("reset_a2", condition = !is.null(a2_file()))
    shinyjs::toggleState("reset_a3", condition = !is.null(a3_file()))
  })
  
  # ==========================================================================
  # RESET HANDLER PER ASUMSI
  # ==========================================================================
  
  # Simpan state file A1/A2/A3 secara reaktif
  # (fileInput tidak bisa di-set NULL langsung, jadi kita pakai reactiveValues)
  a1_file <- reactiveVal(NULL)
  a2_file <- reactiveVal(NULL)
  a3_file <- reactiveVal(NULL)
  
  # Sync input fileInput ke reactiveVal
  observeEvent(input$a1_land_file,   { a1_file(input$a1_land_file) })
  observeEvent(input$a2_lpc_file,    { a2_file(input$a2_lpc_file) })
  observeEvent(input$a3_io_changes_file, { a3_file(input$a3_io_changes_file) })
  
  # Handler tombol Reset A1
  observeEvent(input$reset_a1, {
    a1_file(NULL)
    # Reset UI fileInput dengan JavaScript
    shinyjs::runjs("
      var input = document.getElementById('a1_land_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'luc_projection.xlsx'; }
    ")
    showNotification("Asumsi 1: Perubahan Lahan dihapus.", type = "warning", duration = 3)
  })
  
  # Handler tombol Reset A2
  observeEvent(input$reset_a2, {
    a2_file(NULL)
    shinyjs::runjs("
      var input = document.getElementById('a2_lpc_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'produktivitas_lpc.xlsx'; }
    ")
    showNotification("Asumsi 2: Peningkatan Produktivitas dihapus.", type = "warning", duration = 3)
  })
  
  # Handler tombol Reset A3
  observeEvent(input$reset_a3, {
    a3_file(NULL)
    shinyjs::runjs("
      var input = document.getElementById('a3_io_changes_file');
      if (input) { input.value = ''; }
      var label = input && input.parentElement.querySelector('.form-control');
      if (label) { label.textContent = 'perubahan_io.xlsx'; }
    ")
    showNotification("Asumsi 3: Hilirisasi dihapus.", type = "warning", duration = 3)
  })
  
  # Nama sheet yang diharapkan, dengan kemungkinan alias
  baseline_sheets <- list(
    GDP              = c("GDP", "Analisis Deskriptif", "Analisis_Deskriptif"),
    Leontief_Matrix  = c("Leontief_Matrix"),
    Growth_Rate      = c("Growth_Rate"),
    Multiplier_Output = c("Multiplier_Output"),
    Multiplier_Income = c("Multiplier_Income"),
    Multiplier_Labour = c("Multiplier_Labour"),
    Multiplier_SUB    = c("Multiplier_SUB"),
    Land_Proportion   = c("Land_Proportion"),
    Linkage           = c("Linkage"),
    Full_IO_Table     = c("Full_IO_Table"),
    Code_Registry     = c("Code_Registry")
  )
  
  # ---- Output directory ----
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "wd", roots = volumes, session = session)
  
  output_dir <- reactive({
    if (is.null(input$wd) || is.integer(input$wd)) return(NULL)
    parseDirPath(volumes, input$wd)
  })
  
  dir_text <- reactive({
    d <- output_dir()
    if (is.null(d) || length(d) == 0) "Folder output belum dipilih." else as.character(d)
  })
  
  output$selected_directory <- renderText(dir_text())
  output$selected_dir       <- renderText(paste("Folder output terpilih:", dir_text()))
  
  #### Click-to-open help modals ####
  lapply(names(proj_help_texts), function(id) {
    observeEvent(input[[paste0("proj_help_", id)]], {
      info <- proj_help_texts[[id]]
      showModal(modalDialog(
        title = tagList(icon("circle-info"), " ", info$title),
        HTML(info$body),
        easyClose = TRUE,
        footer = modalButton("Tutup"),
        size = "m"
      ))
    })
  })
  
  # ---- Reactive: baca tahun awal & akhir dari file BAU ----
  tahun_dari_bau <- reactive({
    f <- input$projection_area_file
    if (is.null(f)) {
      return(list(initial_year = NA_integer_, end_year = NA_integer_,
                  time_step = NA_integer_, tahun = integer(0), ok = FALSE))
    }
    
    tryCatch({
      sheets <- readxl::excel_sheets(f$datapath)
      target_sheet <- if ("luc_projection" %in% sheets) "luc_projection" else sheets[1]
      
      df <- readxl::read_excel(f$datapath, sheet = target_sheet, n_max = 1)
      col_names <- names(df)
      
      tahun <- suppressWarnings(as.numeric(col_names))
      tahun <- tahun[!is.na(tahun) & tahun >= 1900 & tahun <= 2200]
      tahun <- sort(unique(tahun))
      
      if (length(tahun) < 1) {
        return(list(initial_year = NA_integer_, end_year = NA_integer_,
                    time_step = NA_integer_, tahun = integer(0), ok = FALSE))
      }
      
      # Hitung time_step dari selisih tahun
      time_step <- if (length(tahun) >= 2) {
        diffs <- diff(tahun)
        diffs <- diffs[diffs > 0]           # buang 0/negatif
        if (length(diffs) > 0) as.integer(min(diffs)) else NA_integer_
      } else {
        NA_integer_
      }
      
      list(
        initial_year = as.integer(min(tahun)),
        end_year     = as.integer(max(tahun)),
        time_step    = time_step,
        tahun        = tahun,
        ok           = TRUE
      )
    }, error = function(e) {
      warning("[tahun_dari_bau] ", conditionMessage(e))
      list(initial_year = NA_integer_, end_year = NA_integer_,
           time_step = NA_integer_, tahun = integer(0), ok = FALSE)
    })
  })
  
  # ---- Tampilkan tahun terdeteksi ----
  output$detected_years <- renderText({
    t <- tahun_dari_bau()
    if (!t$ok) return("(belum terbaca)")
    if (is.na(t$time_step)) {
      sprintf("Tahun: %d – %d (1 titik)", t$initial_year, t$end_year)
    } else {
      sprintf(
        "Tahun: %d – %d (interval %d tahun, %d titik)",
        t$initial_year, t$end_year, t$time_step, length(t$tahun)
      )
    }
  })
  
  # ---- Nilai initial_year & end_year sebagai reactive ----
  initial_year_val <- reactive({ tahun_dari_bau()$initial_year })
  end_year_val     <- reactive({ tahun_dari_bau()$end_year })
  time_step_val    <- reactive({ tahun_dari_bau()$time_step })
  
  # ---- Label periode ----
  periods <- reactive({
    iy    <- initial_year_val()
    ey    <- end_year_val()
    step  <- time_step_val()
    
    validate(
      need(!is.na(iy) && !is.na(ey),
           "Tahun awal/akhir belum terbaca dari file Proyeksi Lahan BAU. ",
           "Pastikan file memiliki kolom tahun (mis. '2024', '2030', ...)."),
      need(!is.na(step) && step >= 1,
           "Interval tahun tidak dapat dihitung dari file BAU. ",
           "Pastikan ada minimal 2 kolom tahun berbeda."),
      need(ey - step >= iy,
           "Rentang tahun tidak cukup untuk interval yang terdeteksi.")
    )
    
    # Mengembalikan urutan tahun saja, misal: 2024, 2025, 2026
    seq(iy, ey, by = step)
  })
  
  output$period_preview <- renderText({
    # Format tampilan menjadi "Tahun: 2024, 2025, 2026"
    paste("Tahun:", paste(periods(), collapse = ", "))
  })
  
  # ---- Nama skenario ----
  scen_label <- reactive({
    nm <- trimws(input$scenario_name)
    if (!nzchar(nm)) nm <- "Skenario"
    if (identical(toupper(nm), "BAU")) nm <- "BAU (skenario)"
    nm
  })
  
  output$scenario_name_text <- renderText({
    nm <- trimws(input$scenario_name)
    if (nzchar(nm)) nm else "belum diisi"
  })
  
  n_assumptions <- reactive({
    sum(
      !is.null(a1_file()),
      !is.null(a2_file()),
      !is.null(a3_file())
    )
  })
  
  # ---- Baseline: sektor & LPC dari data deskriptif ----
  baseline_sectors <- reactive({
    gdp <- tryCatch(
      read_baseline_gdp(isTRUE(input$load_rds),
                        input$descriptive_rds_file,
                        input$io_output_file),
      error = function(e) NULL
    )
    if (is.null(gdp) || !all(c("SECTOR", "CATEGORY", "LPC") %in% names(gdp))) return(NULL)
    
    data.frame(
      Sector_Code  = seq_len(nrow(gdp)),
      Sector       = as.character(gdp$SECTOR),
      Category     = as.character(gdp$CATEGORY),
      LPC_Baseline = as.numeric(gdp$LPC),
      stringsAsFactors = FALSE
    )
  })
  
  # ---- Reactive: populasi proyeksi ----
  pop_projection <- reactive({
    f <- input$population_file
    if (is.null(f)) return(NULL)
    
    tryCatch({
      df <- load_population_projection(f$datapath)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df
    }, error = function(e) {
      warning("[pop_projection] ", conditionMessage(e))
      NULL
    })
  })
  
  # ---- Check: info status file populasi ----
  output$pop_file_check <- renderText({
    f <- input$population_file
    if (is.null(f)) return("")
    
    df <- pop_projection()
    if (is.null(df)) {
      return("⚠ File populasi tidak valid: pastikan punya kolom 'Tahun' dan 'Populasi'.")
    }
    
    sprintf(
      "✓ Populasi: %d baris (%d–%d), %.0f → %.0f jiwa",
      nrow(df),
      min(df$Tahun), max(df$Tahun),
      df$Populasi[1], df$Populasi[nrow(df)]
    )
  })
  
  output$metadata_preview <- renderText({
    use_rds <- isTRUE(input$load_rds)
    f <- if (use_rds) input$descriptive_rds_file else input$io_output_file
    if (is.null(f)) return("Belum ada file baseline dipilih.")
    
    sec   <- baseline_sectors()
    lines <- paste0("File   : ", f$name)
    
    if (!use_rds) {
      sheets <- tryCatch(readxl::excel_sheets(f$datapath),
                         error = function(e) character(0))
      
      # Cek tiap sheet wajib: jika salah satu alias ada, dianggap ada
      missing <- character(0)
      for (canonical in names(baseline_sheets)) {
        aliases <- baseline_sheets[[canonical]]
        if (!any(aliases %in% sheets)) missing <- c(missing, canonical)
      }
      
      lines <- c(
        lines,
        paste0("Sheet  : ",
               if (length(missing) == 0) "lengkap"
               else paste("kurang ->", paste(missing, collapse = ", ")))
      )
    } else {
      lines <- c(lines, "Mode   : .rds (skip Excel)")
    }
    
    lines <- c(lines,
               paste0("Sektor : ",
                      if (is.null(sec)) "belum terbaca" else nrow(sec)))
    paste(lines, collapse = "\n")
  })
  
  a1_sheets <- reactive({
    f <- a1_file()
    if (is.null(f)) return(NULL)
    tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character(0))
  })
  
  output$a1_sheet_check <- renderText({
    s <- a1_sheets()
    if (is.null(s)) return("")
    paste0(
      "luc_projection: ", if ("luc_projection" %in% s) "ada" else "TIDAK ADA",
      " | land_proportion: ", if ("land_proportion" %in% s) "ada" else "tidak ada (opsional)"
    )
  })
  
  # ==========================================================================
  # TEMPLATE DOWNLOAD HANDLERS PER ASUMSI
  # ==========================================================================
  
  # ---- 1. TEMPLATE ASUMSI 1 (Lahan) ----
  output$download_template_a1 <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) "Template_Asumsi1_Lahan.xlsx" else paste0("Template_A1_", nm, ".xlsx")
    },
    content = function(file) {
      init_yr <- ifelse(is.na(initial_year_val()), 2025, initial_year_val())
      end_yr  <- ifelse(is.na(end_year_val()),     2049, end_year_val())
      step_yr <- ifelse(is.na(time_step_val()), 5, time_step_val())
      
      yrs <- seq(init_yr, end_yr, by = step_yr)
      yr_cols <- as.character(yrs)
      
      sec_data <- baseline_sectors()
      p_mat    <- tryCatch(
        read_baseline_p_default(isTRUE(input$load_rds), input$descriptive_rds_file, input$io_output_file),
        error = function(e) NULL
      )
      
      wb <- openxlsx::createWorkbook()
      
      # --- Extract & Olah Matriks P_default ---
      p_df <- NULL
      if (!is.null(p_mat)) {
        p_mat <- as.matrix(p_mat)
        storage.mode(p_mat) <- "numeric"
        
        p_df <- as.data.frame(p_mat, stringsAsFactors = FALSE)
        
        # Buang kolom pertama jika:
        # - nama kolomnya "...1", "...2", atau
        # - isinya index sekuensial (1, 2, 3, ..., n) atau
        # - nama kolomnya "Sektor"/"ID"/"Index"
        first_col_name <- tolower(names(p_df)[1])
        first_col_vals <- p_df[[1]]
        
        is_index_col <- 
          grepl("^\\.\\.\\.", first_col_name) ||                    # "...1", "...2"
          first_col_name %in% c("sektor", "sector", "s", "id", "index", "no") ||
          (is.numeric(first_col_vals) && 
             length(first_col_vals) > 1 &&
             all(first_col_vals == seq_along(first_col_vals), na.rm = TRUE))  # 1,2,3,...,n
        
        if (is_index_col && ncol(p_df) > 1) {
          p_df <- p_df[, -1, drop = FALSE]
        }
      }
      
      # Tentukan nama & jumlah kolom LC
      lc_names_source <- tryCatch(
        read_baseline_lc_names(
          isTRUE(input$load_rds),
          input$descriptive_rds_file,
          input$io_output_file
        ),
        error = function(e) NULL
      )
      
      # Tentukan nama & jumlah kolom LC
      if (!is.null(lc_names_source) && length(lc_names_source) > 0) {
        # Prioritas: nama dari Land_Cover / landuse_table
        lc_names <- lc_names_source
        n_lc     <- length(lc_names)
      } else if (!is.null(p_df) && ncol(p_df) > 0) {
        # Fallback 1: nama kolom dari P_default
        lc_names <- colnames(p_df)
        n_lc     <- ncol(p_df)
        if (is.null(lc_names) || any(lc_names == "")) {
          lc_names <- paste0("LC", seq_len(n_lc))
        }
      } else {
        # Fallback 2: generic
        lc_names <- paste0("LC", 1:5)
        n_lc     <- 5
      }
      
      # Konsistensi: jika ncol(p_df) berbeda dari n_lc, sesuaikan
      if (!is.null(p_df) && ncol(p_df) != n_lc) {
        # Ambil minimum agar tidak error
        n_min <- min(ncol(p_df), n_lc)
        p_df    <- p_df[, 1:n_min, drop = FALSE]
        lc_names <- lc_names[1:n_min]
        n_lc     <- n_min
        warning(sprintf(
          "Jumlah kolom LC dari Land_Cover (%d) tidak sama dengan P_default (%d). Menggunakan %d kolom pertama.",
          length(lc_names_source), ncol(p_df), n_min
        ))
      }
      
      # --- SHEET 1: luc_projection ---
      df_luc <- data.frame(
        ID = seq_len(n_lc),
        LC = lc_names,
        stringsAsFactors = FALSE
      )
      
      for (y in yr_cols) {
        df_luc[[y]] <- NA_real_
      }
      
      openxlsx::addWorksheet(wb, "luc_projection")
      openxlsx::writeData(wb, "luc_projection", df_luc)
      
      # --- SHEET 2: land_proportion ---
      if (!is.null(sec_data) && "Sector" %in% names(sec_data)) {
        sector_list <- sec_data$Sector
      } else if (!is.null(p_df)) {
        sector_list <- paste0("S", seq_len(nrow(p_df)))
      } else {
        sector_list <- paste0("S", 1:10)
      }
      
      df_land <- data.frame(
        Sektor = sector_list,
        stringsAsFactors = FALSE
      )
      
      # Masukkan nilai proporsi dari P_default ke kolom LC masing-masing
      n_sec <- length(sector_list)
      if (!is.null(p_df) && ncol(p_df) > 0) {
        for (j in seq_len(n_lc)) {
          col_nm   <- lc_names[j]
          col_vals <- p_df[[j]]
          
          if (length(col_vals) >= n_sec) {
            df_land[[col_nm]] <- col_vals[1:n_sec]
          } else {
            df_land[[col_nm]] <- c(col_vals, rep(0, n_sec - length(col_vals)))
          }
        }
      } else {
        for (col_nm in lc_names) {
          df_land[[col_nm]] <- 0
        }
      }
      
      openxlsx::addWorksheet(wb, "land_proportion")
      openxlsx::writeData(wb, "land_proportion", df_land)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- 2. TEMPLATE ASUMSI 2 (Produktivitas LPC) ----
  output$download_template_a2 <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) "Template_Asumsi2_Produktivitas.xlsx" else paste0("Template_A2_", nm, ".xlsx")
    },
    content = function(file) {
      sec <- baseline_sectors()
      wb  <- openxlsx::createWorkbook()
      
      # Style definitions
      title_style     <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#1F497D", fontSize = 12)
      sub_style       <- openxlsx::createStyle(textDecoration = "italic", fontColour = "#595959", fontSize = 9)
      param_hdr_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F497D", fontSize = 10)
      tbl_hdr_style   <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F497D", halign = "center", valign = "center", wrapText = TRUE)
      
      bold_style      <- openxlsx::createStyle(textDecoration = "bold")
      yellow_fill     <- openxlsx::createStyle(fgFill = "#FFF2CC", numFmt = "0.0%")
      green_fill      <- openxlsx::createStyle(fgFill = "#E2EFDA", textDecoration = "bold", numFmt = "#,##0.00")
      num_fmt         <- openxlsx::createStyle(numFmt = "#,##0.00")
      center_style    <- openxlsx::createStyle(halign = "center")
      
      # =========================================================
      # SHEET 1: Skenario_LPC
      # =========================================================
      openxlsx::addWorksheet(wb, "Skenario_LPC")
      
      # Title & Subtitles
      openxlsx::writeData(wb, "Skenario_LPC", "TEMPLATE INPUT PRODUKTIVITAS LAHAN (LPC) - SKENARIO 2", startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, "Skenario_LPC", title_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, "Skenario_LPC", "Model Mixed Input-Output Terkendala Lahan (Supply-Constrained Mixed IO Model)", startCol = 1, startRow = 2)
      openxlsx::addStyle(wb, "Skenario_LPC", sub_style, rows = 2, cols = 1)
      
      openxlsx::writeData(wb, "Skenario_LPC", "Panduan: Isilah kolom 'Target Growth Factor' (%) atau langsung ketikkan nilai baru di kolom 'LPC Skenario 2'.", startCol = 1, startRow = 3)
      openxlsx::addStyle(wb, "Skenario_LPC", sub_style, rows = 3, cols = 1)
      
      # Parameter Block (Baris 5 - 10)
      openxlsx::writeData(wb, "Skenario_LPC", "PARAMETER MODEL", startCol = 1, startRow = 5)
      openxlsx::addStyle(wb, "Skenario_LPC", param_hdr_style, rows = 5, cols = 1:2, gridExpand = TRUE)
      
      scen_nm <- if (input$has_scenario == "Ya" && nzchar(trimws(input$scenario_name))) {
        trimws(input$scenario_name)
      } else {
        "Skenario 2: Intensifikasi & Inovasi Teknologi Pertanian"
      }
      
      n_sec <- if (!is.null(sec)) nrow(sec) else 0
      
      param_df <- data.frame(
        Param = c("Skenario", "Asumsi Lahan", "Asumsi Non-Lahan", "Satuan LPC", "Jumlah Sektor Valid"),
        Val   = c(scen_nm, "Proyeksi Tutupan Lahan BAU (Business-As-Usual)", "Growth Rate Historis Baseline", "Juta Rupiah / Hektar (Rp Juta/Ha)", as.character(n_sec)),
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, "Skenario_LPC", param_df, startCol = 1, startRow = 6, colNames = FALSE)
      openxlsx::addStyle(wb, "Skenario_LPC", bold_style, rows = 6:10, cols = 1, gridExpand = TRUE)
      
      # Header Tabel Utama (Baris 12)
      headers <- c("NO", "NAMA SEKTOR EKONOMI", "KATEGORI SEKTOR", "LPC BASELINE (Rp Juta/Ha)", "TARGET GROWTH (%)", "LPC SKENARIO 2 (Rp Juta/Ha)", "KETERANGAN / INTERVENSI KEBIJAKAN", "CROP TYPE")
      openxlsx::writeData(wb, "Skenario_LPC", as.data.frame(t(headers)), startCol = 1, startRow = 12, colNames = FALSE)
      openxlsx::addStyle(wb, "Skenario_LPC", tbl_hdr_style, rows = 12, cols = 1:8, gridExpand = TRUE)
      openxlsx::setRowHeights(wb, "Skenario_LPC", rows = 12, heights = 28)
      
      # Populasikan Data Sektor
      if (!is.null(sec) && nrow(sec) > 0) {
        sector_names <- sec$Sector
        n_sec_data   <- length(sector_names)
        r_rows       <- 12 + seq_len(n_sec_data)
        
        # Buat Dataframe Lengkap (NO terisi 1:length(NAMA SEKTOR ECONOMIC))
        df_rows <- data.frame(
          NO = 1:length(sector_names),
          `NAMA SEKTOR ECONOMIC` = sector_names,
          `KATEGORI SEKTOR` = ifelse(!is.na(sec$LPC_Baseline) & sec$LPC_Baseline > 0, "Constrained (Lahan)", "Unconstrained (Non-Lahan)"),
          `LPC BASELINE (Rp Juta/Ha)` = ifelse(is.na(sec$LPC_Baseline), 0, sec$LPC_Baseline),
          `TARGET GROWTH (%)` = 0,
          `LPC SKENARIO 2 (Rp Juta/Ha)` = NA_real_,
          `KETERANGAN / INTERVENSI KEBIJAKAN` = "",
          `CROP TYPE` = ifelse(!is.na(sec$LPC_Baseline) & sec$LPC_Baseline > 0, "Annual/Perennial", "None"),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        # Tulis Data
        openxlsx::writeData(wb, "Skenario_LPC", df_rows, startCol = 1, startRow = 13, colNames = FALSE)
        
        # Tulis Rumus Vektorisasi untuk Kolom F: =D{row}*(1+E{row})
        fmls <- sprintf("=D%d*(1+E%d)", r_rows, r_rows)
        openxlsx::writeFormula(wb, "Skenario_LPC", x = fmls, startCol = 6, startRow = 13)
        
        # Terapkan Styling Kolom
        openxlsx::addStyle(wb, "Skenario_LPC", center_style, rows = r_rows, cols = c(1, 8), gridExpand = TRUE)
        openxlsx::addStyle(wb, "Skenario_LPC", num_fmt, rows = r_rows, cols = 4, gridExpand = TRUE)
        openxlsx::addStyle(wb, "Skenario_LPC", yellow_fill, rows = r_rows, cols = 5, gridExpand = TRUE)
        openxlsx::addStyle(wb, "Skenario_LPC", green_fill, rows = r_rows, cols = 6, gridExpand = TRUE)
      }
      
      openxlsx::setColWidths(wb, "Skenario_LPC", cols = 1:8, widths = c(6, 42, 25, 25, 20, 25, 45, 15))
      
      # =========================================================
      # SHEET 2: Petunjuk_Penggunaan
      # =========================================================
      openxlsx::addWorksheet(wb, "Petunjuk_Penggunaan")
      
      p_title_style <- openxlsx::createStyle(textDecoration = "bold", fontColour = "#1F497D", fontSize = 12)
      p_h1_style    <- openxlsx::createStyle(textDecoration = "bold", fontSize = 10)
      
      openxlsx::writeData(wb, "Petunjuk_Penggunaan", "PANDUAN PENGGUNAAN TEMPLATE LPC SKENARIO 2", startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, "Petunjuk_Penggunaan", p_title_style, rows = 1, cols = 1)
      
      guide_texts <- list(
        c(3, "1. Maksud & Tujuan Template", TRUE),
        c(4, "Template ini dirancang untuk memfasilitasi penginputan data produktivitas lahan (LPC / Land Productivity Coefficient) pada Skenario 2 (Skenario Intensifikasi / Inovasi Teknologi Pertanian).", FALSE),
        c(6, "2. Definisi Koefisien LPC", TRUE),
        c(7, "LPC diukur dalam satuan Juta Rupiah per Hektar (Rp Juta/Ha). Nilai ini mencerminkan total Rupiah output ekonomi yang dihasilkan oleh 1 Hektar tutupan lahan sektoral.", FALSE),
        c(9, "3. Cara Pengisian Data", TRUE),
        c(10, "Terdapat 2 cara untuk mengedit skenario ini:", FALSE),
        c(12, "a. Melalui Growth Factor (%)", TRUE),
        c(13, "Ketikkan persentase kenaikan target produktivitas pada Kolom E (kuning). Nilai LPC Skenario 2 di Kolom F (hijau) akan otomatis terhitung.", FALSE),
        c(15, "b. Penginputan Manual Direct LPC", TRUE),
        c(16, "Anda juga dapat mengetikkan langsung angka nominal target LPC di Kolom F jika sudah memiliki angka pasti hasil studi teknis/dinas.", FALSE),
        c(18, "4. Integrasi ke Kode R Skenario 2", TRUE),
        c(19, "Simpan file ini sebagai 'produktivitas_lpc.xlsx'. Di R script Skenario 2, panggil kolom F (LPC Skenario 2) untuk memperbarui vektor p_0_co_s2.", FALSE),
        c(21, "5. Penanganan Sektor Non-Lahan", TRUE),
        c(22, "Sektor non-lahan (Unconstrained) memiliki nilai LPC = 0 karena tidak terkendala oleh kapasitas fisik lahan GIS.", FALSE)
      )
      
      for (gt in guide_texts) {
        r_num <- as.numeric(gt[[1]])
        txt   <- gt[[2]]
        is_b  <- as.logical(gt[[3]])
        
        openxlsx::writeData(wb, "Petunjuk_Penggunaan", txt, startCol = 1, startRow = r_num)
        if (is_b) {
          openxlsx::addStyle(wb, "Petunjuk_Penggunaan", p_h1_style, rows = r_num, cols = 1)
        }
      }
      
      openxlsx::setColWidths(wb, "Petunjuk_Penggunaan", cols = 1, widths = 130)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- 3. TEMPLATE ASUMSI 3 (Hilirisasi / Re-alokasi Final Demand) ----
  output$download_template_a3 <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) "Template_Asumsi3_Realokasi_FD.xlsx"
      else paste0("Template_A3_", nm, ".xlsx")
    },
    content = function(file) {
      sec <- baseline_sectors()
      
      # ---- Guard: baseline wajib dimuat ----
      if (is.null(sec) || nrow(sec) == 0) {
        showNotification(
          "Baseline belum dimuat. Klik 'Run Analysis' terlebih dahulu sebelum mengunduh Template A3.",
          type = "warning", duration = 8
        )
        stop("Baseline belum dimuat.")
      }
      
      # ---- Tahun proyeksi dari file BAU ----
      init_yr <- ifelse(is.na(initial_year_val()), 2025, initial_year_val())
      end_yr  <- ifelse(is.na(end_year_val()),     2045, end_year_val())
      step_yr <- ifelse(is.na(time_step_val()),    1,    time_step_val())
      
      yrs     <- seq(init_yr, end_yr, by = step_yr)
      yr_cols <- as.character(yrs)
      
      wb <- openxlsx::createWorkbook()
      
      # =====================================================================
      # STYLE DEFINITIONS
      # =====================================================================
      title_style    <- openxlsx::createStyle(
        textDecoration = "bold", fontColour = "#1F497D", fontSize = 12
      )
      sub_style      <- openxlsx::createStyle(
        textDecoration = "italic", fontColour = "#595959", fontSize = 9
      )
      tbl_hdr_style  <- openxlsx::createStyle(
        textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F497D",
        halign = "center", valign = "center", wrapText = TRUE
      )
      year_hdr_style <- openxlsx::createStyle(
        textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#2E75B6",
        halign = "center", valign = "center", textRotation = 90, fontSize = 10
      )
      yellow_fill    <- openxlsx::createStyle(fgFill = "#FFF2CC")
      yellow_pct     <- openxlsx::createStyle(fgFill = "#FFF2CC", numFmt = "0.0%")
      center_style   <- openxlsx::createStyle(halign = "center")
      param_hdr_style <- openxlsx::createStyle(
        textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F497D", fontSize = 10
      )
      bold_style     <- openxlsx::createStyle(textDecoration = "bold")
      p_h1_style     <- openxlsx::createStyle(textDecoration = "bold", fontSize = 10)
      
      # =====================================================================
      # SHEET 1: Petunjuk_Penggunaan   <-- REORDER: pindah ke paling atas
      # =====================================================================
      sheet1 <- "Petunjuk_Penggunaan"
      openxlsx::addWorksheet(wb, sheet1)
      
      openxlsx::writeData(wb, sheet1,
                          "PETUNJUK PENGGUNAAN TABEL ASUMSI 3 (RE-ALOKASI FINAL DEMAND)",
                          startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, sheet1, title_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, sheet1, "PARAMETER", startCol = 1, startRow = 3)
      openxlsx::writeData(wb, sheet1, "PENJELASAN", startCol = 2, startRow = 3)
      openxlsx::addStyle(wb, sheet1, param_hdr_style,
                         rows = 3, cols = 1:2, gridExpand = TRUE)
      
      guide_items <- list(
        c("Aturan Utama",
          "Nilai persentase pengalihan Final Demand MAKSIMAL adalah 90% (0.90). Jika dimasukkan angka di atas 90%, sistem R akan otomatis membatasi (cap) pada 90%."),
        c("Kolom Sektor Asal",
          "Nama sektor komoditas mentah/asal yang nilai Final Demand-nya akan dialihkan (misal: Padi, Jagung, Karet). Pilih dari dropdown (sheet 'Daftar_Sektor')."),
        c("Kolom Sektor Tujuan",
          "Nama sektor industri pengolahan/pelayanan lokal yang akan menerima tambahan input transaksi antara dari Sektor Asal. Pilih dari dropdown (sheet 'Daftar_Sektor')."),
        c("Komponen Final Demand Asal",
          "Pilih jenis komponen Permintaan Akhir yang dialihkan dari dropdown: Konsumsi Rumah Tangga, Konsumsi Pemerintah, Pembentukan Modal Tetap Bruto, Perubahan Inventori, atau Ekspor Barang."),
        c("Kolom Tahun (2025 - 2045)",
          "Isi persentase akumulatif/target pengalihan pada tahun tersebut. Contoh: 10% di 2029 berarti 10% dari nilai Final Demand baseline dialihkan ke Sektor Tujuan. Format: 0.1 = 10%, 0.9 = 90%."),
        c("Catatan Model",
          "Sistem akan menghitung ΔZ[from, to] = pct × Y_0[from] dan ΔY[from] = -ΔZ, ΔY[to] = +ΔZ. Total output (X) tidak berubah; hanya komposisi FD vs Z yang berubah."),
        c("Capping Otomatis",
          "Jika akumulasi persentase untuk satu sektor asal melebihi 90% dari Y_0-nya, sistem akan memotong (cap) pada 90% dan menampilkan warning."),
        c("Format Nilai Persen",
          "Gunakan format desimal (0.1 = 10%) atau persen dengan tanda '%' (10% = 0.1). Keduanya didukung."),
        c("Urutan Sheet",
          "1. Petunjuk_Penggunaan, 2. Asumsi_3_Realokasi_FD, 3. Daftar_Sektor, 4. Daftar_Final_Demand.")
      )
      
      for (i in seq_along(guide_items)) {
        r <- 3 + i
        openxlsx::writeData(wb, sheet1, guide_items[[i]][1], startCol = 1, startRow = r)
        openxlsx::writeData(wb, sheet1, guide_items[[i]][2], startCol = 2, startRow = r)
        openxlsx::addStyle(wb, sheet1, bold_style, rows = r, cols = 1)
      }
      
      openxlsx::setColWidths(wb, sheet1, cols = 1:2, widths = c(30, 110))
      openxlsx::setRowHeights(wb, sheet1,
                              rows = (3 + seq_along(guide_items)),
                              heights = 35)
      
      # =====================================================================
      # SHEET 2: Asumsi_3_Realokasi_FD
      # =====================================================================
      sheet2 <- "Asumsi_3_Realokasi_FD"
      openxlsx::addWorksheet(wb, sheet2)
      
      openxlsx::writeData(wb, sheet2,
                          "TABEL ASUMSI 3: RE-ALOKASI FINAL DEMAND KE SEKTOR EKONOMI",
                          startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, sheet2, title_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, sheet2,
                          paste0("Petunjuk: Pilih Sektor Asal & Tujuan dari dropdown (sheet 'Daftar_Sektor'), ",
                                 "pilih Komponen Final Demand dari dropdown (sheet 'Daftar_Final_Demand'), ",
                                 "dan isi Persentase Realokasi per Tahun (Maksimal 90%)."),
                          startCol = 1, startRow = 2)
      openxlsx::addStyle(wb, sheet2, sub_style, rows = 2, cols = 1)
      
      hdr_row <- 4
      headers <- c("Sektor Asal", "Sektor Tujuan", "Komponen Final Demand Asal", yr_cols)
      openxlsx::writeData(wb, sheet2,
                          as.data.frame(t(headers)),
                          startCol = 1, startRow = hdr_row, colNames = FALSE)
      
      n_fixed_cols <- 3
      openxlsx::addStyle(wb, sheet2, tbl_hdr_style,
                         rows = hdr_row, cols = 1:n_fixed_cols, gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet2, year_hdr_style,
                         rows = hdr_row,
                         cols = (n_fixed_cols + 1):(n_fixed_cols + length(yr_cols)),
                         gridExpand = TRUE)
      openxlsx::setRowHeights(wb, sheet2, rows = hdr_row, heights = 70)
      
      n_rows     <- 20
      data_start <- hdr_row + 1
      data_end   <- data_start + n_rows - 1
      
      df_main <- data.frame(
        `Sektor Asal`                = rep(NA_character_, n_rows),
        `Sektor Tujuan`              = rep(NA_character_, n_rows),
        `Komponen Final Demand Asal` = rep(NA_character_, n_rows),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      for (yc in yr_cols) df_main[[yc]] <- rep(NA_real_, n_rows)
      
      openxlsx::writeData(wb, sheet2, df_main,
                          startCol = 1, startRow = data_start, colNames = FALSE)
      
      openxlsx::addStyle(wb, sheet2, yellow_fill,
                         rows = data_start:data_end,
                         cols = 1:n_fixed_cols, gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet2, yellow_pct,
                         rows = data_start:data_end,
                         cols = (n_fixed_cols + 1):(n_fixed_cols + length(yr_cols)),
                         gridExpand = TRUE)
      
      # ---- Dropdown Sektor Asal & Tujuan (guard sec NULL) ----
      n_sec_dropdown <- nrow(sec)   # sec sudah di-guard != NULL di atas
      sector_range   <- paste0("Daftar_Sektor!$B$5:$B$", 4 + n_sec_dropdown)
      
      openxlsx::dataValidation(
        wb, sheet2,
        cols = 1, rows = data_start:data_end,
        type = "list", value = sector_range,
        allowBlank = TRUE, showErrorMsg = TRUE
      )
      
      openxlsx::dataValidation(
        wb, sheet2,
        cols = 2, rows = data_start:data_end,
        type = "list", value = sector_range,
        allowBlank = TRUE, showErrorMsg = TRUE
      )
      
      # ---- Dropdown Komponen Final Demand ----
      fd_list <- tryCatch(
        get_final_demand_kbli(baseline_data()$baseline, kbli_range = 301:305),
        error = function(e) NULL
      )
      if (is.null(fd_list) || nrow(fd_list) == 0) {
        fd_names <- c("Konsumsi Rumah Tangga", "Konsumsi Pemerintah",
                      "Pembentukan Modal Tetap Bruto", "Perubahan Inventori",
                      "Ekspor Barang")
      } else {
        fd_names <- fd_list$Label
      }
      
      fd_range <- paste0("Daftar_Final_Demand!$C$5:$C$", 4 + length(fd_names))
      
      openxlsx::dataValidation(
        wb, sheet2,
        cols = 3, rows = data_start:data_end,
        type = "list", value = fd_range,
        allowBlank = TRUE, showErrorMsg = TRUE
      )
      
      # ---- Validasi persen ----
      for (j in seq_along(yr_cols)) {
        openxlsx::dataValidation(
          wb, sheet2,
          cols = n_fixed_cols + j, rows = data_start:data_end,
          type = "decimal", operator = "between", value = c(0, 1),
          allowBlank = TRUE, showErrorMsg = TRUE
        )
      }
      
      openxlsx::freezePane(wb, sheet2, firstActiveRow = data_start, firstActiveCol = 4)
      openxlsx::setColWidths(wb, sheet2, cols = 1:n_fixed_cols, widths = c(30, 35, 28))
      openxlsx::setColWidths(wb, sheet2,
                             cols = (n_fixed_cols + 1):(n_fixed_cols + length(yr_cols)),
                             widths = 8)
      
      # =====================================================================
      # SHEET 3: Daftar_Sektor
      # =====================================================================
      sheet3 <- "Daftar_Sektor"
      openxlsx::addWorksheet(wb, sheet3)
      
      openxlsx::writeData(wb, sheet3,
                          "DAFTAR SEKTOR EKONOMI MODEL INPUT-OUTPUT",
                          startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, sheet3, title_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, sheet3,
                          "Gunakan dropdown di sheet 'Asumsi_3_Realokasi_FD' untuk memilih sektor. Sheet ini hanya referensi.",
                          startCol = 1, startRow = 2)
      openxlsx::addStyle(wb, sheet3, sub_style, rows = 2, cols = 1)
      
      sec_headers <- c("NO SEKTOR", "NAMA SEKTOR OFFICIAL", "KATEGORI SEKTOR", "LPC BASELINE (Rp Juta/Ha)")
      openxlsx::writeData(wb, sheet3,
                          as.data.frame(t(sec_headers)),
                          startCol = 1, startRow = 4, colNames = FALSE)
      openxlsx::addStyle(wb, sheet3, tbl_hdr_style,
                         rows = 4, cols = 1:4, gridExpand = TRUE)
      
      n_sec_data <- nrow(sec)
      df_sec <- data.frame(
        `NO SEKTOR`            = 1:n_sec_data,
        `NAMA SEKTOR OFFICIAL` = sec$Sector,
        `KATEGORI SEKTOR`      = ifelse(
          !is.na(sec$LPC_Baseline) & sec$LPC_Baseline > 0,
          "Constrained (Lahan)", "Unconstrained (Non-Lahan)"
        ),
        `LPC BASELINE (Rp Juta/Ha)` = ifelse(is.na(sec$LPC_Baseline), 0, sec$LPC_Baseline),
        check.names = FALSE, stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, sheet3, df_sec,
                          startCol = 1, startRow = 5, colNames = FALSE)
      openxlsx::addStyle(wb, sheet3, center_style,
                         rows = 5:(4 + n_sec_data), cols = 1, gridExpand = TRUE)
      
      openxlsx::setColWidths(wb, sheet3, cols = 1:4, widths = c(12, 50, 25, 25))
      openxlsx::freezePane(wb, sheet3, firstActiveRow = 5)
      
      # =====================================================================
      # SHEET 4: Daftar_Final_Demand
      # =====================================================================
      sheet4 <- "Daftar_Final_Demand"
      openxlsx::addWorksheet(wb, sheet4)
      
      openxlsx::writeData(wb, sheet4,
                          "DAFTAR KOMPONEN FINAL DEMAND (KODE 301–305)",
                          startCol = 1, startRow = 1)
      openxlsx::addStyle(wb, sheet4, title_style, rows = 1, cols = 1)
      
      openxlsx::writeData(wb, sheet4,
                          "Sumber: Code_Registry (Type = 'Final_Demand'). Pilih dari dropdown di sheet 'Asumsi_3_Realokasi_FD'.",
                          startCol = 1, startRow = 2)
      openxlsx::addStyle(wb, sheet4, sub_style, rows = 2, cols = 1)
      
      fd_headers <- c("NO", "CODE", "NAMA KOMPONEN FINAL DEMAND", "TYPE", "DIGUNAKAN DI KOLOM")
      openxlsx::writeData(wb, sheet4,
                          as.data.frame(t(fd_headers)),
                          startCol = 1, startRow = 4, colNames = FALSE)
      openxlsx::addStyle(wb, sheet4, tbl_hdr_style,
                         rows = 4, cols = 1:5, gridExpand = TRUE)
      
      if (is.null(fd_list) || nrow(fd_list) == 0) {
        fd_kbli_df <- data.frame(
          Code  = as.character(301:305),
          Label = c("Konsumsi Rumah Tangga", "Konsumsi Pemerintah",
                    "Pembentukan Modal Tetap Bruto", "Perubahan Inventori",
                    "Ekspor Barang"),
          Type  = "Final_Demand",
          stringsAsFactors = FALSE
        )
      } else {
        fd_kbli_df <- fd_list
      }
      
      df_fd <- data.frame(
        NO   = seq_len(nrow(fd_kbli_df)),
        CODE = fd_kbli_df$Code,
        `NAMA KOMPONEN FINAL DEMAND` = fd_kbli_df$Label,
        TYPE = fd_kbli_df$Type,
        `DIGUNAKAN DI KOLOM` = "Komponen Final Demand Asal",
        check.names = FALSE, stringsAsFactors = FALSE
      )
      
      openxlsx::writeData(wb, sheet4, df_fd,
                          startCol = 1, startRow = 5, colNames = FALSE)
      
      r_range <- 5:(4 + nrow(df_fd))
      openxlsx::addStyle(wb, sheet4, center_style,
                         rows = r_range, cols = c(1, 2, 4), gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet4, bold_style,
                         rows = r_range, cols = c(2, 3), gridExpand = TRUE)
      
      note_row <- 4 + nrow(df_fd) + 2
      openxlsx::writeData(wb, sheet4,
                          paste0("Catatan: Kolom 'Komponen Final Demand Asal' di sheet 'Asumsi_3_Realokasi_FD' ",
                                 "hanya menerima nilai dari daftar di atas (", nrow(df_fd), " komponen)."),
                          startCol = 1, startRow = note_row)
      openxlsx::addStyle(wb, sheet4, sub_style, rows = note_row, cols = 1)
      
      openxlsx::setColWidths(wb, sheet4, cols = 1:5, widths = c(6, 10, 45, 16, 32))
      openxlsx::freezePane(wb, sheet4, firstActiveRow = 5)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ==========================================================================
  # CORE: REACTIVE BASELINE & PROYEKSI
  # ==========================================================================
  
  # ---- Reactive: baseline ----
  baseline_data <- eventReactive(input$processTAReg2, {
    use_rds <- isTRUE(input$load_rds)
    
    # --- Tentukan path sumber baseline ---
    if (use_rds) {
      req(input$descriptive_rds_file)
      baseline_path <- input$descriptive_rds_file$datapath
    } else {
      req(input$io_output_file)
      baseline_path <- input$io_output_file$datapath
    }
    
    # --- Baca area_matrix_bau dari file BAU ---
    req(input$projection_area_file)
    area_file <- input$projection_area_file$datapath
    
    # --- Panggil loader universal ---
    message("[BASELINE] Loading from: ", baseline_path)
    baseline <- load_projection_baseline(
      path                     = baseline_path,
      projection_area_file_bau = area_file
    )
    
    # --- Validasi field wajib ---
    required <- c(
      "gdp_baseline", "intermediate_consumption", "P_default",
      "income_multiplier", "labour_multiplier", "SUB_multiplier",
      "output_multiplier", "linkage_table", "leontief_matrix",
      "growth_rate", "code_registry", "sector_idx",
      "area_matrix_bau", "constrained", "unconstrained"
    )
    
    nulls <- required[vapply(required, function(nm) is.null(baseline[[nm]]), logical(1))]
    if (length(nulls) > 0) {
      stop("Field wajib NULL di baseline: ", paste(nulls, collapse = ", "))
    }
    
    message(sprintf(
      "✓ Baseline loaded: %d sektor × %d tutupan lahan, %d periode",
      nrow(baseline$P_default),
      ncol(baseline$P_default),
      ncol(baseline$area_matrix_bau)
    ))
    
    list(
      baseline        = baseline,
      area_matrix_bau = baseline$area_matrix_bau,
      profile         = baseline$profile
    )
  }, ignoreNULL = FALSE)
  
  # ---- Reactive: hasil proyeksi ----
  hasil_proyeksi <- eventReactive(input$processTAReg2, {
    
    bl <- baseline_data()
    req(bl$baseline, bl$area_matrix_bau)
    
    baseline        <- bl$baseline
    area_matrix_bau <- bl$area_matrix_bau
    
    # --- Args Asumsi 1 ---
    args_a1 <- list()
    if (!is.null(a1_file())) {
      sc_a1 <- load_scenario_inputs(
        list(
          luc_projection    = a1_file()$datapath,
          land_distribution = a1_file()$datapath
        ),
        P_default = baseline$P_default
      )
      args_a1$area_matrix_scenario <- sc_a1$area_matrix_scenario
      args_a1$P_scenario           <- sc_a1$P
    }
    
    # --- Args Asumsi 2 ---
    args_a2 <- list()
    if (!is.null(a2_file())) {
      sc_a2 <- load_scenario_inputs(
        list(lpc = a2_file()$datapath),
        P_default = baseline$P_default
      )
      args_a2$lpc_scenario    <- sc_a2$lpc_scenario
      args_a2$lpc_target_year <- end_year_val()
    }
    
    # --- Args Asumsi 3 ---
    args_a3 <- list()
    if (!is.null(a3_file())) {
      sc_a3 <- load_scenario_inputs(
        list(io_changes = a3_file()$datapath),
        P_default = baseline$P_default
      )
      args_a3$io_changes <- sc_a3$io_changes
    }
    
    options(initial_year = initial_year_val())
    
    # --- Panggil run_projection dengan tryCatch ---
    hasil <- tryCatch(
      do.call(
        run_projection_scenario,
        c(
          list(
            gdp_baseline      = baseline$gdp_baseline,
            income_multiplier = baseline$income_multiplier,
            labour_multiplier = baseline$labour_multiplier,
            SUB_multiplier    = baseline$SUB_multiplier,
            output_multiplier = baseline$output_multiplier,
            linkage_table     = baseline$linkage_table,
            leontief_matrix   = baseline$leontief_matrix,
            full_io_table     = baseline$intermediate_consumption,
            sector_idx        = baseline$sector_idx,
            P_baseline        = baseline$P_default,
            area_matrix_bau   = area_matrix_bau,
            growth_rate       = baseline$growth_rate,
            constrained       = baseline$constrained,
            unconstrained     = baseline$unconstrained,
            pop_projection    = pop_projection()
          ),
          args_a1,
          args_a2,
          args_a3
        )
      ),
      error = function(e) {
        warning("[hasil_proyeksi] Error: ", conditionMessage(e))
        NULL
      }
    )
    
    # --- DEBUG (hapus setelah stabil) ---
    cat("==== DEBUG hasil_proyeksi ====\n")
    cat("class(hasil):", paste(class(hasil), collapse = ", "), "\n")
    cat("is.null(hasil):", is.null(hasil), "\n")
    if (!is.null(hasil)) {
      cat("nrow(hasil):", nrow(hasil), "\n")
      cat("colnames(hasil):", paste(colnames(hasil), collapse = ", "), "\n")
      cat("active:", paste(attr(hasil, "active_assumptions"), collapse = " + "), "\n")
      cat("sector_names length:", length(attr(hasil, "sector_names")), "\n")
    }
    cat("==============================\n")
    
    if (is.null(hasil)) return(NULL)
    
    list(
      scenario  = hasil,
      active    = attr(hasil, "active_assumptions"),
      baseline_meta = list(
        sector_names = attr(hasil, "sector_names"),
        years        = attr(hasil, "years")
      )
    )
  }, ignoreNULL = FALSE)
  
  # ---- Reactive: proyeksi BAU (untuk perbandingan) ----
  hasil_bau <- eventReactive(input$processTAReg2, {
    
    bl <- baseline_data()
    req(bl$baseline, bl$area_matrix_bau)
    
    baseline        <- bl$baseline
    area_matrix_bau <- bl$area_matrix_bau
    
    options(initial_year = initial_year_val())
    
    hasil <- tryCatch(
      run_projection_bau(
        gdp_baseline      = baseline$gdp_baseline,
        income_multiplier = baseline$income_multiplier,
        labour_multiplier = baseline$labour_multiplier,
        SUB_multiplier    = baseline$SUB_multiplier,
        output_multiplier = baseline$output_multiplier,
        linkage_table     = baseline$linkage_table,
        leontief_matrix   = baseline$leontief_matrix,
        full_io_table     = baseline$intermediate_consumption,
        sector_idx        = baseline$sector_idx,
        P                 = baseline$P_default,      # ganti nama argumen
        area_matrix       = area_matrix_bau,          # ganti nama argumen
        growth_rate       = baseline$growth_rate,
        constrained       = baseline$constrained,
        unconstrained     = baseline$unconstrained,
        lpc_scenario      = NULL,   # BAU -> LPC statis
        io_changes        = NULL,   # BAU -> Z baseline
        intensity_table   = NULL,
        pop_projection    = pop_projection()          # argumen baru di run_projection
      ),
      error = function(e) {
        warning("[hasil_bau] Error: ", conditionMessage(e))
        NULL
      }
    )
    
    # --- DEBUG ---
    cat("==== DEBUG hasil_bau ====\n")
    cat("class(hasil):", paste(class(hasil), collapse = ", "), "\n")
    cat("is.null(hasil):", is.null(hasil), "\n")
    if (!is.null(hasil)) {
      cat("nrow(hasil):", nrow(hasil), "\n")
    }
    cat("=========================\n")
    
    if (is.null(hasil)) return(NULL)
    
    list(
      scenario  = hasil,
      active    = attr(hasil, "active_assumptions"),
      baseline_meta = list(
        sector_names = attr(hasil, "sector_names"),
        years        = attr(hasil, "years")
      )
    )
  }, ignoreNULL = FALSE)
  
  # ==========================================================================
  # VISUALISASI
  # ==========================================================================
  
  agg_by_year <- function(df, indicator_col) {
    if (is.null(df)) return(NULL)
    if (!is.data.frame(df) && !is_tibble(df)) return(NULL)
    if (nrow(df) == 0) return(NULL)
    if (!indicator_col %in% names(df)) return(NULL)
    
    # Kolom yang nilainya konstan per sektor di tiap tahun (per-kapita):
    # ambil nilai unik (first), bukan di-sum.
    is_constant_per_year <- indicator_col %in% c("GDP_PER_CAPITA", "INCOME_PER_CAPITA")
    
    if (is_constant_per_year) {
      df |>
        dplyr::group_by(YEAR) |>
        dplyr::summarise(
          Total = dplyr::first(.data[[indicator_col]]),
          .groups = "drop"
        )
    } else {
      df |>
        dplyr::group_by(YEAR) |>
        dplyr::summarise(
          Total = sum(.data[[indicator_col]], na.rm = TRUE),
          .groups = "drop"
        )
    }
  }
  
  output$projection_plot <- renderPlot({
    res_sc  <- hasil_proyeksi()
    res_bau <- hasil_bau()
    
    req(res_sc,  res_bau)
    req(!is.null(res_sc$scenario),  !is.null(res_bau$scenario))
    
    ind_col   <- indicator_meta[[input$chart_indicator]]$col
    ind_label <- indicator_meta[[input$chart_indicator]]$label
    
    df_bau <- agg_by_year(res_bau$scenario, ind_col)
    df_sc  <- agg_by_year(res_sc$scenario,  ind_col)
    
    req(df_bau, df_sc)
    
    df_bau <- df_bau |> dplyr::mutate(Seri = "BAU")
    df_sc  <- df_sc  |> dplyr::mutate(Seri = scen_label())
    df_plot <- dplyr::bind_rows(df_bau, df_sc)
    
    if (identical(res_sc$active, "BAU")) {
      df_plot <- df_bau
      df_plot$Seri <- factor(df_plot$Seri, levels = "BAU")
    } else {
      df_plot$Seri <- factor(df_plot$Seri, levels = c("BAU", scen_label()))
    }
    
    p <- ggplot(df_plot, aes(x = YEAR, y = Total, color = Seri, group = Seri)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = unique(df_plot$YEAR)) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Proyeksi", ind_label),
        x     = "Tahun",
        y     = ind_label,
        color = NULL
      )
    
    if (!identical(res_sc$active, "BAU")) {
      p + scale_color_manual(
        values = setNames(c("#7f8c8d", "#27ae60"), c("BAU", scen_label()))
      )
    } else {
      p + scale_color_manual(values = c("BAU" = "#7f8c8d"))
    }
  })
  
  output$recap_table <- renderTable({
    res_sc  <- hasil_proyeksi()
    res_bau <- hasil_bau()
    
    req(res_sc,  res_bau)
    req(!is.null(res_sc$scenario),  !is.null(res_bau$scenario))
    
    ind_col <- indicator_meta[[input$chart_indicator]]$col
    df_bau  <- agg_by_year(res_bau$scenario, ind_col)
    df_sc   <- agg_by_year(res_sc$scenario,  ind_col)
    
    req(df_bau, df_sc)
    
    merged <- merge(df_bau, df_sc, by = "YEAR",
                    suffixes = c("_bau", "_sc"))
    merged$Selisih <- sprintf("%+.2f", (merged$Total_sc / merged$Total_bau - 1) * 100)
    
    out <- data.frame(
      Tahun    = as.character(merged$YEAR),
      BAU      = formatC(round(merged$Total_bau), format = "f", digits = 0, big.mark = ","),
      Skenario = formatC(round(merged$Total_sc),  format = "f", digits = 0, big.mark = ","),
      Selisih  = merged$Selisih,
      stringsAsFactors = FALSE
    )
    names(out) <- c("Tahun", "BAU", scen_label(), "Selisih (%)")
    out
  })
  
  # ---- Linkage ITBL & ITFL (hanya jika Asumsi 3 aktif) ----
  output$linkage_section <- renderUI({
    res <- hasil_proyeksi()
    if (is.null(res) || !("A3" %in% res$active)) {
      return(p("Linkage (ITBL dan ITFL) muncul jika Asumsi 3: Hilirisasi diisi.",
               class = "note"))
    }
    tagList(
      h5("Linkage (Asumsi 3: Hilirisasi)"),
      radioButtons(
        "linkage_type", NULL,
        choices  = c("ITBL (Backward)" = "ITBL", "ITFL (Forward)" = "ITFL"),
        selected = "ITBL",
        inline   = TRUE
      ),
      tableOutput("linkage_table"),
      plotOutput("linkage_chart", height = "350px")
    )
  })
  
  linkage_df <- reactive({
    res <- hasil_proyeksi()
    req(res, "A3" %in% res$active)
    lk <- attr(res$scenario, "linkage_compare")
    req(!is.null(lk))
    lk
  })
  
  # Jenis indeks terpilih (default ITBL)
  linkage_type_sel <- reactive({
    if (is.null(input$linkage_type)) "ITBL" else input$linkage_type
  })
  
  # Tabel: ITBL dan ITFL tampil bersamaan, diurutkan menurut |delta| indeks terpilih
  output$linkage_table <- renderTable({
    lk   <- linkage_df()
    type <- linkage_type_sel()
    lk   <- lk[order(-abs(lk[[paste0("DELTA_", type)]])), ]
    lk   <- head(lk, 10)
    
    data.frame(
      Sektor         = lk$SECTOR,
      `ITBL Sebelum` = lk$ITBL_BEFORE,
      `ITBL Sesudah` = lk$ITBL_AFTER,
      `ΔITBL`        = sprintf("%+.4f", lk$DELTA_ITBL),
      `ITFL Sebelum` = lk$ITFL_BEFORE,
      `ITFL Sesudah` = lk$ITFL_AFTER,
      `ΔITFL`        = sprintf("%+.4f", lk$DELTA_ITFL),
      check.names = FALSE
    )
  }, digits = 4)
  
  # Grafik: ikut pilihan ITBL / ITFL
  output$linkage_chart <- renderPlot({
    lk   <- linkage_df()
    type <- linkage_type_sel()
    
    d <- data.frame(
      Sektor  = lk$SECTOR,
      Sebelum = lk[[paste0(type, "_BEFORE")]],
      Sesudah = lk[[paste0(type, "_AFTER")]],
      stringsAsFactors = FALSE
    )
    d$Delta <- d$Sesudah - d$Sebelum
    d <- head(d[order(-abs(d$Delta)), ], 10)
    
    d_long <- tidyr::pivot_longer(
      d, cols = c("Sebelum", "Sesudah"),
      names_to = "Status", values_to = "Indeks"
    )
    d_long$Status <- factor(d_long$Status, levels = c("Sebelum", "Sesudah"))
    
    ggplot(d_long, aes(x = reorder(Sektor, Indeks), y = Indeks, fill = Status)) +
      geom_col(position = "dodge") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      coord_flip() +
      scale_fill_manual(values = c(Sebelum = "#bdc3c7", Sesudah = "#27ae60")) +
      theme_minimal(base_size = 12) +
      labs(title = paste0("Perubahan ", type, " (Top 10)"),
           x = NULL, y = paste("Indeks", type), fill = NULL)
  })
  
  # ==========================================================================
  # RUN HANDLER & DOWNLOAD
  # ==========================================================================
  
  status_text <- reactiveVal("Belum ada proses dijalankan.")
  error_text  <- reactiveVal("")
  
  status_type  <- reactiveVal("none")   # "none" | "message" | "warning" | "danger"
  status_title <- reactiveVal("")       # kosong = pakai judul default per tipe
  status_msg   <- reactiveVal("")
  
  observeEvent(input$processTAReg2, {
    problems <- character(0)
    
    if (isTRUE(input$load_rds)) {
      if (is.null(input$descriptive_rds_file)) {
        problems <- c(problems, "Berkas RDS Analisis Deskriptif belum dipilih.")
      }
    } else if (is.null(input$io_output_file)) {
      problems <- c(problems, "Berkas Hasil Analisis Input-Output Deskriptif belum dipilih.")
    }
    
    if (is.null(input$projection_area_file)) {
      problems <- c(problems, "Berkas Proyeksi Luas Lahan BAU belum dipilih.")
    }
    
    per <- tryCatch(periods(), error = function(e) NULL)
    if (is.null(per)) {
      problems <- c(problems, "Periode proyeksi belum valid.")
    }
    
    if (n_assumptions() > 0 && !nzchar(trimws(input$scenario_name))) {
      problems <- c(problems, "Nama skenario belum diisi.")
    }
    
    if (!is.null(a1_file()) && !("luc_projection" %in% a1_sheets())) {
      problems <- c(problems, "File Asumsi 1 tidak punya sheet luc_projection.")
    }
    
    d <- output_dir()
    if (is.null(d) || length(d) == 0) {
      problems <- c(problems, "Folder output belum dipilih.")
    }
    
    if (length(problems) > 0) {
      problem_text <- paste("-", problems, collapse = "\n")
      
      error_text(problem_text)
      status_text("Proses dihentikan. Lengkapi input berikut.")
      
      status_type("warning")
      status_title("Proses dihentikan. Lengkapi input berikut.")
      status_msg(problem_text)
      
      showNotification(
        tagList(
          tags$b("Proses dihentikan. Lengkapi input berikut."),
          tags$ul(
            style = "margin: 6px 0 0 0; padding-left: 18px;",
            lapply(problems, tags$li)
          )
        ),
        type     = "warning",
        duration = 8
      )
      return()
    }
    
    # --- Eksekusi proyeksi ---
    error_text("")
    status_text("Menjalankan proyeksi...")
    status_type("none")
    status_title("")
    
    withProgress(message = "Menjalankan analisis...", value = 0, {
      
      incProgress(0.1, detail = "Memuat baseline")
      
      result <- tryCatch({
        res <- hasil_proyeksi()
        list(ok = TRUE, res = res)
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      })
      
      if (!result$ok) {
        error_text(paste("Error:", result$msg))
        status_text("Proyeksi gagal. Periksa log error di bawah.")
        return()
      }
      
      incProgress(0.2, detail = "Menyiapkan tabel rekapitulasi")
      
      active <- result$res$active
      
      # --- Siapkan data untuk report ---
      indicator_labels <- list(
        "GDP"     = "PDRB (Rp Juta)",
        "LABOUR"  = "Serapan Tenaga Kerja (Orang)",
        "INCOME"  = "Pendapatan Tenaga Kerja (Rp Juta)",
        "SURPLUS" = "Surplus Usaha Bruto (Rp Juta)",
        "OUTPUT"  = "Output Total (Rp Juta)"
      )
      
      # --- Render HTML report ---
      incProgress(0.3, detail = "Rendering HTML report")
      
      scenario_slug <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name %||% ""))
      scenario_slug <- gsub("^_+|_+$", "", scenario_slug)
      if (!nzchar(scenario_slug)) scenario_slug <- "BAU"
      
      output_file <- paste0(
        "Projection_Report_",
        scenario_slug, "_",
        format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
        ".html"
      )
      
      report_template <- find_report_template("ta-regeco-projection.Rmd")
      message("[REPORT] Template: ", report_template)
      message("[REPORT] Exists  : ", file.exists(report_template))
      
      if (!file.exists(report_template)) {
        stop("Template tidak ditemukan di: ", report_template)
      }
      
      file_baseline_path <- if (isTRUE(input$load_rds)) {
        if (!is.null(input$descriptive_rds_file)) input$descriptive_rds_file$datapath else NULL
      } else {
        if (!is.null(input$io_output_file)) input$io_output_file$datapath else NULL
      }
      
      file_bau_path <- if (!is.null(input$projection_area_file)) input$projection_area_file$datapath else NULL
      file_a1_path  <- if (!is.null(a1_file())) a1_file()$datapath else NULL
      file_a2_path  <- if (!is.null(a2_file())) a2_file()$datapath else NULL
      file_a3_path  <- if (!is.null(a3_file())) a3_file()$datapath else NULL
      
      profile_info <- baseline_data()$profile
      
      # Fallback jika profile NULL / tidak lengkap
      if (is.null(profile_info)) {
        warning("[REPORT] profile NULL; menggunakan default.")
        profile_info <- list(
          area_name = "Wilayah Tidak Diketahui",
          currency  = "Rp Juta"
        )
      }
      
      unit_value <- if (!is.null(profile_info$currency) &&
                        nzchar(as.character(profile_info$currency)) &&
                        !is.na(profile_info$currency)) {
        as.character(profile_info$currency)
      } else {
        "Rp Juta"
      }
      
      location_val <- if (!is.null(profile_info$area_name) &&
                          nzchar(as.character(profile_info$area_name)) &&
                          !is.na(profile_info$area_name)) {
        as.character(profile_info$area_name)
      } else {
        "Wilayah Tidak Diketahui"
      }
      
      # --- LOG DEBUG (opsional, hapus setelah stabil) ---
      message("[REPORT] profile_info$area_name = ", profile_info$area_name)
      message("[REPORT] profile_info$currency  = ", profile_info$currency)
      message("[REPORT] unit_value             = ", unit_value)
      message("[REPORT] location_val           = ", location_val)
      
      message("[DEBUG] length(attr(hasil, 'sector_names'))  : ",
              length(attr(result$res$scenario, "sector_names")))
      message("[DEBUG] class(attr(hasil, 'sector_names'))   : ",
              paste(class(attr(result$res$scenario, "sector_names")), collapse = ", "))
      message("[DEBUG] length(result$res$baseline_meta$sector_names): ",
              length(result$res$baseline_meta$sector_names))
      message("[DEBUG] head(sector_names, 3): ",
              paste(head(result$res$baseline_meta$sector_names, 3), collapse = ", "))
      message("[DEBUG] any NA di sector_names: ",
              any(is.na(result$res$baseline_meta$sector_names)))
      message("[DEBUG] any empty di sector_names: ",
              any(!nzchar(as.character(result$res$baseline_meta$sector_names))))
      
      start_time <- Sys.time()
      
      tryCatch({
        rmarkdown::render(
          input       = report_template,
          output_file = output_file,
          output_dir  = output_dir(),
          params = list(
            start_time         = format(start_time, "%Y-%m-%d %H:%M:%S"),
            end_time           = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            scenario_name      = scen_label(),
            assumption_active  = active,
            initial_year       = initial_year_val(),
            end_year           = end_year_val(),
            time_step          = time_step_val(),
            unit     = unit_value,
            location = location_val,
            years              = result$res$baseline_meta$years,
            sector_names       = result$res$baseline_meta$sector_names,
            result_scenario    = as.data.frame(result$res$scenario),
            result_bau         = as.data.frame(hasil_bau()$scenario),
            recap_scenario     = as.data.frame(agg_by_year(result$res$scenario, "GDP")),
            recap_bau          = as.data.frame(agg_by_year(hasil_bau()$scenario, "GDP")),
            indicator_labels   = indicator_labels,
            linkage_compare    = attr(result$res$scenario, "linkage_compare"),
            output_dir         = output_dir(),
            file_baseline      = file_baseline_path,
            file_bau           = file_bau_path,
            file_a1            = file_a1_path,
            file_a2            = file_a2_path,
            file_a3            = file_a3_path,
            file_baseline_name = if (isTRUE(input$load_rds)) {
              if (!is.null(input$descriptive_rds_file)) input$descriptive_rds_file$name else NULL
            } else {
              if (!is.null(input$io_output_file)) input$io_output_file$name else NULL
            },
            file_bau_name      = if (!is.null(input$projection_area_file)) input$projection_area_file$name else NULL,
            file_a1_name       = if (!is.null(a1_file())) a1_file()$name else NULL,
            file_a2_name       = if (!is.null(a2_file())) a2_file()$name else NULL,
            file_a3_name       = if (!is.null(a3_file())) a3_file()$name else NULL
          ),
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
        
        incProgress(0.3, detail = "Report selesai")
        
        # DELAY 1.5 detik untuk memperhalus progress bar sebelum notifikasi hijau muncul
        Sys.sleep(1.5)
        
        # Notifikasi & Visual Alert HIJAU (BERHASIL)
        successMsg <- paste0("Skenario: ", scen_label(), " | Asumsi: ", paste(active, collapse = " + "))
        status_type("message")
        status_msg(successMsg)
        
        status_text(paste0(
          "Proyeksi selesai.\n",
          "Skenario   : ", scen_label(), "\n",
          "Asumsi     : ", paste(active, collapse = " + "), "\n",
          "Periode    : ", paste(periods(), collapse = ", "), "\n",
          "Sektor     : ", length(result$res$baseline_meta$sector_names), "\n",
          "Report     : ", output_file
        ))
        
        showNotification(
          "Analisis Berhasil Dijalankan!",
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        error_text(paste("Gagal render report:", conditionMessage(e)))
        status_text("Proyeksi selesai, tapi report gagal di-generate.")
        
        status_type("warning")
        status_msg("Proyeksi selesai, namun file HTML report gagal di-generate.")
        
        showNotification(
          "Analisis Selesai Sebagian: Report gagal di-generate.",
          type = "warning",
          duration = 5
        )
      })
    })
    
    show("download_results")
    show("open_report")
    show("open_output_folder")
  })
  
  # ---- Download hasil proyeksi (.xlsx) ----
  output$download_results <- downloadHandler(
    filename = function() {
      nm <- gsub("[^A-Za-z0-9_-]+", "_", trimws(input$scenario_name))
      if (!nzchar(nm)) nm <- "Skenario"
      paste0("Hasil_Proyeksi_", nm, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(hasil_proyeksi(), hasil_bau())
      
      wb <- openxlsx::createWorkbook()
      
      recap_sc <- agg_by_year(hasil_proyeksi()$scenario, "GDP")
      openxlsx::addWorksheet(wb, "Recap_Skenario")
      openxlsx::writeData(wb, "Recap_Skenario", recap_sc)
      
      recap_bau <- agg_by_year(hasil_bau()$scenario, "GDP")
      openxlsx::addWorksheet(wb, "Recap_BAU")
      openxlsx::writeData(wb, "Recap_BAU", recap_bau)
      
      if ("GDP_PER_CAPITA" %in% names(hasil_proyeksi()$scenario)) {
        per_kapita_sc <- hasil_proyeksi()$scenario |>
          dplyr::group_by(YEAR) |>
          dplyr::summarise(
            Populasi            = dplyr::first(POPULATION),
            PDRB_Total_Juta     = sum(GDP, na.rm = TRUE),
            PDRB_per_Kapita     = dplyr::first(GDP_PER_CAPITA),
            Income_Total_Juta   = sum(INCOME, na.rm = TRUE),
            Income_per_Kapita   = dplyr::first(INCOME_PER_CAPITA),
            .groups = "drop"
          )
        openxlsx::addWorksheet(wb, "Per_Kapita")
        openxlsx::writeData(wb, "Per_Kapita", per_kapita_sc)
      }
      
      openxlsx::addWorksheet(wb, "Detail_Skenario")
      openxlsx::writeData(wb, "Detail_Skenario", hasil_proyeksi()$scenario)
      
      openxlsx::addWorksheet(wb, "Detail_BAU")
      openxlsx::writeData(wb, "Detail_BAU", hasil_bau()$scenario)
      
      lk <- attr(hasil_proyeksi()$scenario, "linkage_compare")
      if (!is.null(lk)) {
        openxlsx::addWorksheet(wb, "Linkage")
        openxlsx::writeData(wb, "Linkage", lk)
      }
      
      meta <- data.frame(
        Parameter = c("Nama Skenario", "Asumsi Aktif", "Tahun Awal", "Tahun Akhir",
                      "Interval", "Jumlah Sektor", "Tanggal Proses"),
        Nilai = c(scen_label(),
                  paste(hasil_proyeksi()$active, collapse = " + "),
                  initial_year_val(), end_year_val(), time_step_val(),
                  length(hasil_proyeksi()$baseline_meta$sector_names),
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )
      openxlsx::addWorksheet(wb, "Metadata")
      openxlsx::writeData(wb, "Metadata", meta)
      
      if (!is.null(output_dir())) {
        out_path <- file.path(output_dir(), basename(file))
        try(openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE), silent = TRUE)
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  output$status_messages <- renderText(status_text())
  output$error_messages  <- renderText(error_text())
  
  observeEvent(input$open_report, {
    d <- output_dir()
    if (is.null(d) || length(d) == 0) {
      showNotification("Folder output belum dipilih.", type = "warning")
      return()
    }
    
    # Cari file report HTML terbaru di folder output
    reports <- list.files(
      d,
      pattern    = "^Projection_Report.*\\.html$",
      full.names = TRUE
    )
    
    if (length(reports) == 0) {
      showNotification(
        "Belum ada file report di folder output. Jalankan Run Analysis terlebih dahulu.",
        type = "warning", duration = 5
      )
      return()
    }
    
    # Ambil file yang paling baru dimodifikasi
    report_path <- reports[which.max(file.info(reports)$mtime)]
    
    # Buka di browser default (cross-platform)
    tryCatch({
      if (.Platform$OS.type == "windows") {
        shell.exec(report_path)
      } else if (Sys.info()[["sysname"]] == "Darwin") {
        system2("open", shQuote(report_path))
      } else {
        system2("xdg-open", shQuote(report_path))
      }
      
      showNotification(
        paste("Membuka:", basename(report_path)),
        type = "message", duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Gagal membuka report:", conditionMessage(e)),
        type = "error", duration = 5
      )
    })
  })
  
  observeEvent(input$open_output_folder, {
    d <- output_dir()
    if (!is.null(d) && length(d) > 0 && dir.exists(d)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(normalizePath(d))
      } else if (Sys.info()[["sysname"]] == "Darwin") {
        system2("open", shQuote(normalizePath(d)))
      } else {
        system2("xdg-open", shQuote(normalizePath(d)))
      }
    }
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    stopApp()
  })
  
  # ---- User guide ----
  output$user_guide <- renderUI({
    guide_paths <- c(
      "helpfile/quick_user_guide_ID.Rmd",
      "../helpfile/quick_user_guide.Rmd",
      file.path(getwd(), "helpfile", "quick_user_guide_ID.Rmd"),
      file.path(dirname(getwd()), "helpfile", "quick_user_guide_ID.Rmd")
    )
    
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- tryCatch(
          rmarkdown::render(
            path,
            output_format = "html_fragment",
            quiet         = TRUE,
            envir         = new.env(parent = globalenv())
          ),
          error = function(e) {
            warning("[user_guide] Gagal render: ", conditionMessage(e))
            NULL
          }
        )
        
        if (!is.null(html_content) && file.exists(html_content)) {
          return(HTML(paste(readLines(html_content), collapse = "\n")))
        }
      }
    }
    
    HTML("<p style='color:#6c757d;'>User guide file not found. Expected at: <code>helpfile/quick_user_guide.Rmd</code></p>")
  })
}

shinyApp(ui, server)