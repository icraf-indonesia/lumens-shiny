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

# --- Load TARegEco package ---
if (requireNamespace("TARegEco", quietly = TRUE)) {
  library(TARegEco)
} else {
  devtools::load_all(".")
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
  
  titlePanel("Regional Economic Descriptive Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      # --- Opsi: Load dari RDS preprocessing (opsional) ---
      checkboxInput(
        "load_rds",
        "Muat dari RDS praproses (lewati pembacaan Excel)",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.load_rds == true",
        fileInput(
          "preprocessing_rds_file",
          "Preprocessing RDS (.rds)",
          accept = c(".rds")
        ),
        checkboxInput(
          "new_lc_area",
          "Perbarui luas tutupan lahan dari berkas baru",
          value = FALSE
        )
      ),
      # --- Input File IO (Excel, multi-sheet) ---
      fileInput(
        "io_file",
        "Input-Output File (.xlsx)",
        accept = c(".xlsx", ".xls"),
        placeholder = "Pilih file Excel IO (multi-sheet)"
      ),
      
      # --- Input File Land Use Lookup Table (Excel) ---
      fileInput(
        "landuse_table_file",
        "Land Use Lookup Table (.xlsx)",
        accept = c(".xlsx", ".xls"),
        placeholder = "Pilih file Excel lookup table"
      ),
      
      # --- Metadata (read-only, dari profil) ---
      tags$hr(),
      tags$b("Metadata (auto-detected)"),
      verbatimTextOutput("metadata_preview"),
      
      tags$hr(),
      
      # --- Output directory ---
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
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
    wd           = "",
    report_file  = NULL,
    load_rds     = FALSE,
    new_lc_area  = FALSE,
    profile      = NULL
  )
  
  # --- Output directory ---
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "wd", roots = volumes, session = session)
  
  observeEvent(input$wd, {
    rv$wd <- parseDirPath(volumes, input$wd)
  })
  
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if (length(rv$wd) == 0) return()
    paste0("Selected output directory: ", rv$wd)
  })
  
  # --- User guide ---
  output$user_guide <- renderUI({
    guide_paths <- c(
      "helpfile/ta-reg1_quick_user_guide.Rmd",
      "../../08_ta-regional1/helpfile/ta-reg1_quick_user_guide.Rmd"
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
  
  # --- Preview metadata ---
  output$metadata_preview <- renderText({
    # Opsi 1: dari RDS
    if (rv$load_rds) {
      if (is.null(input$preprocessing_rds_file)) {
        return("RDS: menunggu file...")
      }
      obj <- tryCatch(
        readRDS(input$preprocessing_rds_file$datapath),
        error = function(e) NULL
      )
      if (!is.null(obj) && !is.null(obj$io$profile)) {
        p <- obj$io$profile
        return(paste0(
          "Source  : RDS\n",
          "Area    : ", p$area_name, "\n",
          "Year    : ", p$year, "\n",
          "Currency: ", p$currency, "\n",
          "IO Src  : ", p$io_source %||% "-"
        ))
      }
      return("RDS: gagal dibaca.")
    }
    
    # Opsi 2: dari Excel IO
    if (!is.null(input$io_file)) {
      profil_io <- tryCatch(
        read_input_file(input$io_file$datapath,
                        sheet = "0. Profil Data",
                        skip = 2, header = TRUE),
        error = function(e) NULL
      )
      if (!is.null(profil_io) && nrow(profil_io) >= 3) {
        profile <- list(
          area_name = trimws(profil_io$Deskripsi[1]),
          year      = as.numeric(profil_io$Deskripsi[2]),
          currency  = trimws(profil_io$Deskripsi[3]),
          io_source = trimws(profil_io$Deskripsi[4])
        )
        rv$profile <- profile
        return(paste0(
          "Source  : Excel\n",
          "Area    : ", profile$area_name, "\n",
          "Year    : ", profile$year, "\n",
          "Currency: ", profile$currency, "\n",
          "IO Src  : ", profile$io_source
        ))
      }
      return("Excel: gagal membaca sheet '0. Profil Data'.")
    }
    
    "Belum ada input."
  })
  
  # --- Process Analysis ---
  observeEvent(input$processTAReg1, {
    
    # --- Validasi input ---
    missing_inputs <- c()
    
    if (!rv$load_rds) {
      if (is.null(input$io_file))            missing_inputs <- c(missing_inputs, "IO File (.xlsx)")
      if (is.null(input$landuse_table_file)) missing_inputs <- c(missing_inputs, "Land Use Lookup Table (.xlsx)")
    } else {
      if (is.null(input$preprocessing_rds_file)) {
        missing_inputs <- c(missing_inputs, "Preprocessing RDS (.rds)")
      }
      if (rv$new_lc_area && is.null(input$landuse_table_file)) {
        missing_inputs <- c(missing_inputs, "Land Use Lookup Table (.xlsx)")
      }
    }
    
    if (is.null(rv$wd) || rv$wd == "") {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Please upload the following inputs:",
              paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    withProgress(message = "Running TA Regional 1 Analysis", value = 0, {
      tryCatch({
        
        output_dir <- rv$wd
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
        # SAVE CORE ANALYSIS RDS
        # =====================================================================
        incProgress(0.05, detail = "Saving core analysis RDS")
        
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
        
        save_core_analysis_data(core_analysis_results)
        
        # =====================================================================
        # EXPORT EXCEL & RDS
        # =====================================================================
        incProgress(0.05, detail = "Exporting Excel & RDS")
        
        export_io_analysis(
          sector          = sector,
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
          output_path     = file.path(output_dir, "IO_Analysis_Output.xlsx")
        )
        
        save_analysis_data(
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
    if (!is.null(rv$wd) && dir.exists(rv$wd)) {
      if (.Platform$OS.type == "windows") shell.exec(rv$wd) else system2("open", args = rv$wd)
    } else {
      showNotification("Output directory not found", type = "error")
    }
  })
  
  # --- Open report ---
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") shell.exec(rv$report_file) else system2("open", args = rv$report_file)
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