#' Check and Install Required Packages
#'
#' This function checks if a list of required packages are installed and loaded.
#' If any packages are missing or cannot be loaded, it prompts the user to install them.
#'
#' @param required_packages A character vector of package names to check and potentially install.
#'
#' @return None. This function is called for its side effects.
#'
#' @details
#' The function performs the following steps:
#' 1. Checks if each package in the list is installed.
#' 2. Attempts to load each installed package.
#' 3. If any packages are missing or fail to load, prompts the user to install them.
#' 4. If the user agrees, attempts to install and load the missing packages.
#'
#' @examples
#' \dontrun{
#' required_packages <- c("dplyr", "ggplot2", "tidyr")
#' check_and_install_packages(required_packages)
#' }
#'
#' @export
check_and_install_packages <- function(required_packages) {
  # Check if each package is installed and can be loaded
  missing_packages <- character(0)
  for (package in required_packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      missing_packages <- c(missing_packages, package)
    } else {
      tryCatch(
        {
          library(package, character.only = TRUE)
          cat(paste0("Package '", package, "' is installed and loaded.\n"))
        },
        error = function(e) {
          missing_packages <<- c(missing_packages, package)
          cat(paste0("Package '", package, "' is installed but could not be loaded: ", e$message, "\n"))
        }
      )
    }
  }
  
  # If there are missing packages, ask the user if they want to install them
  if (length(missing_packages) > 0) {
    cat("\nThe following packages are missing or could not be loaded:\n")
    cat(paste0("- ", missing_packages, "\n"))
    
    install_choice <- readline(prompt = "Do you want to install/reinstall these packages? (y/n): ")
    
    if (tolower(install_choice) == "y") {
      for (package in missing_packages) {
        cat(paste0("\nAttempting to install package '", package, "'...\n"))
        tryCatch(
          {
            install.packages(package)
            library(package, character.only = TRUE)
            cat(paste0("Package '", package, "' has been successfully installed and loaded.\n"))
          },
          error = function(e) {
            cat(paste0("Failed to install package '", package, "': ", e$message, "\n"))
          }
        )
      }
    } else {
      cat("\nPackage installation skipped. Some required packages are missing.\n")
    }
  } else {
    cat("\nAll required packages are installed and loaded.\n")
  }
}

#' Create area matrix from long projection data
#'
#' Converts a long data frame of land cover areas into a matrix where rows are
#' land cover IDs and columns are projection years.
#'
#' @param projection_long A data frame with columns `ID`, `YEAR`, and `AREA`.
#'   Usually created using `tidyr::pivot_longer()`.
#'
#' @return A numeric matrix with:
#'   \item{rows}{Land cover IDs}
#'   \item{columns}{Projection years (as character)}
#'
#' @examples
#' \dontrun{
#' area_matrix <- create_area_matrix(projection_long)
#' }
#'
#' @export
create_area_matrix <- function(projection_long) {
  years <- sort(unique(projection_long$YEAR))
  ids <- sort(unique(projection_long$ID))
  
  area_matrix <- matrix(
    0,
    nrow = length(ids),
    ncol = length(years),
    dimnames = list(ids, years)
  )
  
  for (i in seq_len(nrow(projection_long))) {
    area_matrix[
      as.character(projection_long$ID[i]),
      as.character(projection_long$YEAR[i])
    ] <- projection_long$AREA[i]
  }
  
  area_matrix
}

#' Read input file (Excel or CSV) into a tibble or matrix
#'
#' Reads an input file from Excel (`.xlsx`, `.xls`) or CSV (`.csv`, `.txt`),
#' with optional conversion to a numeric matrix.
#'
#' @param path Character. Path to the input file.
#' @param sheet Character or `NULL`. Sheet name for Excel files. If `NULL`,
#'   the first sheet is used.
#' @param skip Integer. Number of rows to skip before reading data.
#' @param header Logical. If `TRUE`, the first row after `skip` is used as
#'   column names.
#' @param as_matrix Logical. If `TRUE`, the data is returned as a matrix.
#'
#' @return A `tibble` if `as_matrix = FALSE`, or a `matrix` if `TRUE`.
#'
#' @details
#' The function automatically detects the file extension. For Excel files,
#' the `readxl` package is used. For CSV/TXT files, `utils::read.csv` is used.
#'
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom tibble as_tibble
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' sector <- read_input_file("data/io.xlsx", sheet = "sector", header = TRUE)
#' ic <- read_input_file("data/io.xlsx", sheet = "intermediate_consumption",
#'                        as_matrix = TRUE)
#' }
#'
#' @export
read_input_file <- function(path, sheet = NULL, skip = 0, header = FALSE, as_matrix = FALSE) {
  ext <- tolower(tools::file_ext(path))
  
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required for Excel files.")
    }
    data <- readxl::read_excel(path, sheet = sheet, skip = skip, col_names = header)
  } else if (ext %in% c("csv", "txt")) {
    data <- utils::read.csv(path, skip = skip, header = header)
  } else {
    stop("Unsupported file format. Please upload CSV or Excel.")
  }
  
  data <- tibble::as_tibble(data)
  
  if (as_matrix) {
    return(as.matrix(data))
  }
  
  return(data)
}

#' Load projection baseline data from Excel or RDS
#'
#' Loads descriptive analysis results required to run the projection module.
#' The input can be either an Excel workbook (`IO_Analysis_Output.xlsx`) or
#' an RDS file produced by the descriptive analysis module.
#'
#' @param path Character. Path to the descriptive analysis file. Supported
#'   extensions are `.xlsx`, `.xls`, and `.rds`.
#' @param projection_area_file_bau Character. Path to the BAU land projection
#'   file (Excel/CSV) containing columns `ID`, `LC`, and one column per
#'   projection year.
#'
#' @return A list containing:
#'   \item{gdp_baseline}{Tibble of baseline GDP and related coefficients}
#'   \item{leontief_matrix}{Leontief inverse matrix}
#'   \item{full_io_table}{Complete IO matrix}
#'   \item{code_registry}{Code registry tibble}
#'   \item{sector_idx}{Index of sector rows in the IO table}
#'   \item{income_multiplier}{Income multiplier table}
#'   \item{labour_multiplier}{Labour multiplier table}
#'   \item{SUB_multiplier}{Gross operating surplus multiplier table}
#'   \item{output_multiplier}{Output multiplier table}
#'   \item{linkage_table}{Linkage table}
#'   \item{land_prop}{Land proportion matrix}
#'   \item{P_default}{Default land proportion matrix}
#'   \item{growth_rate}{Baseline growth rate data}
#'   \item{area_matrix_bau}{BAU area matrix}
#'   \item{projection_area_bau}{Raw BAU land projection table}
#'
#' @examples
#' \dontrun{
#' baseline <- load_projection_baseline(
#'   path = "output/descriptive_analysis_module.rds",
#'   projection_area_file_bau = "data/luc_projection_bau.xlsx"
#' )
#' }
#'
#' @export
load_projection_baseline <- function(path, projection_area_file_bau) {
  
  profile <- list(
    area_name = "Wilayah Tidak Diketahui",
    year      = NA_integer_,
    currency  = "Rp Juta",
    io_source = "-"
  )
  
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    
    # =====================================================
    # RDS BRANCH
    # =====================================================
    io <- readRDS(path)
    
    gdp_baseline      <- io$gdp_baseline
    leontief_matrix   <- io$linkage$Leontief
    full_io_table     <- io$full_io_table
    code_registry     <- io$code_registry
    sector_idx        <- which(code_registry$Type == "Sector")
    income_multiplier <- io$multipliers$income
    labour_multiplier <- io$multipliers$labour
    SUB_multiplier    <- io$multipliers$sub
    output_multiplier <- io$multipliers$output
    linkage_table     <- io$linkage
    growth_rate       <- io$growth_rate
    
    if (!is.null(io$profile) && is.list(io$profile)) {
      profile <- list(
        area_name = io$profile$area_name %||% profile$area_name,
        year      = io$profile$year      %||% profile$year,
        currency  = io$profile$currency  %||% profile$currency,
        io_source = io$profile$io_source %||% profile$io_source
      )
    } else if (!is.null(io$metadata$profile) && is.list(io$metadata$profile)) {
      profile <- list(
        area_name = io$metadata$profile$area_name %||% profile$area_name,
        year      = io$metadata$profile$year      %||% profile$year,
        currency  = io$metadata$profile$currency  %||% profile$currency,
        io_source = io$metadata$profile$io_source %||% profile$io_source
      )
    }
    
    # Intermediate consumption: langsung dari RDS, fallback ke full_io_table
    if (!is.null(io$intermediate_consumption)) {
      intermediate_consumption <- io$intermediate_consumption
    } else {
      sector_idx <- which(code_registry$Type == "Sector")
      intermediate_consumption <- full_io_table[sector_idx, sector_idx, drop = FALSE]
    }
    storage.mode(intermediate_consumption) <- "numeric"
    
    # Land proportion
    land_prop <- io$land_proportion
    P_default <- as.matrix(land_prop)
    storage.mode(P_default) <- "numeric"
    
    # Sector index: constrained / unconstrained
    if (!is.null(io$constrained) && !is.null(io$unconstrained)) {
      constrained   <- io$constrained
      unconstrained <- io$unconstrained
    } else {
      berbasis_lahan <- tolower(trimws(as.character(io$sector$Berbasis_Lahan)))
      constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
      unconstrained  <- 1 - constrained
    }
    
  } else if (grepl("\\.xlsx$|\\.xls$", path, ignore.case = TRUE)) {
    
    # =====================================================
    # EXCEL BRANCH
    # =====================================================
    sheets <- readxl::excel_sheets(path)
    
    profile_sheet <- intersect(
      c("Profile Data", "Profil Data", "0. Profil Data", "Profile", "Profil"),
      sheets
    )[1]
    
    if (!is.na(profile_sheet) && length(profile_sheet) == 1) {
      profile_raw <- tryCatch(
        readxl::read_excel(path, sheet = profile_sheet, col_names = TRUE),
        error = function(e) NULL
      )
      
      if (!is.null(profile_raw) && nrow(profile_raw) > 0) {
        pnames <- tolower(trimws(as.character(profile_raw[[1]])))
        pvals  <- as.character(profile_raw[[2]])
        
        pick <- function(pattern) {
          idx <- grep(pattern, pnames, ignore.case = TRUE)
          if (length(idx) > 0) trimws(pvals[idx[1]]) else NA_character_
        }
        
        area_name <- pick("lokasi|area")
        year_val  <- pick("tahun|year")
        currency  <- pick("mata uang|currency")
        io_source <- pick("sumber|source")
        
        profile <- list(
          area_name = if (!is.na(area_name)) area_name else profile$area_name,
          year      = if (!is.na(year_val))  as.integer(year_val) else profile$year,
          currency  = if (!is.na(currency))  currency  else profile$currency,
          io_source = if (!is.na(io_source)) io_source else profile$io_source
        )
      }
    }
    
    gdp_sheet_candidates <- c("GDP", "Analisis Deskriptif", "Analisis_Deskriptif",
                              "Gdp", "gdp")
    gdp_sheet <- intersect(gdp_sheet_candidates, sheets)[1]
    
    if (is.na(gdp_sheet) || length(gdp_sheet) == 0) {
      stop("Sheet 'GDP' (atau alias 'Analisis Deskriptif') tidak ditemukan. ",
           "Sheet tersedia: ", paste(sheets, collapse = ", "))
    }
    
    gdp_baseline <- read_input_file(path, sheet = gdp_sheet, header = TRUE)
    if (is.null(gdp_baseline$CATEGORY)) {
      gdp_baseline$CATEGORY <- rep("Unknown", nrow(gdp_baseline))
    }
    
    leontief_matrix <- as.matrix(
      read_input_file(path, sheet = "Leontief_Matrix", as_matrix = TRUE)
    )
    
    full_io_table <- as.matrix(
      read_input_file(path, sheet = "Full_IO_Table", as_matrix = TRUE)
    )
    
    code_registry <- read_input_file(path, sheet = "Code_Registry", header = TRUE)
    
    income_multiplier <- read_input_file(path, sheet = "Multiplier_Income", header = TRUE)
    labour_multiplier <- read_input_file(path, sheet = "Multiplier_Labour", header = TRUE)
    SUB_multiplier    <- read_input_file(path, sheet = "Multiplier_SUB",    header = TRUE)
    output_multiplier <- read_input_file(path, sheet = "Multiplier_Output", header = TRUE)
    linkage_table     <- read_input_file(path, sheet = "Linkage",           header = TRUE)
    
    # Land proportion — deteksi fleksibel berbagai format
    land_prop_raw <- read_input_file(path, sheet = "Land_Proportion", header = TRUE)
    col_names     <- colnames(land_prop_raw)
    
    # ---- Deteksi format ----
    # A. Format baru: kolom "V1", "V2", ... (LC codes) → orientasi Sektor × LC
    # B. Format lama: kolom "S1", "S2", ... (Sector codes) → orientasi LC × Sektor (perlu transpose)
    # C. Format label: ada kolom "Sektor"/"ID" + kolom numerik
    
    sector_cols <- grep("^S\\d+$", col_names, value = TRUE)   # S1, S2, ...
    lc_cols     <- grep("^V\\d+$", col_names, value = TRUE)   # V1, V2, ...
    
    if (length(lc_cols) > 0) {
      # ---- Format A: V1, V2, ... (26 LC) — orientasi sudah Sektor × LC ----
      P_default <- as.matrix(land_prop_raw[, lc_cols, drop = FALSE])
      storage.mode(P_default) <- "numeric"
      
    } else if (length(sector_cols) > 0) {
      # ---- Format B: S1, S2, ... (66 sektor) — orientasi LC × Sektor, perlu transpose ----
      P_default <- t(as.matrix(land_prop_raw[, sector_cols, drop = FALSE]))
      storage.mode(P_default) <- "numeric"
      
    } else {
      # ---- Format C: kolom pertama = label, sisanya LC ----
      label_candidates <- c("Sektor", "SECTOR", "Sector", "sector",
                            "ID", "id", "Kode", "KODE", "LC")
      label_col <- intersect(label_candidates, col_names)
      
      numeric_cols <- setdiff(col_names, label_col)
      if (length(numeric_cols) == 0) {
        stop("Land_Proportion sheet tidak memiliki kolom tutupan lahan yang valid.")
      }
      
      P_default <- suppressWarnings(
        as.matrix(land_prop_raw[, numeric_cols, drop = FALSE])
      )
      P_default <- apply(P_default, 2, function(x) as.numeric(as.character(x)))
      if (is.null(dim(P_default))) P_default <- matrix(P_default, ncol = 1)
      P_default[is.na(P_default)] <- 0
      storage.mode(P_default) <- "numeric"
    }
    
    # Intermediate consumption from Full IO Table
    sector_idx <- which(code_registry$Type == "Sector")
    intermediate_consumption <- full_io_table[sector_idx, sector_idx, drop = FALSE]
    storage.mode(intermediate_consumption) <- "numeric"
    
    # Growth rate
    growth_rate <- tryCatch(
      read_input_file(path, sheet = "Growth_Rate", header = TRUE),
      error = function(e) NULL
    )
    
    # Sector index: constrained / unconstrained from sector sheet
    sector_tbl <- read_input_file(path, sheet = "Sector", header = TRUE)
    berbasis_lahan <- tolower(trimws(as.character(sector_tbl[[3]])))
    constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
    unconstrained  <- 1 - constrained
    
  } else {
    stop("Unsupported file format. Please provide .xlsx or .rds.")
  }
  
  # =====================================================
  # AREA MATRIX BAU
  # =====================================================
  sector_idx <- which(code_registry$Type == "Sector")
  
  projection_area_bau <- read_input_file(projection_area_file_bau, header = TRUE)
  
  projection_long_bau <- projection_area_bau |>
    tidyr::pivot_longer(
      cols = -c(ID, LC),
      names_to = "YEAR",
      values_to = "AREA"
    ) |>
    dplyr::mutate(YEAR = as.numeric(YEAR))
  
  area_matrix_bau <- create_area_matrix(projection_long_bau)
  
  # =====================================================
  # RETURN
  # =====================================================
  list(
    gdp_baseline             = gdp_baseline,
    leontief_matrix          = leontief_matrix,
    full_io_table            = full_io_table,
    code_registry            = code_registry,
    sector_idx               = sector_idx,
    intermediate_consumption = intermediate_consumption,
    income_multiplier        = income_multiplier,
    labour_multiplier        = labour_multiplier,
    SUB_multiplier           = SUB_multiplier,
    output_multiplier        = output_multiplier,
    linkage_table            = linkage_table, 
    land_prop                = P_default, # proporsi lahan dari tahap IO, Data input statis
    P_default                = P_default, # parameter awal untuk proyeksi, Parameter yang bisa di-override
    growth_rate              = growth_rate,
    area_matrix_bau          = area_matrix_bau,
    projection_area_bau      = projection_area_bau,
    constrained              = constrained,
    unconstrained            = unconstrained,
    profile                  = profile
  )
}

#' Load scenario inputs with fallback to baseline
#'
#' @param sc_input Named list berisi path file skenario:
#'   `luc_projection`, `land_distribution`, `growth_rate`, `lpc`,
#'   `io_changes`, `intensity`.
#' @param P_default Matriks proporsi lahan baseline.
#' @param growth_rate Data growth rate baseline.
#'
#' @return List berisi:
#'   \item{luc_projection}{Path ke file proyeksi lahan}
#'   \item{area_matrix_scenario}{Matriks tutupan lahan skenario (opsional)}
#'   \item{P}{Matriks proporsi lahan skenario}
#'   \item{lpc_scenario}{Tibble LPC skenario atau NULL}
#'   \item{io_changes}{Tibble perubahan IO atau NULL}
#'
#' @export
load_scenario_inputs <- function(sc_input, P_default) {
  
  # --- A. Land Distribution (sheet "land_proportion") ------------------------
  # Default: gunakan P_default baseline
  P_sc <- P_default
  
  if (!is.null(sc_input$land_distribution) &&
      file.exists(sc_input$land_distribution)) {
    
    # Cek apakah sheet "land_proportion" ada di file
    sheet_names <- readxl::excel_sheets(sc_input$land_distribution)
    
    if ("land_proportion" %in% sheet_names) {
      # ✓ Sheet ada → baca dan proses
      cat("✓ Sheet 'land_proportion' ditemukan → gunakan proporsi skenario\n")
      
      land_prop_sc <- read_input_file(
        sc_input$land_distribution,
        header = TRUE,
        sheet  = "land_proportion"
      )
      
      # Kolom LC = semua kolom selain ID dan Sektor
      lc_cols <- setdiff(colnames(land_prop_sc), c("ID", "Sektor"))
      
      if (length(lc_cols) == 0) {
        stop("Tidak ditemukan kolom tutupan lahan pada sheet 'land_proportion'.")
      }
      
      # Keluarkan kolom ID & Sektor → matriks numerik
      P_sc <- as.matrix(land_prop_sc[, lc_cols])
      storage.mode(P_sc) <- "numeric"
      
      # Set nama baris = Sektor, kolom = nama tutupan lahan
      if ("Sektor" %in% colnames(land_prop_sc)) {
        rownames(P_sc) <- as.character(land_prop_sc$Sektor)
      }
      
      # Validasi dimensi
      if (ncol(P_sc) != ncol(P_default)) {
        stop("Jumlah kolom tutupan lahan pada skenario (", ncol(P_sc),
             ") tidak sesuai dengan baseline (", ncol(P_default), ").")
      }
      if (nrow(P_sc) != nrow(P_default)) {
        stop("Jumlah baris sektor pada skenario (", nrow(P_sc),
             ") tidak sesuai dengan baseline (", nrow(P_default), ").")
      }
      
      # Samakan nama dengan P_default
      rownames(P_sc) <- rownames(P_default)
      colnames(P_sc) <- colnames(P_default)
      
    } else {
      # ✗ Sheet tidak ada → pakai P_default
      cat("ℹ Sheet 'land_proportion' tidak ditemukan → gunakan P_default baseline\n")
      P_sc <- P_default
    }
    
  } else {
    cat("ℹ File land_distribution tidak tersedia → gunakan P_default baseline\n")
    P_sc <- P_default
  }
  
  # --- B. LUC Projection (sheet "luc_projection") ----------------------------
  area_matrix_sc <- NULL
  if (!is.null(sc_input$luc_projection) &&
      file.exists(sc_input$luc_projection)) {
    
    sheet_names  <- readxl::excel_sheets(sc_input$luc_projection)
    sheet_target <- if ("luc_projection" %in% sheet_names) {
      "luc_projection"
    } else {
      1  # fallback: sheet pertama
    }
    
    df_luc <- read_input_file(
      sc_input$luc_projection,
      header = TRUE,
      sheet  = sheet_target
    )
    
    # Asumsi: kolom pertama = ID, kolom kedua = LC, sisanya tahun
    projection_long <- df_luc |>
      tidyr::pivot_longer(
        cols      = -c(ID, LC),
        names_to  = "YEAR",
        values_to = "AREA"
      ) |>
      dplyr::mutate(YEAR = as.numeric(YEAR))
    
    area_matrix_sc <- create_area_matrix(projection_long)
  }
  
  # --- C. LPC Productivity ---------------------------------------------------
  lpc_sc <- NULL
  
  if (!is.null(sc_input$lpc) && file.exists(sc_input$lpc)) {
    # 1. Panggil read_input_file dengan parameter sheet dan skip
    # Catatan: skip = 11 melompati 11 baris judul/info sehingga Baris 12 menjadi Header Kolom.
    lpc_sc <- read_input_file(
      path = sc_input$lpc,
      sheet = "Skenario_LPC",
      skip = 11, # Sesuaikan dengan template
      header = TRUE
    )
    
    # 2. Ekstraksi & Konversi Kolom secara Fleksibel (Mendukung Template Baru & Lama)
    if ("LPC SKENARIO 2 (Rp Juta/Ha)" %in% colnames(lpc_sc)) {
      # Format Template produktivitas_lpc.xlsx Baru
      lpc_sc$LPC_S2 <- as.numeric(lpc_sc$`LPC SKENARIO 2 (Rp Juta/Ha)`)
      
      if ("TARGET GROWTH FACTOR (%)" %in% colnames(lpc_sc)) {
        lpc_sc$Growth_Factor <- as.numeric(lpc_sc$`TARGET GROWTH FACTOR (%)`)
      }
    } else if ("Increment_Rate" %in% colnames(lpc_sc)) {
      # Format Skrip Lama
      lpc_sc$Increment_Rate <- as.numeric(lpc_sc$Increment_Rate)
      
      if ("Area_Implemented" %in% colnames(lpc_sc)) {
        lpc_sc$Area_Implemented <- as.numeric(lpc_sc$Area_Implemented)
      }
    }
  }
  
  # --- D. IO Changes / Restrukturisasi (sheet "Restrukturisasi") ------------
  io_sc <- NULL
  if (!is.null(sc_input$io_changes) && file.exists(sc_input$io_changes)) {
    sheet_names <- readxl::excel_sheets(sc_input$io_changes)
    
    # Deteksi nama sheet Restrukturisasi (fleksibel "Restrukturisasi" atau "Skenario_4_Restrukturisasi")
    if ("Restrukturisasi" %in% sheet_names) {
      sheet_target <- "Restrukturisasi"
      skip_rows <- 11
    } else if ("Skenario_4_Restrukturisasi" %in% sheet_names) {
      sheet_target <- "Skenario_4_Restrukturisasi"
      skip_rows <- 11
    } else {
      sheet_target <- 1
      skip_rows <- 0
    }
    
    io_sc <- read_input_file(
      path   = sc_input$io_changes,
      sheet  = sheet_target,
      skip   = skip_rows,
      header = TRUE
    )
    
    # Penanganan format kolom lama (jika ada Change_Pct)
    if ("Change_Pct" %in% colnames(io_sc)) {
      io_sc$Change_Pct <- as.numeric(io_sc$Change_Pct) / 100
    }
  }
  
  
  list(
    luc_projection       = sc_input$luc_projection,
    area_matrix_scenario = area_matrix_sc,
    P                    = P_sc,
    lpc_scenario         = lpc_sc,
    io_changes           = io_sc
  )
}

#' Load Population Projection from Excel or CSV
#'
#' Membaca file proyeksi penduduk dengan 2 kolom wajib:
#' `Tahun` (atau `YEAR`) dan `Populasi` (atau `POPULATION`).
#' Nama kolom dideteksi secara fleksibel (case-insensitive).
#'
#' @param path Character. Path ke file `.xlsx`, `.xls`, atau `.csv`.
#' @param sheet Character atau Integer. Nama/indeks sheet untuk file Excel.
#'   Default: sheet pertama.
#'
#' @return Tibble dengan kolom `Tahun` (integer) dan `Populasi` (numeric),
#'   terurut menaik berdasarkan Tahun. Mengembalikan `NULL` jika file tidak
#'   valid atau kolom wajib tidak ditemukan.
#'
#' @export
load_population_projection <- function(path, sheet = 1) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  # ---- 1. Baca file sesuai ekstensi ----
  ext <- tolower(tools::file_ext(path))
  
  df <- tryCatch({
    if (ext %in% c("xlsx", "xls")) {
      as.data.frame(readxl::read_excel(path, sheet = sheet))
    } else if (ext == "csv") {
      as.data.frame(readr::read_csv(path, show_col_types = FALSE))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    warning("[load_population_projection] Gagal baca file: ", conditionMessage(e))
    NULL
  })
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # ---- 2. Deteksi kolom Tahun dan Populasi secara fleksibel ----
  col_names <- tolower(trimws(colnames(df)))
  
  year_candidates <- c("tahun", "year", "yr", "periode", "period")
  pop_candidates  <- c("populasi", "population", "pop", "jumlah_penduduk",
                       "jumlah penduduk", "penduduk", "total_population")
  
  year_col <- which(col_names %in% year_candidates)
  pop_col  <- which(col_names %in% pop_candidates)
  
  if (length(year_col) == 0 || length(pop_col) == 0) {
    warning(
      "[load_population_projection] Kolom 'Tahun' dan/atau 'Populasi' tidak ditemukan. ",
      "Kolom tersedia: ", paste(colnames(df), collapse = ", ")
    )
    return(NULL)
  }
  
  # Ambil kolom pertama jika ada duplikat
  year_col <- year_col[1]
  pop_col  <- pop_col[1]
  
  # ---- 3. Sanitasi nilai ----
  years <- suppressWarnings(as.integer(df[[year_col]]))
  pops  <- suppressWarnings(as.numeric(df[[pop_col]]))
  
  # Buang baris dengan NA
  valid_idx <- !is.na(years) & !is.na(pops) & pops > 0
  years <- years[valid_idx]
  pops  <- pops[valid_idx]
  
  if (length(years) == 0) {
    warning("[load_population_projection] Tidak ada baris valid setelah sanitasi.")
    return(NULL)
  }
  
  # ---- 4. Urutkan dan kembalikan tibble ----
  ord <- order(years)
  tibble::tibble(
    Tahun    = years[ord],
    Populasi = pops[ord]
  )
}


#' Interpolate Population Vector to Match Projection Years
#'
#' Menginterpolasi (dan ekstrapolasi linear) vektor populasi agar sesuai
#' dengan rentang tahun proyeksi. Jika tahun berada di luar rentang data,
#' ekstrapolasi linear menggunakan 2 titik terdekat.
#'
#' @param pop_df Tibble hasil `load_population_projection()`.
#' @param target_years Numeric vector. Tahun proyeksi yang diinginkan.
#'
#' @return Numeric vector sepanjang `length(target_years)` dengan nilai
#'   populasi hasil interpolasi.
#'
#' @export
interpolate_population <- function(pop_df, target_years) {
  if (is.null(pop_df) || nrow(pop_df) == 0) {
    stop("[interpolate_population] pop_df kosong.")
  }
  
  pop_df <- pop_df[order(pop_df$Tahun), ]
  
  # Jika hanya 1 titik, anggap populasi konstan
  if (nrow(pop_df) == 1) {
    return(rep(pop_df$Populasi[1], length(target_years)))
  }
  
  # Interpolasi linear; ekstrapolasi otomatis dengan rule = 2
  # (batas kiri/kanan = titik terdekat)
  approx(
    x    = pop_df$Tahun,
    y    = pop_df$Populasi,
    xout = target_years,
    method = "linear",
    rule   = 2,
    ties   = mean
  )$y
}

#' Calculate LPC growth rate
#'
#' Calculates the Land Productivity Coefficient (LPC) growth rate for a given
#' sector and projection period, based on crop type.
#'
#' @param lpc_scenario Data frame containing LPC scenario inputs with columns:
#'   `Sector_Code`, `Start_Year`, `End_Year`, `Increment_Rate`,
#'   `Area_Implemented`, and `Crop_Type`.
#' @param sector Numeric. Sector code to be evaluated.
#' @param period_idx Numeric. Index of the current projection period.
#' @param periods_df Data frame containing `Start_Year` and `End_Year`.
#'
#' @return Numeric. LPC growth rate.
#'
#' @details
#' For annual crops, the growth rate is:
#' \deqn{growth = IncrementRate \times AreaImplemented}
#'
#' For perennial crops, the growth rate is:
#' \deqn{growth = AreaImplemented_{now} \times \sum_{k=1}^{now} IncrementRate_k}
#'
#' @examples
#' \dontrun{
#' gr <- calculate_lpc_growth_rate(lpc_scenario, 1, 1, periods_df)
#' }
#'
#' @export
calculate_lpc_growth_rate <- function(lpc_scenario, sector, period_idx, periods_df) {
  if (is.null(lpc_scenario)) return(0)
  
  sub <- lpc_scenario[
    lpc_scenario$Sector_Code == sector &
      lpc_scenario$Start_Year <= periods_df$End_Year[period_idx],
  ]
  
  if (nrow(sub) == 0) return(0)
  
  # Urutkan agar periode berurutan
  sub <- sub[order(sub$Start_Year), ]
  
  crop_type <- sub$Crop_Type[1]
  
  if (crop_type == "annual") {
    row_now <- sub[sub$Start_Year == periods_df$Start_Year[period_idx], ]
    if (nrow(row_now) == 0) return(0)
    return(row_now$Increment_Rate * row_now$Area_Implemented)
  } else if (crop_type == "perennial") {
    area_now <- sub$Area_Implemented[nrow(sub)]
    total_increment <- sum(sub$Increment_Rate)
    return(area_now * total_increment)
  } else {
    stop("Crop_Type tidak dikenal: ", crop_type)
  }
}

#' Apply IO table changes for a specific period
#'
#' Modifies cells in the full IO table according to user-defined scenario
#' changes for the given projection period.
#'
#' @param full_io_table Numeric matrix. Complete Input-Output table.
#' @param io_changes Data frame with columns `Start_Year`, `End_Year`,
#'   `Row_Code`, `Col_Code`, and `Change_Pct`. Can be `NULL` if no changes.
#' @param period_start Numeric. Start year of the period.
#' @param period_end Numeric. End year of the period.
#'
#' @return Numeric matrix. Updated full IO table.
#'
#' @examples
#' \dontrun{
#' new_io <- apply_io_changes(full_io_table, io_changes, 2024, 2029)
#' }
#'
#' @export
apply_io_changes <- function(full_io_table, io_changes, period_start, period_end) {
  if (is.null(io_changes)) return(full_io_table)
  
  changes_period <- io_changes[
    io_changes$Start_Year == period_start &
      io_changes$End_Year == period_end,
  ]
  
  if (nrow(changes_period) == 0) return(full_io_table)
  
  for (i in seq_len(nrow(changes_period))) {
    r <- changes_period$Row_Code[i]
    c <- changes_period$Col_Code[i]
    pct <- changes_period$Change_Pct[i]
    full_io_table[r, c] <- full_io_table[r, c] * (1 + pct)
  }
  
  return(full_io_table)
}

#' Recalculate Leontief inverse after IO changes
#'
#' Recalculates the Leontief inverse matrix using the current full IO table
#' and sector output.
#'
#' @param full_io_table Numeric matrix. Complete IO table.
#' @param sector_idx Integer vector. Row/column indices of sectors.
#' @param output_total Numeric vector. Total output per sector.
#'
#' @return Numeric matrix. New Leontief inverse.
#'
#' @examples
#' \dontrun{
#' L <- recalculate_leontief(full_io_table, sector_idx, output_baseline)
#' }
#'
#' @export
recalculate_leontief <- function(full_io_table, sector_idx, output_total) {
  intermediate <- full_io_table[sector_idx, sector_idx, drop = FALSE]
  A <- sweep(intermediate, 2, output_total, "/")
  A[!is.finite(A)] <- 0
  I <- diag(nrow(A))
  L <- solve(I - A)
  return(L)
}

#' Get intensity for a sector and period
#'
#' Returns the implementation intensity for a given sector in a specific
#' projection period. If no intensity table is provided, returns 1 (100%).
#'
#' @param intensity_table Data frame with columns `Sector_Code`,
#'   `Start_Year`, `End_Year`, and `Intensity`. Can be `NULL`.
#' @param sector Numeric. Sector code.
#' @param period_start Numeric. Start year of the period.
#' @param period_end Numeric. End year of the period.
#'
#' @return Numeric. Intensity value (0–1).
#'
#' @examples
#' \dontrun{
#' inten <- get_intensity(intensity_table, 1, 2024, 2029)
#' }
#'
#' @export
get_intensity <- function(intensity_table, sector, period_start, period_end) {
  if (is.null(intensity_table)) return(1.0)
  
  row <- intensity_table[
    intensity_table$Sector_Code == sector &
      intensity_table$Start_Year == period_start &
      intensity_table$End_Year == period_end,
  ]
  
  if (nrow(row) == 0) return(1.0)
  return(row$Intensity[1])
}

#' Run projection (Supply-Constrained Mixed Input-Output Model)
#'
#' Simulates regional economic indicators over time using a supply-constrained
#' mixed input-output model. Land-constrained (co) sectors are driven by
#' land availability × land productivity coefficient (LPC), while
#' non-land (nc) sectors are solved via Leontief inverse.
#'
#' @param gdp_baseline Data frame. Baseline GDP table from descriptive analysis.
#' @param income_multiplier Data frame. Income multiplier table (kolom Labour_Compensation).
#' @param labour_multiplier Data frame. Labour multiplier table (kolom Labour).
#' @param SUB_multiplier Data frame. SUB multiplier table (kolom Surplus).
#' @param output_multiplier Data frame. Output multiplier table (unused in this model).
#' @param linkage_table Data frame. Linkage table with ITFL and ITBL.
#' @param leontief_matrix Numeric matrix. Baseline Leontief inverse (unused here).
#' @param full_io_table Numeric matrix. Complete IO table (Z matrix).
#' @param sector_idx Integer vector. Sector indices in full IO table.
#' @param P Numeric matrix. Land proportion matrix [n_sektor × n_tutupan].
#' @param area_matrix Numeric matrix. Land area matrix [n_tutupan × n_tahun].
#' @param growth_rate Numeric vector. Growth rate permintaan akhir per sektor.
#' @param lpc_scenario Data frame or `NULL`. LPC scenario inputs.
#' @param io_changes Data frame or `NULL`. IO change scenarios.
#' @param intensity_table Data frame or `NULL`. Intensity scenarios.
#' @param constrained Integer vector. Indikator sektor co (1 = constrained).
#' @param unconstrained Integer vector. Indikator sektor nc (1 = unconstrained).
#'
#' @return A data frame with projected indicators for all sectors and years.
#'
#' @export
run_projection <- function(
    gdp_baseline,
    income_multiplier,
    labour_multiplier,
    SUB_multiplier,
    output_multiplier,
    linkage_table,
    leontief_matrix,
    full_io_table,
    sector_idx,
    P,
    area_matrix,
    growth_rate,
    lpc_scenario    = NULL,
    io_changes      = NULL,
    intensity_table = NULL,
    constrained     = NULL,
    unconstrained   = NULL
) {
  
  # ============================================================================
  # 1. PERSIAPAN DIMENSI & VEKTOR BASELINE
  # ============================================================================
  
  years <- as.numeric(colnames(area_matrix))
  years <- sort(years)
  t_max <- length(years)
  
  if (length(years) > 1) {
    time_step <- years[2] - years[1]
  } else {
    time_step <- 1
  }
  
  initial_year <- if (!is.null(getOption("initial_year"))) {
    getOption("initial_year")
  } else {
    years[1] - time_step
  }
  
  # Ekstrak vektor dari gdp_baseline
  sektor_names <- as.character(unlist(gdp_baseline$SECTOR))
  Y_0          <- as.numeric(unlist(gdp_baseline$FD));          names(Y_0)    <- sektor_names
  x_0          <- as.numeric(unlist(gdp_baseline$OUTPUT));      names(x_0)    <- sektor_names
  v_0          <- as.numeric(unlist(gdp_baseline$GDP));         names(v_0)    <- sektor_names
  Land_0       <- as.numeric(unlist(gdp_baseline$Land_Requirement))
  LPC_0        <- as.numeric(unlist(gdp_baseline$LPC))
  
  # Z matrix (intermediate consumption)
  Z <- as.matrix(full_io_table)
  storage.mode(Z) <- "numeric"
  rownames(Z) <- colnames(Z) <- sektor_names
  
  # Filter sektor dummy (X_0 = 0)
  idx_valid    <- which(x_0 > 0)
  Z            <- Z[idx_valid, idx_valid, drop = FALSE]
  x_0          <- x_0[idx_valid]
  Y_0          <- Y_0[idx_valid]
  v_0          <- v_0[idx_valid]
  Land_0       <- Land_0[idx_valid]
  LPC_0        <- LPC_0[idx_valid]
  sektor_names <- sektor_names[idx_valid]
  n            <- length(idx_valid)
  
  # Koefisien input primer (proporsi terhadap output baseline)
  v_c <- as.numeric(unlist(gdp_baseline$GDP_PROP))[idx_valid]
  
  w_c <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid] / x_0
  l_c <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid] / x_0
  sub_c <- as.numeric(unlist(SUB_multiplier$Surplus))[idx_valid] / x_0
  
  # Growth rate per sektor
  growth_rate <- as.numeric(unlist(growth_rate))
  if (length(growth_rate) != nrow(gdp_baseline)) {
    warning("Panjang growth_rate tidak sesuai. Gunakan default 0.")
    growth_rate <- rep(0, nrow(gdp_baseline))
  }
  growth_rate <- growth_rate[idx_valid]
  
  # ============================================================================
  # 2. IDENTIFIKASI SEKTOR CO & NC
  # ============================================================================
  
  # Kalau constrained/unconstrained tidak diberikan, deteksi dari LPC > 0
  if (is.null(constrained) || is.null(unconstrained)) {
    constrained   <- as.integer(LPC_0 > 0 & Land_0 > 0)
    unconstrained <- as.integer(!(LPC_0 > 0 & Land_0 > 0))
  }
  
  co_idx <- which(idx_valid %in% which(constrained == 1))
  no_idx <- which(idx_valid %in% which(unconstrained == 1))
  
  # Reklasifikasi: sektor co dengan L=0 atau LPC=0 → pindah ke nc
  mask_bermasalah <- (Land_0[co_idx] <= 0) | (LPC_0[co_idx] <= 0)
  
  if (any(mask_bermasalah)) {
    co_idx_pindah <- co_idx[mask_bermasalah]
    co_idx        <- co_idx[!mask_bermasalah]
    no_idx        <- sort(c(no_idx, co_idx_pindah))
  }
  
  idx_co <- co_idx
  idx_nc <- no_idx
  p_0_co <- as.numeric(LPC_0[idx_co])
  m      <- length(idx_co)
  
  # ============================================================================
  # 3. FORMULASI MATRIKS LEONTIEF & SUB-PARTISI
  # ============================================================================
  
  A_full <- sweep(Z, 2, x_0, "/")
  M_full <- diag(n) - A_full
  B_full <- solve(M_full)
  
  # Indeks keterkaitan (ITBL & ITFL)
  col_sum_B   <- colSums(B_full)
  row_sum_B   <- rowSums(B_full)
  total_sum_B <- sum(B_full)
  
  ITBL_vector <- (n * col_sum_B) / total_sum_B
  ITFL_vector <- (n * row_sum_B) / total_sum_B
  
  # Sub-partisi M
  M_nc_co     <- M_full[idx_nc, idx_co, drop = FALSE]
  M_nc_nc     <- M_full[idx_nc, idx_nc, drop = FALSE]
  M_nc_nc_inv <- solve(M_nc_nc)
  
  # P matrix untuk sektor co: [n_co × n_tutupan]
  P_sub <- as.matrix(P[idx_co, , drop = FALSE])
  storage.mode(P_sub) <- "numeric"
  
  # ============================================================================
  # 4. ITERASI PROYEKSI MULTI-INDIKATOR
  # ============================================================================
  
  x_out             <- matrix(0, nrow = t_max, ncol = n)
  PDRB_sektoral     <- matrix(0, nrow = t_max, ncol = n)
  Inc_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  Lab_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  SUB_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  L_co_matrix       <- matrix(0, nrow = t_max, ncol = m)
  LPC_growth_matrix <- matrix(0, nrow = t_max, ncol = m)
  
  # LPC dinamis untuk skenario
  current_lpc_co <- p_0_co   # salinan, akan diupdate tiap tahun
  
  for (k in seq_len(t_max)) {
    yr             <- years[k]
    selisih_tahun  <- yr - initial_year
    
    # --- A. Permintaan akhir sektor nc (eksogen) ---
    Y_nc_t <- Y_0[idx_nc] * ((1 + growth_rate[idx_nc]) ^ selisih_tahun)
    
    # --- B. Update LPC per sektor co (jika ada lpc_scenario) ---
    lpc_gr_vec <- rep(0, m)
    if (!is.null(lpc_scenario)) {
      for (j in seq_along(idx_co)) {
        sector_code <- gdp_baseline$Sector_Code[idx_co[j]]
        lpc_gr <- calculate_lpc_growth_rate(
          lpc_scenario, sector_code, k,
          data.frame(Start_Year = yr, End_Year = yr)
        )
        lpc_gr_vec[j] <- lpc_gr
        current_lpc_co[j] <- current_lpc_co[j] * (1 + lpc_gr)
      }
    }
    current_lpc_co[!is.finite(current_lpc_co)] <- 0
    
    # --- C. Output sektor co dibatasi lahan ---
    luas_tutupan_t <- as.numeric(unlist(area_matrix[, k]))   # [n_tutupan]
    L_co_t         <- as.vector(P_sub %*% luas_tutupan_t)    # [n_co]
    x_co_t         <- matrix(L_co_t * current_lpc_co, ncol = 1)   # [n_co × 1]
    
    # --- D. Mixed IO solve untuk sektor nc ---
    rhs    <- Y_nc_t - (M_nc_co %*% x_co_t)
    x_nc_t <- M_nc_nc_inv %*% rhs
    
    # --- E. Rekonstruksi output total ---
    x_t             <- numeric(n)
    x_t[idx_co]     <- as.vector(x_co_t)
    x_t[idx_nc]     <- as.vector(x_nc_t)
    
    # --- F. Multi-indikator ---
    PDRB_sektoral[k, ] <- v_c * x_t
    Inc_sektoral[k, ]  <- w_c * x_t
    Lab_sektoral[k, ]  <- l_c * x_t
    SUB_sektoral[k, ]  <- sub_c * x_t
    x_out[k, ]         <- x_t
    L_co_matrix[k, ]   <- L_co_t
    LPC_growth_matrix[k, ] <- lpc_gr_vec
  }
  
  # ============================================================================
  # 5. REKAPITULASI HASIL DALAM FORMAT TIBBLE (PER TAHUN × SEKTOR)
  # ============================================================================
  
  # Bangun hasil per (tahun × sektor)
  results_list <- list()
  
  for (k in seq_len(t_max)) {
    yr <- years[k]
    
    # Vektor per sektor (panjang n)
    land_req_t      <- numeric(n)
    land_req_t[idx_co] <- L_co_matrix[k, ]
    land_req_t[idx_nc] <- 0   # sektor nc tidak pakai lahan
    
    lpc_growth_t      <- numeric(n)
    lpc_growth_t[idx_co] <- LPC_growth_matrix[k, ]
    
    # LPC per sektor (hanya co yang > 0)
    lpc_t      <- numeric(n)
    lpc_t[idx_co] <- current_lpc_co  # setelah update
    
    results_list[[as.character(yr)]] <- tibble::tibble(
      YEAR             = yr,
      SECTOR           = sektor_names,
      LAND_REQUIREMENT = land_req_t,
      LPC              = lpc_t,
      LPC_GROWTH_RATE  = lpc_growth_t,
      FD               = Y_0,                     # permintaan akhir baseline
      OUTPUT           = x_out[k, ],
      GDP              = PDRB_sektoral[k, ],
      INCOME           = Inc_sektoral[k, ],
      LABOUR           = Lab_sektoral[k, ],
      SURPLUS          = SUB_sektoral[k, ],
      ITBL             = ITBL_vector,
      ITFL             = ITFL_vector
    )
  }
  
  final_result <- do.call(rbind, results_list)
  final_result <- final_result |>
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(!is.finite(.x), 0, .x)
      )
    )
  
  return(final_result)
}

#' Indeks keterkaitan total (Rasmussen) dari matriks invers Leontief
#'
#' ITBL_j = n * sum_i(L_ij) / sum(L)   (backward, per kolom)
#' ITFL_i = n * sum_j(L_ij) / sum(L)   (forward,  per baris)
#'
#' Dipakai untuk kondisi "sebelum" (invers Leontief baseline) dan "sesudah"
#' (L_new hasil RAS) supaya definisinya identik.
#'
#' @param L Matriks invers Leontief [n x n].
#' @param sector_names Vektor nama sektor (opsional).
#' @return List berisi `itbl` dan `itfl` (vektor numerik panjang n).
#' @keywords internal
#' @noRd
compute_linkage_index <- function(L, sector_names = NULL) {
  L     <- as.matrix(L)
  n     <- nrow(L)
  total <- sum(L)
  
  itbl <- as.numeric((n * colSums(L)) / total)
  itfl <- as.numeric((n * rowSums(L)) / total)
  
  if (!is.null(sector_names)) {
    names(itbl) <- sector_names
    names(itfl) <- sector_names
  }
  
  list(itbl = itbl, itfl = itfl)
}

#' Diagnostik io_changes terhadap baseline (hanya peringatan/pesan)
#'
#' Mengecek dua hal yang membuat Asumsi 3 gagal senyap:
#' (1) nama sektor asal/tujuan tidak cocok dengan baseline, dan
#' (2) sel [From, To] di Z baseline bernilai 0. RAS hanya menskalakan sel yang
#' sudah ada, jadi hubungan baru tidak terbentuk lewat penambahan transaksi.
#' Hanya berjalan jika kolom `SEKTOR ASAL (FROM SECTOR)` dan
#' `SEKTOR TUJUAN (TO SECTOR)` ada.
#'
#' @keywords internal
#' @noRd
check_io_changes <- function(io_changes, sektor_names, Z) {
  from_col <- "SEKTOR ASAL (FROM SECTOR)"
  to_col   <- "SEKTOR TUJUAN (TO SECTOR)"
  if (!all(c(from_col, to_col) %in% names(io_changes))) return(invisible(NULL))
  
  from_nm <- as.character(io_changes[[from_col]])
  to_nm   <- as.character(io_changes[[to_col]])
  keep    <- !is.na(from_nm) & !is.na(to_nm)
  from_nm <- from_nm[keep]
  to_nm   <- to_nm[keep]
  
  unknown <- unique(c(setdiff(from_nm, sektor_names), setdiff(to_nm, sektor_names)))
  if (length(unknown) > 0) {
    warning(
      "Nama sektor pada io_changes tidak ditemukan di baseline (baris dilewati): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  
  i  <- match(from_nm, sektor_names)
  j  <- match(to_nm,   sektor_names)
  ok <- !is.na(i) & !is.na(j)
  
  if (any(ok)) {
    n_zero <- sum(Z[cbind(i[ok], j[ok])] == 0, na.rm = TRUE)
    if (n_zero > 0) {
      message(sprintf(
        "  ! %d dari %d pasangan From-To bernilai 0 di Z baseline. RAS tidak membentuk hubungan baru dari sel nol.",
        n_zero, sum(ok)
      ))
    }
  }
  
  invisible(NULL)
}

#' Run Projection Scenario (Supply-Constrained Mixed Input-Output Model)
#'
#' Accepts any free combination of BAU scenario, Assumption 1, Assumption 2,
#' and Assumption 3. All scenario parameters are optional. The function will
#' automatically enable the appropriate logic based on which parameters are
#' not NULL.
#'
#' Supported combinations:
#' \enumerate{
#'   \item BAU only                              (area_bau, lpc=NULL,  io=NULL)
#'   \item BAU + A1                              (area_sc,  lpc=NULL,  io=NULL)
#'   \item BAU + A1 + A2                         (area_sc,  lpc=...,  io=NULL)
#'   \item BAU + A1 + A3                         (area_sc,  lpc=NULL,  io=...)
#'   \item BAU + A2                              (area_bau, lpc=...,  io=NULL)
#'   \item BAU + A2 + A3                         (area_bau, lpc=...,  io=...)
#'   \item BAU + A3                              (area_bau, lpc=NULL,  io=...)
#' }
#'
#' @param gdp_baseline Data frame. Baseline GDP table.
#' @param income_multiplier,labour_multiplier,SUB_multiplier,output_multiplier,linkage_table
#'   Data frame. Baseline multiplier & linkage tables.
#' @param leontief_matrix Numeric matrix. Baseline Leontief inverse (unused).
#' @param full_io_table Numeric matrix. Complete Z matrix.
#' @param sector_idx Integer vector. Sector indices in the full IO table.
#' @param P_baseline Numeric matrix. Baseline land proportion [n_sector x n_cover].
#' @param area_matrix_bau Numeric matrix. BAU land area [n_cover x n_year].
#' @param growth_rate Numeric vector. Baseline final demand growth rate.
#' @param constrained,unconstrained Integer vector. Co/nc indicator flags.
#'
#' @param P_scenario Numeric matrix or NULL. Scenario land proportion (Assumption 1).
#' @param area_matrix_scenario Numeric matrix or NULL. Scenario land area (Assumption 1).
#' @param lpc_scenario Data frame or NULL. New LPC targets (Assumption 2).
#' @param lpc_target_year Integer or NULL. LPC target year (Assumption 2). Default: last year.
#' @param io_changes Data frame or NULL. Scenario ΔZ & ΔX (Assumption 3).
#' @param growth_rate_scenario Numeric vector or NULL. Growth rate override.
#' @param pop_projection Data frame atau NULL. Proyeksi penduduk dengan kolom
#'   `Tahun` dan `Populasi`. Jika NULL, kolom Pendapatan/PDRB per Kapita akan
#'   bernilai NA. (Opsional)
#'
#' @return Tibble of projection results per (year x sector) with columns:
#'   YEAR, SECTOR, LAND_REQUIREMENT, LPC, LPC_GROWTH_RATE, FD, OUTPUT,
#'   GDP, INCOME, LABOUR, SURPLUS, ITBL, ITFL (end/active condition values).
#'   Diagnostic attributes, including `linkage_compare`: a data frame per sector
#'   containing SECTOR, ITBL_BEFORE, ITBL_AFTER, DELTA_ITBL, ITFL_BEFORE,
#'   ITFL_AFTER, DELTA_ITFL. Without Assumption 3, BEFORE values equal AFTER values.
#'
#' @export
run_projection_scenario <- function(
    # --- Required baseline ---
  gdp_baseline,
  income_multiplier,
  labour_multiplier,
  SUB_multiplier,
  output_multiplier,
  linkage_table,
  leontief_matrix,
  full_io_table,
  sector_idx,
  P_baseline,
  area_matrix_bau,
  growth_rate,
  constrained     = NULL,
  unconstrained   = NULL,
  
  # --- Assumption 1: Land Use Change ---
  P_scenario           = NULL,
  area_matrix_scenario = NULL,
  
  # --- Assumption 2: Productivity Improvement ---
  lpc_scenario    = NULL,
  lpc_target_year = NULL,
  
  # --- Assumption 3: Downstreaming (Hilirisasi) ---
  io_changes = NULL,
  
  # --- Optional override ---
  growth_rate_scenario = NULL,
  
  # --- Population Projection ---
  pop_projection = NULL
) {
  
  # ==========================================================================
  # 0. ACTIVE SCENARIO LOG (for debugging & Shiny)
  # ==========================================================================
  use_a1 <- !is.null(area_matrix_scenario)
  use_a2 <- !is.null(lpc_scenario) && nrow(lpc_scenario) > 0
  use_a3 <- !is.null(io_changes)   && nrow(io_changes)   > 0
  
  active_assumptions <- c(
    if (use_a1) "A1",
    if (use_a2) "A2",
    if (use_a3) "A3"
  )
  if (length(active_assumptions) == 0) active_assumptions <- "BAU"
  
  message(sprintf("[run_projection] Active assumptions: %s",
                  paste(active_assumptions, collapse = " + ")))
  
  # ==========================================================================
  # 1. BASELINE DIMENSION & VECTOR PREPARATION
  # ==========================================================================
  
  # ---- Area matrix: use scenario if available, fallback to BAU ----
  area_matrix <- if (use_a1) area_matrix_scenario else area_matrix_bau
  
  years <- sort(as.numeric(colnames(area_matrix)))
  t_max <- length(years)
  time_step <- if (length(years) > 1) years[2] - years[1] else 1
  
  initial_year <- if (!is.null(getOption("initial_year"))) {
    getOption("initial_year")
  } else {
    years[1] - time_step
  }
  
  # ---- Interpolasi populasi ke rentang tahun proyeksi ----
  pop_vector <- NULL
  has_population <- !is.null(pop_projection) && 
    is.data.frame(pop_projection) && 
    nrow(pop_projection) > 0
  
  if (has_population) {
    pop_vector <- tryCatch(
      interpolate_population(pop_projection, years),
      error = function(e) {
        warning("[run_projection] Gagal interpolasi populasi: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(pop_vector) && all(pop_vector > 0)) {
      message(sprintf(
        "[run_projection] Population projection loaded: %d years, %.0f → %.0f",
        t_max, pop_vector[1], pop_vector[t_max]
      ))
    } else {
      has_population <- FALSE
      pop_vector <- NULL
    }
  } else {
    message("[run_projection] No population projection provided; per-capita indicators will be NA.")
  }
  
  # ---- P matrix: use scenario if available, fallback to baseline ----
  P <- if (!is.null(P_scenario)) P_scenario else P_baseline
  
  # ---- Vector extraction ----
  sektor_names <- as.character(unlist(gdp_baseline$SECTOR))
  Y_0    <- as.numeric(unlist(gdp_baseline$FD));      names(Y_0)    <- sektor_names
  x_0    <- as.numeric(unlist(gdp_baseline$OUTPUT));  names(x_0)    <- sektor_names
  v_0    <- as.numeric(unlist(gdp_baseline$GDP));     names(v_0)    <- sektor_names
  Land_0 <- as.numeric(unlist(gdp_baseline$Land_Requirement))
  LPC_0  <- as.numeric(unlist(gdp_baseline$LPC))
  
  # ---- Z matrix ----
  Z <- as.matrix(full_io_table); storage.mode(Z) <- "numeric"
  rownames(Z) <- colnames(Z) <- sektor_names
  
  # ---- Filter dummy sectors ----
  idx_valid    <- which(x_0 > 0)
  Z            <- Z[idx_valid, idx_valid, drop = FALSE]
  x_0          <- x_0[idx_valid]
  Y_0          <- Y_0[idx_valid]
  v_0          <- v_0[idx_valid]
  Land_0       <- Land_0[idx_valid]
  LPC_0        <- LPC_0[idx_valid]
  sektor_names <- sektor_names[idx_valid]
  n            <- length(idx_valid)
  
  # ---- Primary input coefficients ----
  v_c   <- as.numeric(unlist(gdp_baseline$GDP_PROP))[idx_valid]
  w_c   <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid] / x_0
  l_c   <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid] / x_0
  sub_c <- as.numeric(unlist(SUB_multiplier$Surplus))[idx_valid] / x_0
  
  # Nominal vectors for RAS
  labour_nominal <- as.numeric(unlist(labour_multiplier$Labour))[idx_valid]
  wage_nominal   <- as.numeric(unlist(labour_multiplier$Labour_Compensation))[idx_valid]
  
  # ---- Growth rate: override if provided ----
  gr_vec <- if (!is.null(growth_rate_scenario)) growth_rate_scenario else growth_rate
  gr_vec <- as.numeric(unlist(gr_vec))
  if (length(gr_vec) != nrow(gdp_baseline)) {
    warning("growth_rate length mismatch, falling back to 0.")
    gr_vec <- rep(0, nrow(gdp_baseline))
  }
  gr_vec <- gr_vec[idx_valid]
  gr_vec[is.na(gr_vec)] <- 0
  
  # ==========================================================================
  # 2. IDENTIFY CO / NC SECTORS
  # ==========================================================================
  if (is.null(constrained) || is.null(unconstrained)) {
    constrained   <- as.integer(LPC_0 > 0 & Land_0 > 0)
    unconstrained <- as.integer(!(LPC_0 > 0 & Land_0 > 0))
  }
  
  co_idx <- which(idx_valid %in% which(constrained == 1))
  no_idx <- which(idx_valid %in% which(unconstrained == 1))
  
  mask_bermasalah <- (Land_0[co_idx] <= 0) | (LPC_0[co_idx] <= 0)
  if (any(mask_bermasalah)) {
    co_idx_pindah <- co_idx[mask_bermasalah]
    co_idx        <- co_idx[!mask_bermasalah]
    no_idx        <- sort(c(no_idx, co_idx_pindah))
  }
  
  idx_co <- co_idx
  idx_nc <- no_idx
  p_0_co <- as.numeric(LPC_0[idx_co])
  m      <- length(idx_co)
  
  # ==========================================================================
  # 3. BASELINE LEONTIEF MATRIX & "BEFORE" LINKAGE FORMULATION
  # ==========================================================================
  A_full <- sweep(Z, 2, x_0, "/")
  M_full <- diag(n) - A_full
  B_full <- solve(M_full)
  
  # "Before" linkage: from baseline Leontief inverse (A = Z / x_0, same basis
  # as the RAS starting point, so the before-vs-after comparison is apples-to-apples)
  link_before <- compute_linkage_index(B_full, sektor_names)
  ITBL_vector <- link_before$itbl
  ITFL_vector <- link_before$itfl
  
  # ==========================================================================
  # 4. ASSUMPTION 3: IO SYSTEM OVERRIDE VIA RAS
  # ==========================================================================
  sc_io_system <- NULL
  
  if (use_a3) {
    message("[run_projection] Running RAS balancing (Assumption 3)...")
    
    # Diagnostics: sector name mismatches / From-To sectors with zero values
    check_io_changes(io_changes, sektor_names, Z)
    
    sc_io_system <- process_sc_ras_system(
      df_input     = io_changes,
      Z_base       = Z,
      X_base       = x_0,
      Labour_base  = labour_nominal,
      Wage_base    = wage_nominal,
      sektor_names = sektor_names
    )
    
    message(sprintf("  ✓ RAS iterations=%d, max diff=%.3e",
                    sc_io_system$ras_iterations, sc_io_system$ras_diff))
    
    # ==========================================================
    # GUARD: Cek apakah RAS gagal konvergen / menghasilkan NaN
    # ==========================================================
    is_ras_failed <- is.na(sc_io_system$ras_diff) || 
      !is.finite(sc_io_system$ras_diff) || 
      sc_io_system$ras_diff > 1e3
    
    if (is_ras_failed) {
      warning(
        "\n[RAS GAGAL] Target Output (ΔX) atau Transaksi (ΔZ) di file Asumsi 3 terlalu besar ",
        "dibandingkan baseline (max diff = ", sc_io_system$ras_diff, "). ",
        "Mengabaikan Asumsi 3 dan menggunakan tabel I-O baseline."
      )
      
      # Fallback: Jangan pakai hasil RAS, kembalikan ke baseline
      use_a3 <- FALSE
      active_assumptions <- setdiff(active_assumptions, "A3")
      
      L_new <- NULL
      link_after <- link_before
      
      A_full      <- sweep(Z, 2, x_0, "/")
      M_full      <- diag(n) - A_full
      B_full      <- solve(M_full)
      # v_c, w_c, l_c, Y_0, x_0, ITBL, ITFL dibiarkan menggunakan nilai baseline
      
    } else {
      # Jika RAS berhasil, lanjutkan seperti biasa
      L_new <- sc_io_system$L_new
      if (is.null(L_new)) L_new <- solve(diag(n) - sc_io_system$A_new)
      
      link_after <- compute_linkage_index(L_new, sektor_names)
      
      A_full      <- sc_io_system$A_new
      M_full      <- diag(n) - A_full
      B_full      <- L_new
      v_c         <- sc_io_system$v_c_new
      w_c         <- sc_io_system$w_c_new
      l_c         <- sc_io_system$l_c_new
      Y_0         <- sc_io_system$Y_new
      x_0         <- sc_io_system$X_new
      ITBL_vector <- link_after$itbl
      ITFL_vector <- link_after$itfl
      
      d_itbl <- max(abs(link_after$itbl - link_before$itbl), na.rm = TRUE)
      d_itfl <- max(abs(link_after$itfl - link_before$itfl), na.rm = TRUE)
      message(sprintf("  ✓ max|dITBL|=%.4g, max|dITFL|=%.4g", d_itbl, d_itfl))
    }
    
    if (max(d_itbl, d_itfl) < 1e-8) {
      warning(
        "Assumption 3 is active but ITBL/ITFL did not change. Check: ",
        "(1) sector names in io_changes exactly match baseline sectors, ",
        "(2) the delta Z column contains values > 0.",
        call. = FALSE
      )
    }
  }
  
  # ==========================================================================
  # 5. SUB-PARTITION (AFTER A3 OVERRIDE)
  # ==========================================================================
  M_nc_co     <- M_full[idx_nc, idx_co, drop = FALSE]
  M_nc_nc     <- M_full[idx_nc, idx_nc, drop = FALSE]
  M_nc_nc_inv <- solve(M_nc_nc)
  
  P_sub <- as.matrix(P[idx_co, , drop = FALSE])
  storage.mode(P_sub) <- "numeric"
  
  # ==========================================================================
  # 6. ASSUMPTION 2: PREPARE LPC TARGET & TARGET YEAR
  # ==========================================================================
  p_0_co_target <- p_0_co
  target_year   <- if (!is.null(lpc_target_year)) lpc_target_year else max(years)
  
  if (use_a2) {
    message("[run_projection] Preparing LPC target (Assumption 2)...")
    
    # Flexible LPC column name detection
    lpc_col <- NULL
    candidates <- c(
      "LPC SKENARIO 2 (Rp Juta/Ha)",
      "LPC_S2", "LPC", "LPC_Scenario", "LPC Scenario"
    )
    for (cand in candidates) {
      if (cand %in% colnames(lpc_scenario)) {
        lpc_col <- lpc_scenario[[cand]]
        break
      }
    }
    
    if (is.null(lpc_col)) {
      warning("LPC column not found in lpc_scenario. Assumption 2 ignored.")
      use_a2 <- FALSE
      active_assumptions <- setdiff(active_assumptions, "A2")
    } else {
      lpc_new <- as.numeric(lpc_col)
      
      # --- Handle length mismatches ---
      if (length(lpc_new) == nrow(gdp_baseline)) {
        lpc_new <- lpc_new[idx_valid]
      } else if (length(lpc_new) == n) {
        # already matching, ok
      } else {
        warning(sprintf(
          "Scenario LPC vector length (%d) != number of baseline sectors (%d) or valid sectors (%d).",
          length(lpc_new), nrow(gdp_baseline), n
        ))
        use_a2 <- FALSE
      }
      
      if (use_a2) {
        lpc_new[is.na(lpc_new)] <- 0
        
        # Fallback: if zero, use baseline (do not collapse)
        zero_mask <- lpc_new == 0
        lpc_new[zero_mask] <- LPC_0[zero_mask]
        
        p_0_co_target <- as.numeric(lpc_new[idx_co])
        
        message(sprintf("  ✓ LPC target prepared for %d co sectors (interpolation %d → %d)",
                        length(p_0_co_target), initial_year, target_year))
      }
    }
  }
  
  # ==========================================================================
  # 7. PROJECTION ITERATION
  # ==========================================================================
  x_out             <- matrix(0, nrow = t_max, ncol = n)
  PDRB_sektoral     <- matrix(0, nrow = t_max, ncol = n)
  Inc_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  Lab_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  SUB_sektoral      <- matrix(0, nrow = t_max, ncol = n)
  L_co_matrix       <- matrix(0, nrow = t_max, ncol = m)
  LPC_growth_matrix <- matrix(0, nrow = t_max, ncol = m)
  LPC_t_matrix      <- matrix(0, nrow = t_max, ncol = m)
  
  for (k in seq_len(t_max)) {
    yr            <- years[k]
    selisih_tahun <- yr - initial_year
    
    # --- A. Final demand for nc sectors ---
    Y_nc_t <- Y_0[idx_nc] * ((1 + gr_vec[idx_nc]) ^ selisih_tahun)
    
    # --- B. Dynamic LPC for A2, static otherwise ---
    if (use_a2 && yr > initial_year) {
      if (yr >= target_year) {
        p_0_co_t <- p_0_co_target
      } else {
        prop     <- (yr - initial_year) / (target_year - initial_year)
        p_0_co_t <- p_0_co + prop * (p_0_co_target - p_0_co)
      }
      lpc_gr_vec <- (p_0_co_t / p_0_co) - 1
    } else {
      p_0_co_t   <- p_0_co
      lpc_gr_vec <- rep(0, m)
    }
    
    # --- C. Output of co sectors constrained by land ---
    luas_tutupan_t <- as.numeric(unlist(area_matrix[, k]))
    L_co_t         <- as.vector(P_sub %*% luas_tutupan_t)
    x_co_t         <- matrix(L_co_t * p_0_co_t, ncol = 1)
    
    # --- D. Mixed IO solve ---
    rhs    <- Y_nc_t - (M_nc_co %*% x_co_t)
    x_nc_t <- M_nc_nc_inv %*% rhs
    
    # --- E. Output reconstruction ---
    x_t         <- numeric(n)
    x_t[idx_co] <- as.vector(x_co_t)
    x_t[idx_nc] <- as.vector(x_nc_t)
    
    # --- F. Multi-indicator ---
    PDRB_sektoral[k, ]     <- v_c * x_t
    Inc_sektoral[k, ]      <- w_c * x_t
    Lab_sektoral[k, ]      <- l_c * x_t
    SUB_sektoral[k, ]      <- sub_c * x_t
    x_out[k, ]             <- x_t
    L_co_matrix[k, ]       <- L_co_t
    LPC_growth_matrix[k, ] <- lpc_gr_vec
    LPC_t_matrix[k, ]      <- p_0_co_t
  }
  
  # ==========================================================================
  # 8. RESULT RECAPITULATION
  # ==========================================================================
  results_list <- list()
  
  for (k in seq_len(t_max)) {
    yr <- years[k]
    
    land_req_t         <- numeric(n)
    land_req_t[idx_co] <- L_co_matrix[k, ]
    
    lpc_growth_t         <- numeric(n)
    lpc_growth_t[idx_co] <- LPC_growth_matrix[k, ]
    
    lpc_t         <- numeric(n)
    lpc_t[idx_co] <- LPC_t_matrix[k, ]
    
    # ---- Total makroekonomi per tahun ----
    total_pdrb   <- sum(PDRB_sektoral[k, ], na.rm = TRUE)
    total_inc_tk <- sum(Inc_sektoral[k, ],  na.rm = TRUE)
    
    # ---- Indikator per kapita ----
    pop_k <- if (has_population) pop_vector[k] else NA_real_
    
    pdrb_per_kapita <- if (!is.na(pop_k) && pop_k > 0) total_pdrb   / pop_k else NA_real_
    inc_per_kapita  <- if (!is.na(pop_k) && pop_k > 0) total_inc_tk / pop_k else NA_real_
    
    results_list[[as.character(yr)]] <- tibble::tibble(
      YEAR                           = yr,
      SECTOR                         = sektor_names,
      LAND_REQUIREMENT               = land_req_t,
      LPC                            = lpc_t,
      LPC_GROWTH_RATE                = lpc_growth_t,
      FD                             = Y_0,
      OUTPUT                         = x_out[k, ],
      GDP                            = PDRB_sektoral[k, ],
      INCOME                         = Inc_sektoral[k, ],
      LABOUR                         = Lab_sektoral[k, ],
      SURPLUS                        = SUB_sektoral[k, ],
      ITBL                           = as.numeric(ITBL_vector),
      ITFL                           = as.numeric(ITFL_vector),
      # ---- per-kapita untuk sektor terkait ----
      POPULATION                     = pop_k,
      GDP_PER_CAPITA                 = pdrb_per_kapita,
      INCOME_PER_CAPITA              = inc_per_kapita
    )
  }
  
  final_result <- do.call(rbind, results_list) |>
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ ifelse(!is.finite(.x), 0, .x))
    )
  
  # ==========================================================================
  # 9. LINKAGE COMPARISON BEFORE vs AFTER (ITBL & ITFL)
  # ==========================================================================
  linkage_compare <- data.frame(
    SECTOR      = sektor_names,
    ITBL_BEFORE = as.numeric(link_before$itbl),
    ITBL_AFTER  = as.numeric(ITBL_vector),
    ITFL_BEFORE = as.numeric(link_before$itfl),
    ITFL_AFTER  = as.numeric(ITFL_vector),
    stringsAsFactors = FALSE
  )
  linkage_compare$DELTA_ITBL <- linkage_compare$ITBL_AFTER - linkage_compare$ITBL_BEFORE
  linkage_compare$DELTA_ITFL <- linkage_compare$ITFL_AFTER - linkage_compare$ITFL_BEFORE
  linkage_compare <- linkage_compare[, c(
    "SECTOR",
    "ITBL_BEFORE", "ITBL_AFTER", "DELTA_ITBL",
    "ITFL_BEFORE", "ITFL_AFTER", "DELTA_ITFL"
  )]
  
  # ==========================================================================
  # 10. METADATA (for Shiny / debugging)
  # ==========================================================================
  attr(final_result, "active_assumptions") <- active_assumptions
  attr(final_result, "use_a1") <- use_a1
  attr(final_result, "use_a2") <- use_a2
  attr(final_result, "use_a3") <- use_a3
  attr(final_result, "ras_iterations") <- if (!is.null(sc_io_system)) sc_io_system$ras_iterations else NA
  attr(final_result, "ras_diff")       <- if (!is.null(sc_io_system)) sc_io_system$ras_diff       else NA
  attr(final_result, "linkage_compare")  <- linkage_compare
  # Preserved so that older code using these attributes continues to work
  attr(final_result, "backward_linkage") <- as.numeric(ITBL_vector)
  attr(final_result, "forward_linkage")  <- as.numeric(ITFL_vector)
  attr(final_result, "sector_names")     <- sektor_names
  attr(final_result, "years")            <- years
  attr(final_result, "has_population")   <- has_population
  attr(final_result, "pop_vector")       <- pop_vector
  
  return(final_result)
}

#' Save projection scenario results to RDS
#'
#' Saves the result of a single projection scenario (BAU or intervention) into
#' an RDS file. The file name is derived from the scenario label after
#' sanitising invalid characters.
#'
#' @param scenario_label Character. Name of the scenario, e.g., `"BAU"` or
#'   `"Skenario_Pertanian_Maju"`.
#' @param result Data frame. Projection result returned by [run_projection()].
#' @param area_matrix Numeric matrix. Area matrix used in the projection.
#' @param inputs List. Input parameters used in the projection.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the full path to the saved RDS file.
#' @export
save_projection_results <- function(scenario_label,
                                    result,
                                    area_matrix,
                                    inputs,
                                    output_dir = "inst/extdata/output") {
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  safe_label <- gsub("[^A-Za-z0-9_]", "_", scenario_label)
  file_name <- paste0("projection_", safe_label, ".rds")
  full_path <- file.path(output_dir, file_name)
  
  output <- list(
    scenario    = scenario_label,
    result      = result,
    area_matrix = area_matrix,
    inputs      = inputs
  )
  
  saveRDS(output, full_path)
  invisible(full_path)
}

#' @title RAS Biproportional Matrix Balancing
#' @keywords internal
#' @noRd
ras_balance_io <- function(A_0, x_target, u_target, v_target,
                           tol = 1e-6, max_iter = 1000) {
  n    <- length(x_target)
  A_k  <- A_0
  iter <- 0
  diff <- Inf
  
  while (diff > tol && iter < max_iter) {
    iter <- iter + 1
    
    Z_k <- sweep(A_k, 2, x_target, "*")
    
    u_k <- rowSums(Z_k)
    r   <- ifelse(u_k == 0, 1, u_target / u_k)
    r[!is.finite(r)] <- 1
    A_k <- sweep(A_k, 1, r, "*")
    
    Z_k <- sweep(A_k, 2, x_target, "*")
    v_k <- colSums(Z_k)
    s   <- ifelse(v_k == 0, 1, v_target / v_k)
    s[!is.finite(s)] <- 1
    A_k <- sweep(A_k, 2, s, "*")
    
    Z_k  <- sweep(A_k, 2, x_target, "*")
    diff <- max(abs(c(rowSums(Z_k) - u_target,
                      colSums(Z_k) - v_target)), na.rm = TRUE)
  }
  
  list(
    A_balanced = A_k,
    Z_balanced = Z_k,
    L_balanced = solve(diag(n) - A_k),
    iterations = iter,
    max_diff   = diff
  )
}


#' @title Proses Restrukturisasi IO dengan RAS Balancing (Asumsi 3)
#' @keywords internal
#' @noRd
process_sc_ras_system <- function(df_input, Z_base, X_base, Labour_base, Wage_base,
                                  sektor_names) {
  Z_base <- as.matrix(Z_base); storage.mode(Z_base) <- "numeric"
  n      <- length(sektor_names)
  
  stopifnot(nrow(Z_base) == n, ncol(Z_base) == n)
  
  X_base      <- as.numeric(unlist(X_base))
  Labour_base <- as.numeric(unlist(Labour_base))
  Wage_base   <- as.numeric(unlist(Wage_base))
  
  # Filter ΔZ > 0
  if (!is.null(df_input) && nrow(df_input) > 0) {
    df_input <- df_input |>
      dplyr::filter(
        !is.na(`SEKTOR ASAL (FROM SECTOR)`),
        `PENAMBAHAN TRANSAKSI ANTARA / ΔZ (Rp Juta)` > 0
      )
  } else {
    df_input <- data.frame()
  }
  
  Z_init <- Z_base
  X_new  <- X_base
  
  # Injeksi ΔZ dan ΔX
  if (nrow(df_input) > 0) {
    for (i in seq_len(nrow(df_input))) {
      from_name    <- df_input$`SEKTOR ASAL (FROM SECTOR)`[i]
      to_name      <- df_input$`SEKTOR TUJUAN (TO SECTOR)`[i]
      delta_z      <- as.numeric(df_input$`PENAMBAHAN TRANSAKSI ANTARA / ΔZ (Rp Juta)`[i])
      delta_x_from <- as.numeric(df_input$`PERUBAHAN OUTPUT ASAL ΔX (Rp Juta)`[i])
      delta_x_to   <- as.numeric(df_input$`PERUBAHAN OUTPUT TUJUAN ΔX (Rp Juta)`[i])
      
      idx_from <- which(sektor_names == from_name)
      idx_to   <- which(sektor_names == to_name)
      
      if (length(idx_from) > 0 && length(idx_to) > 0) {
        Z_init[idx_from, idx_to] <- Z_init[idx_from, idx_to] + delta_z
        if (!is.na(delta_x_from)) X_new[idx_from] <- X_new[idx_from] + delta_x_from
        if (!is.na(delta_x_to))   X_new[idx_to]   <- X_new[idx_to]   + delta_x_to
      }
    }
  }
  
  u_target <- rowSums(Z_init)
  v_target <- colSums(Z_init)
  
  X_safe <- ifelse(X_base == 0, 1, X_base)
  A_0    <- sweep(Z_base, 2, X_safe, "/")
  
  ras_res <- ras_balance_io(A_0, X_new, u_target, v_target)
  
  Y_new  <- X_new - rowSums(ras_res$Z_balanced)
  VA_new <- X_new - colSums(ras_res$Z_balanced)
  
  X_safe_new <- ifelse(X_new == 0, 1, X_new)
  
  L_new   <- ras_res$L_balanced
  sum_L   <- sum(L_new)
  
  list(
    Z_new             = ras_res$Z_balanced,
    A_new             = ras_res$A_balanced,
    L_new             = L_new,
    X_new             = X_new,
    Y_new             = Y_new,
    v_c_new           = VA_new / X_safe_new,
    w_c_new           = Wage_base / X_safe_new,
    l_c_new           = Labour_base / X_safe_new,
    output_multiplier = colSums(L_new),
    backward_linkage  = (n * colSums(L_new)) / sum_L,
    forward_linkage   = (n * rowSums(L_new)) / sum_L,
    ras_iterations    = ras_res$iterations,
    ras_diff          = ras_res$max_diff
  )
}


# ==============================================================================
# UI HELP DICTIONARY & HELPER FUNCTIONS
# ==============================================================================

#' Regional Economic Projection UI Help Text Dictionary
#'
#' Named list of help dialog content (title + HTML body) for each input in the
#' Regional Economic Projection Shiny module.
#'
#' @format A named list of lists with \code{title} and \code{body}.
#' @export
proj_help_texts <- list(
  io_output_file = list(
    title = "Hasil Analisis Input-Output Deskriptif",
    body = "<p>Berkas spreadsheet Excel (.xlsx) hasil ekspor modul Analisis Deskriptif (misal <code>Hasil_Analisis_Modul_Deskriptif.xlsx</code> atau <code>IO_Analysis_Output.xlsx</code>).</p>
            <p>Memuat seluruh sheet data dasar yang diperlukan: <code>GDP</code>, <code>Leontief_Matrix</code>, <code>Full_IO_Table</code>, <code>Land_Proportion</code>, <code>Multiplier_*</code>, <code>Linkage</code>, dan <code>Code_Registry</code>.</p>"
  ),
  descriptive_rds_file = list(
    title = "RDS Analisis Deskriptif",
    body = "<p>Berkas RDS hasil olahan modul Analisis Deskriptif (misal <code>descriptive_analysis_module_output.rds</code>).</p>
            <p>Menyimpan seluruh objek matriks, koefisien, dan parameter baseline secara terkompresi. Penggunaan berkas RDS ini sangat disarankan untuk mempercepat pemrosesan data dasar tanpa perlu membaca ulang file Excel.</p>"
  ),
  projection_area_file = list(
    title = "Proyeksi Luas Lahan BAU",
    body = "<p>Berkas spreadsheet Excel (.xlsx) yang memuat proyeksi luas tutupan lahan untuk skenario dasar (<i>Business-As-Usual</i> / BAU).</p>
            <p>Memiliki kolom <code>ID</code>, <code>LC</code> (nama tutupan lahan), serta kolom tahun proyeksi (misal <code>2025</code>, <code>2030</code>, <code>2035</code>, dst.). Tahun awal, tahun akhir, dan interval proyeksi akan dideteksi secara otomatis dari kolom-kolom tahun di berkas ini.</p>"
  ),
  population_file = list(
    title = "Proyeksi Jumlah Penduduk",
    body = "<p>Berkas tabel (.xlsx atau .csv) yang memuat proyeksi jumlah penduduk di wilayah studi sepanjang horizon proyeksi.</p>
            <p>Wajib memiliki minimal dua kolom: <code>Tahun</code> dan <code>Populasi</code> (atau <code>Jumlah_Penduduk</code>). Digunakan untuk menghitung indikator per kapita (PDRB per kapita dan pendapatan tenaga kerja per kapita).</p>"
  ),
  has_scenario = list(
    title = "Skenario Intervensi",
    body = "<p>Tentukan apakah analisis proyeksi akan menerapkan skenario intervensi kebijakan atau hanya menghitung skenario dasar (BAU).</p>
            <p>Jika memilih <b>Ya</b>, Anda dapat menentukan nama skenario dan menggabungkan satu atau lebih asumsi (Asumsi 1: Lahan, Asumsi 2: Produktivitas, Asumsi 3: Hilirisasi).</p>"
  ),
  scenario_name = list(
    title = "Nama Skenario Intervensi",
    body = "<p>Nama atau label deskriptif untuk skenario intervensi (misal <i>Skenario Pembangunan Rendah Karbon</i> atau <i>Skenario Hilirisasi Pertanian</i>).</p>
            <p>Label ini akan ditampilkan pada judul grafik dinamika indikator, tabel perbandingan, dan laporan proyeksi.</p>"
  ),
  a1_land_file = list(
    title = "Asumsi 1: Perubahan Lahan",
    body = "<p>Berkas spreadsheet Excel (.xlsx) yang menyimulasikan dampak perubahan luas atau alokasi penggunaan lahan.</p>
            <p>Sheet wajib: <code>luc_projection</code> (proyeksi luas tutupan lahan intervensi). Sheet opsional: <code>land_proportion</code> (proporsi alokasi tutupan lahan ke sektor ekonomi). Disarankan menggunakan tombol <b>Unduh Template Asumsi 1</b> untuk struktur kolom yang valid.</p>"
  ),
  a2_lpc_file = list(
    title = "Asumsi 2: Peningkatan Produktivitas",
    body = "<p>Berkas spreadsheet Excel (.xlsx) yang menyimulasikan peningkatan Koefisien Produktivitas Lahan (<i>Land Productivity Coefficient</i> / LPC) dari waktu ke waktu menuju target yang ditentukan.</p>
            <p>Meningkatkan produktivitas output sektor berbasis lahan per hektare tanpa perlu memperluas area tutupan lahan. Disarankan menggunakan tombol <b>Unduh Template Asumsi 2</b>.</p>"
  ),
  a3_io_changes_file = list(
    title = "Asumsi 3: Hilirisasi Industri",
    body = "<p>Berkas spreadsheet Excel (.xlsx) yang menyimulasikan perubahan struktural tabel I-O akibat industri pengolahan hilir.</p>
            <p>Memuat perubahan transaksi antar-sektor (&Delta;Z) dan perubahan output (&Delta;X). Keseimbangan tabel I-O baru akan dihitung secara otomatis menggunakan algoritma <b>RAS balancing</b>. Disarankan menggunakan tombol <b>Unduh Template Asumsi 3</b>.</p>"
  ),
  wd = list(
    title = "Folder Output",
    body = "<p>Direktori tempat seluruh hasil analisis proyeksi akan disimpan, meliputi:</p>
            <ul>
              <li>Laporan proyeksi interaktif dalam format HTML (<code>ta_regional2_report_*.html</code>)</li>
              <li>Ekspor hasil proyeksi lengkap dalam buku kerja Excel multi-sheet (<code>Hasil_Proyeksi_Ekonomi_Regional.xlsx</code>)</li>
              <li>Berkas data proyeksi untuk keperluan analisis lanjutan</li>
            </ul>"
  )
)

#' Clickable Help Icon for Regional Economic Projection UI
#'
#' @param id Help topic ID (matches a key in [proj_help_texts]).
#' @return A `shiny.tag` containing an `actionLink` with a circle-question icon.
#' @export
proj_help_icon <- function(id) {
  tags$span(
    class = "alloc-help-icon",
    actionLink(
      inputId = paste0("proj_help_", id),
      label   = NULL,
      icon    = icon("circle-question")
    )
  )
}

#' Label With a Right-Aligned Help Icon
#'
#' @param text Label text.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` combining the label and a help icon.
#' @export
label_with_help <- function(text, help_id) {
  tags$div(
    class = "alloc-label",
    tags$span(text),
    proj_help_icon(help_id)
  )
}

#' Directory-Picker Button With an Inline Help Icon
#'
#' @param id Shiny input ID.
#' @param label Button label.
#' @param title Button title tooltip.
#' @param help_id Help topic ID.
#' @return A `shiny.tag` containing a directory-picker button and help icon.
#' @export
dir_button_with_help <- function(id, label, title, help_id) {
  tags$div(
    class = "dir-btn-wrapper",
    shinyDirButton(id, label, title),
    tags$div(
      class = "dir-help-overlay",
      proj_help_icon(help_id)
    )
  )
}
