# ==============================================================================
# functions_ta_regeco1.R
# Fungsi kalkulasi untuk Regional Economic Descriptive Analysis
# Mengacu pada: TA Regional Economy Module - Descriptive Analysis Notebook
# ==============================================================================

# ==============================================================================
# 0. UTILITY FUNCTIONS
# ==============================================================================

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

# ==============================================================================
# 1. PREPROCESSING & VALIDATION
# ==============================================================================

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

#' Build full Input-Output table and code registry
#'
#' Combines Intermediate Consumption, Final Demand, and Added Value into a
#' single complete matrix, along with a Code Registry that maps each code to
#' label, type, and row/column position.
#'
#' @param sector A data frame or tibble with at least one column of sector names.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param final_demand_component Data frame or tibble with two columns:
#'   code and label for final demand components.
#' @param added_value_m Numeric matrix of added value.
#' @param added_value_lookup Data frame or tibble with two columns:
#'   code and label for added value components.
#'
#' @return A list with elements:
#'   \item{full_io_table}{Complete IO matrix}
#'   \item{code_registry}{Tibble with code mapping}
#'   \item{n_sector}{Number of sectors}
#'
#' @examples
#' \dontrun{
#' full <- build_full_io_table(sector, ic, fd, fd_comp, av, av_lookup)
#' }
#'
#' @export
build_full_io_table <- function(sector, intermediate_consumption_m, final_demand_m,
                                final_demand_component, added_value_m, added_value_lookup) {
  n_sector   <- nrow(sector)
  sector_codes <- as.character(seq_len(n_sector))
  fd_codes   <- as.character(final_demand_component[[1]])
  va_codes   <- as.character(added_value_lookup[[1]])
  
  if (anyNA(fd_codes) || any(trimws(fd_codes) == ""))
    stop("There are empty/NA codes in the first column of 'final_demand'.", call. = FALSE)
  if (anyNA(va_codes) || any(trimws(va_codes) == ""))
    stop("There are empty/NA codes in the first column of 'added_value'.", call. = FALSE)
  if (any(fd_codes %in% sector_codes))
    stop("Final Demand codes conflict with sector codes (1..N).")
  if (any(va_codes %in% sector_codes))
    stop("Added Value codes conflict with sector codes (1..N).")
  if (any(fd_codes %in% va_codes))
    stop("Final Demand codes conflict with Added Value codes.")
  
  row_codes <- c(sector_codes, va_codes)
  col_codes <- c(sector_codes, fd_codes)
  n_row <- length(row_codes)
  n_col <- length(col_codes)
  
  full_io <- matrix(NA_real_, nrow = n_row, ncol = n_col,
                    dimnames = list(row_codes, col_codes))
  full_io[seq_len(n_sector), seq_len(n_sector)] <- intermediate_consumption_m
  full_io[seq_len(n_sector), (n_sector + 1):n_col] <- final_demand_m
  full_io[(n_sector + 1):n_row, seq_len(n_sector)] <- added_value_m
  
  code_registry <- dplyr::bind_rows(
    tibble::tibble(Code = sector_codes, Label = as.character(sector[[1]]),
                   Type = "Sector", Axis = "Both",
                   Row_Index = seq_len(n_sector), Col_Index = seq_len(n_sector)),
    tibble::tibble(Code = fd_codes, Label = as.character(final_demand_component[[2]]),
                   Type = "Final_Demand", Axis = "Column",
                   Row_Index = NA_integer_, Col_Index = (n_sector + 1):n_col),
    tibble::tibble(Code = va_codes, Label = as.character(added_value_lookup[[2]]),
                   Type = "Added_Value", Axis = "Row",
                   Row_Index = (n_sector + 1):n_row, Col_Index = NA_integer_)
  )
  
  list(full_io_table = full_io, code_registry = code_registry, n_sector = n_sector)
}

#' Find row alias in matrix or data frame rownames
#'
#' Searches for the first row name that matches one of the provided aliases,
#' tolerating differences in capitalization and leading/trailing spaces.
#'
#' @param data A matrix or data frame with row names.
#' @param aliases Character vector. Candidate row names.
#'
#' @return The first matching row name, or `NULL` if none is found.
#'
#' @keywords internal
find_row_alias <- function(data, aliases) {
  available_rows <- trimws(tolower(rownames(data)))
  aliases_clean  <- trimws(tolower(aliases))
  idx <- match(aliases_clean, available_rows)
  pos <- which(!is.na(idx))
  if (length(pos) == 0) return(NULL)
  aliases[pos[1]]
}

#' Validate Input-Output data with comprehensive exploded report
#'
#' Checks dimensions, sector names, data types, missing values, non‑negativity
#' (for IC and labour only), land proportions, land use table consistency,
#' growth rates, and the fundamental IO identity (warning only).
#' Prints a detailed validation report; stops only on critical failures.
#'
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param intermediate_consumption A numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param final_demand A numeric matrix of final demand
#'   (Sector x Final Demand component).
#' @param added_value A numeric matrix of added value
#'   (Component x Sector).
#' @param labour A vector, one-column matrix, or one-column data frame/tibble
#'   containing labour per sector.
#' @param land_proportion A numeric matrix of land proportion
#'   (Sector x Land Cover Class).
#' @param landuse_table Optional data frame or tibble with at least columns
#'   "LC" (land cover name) and "area" (hectares). Number of rows must
#'   equal the number of columns of `land_proportion`. If `NULL` (default),
#'   the land use checks are reported as `SKIP` and not run.
#' @param growth_rate Optional numeric vector of length `n_sec` containing
#'   sectoral growth rates (in decimal, e.g., 0.02 for 2%). If `NULL`
#'   (default), the growth rate checks are reported as `SKIP` and not run.
#' @param tolerance_io Numeric; maximum allowed absolute relative difference
#'   for the IO identity. Default 0.01 (1%).
#' @param stop_on_fail Logical; if `TRUE` (default), stops on any `FAIL`.
#'
#' @return A tibble with validation results (invisibly if all pass).
#' @export
validate_io_data <- function(sector,
                             intermediate_consumption,
                             final_demand,
                             added_value,
                             labour,
                             land_proportion,
                             landuse_table = NULL,
                             growth_rate = NULL,
                             tolerance_io = 0.01,
                             stop_on_fail = TRUE) {
  
  # Helper functions 
  are_auto_names <- function(nms) {
    if (is.null(nms)) return(TRUE)
    default_pattern <- grepl("^(?:\\.\\.[0-9]+|\\.\\.\\.[0-9]+|[A-Za-z]+\\.?[0-9]+)$", nms)
    if (all(default_pattern)) return(TRUE)
    suppressWarnings({
      nums <- as.numeric(nms)
      if (!anyNA(nums)) return(TRUE)
    })
    FALSE
  }
  
  align_dimnames <- function(mat, expected_names, dim = c("row", "col")) {
    dim <- match.arg(dim)
    current_names <- if (dim == "row") rownames(mat) else colnames(mat)
    
    if (is.null(current_names) || are_auto_names(current_names)) {
      if (dim == "row") rownames(mat) <- expected_names
      else colnames(mat) <- expected_names
    } else {
      if (!identical(sort(current_names), sort(expected_names))) {
        stop(sprintf(
          "Dimension names on %s do not match sector list. Current names: %s",
          dim, paste(current_names, collapse = ", ")
        ))
      }
      if (dim == "row") {
        mat <- mat[expected_names, , drop = FALSE]
      } else {
        mat <- mat[, expected_names, drop = FALSE]
      }
    }
    mat
  }
  
  # Reporting framework
  report <- list()
  
  add_check <- function(component, check, status, details = NA_character_) {
    report <<- c(report, list(tibble::tibble(
      Component = component,
      Check = check,
      Status = status,
      Details = details
    )))
  }
  
  # Matrix validation helper
  validate_and_report_matrix <- function(mat, nrow_expected, ncol_expected,
                                         mat_name, check_negative) {
    # 1. Is matrix
    if (!is.matrix(mat)) {
      add_check(mat_name, "Is a matrix", "FAIL", "Object is not a matrix.")
      add_check(mat_name, "Rows match expected", "FAIL", "Skipped (not a matrix)")
      if (!is.null(ncol_expected)) {
        add_check(mat_name, "Columns match expected", "FAIL", "Skipped (not a matrix)")
      }
      add_check(mat_name, "Is numeric", "FAIL", "Skipped (not a matrix)")
      add_check(mat_name, "No missing (NA) values", "FAIL", "Skipped (not a matrix)")
      if (check_negative) {
        add_check(mat_name, "No negative values", "FAIL", "Skipped (not a matrix)")
      }
      return(invisible(FALSE))
    }
    add_check(mat_name, "Is a matrix", "PASS", NA)
    
    # 2. Rows
    if (nrow(mat) != nrow_expected) {
      add_check(mat_name, sprintf("Rows match expected (%d)", nrow_expected),
                "FAIL", sprintf("Has %d rows.", nrow(mat)))
    } else {
      add_check(mat_name, sprintf("Rows match expected (%d)", nrow_expected),
                "PASS", NA)
    }
    
    # 3. Columns 
    if (!is.null(ncol_expected)) {
      if (ncol(mat) != ncol_expected) {
        add_check(mat_name, sprintf("Columns match expected (%d)", ncol_expected),
                  "FAIL", sprintf("Has %d columns.", ncol(mat)))
      } else {
        add_check(mat_name, sprintf("Columns match expected (%d)", ncol_expected),
                  "PASS", NA)
      }
    }
    
    # 4. Numeric
    if (!is.numeric(mat)) {
      add_check(mat_name, "Is numeric", "FAIL",
                sprintf("Is type: %s", paste(class(mat), collapse = ", ")))
      add_check(mat_name, "No missing (NA) values", "FAIL", "Skipped (not numeric)")
      if (check_negative) {
        add_check(mat_name, "No negative values", "FAIL", "Skipped (not numeric)")
      }
      return(invisible(FALSE))
    }
    add_check(mat_name, "Is numeric", "PASS", NA)
    
    # 5. NA
    if (any(is.na(mat))) {
      add_check(mat_name, "No missing (NA) values", "FAIL",
                sprintf("Has %d NA(s).", sum(is.na(mat))))
    } else {
      add_check(mat_name, "No missing (NA) values", "PASS", NA)
    }
    
    # 6. Negative 
    if (check_negative) {
      if (any(mat < 0, na.rm = TRUE)) {
        add_check(mat_name, "No negative values", "FAIL",
                  sprintf("Has %d negative value(s).", sum(mat < 0, na.rm = TRUE)))
      } else {
        add_check(mat_name, "No negative values", "PASS", NA)
      }
    }
    
    invisible(TRUE)
  }
  
  
  # A. Sector ---------------------------------------------------------------
  if (!is.data.frame(sector) || ncol(sector) < 2) {
    add_check("sector", "Is a data frame with ≥2 columns", "FAIL",
              "Must be a data frame or tibble with at least 2 columns.")
  } else {
    add_check("sector", "Is a data frame with ≥2 columns", "PASS")
  }
  
  sector_names <- as.character(sector[[1]])
  n_sec <- length(sector_names)
  
  # Unique sector names
  if (any(duplicated(sector_names))) {
    dup <- paste(sector_names[duplicated(sector_names)], collapse = ", ")
    add_check("sector", "Unique sector names", "FAIL",
              paste("Duplicates found:", dup))
  } else {
    add_check("sector", "Unique sector names", "PASS")
  }
  
  # Non-empty and non-NA sector names
  if (any(is.na(sector_names) | sector_names == "")) {
    add_check("sector", "Sector names are non‑empty and non‑NA", "FAIL",
              "Empty or NA names found.")
  } else {
    add_check("sector", "Sector names are non‑empty and non‑NA", "PASS")
  }
  
  
  # B. Intermediate consumption ---------------------------------------------
  intermediate_consumption <- align_dimnames(intermediate_consumption, sector_names, "row")
  intermediate_consumption <- align_dimnames(intermediate_consumption, sector_names, "col")
  validate_and_report_matrix(intermediate_consumption, n_sec, n_sec,
                             "intermediate_consumption", check_negative = TRUE)
  
  
  # C. Final demand ---------------------------------------------------------
  n_fd <- ncol(final_demand)
  if (n_fd < 1) {
    add_check("final_demand", "At least one column", "FAIL",
              "Must have at least one column.")
  } else {
    add_check("final_demand", "At least one column", "PASS")
  }
  
  final_demand <- align_dimnames(final_demand, sector_names, "row")
  validate_and_report_matrix(final_demand, n_sec, n_fd,
                             "final_demand", check_negative = FALSE)
  
  
  # D. Added value ----------------------------------------------------------
  n_av <- nrow(added_value)
  if (n_av < 1) {
    add_check("added_value", "At least one row", "FAIL",
              "Must have at least one row.")
  } else {
    add_check("added_value", "At least one row", "PASS")
  }
  
  added_value <- align_dimnames(added_value, sector_names, "col")
  validate_and_report_matrix(added_value, n_av, n_sec,
                             "added_value", check_negative = FALSE)
  
  
  # E. Labour ---------------------------------------------------------------
  labour_vec <- NULL
  coercion_error <- NULL
  
  if (is.matrix(labour)) {
    if (ncol(labour) == 1) {
      labour_vec <- as.numeric(labour[, 1])
    } else {
      coercion_error <- "Matrix has more than one column"
    }
  } else if (is.data.frame(labour)) {
    if (ncol(labour) == 1) {
      labour_vec <- as.numeric(labour[[1]])
    } else {
      coercion_error <- "Data frame has more than one column"
    }
  } else if (is.vector(labour)) {
    labour_vec <- as.numeric(labour)
  } else {
    coercion_error <- paste("Unsupported type:", class(labour)[1])
  }
  
  if (!is.null(coercion_error)) {
    add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec),
              "FAIL", paste("Coercion failed:", coercion_error))
    add_check("labour", "No missing (NA) values",
              "FAIL", paste("Coercion failed:", coercion_error))
    add_check("labour", "No negative values",
              "FAIL", paste("Coercion failed:", coercion_error))
  } else {
    # Length 
    if (length(labour_vec) != n_sec) {
      add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec),
                "FAIL", sprintf("Length: %d", length(labour_vec)))
    } else {
      add_check("labour", sprintf("Length matches number of sectors (%d)", n_sec), "PASS")
    }
    
    # Missing values 
    if (any(is.na(labour_vec))) {
      na_count <- sum(is.na(labour_vec))
      add_check("labour", "No missing (NA) values", "FAIL",
                sprintf("Contains %d NA(s).", na_count))
    } else {
      add_check("labour", "No missing (NA) values", "PASS")
    }
    
    # Negative values 
    if (any(labour_vec < 0, na.rm = TRUE)) {
      neg_count <- sum(labour_vec < 0, na.rm = TRUE)
      add_check("labour", "No negative values", "FAIL",
                sprintf("Contains %d negative value(s).", neg_count))
    } else {
      add_check("labour", "No negative values", "PASS")
    }
  }
  
  # F. Land proportion ------------------------------------------------------
  land_proportion <- align_dimnames(land_proportion, sector_names, "row")
  validate_and_report_matrix(land_proportion, n_sec, NULL,
                             "land_proportion", check_negative = FALSE)
  
  # Column sums = 1
  if (is.matrix(land_proportion) && is.numeric(land_proportion) &&
      nrow(land_proportion) == n_sec) {
    col_sums <- colSums(land_proportion)
    if (any(abs(col_sums - 1) > 1e-6)) {
      bad <- paste(which(abs(col_sums - 1) > 1e-6), collapse = ", ")
      add_check("land_proportion", "Column sums equal 1", "FAIL",
                paste("Problem columns:", bad))
    } else {
      add_check("land_proportion", "Column sums equal 1", "PASS")
    }
    
    # Values in [0,1]
    if (any(land_proportion < 0 | land_proportion > 1, na.rm = TRUE)) {
      add_check("land_proportion", "All values in [0,1]", "FAIL",
                "Values outside range found.")
    } else {
      add_check("land_proportion", "All values in [0,1]", "PASS")
    }
  } else {
    add_check("land_proportion", "Column sums equal 1", "FAIL",
              "Skipped due to prior errors.")
    add_check("land_proportion", "All values in [0,1]", "FAIL",
              "Skipped due to prior errors.")
  }
  
  
  # G. Landuse table --------------------------------------------------------
  n_lc <- ncol(land_proportion) 
  
  if (is.null(landuse_table)) {
    add_check("landuse_table", "Supplied", "SKIP",
              "Not supplied; land use checks skipped.")
  } else if (!is.data.frame(landuse_table)) {
    add_check("landuse_table", "Is a data frame", "FAIL", "Not a data frame/tibble.")
  } else if (ncol(landuse_table) < 2) {
    add_check("landuse_table", "Has at least 2 columns (LC, area)", "FAIL",
              "Only ", ncol(landuse_table), " columns found.")
  } else {
    add_check("landuse_table", "Is a data frame", "PASS")
    add_check("landuse_table", "Has at least 2 columns (LC, area)", "PASS")
    
    # Check required columns exist
    required_cols <- c("LC", "area")
    missing <- required_cols[!required_cols %in% colnames(landuse_table)]
    if (length(missing) > 0) {
      add_check("landuse_table", "Contains columns 'LC' and 'area'", "FAIL",
                paste("Missing:", paste(missing, collapse = ", ")))
    } else {
      add_check("landuse_table", "Contains columns 'LC' and 'area'", "PASS")
    }
    
    # Rows match land cover columns
    if (nrow(landuse_table) != n_lc) {
      add_check("landuse_table", sprintf("Rows match land cover count (%d)", n_lc),
                "FAIL", sprintf("Has %d rows.", nrow(landuse_table)))
    } else {
      add_check("landuse_table", sprintf("Rows match land cover count (%d)", n_lc),
                "PASS")
    }
    
    # Landuse name: unique, non-NA, non-empty
    lc_vals <- as.character(landuse_table[["LC"]])
    if (any(is.na(lc_vals) | lc_vals == "")) {
      add_check("landuse_table", "Landuse name are non‑empty and non‑NA", "FAIL",
                "Empty or NA LC found.")
    } else if (any(duplicated(lc_vals))) {
      dup <- paste(lc_vals[duplicated(lc_vals)], collapse = ", ")
      add_check("landuse_table", "Landuse name are unique", "FAIL",
                paste("Duplicates:", dup))
    } else {
      add_check("landuse_table", "Landuse name are unique and non‑NA", "PASS")
    }
    
    # Area: numeric, positive, no NA
    area_vals <- landuse_table[["area"]]
    if (!is.numeric(area_vals)) {
      add_check("landuse_table", "Area is numeric", "FAIL",
                paste("Type:", class(area_vals)[1]))
    } else {
      add_check("landuse_table", "Area is numeric", "PASS")
      if (any(is.na(area_vals))) {
        add_check("landuse_table", "Area has no NA", "FAIL",
                  "Contains NA values.")
      } else {
        add_check("landuse_table", "Area has no NA", "PASS")
        if (any(area_vals <= 0)) {
          add_check("landuse_table", "Area is positive", "FAIL",
                    "Contains zero or negative values.")
        } else {
          add_check("landuse_table", "Area is positive", "PASS")
        }
      }
    }
    
    # Check that land_proportion columns match Landuse name
    lp_colnames <- colnames(land_proportion)
    if (!is.null(lp_colnames) && !are_auto_names(lp_colnames)) {
      if (!identical(sort(lp_colnames), sort(lc_vals))) {
        add_check("landuse_table", "Land cover names match land_proportion columns", "FAIL",
                  "Column names of land_proportion do not match landuse name.")
      } else {
        add_check("landuse_table", "Land cover names match land_proportion columns", "PASS")
      }
    } else {
      add_check("landuse_table", "Land cover names match land_proportion columns", "WARN")
    }
  }
  
  # H. Growth rate ----------------------------------------------------------
  if (is.null(growth_rate)) {
    add_check("growth_rate", "Supplied", "SKIP",
              "Not supplied; growth rate checks skipped.")
  } else if (!is.numeric(growth_rate)) {
    add_check("growth_rate", "Is numeric", "FAIL",
              paste("Type:", class(growth_rate)[1]))
  } else if (length(growth_rate) != n_sec) {
    add_check("growth_rate", sprintf("Length matches sectors (%d)", n_sec),
              "FAIL", sprintf("Length: %d", length(growth_rate)))
  } else {
    add_check("growth_rate", "Is numeric", "PASS")
    add_check("growth_rate", sprintf("Length matches sectors (%d)", n_sec),
              "PASS")
    if (any(is.na(growth_rate))) {
      add_check("growth_rate", "No NA values", "FAIL", "Contains NA values.")
    } else {
      add_check("growth_rate", "No NA values", "PASS")
    }
    if (any(growth_rate < 0)) {
      add_check("growth_rate", "Non‑negative values", "FAIL",
                "Contains negative growth rates.")
    } else {
      add_check("growth_rate", "Non‑negative values", "PASS")
    }
    if (any(growth_rate > 1)) {
      add_check("growth_rate", "Plausible range (≤1)", "WARN",
                "Some growth rates exceed 100%.")
    } else {
      add_check("growth_rate", "Plausible range (≤1)", "PASS")
    }
  }
  
  # I. Fundamental IO identity ----------------------------------------------
  if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption) &&
      is.matrix(final_demand) && is.numeric(final_demand) &&
      is.matrix(added_value) && is.numeric(added_value) &&
      nrow(intermediate_consumption) == n_sec &&
      nrow(final_demand) == n_sec &&
      ncol(added_value) == n_sec) {
    
    row_ic <- rowSums(intermediate_consumption)
    row_fd <- rowSums(final_demand)
    row_output <- row_ic + row_fd
    
    col_ic <- colSums(intermediate_consumption)
    col_av <- colSums(added_value)
    col_input <- col_ic + col_av
    
    denom <- pmax(abs(row_output), abs(col_input), 1e-9)
    rel_diff <- abs(row_output - col_input) / denom
    
    if (any(rel_diff > tolerance_io)) {
      bad <- which(rel_diff > tolerance_io)
      details <- sprintf(
        "Sectors exceeding %s%% tolerance: %s; Relative differences: %s",
        round(tolerance_io * 100, 2),
        paste(sector_names[bad], collapse = ", "),
        paste(signif(rel_diff[bad], 4), collapse = ", ")
      )
      add_check("Global", "IO identity (row output = column input)", "WARN", details)
    } else {
      add_check("Global", "IO identity (row output = column input)", "PASS")
    }
  } else {
    add_check("Global", "IO identity (row output = column input)", "FAIL",
              "Skipped due to earlier matrix errors.")
  }
  
  
  # J. Land‑based consistency -----------------------------------------------
  if ("Berbasis_Lahan" %in% colnames(sector)) {
    land_based <- sector$Berbasis_Lahan == "Ya"
    if (is.matrix(land_proportion) && nrow(land_proportion) == n_sec) {
      row_sums_land <- rowSums(land_proportion)
      
      zero_land <- which(land_based & row_sums_land == 0)
      if (length(zero_land) > 0) {
        details <- sprintf("Sectors: %s", paste(sector_names[zero_land], collapse = ", "))
        add_check("sector/land", "Land‑based sectors have positive land", "WARN", details)
      }
      
      pos_land <- which(!land_based & row_sums_land > 0)
      if (length(pos_land) > 0) {
        details <- sprintf("Sectors: %s", paste(sector_names[pos_land], collapse = ", "))
        add_check("sector/land", "Non‑land‑based sectors have zero land", "WARN", details)
      }
    }
  }
  
  
  # Prepare report ----------------------------------------------------------
  report_tbl <- dplyr::bind_rows(report)
  has_fail <- any(report_tbl$Status == "FAIL")
  
  if (has_fail && stop_on_fail) {
    stop("Validation FAILED. See report above for details.", call. = FALSE)
  }
  if (!has_fail) {
    message("All critical checks passed. ", n_sec, " sectors confirmed consistent.")
  } else {
    message("Validation completed with failures (stop_on_fail = FALSE).")
  }
  invisible(report_tbl)
}

#' Extract metadata from Input-Output components
#'
#' For each component (sector, intermediate_consumption, final_demand,
#' added_value, labour, land_proportion, landuse_table, growth_rate),
#' this function returns a tidy tibble with descriptive metadata:
#' - class, dimensions, missing values, min/max, names, hardcoded descriptions,
#'   and user‑provided units.
#'
#' @param sector A data frame/tibble with sector names in first column.
#' @param intermediate_consumption Numeric matrix.
#' @param final_demand Numeric matrix.
#' @param added_value Numeric matrix.
#' @param labour Vector, 1-column matrix, or 1-column data frame.
#' @param land_proportion Numeric matrix.
#' @param landuse_table Data frame/tibble with "LC" and "area".
#' @param growth_rate Numeric vector.
#' @param sector_units Character string for sector units (default NA).
#' @param ic_units Character string for intermediate consumption units (default NA).
#' @param fd_units Character string for final demand units (default NA).
#' @param av_units Character string for added value units (default NA).
#' @param labour_units Character string for labour units (default NA).
#' @param lp_units Character string for land proportion units (default NA).
#' @param lu_units Character string for landuse table units (default NA).
#' @param gr_units Character string for growth rate units (default NA).
#'
#' @return A list with two elements:
#'   \item{metadata}{A tibble with one row per component and columns for key attributes including Description and Units.}
#'   \item{details}{A list containing more detailed metadata for each component (e.g., names, per‑column summaries).}
#' @export
get_io_metadata <- function(sector,
                            intermediate_consumption,
                            final_demand,
                            added_value,
                            labour,
                            land_proportion,
                            landuse_table,
                            growth_rate,
                            sector_units = NA_character_,
                            ic_units = NA_character_,
                            fd_units = NA_character_,
                            av_units = NA_character_,
                            labour_units = NA_character_,
                            lp_units = NA_character_,
                            lu_units = NA_character_,
                            gr_units = NA_character_) {
  
  # Helper
  safe_attr <- function(obj, attr, default = NA) {
    tryCatch({
      val <- attr(obj, attr)
      if (is.null(val)) default else val
    }, error = function(e) default)
  }
  
  name_summary <- function(nms, max_show = 5) {
    if (is.null(nms) || length(nms) == 0) return("No names")
    n <- length(nms)
    if (n <= max_show) {
      paste(nms, collapse = ", ")
    } else {
      paste(paste(head(nms, max_show), collapse = ", "), "... and", n - max_show, "more")
    }
  }
  details <- list()
  
  # 1. Sector
  sector_meta <- list(
    class = class(sector)[1],
    nrow = if (is.data.frame(sector)) nrow(sector) else NA,
    ncol = if (is.data.frame(sector)) ncol(sector) else NA,
    sector_names = if (is.data.frame(sector) && ncol(sector) >= 1) name_summary(as.character(sector[[1]])) else NA,
    n_sectors = if (is.data.frame(sector) && ncol(sector) >= 1) length(unique(sector[[1]])) else NA,
    description = "Master sector list with categories and land-use flags",
    units = sector_units
  )
  details$sector <- sector_meta
  
  # 2. Intermediate consumption
  ic_meta <- list(
    class = class(intermediate_consumption)[1],
    is_matrix = is.matrix(intermediate_consumption),
    dim = if (is.matrix(intermediate_consumption)) dim(intermediate_consumption) else NA,
    rownames = if (is.matrix(intermediate_consumption)) name_summary(rownames(intermediate_consumption)) else NA,
    colnames = if (is.matrix(intermediate_consumption)) name_summary(colnames(intermediate_consumption)) else NA,
    n_na = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) sum(is.na(intermediate_consumption)) else NA,
    min = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) min(intermediate_consumption, na.rm = TRUE) else NA,
    max = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) max(intermediate_consumption, na.rm = TRUE) else NA,
    mean = if (is.matrix(intermediate_consumption) && is.numeric(intermediate_consumption)) mean(intermediate_consumption, na.rm = TRUE) else NA,
    description = "Intermediate consumption matrix (Sector × Sector)",
    units = ic_units
  )
  details$intermediate_consumption <- ic_meta
  
  # 3. Final demand
  fd_meta <- list(
    class = class(final_demand)[1],
    is_matrix = is.matrix(final_demand),
    dim = if (is.matrix(final_demand)) dim(final_demand) else NA,
    rownames = if (is.matrix(final_demand)) name_summary(rownames(final_demand)) else NA,
    colnames = if (is.matrix(final_demand)) name_summary(colnames(final_demand)) else NA,
    n_na = if (is.matrix(final_demand) && is.numeric(final_demand)) sum(is.na(final_demand)) else NA,
    min = if (is.matrix(final_demand) && is.numeric(final_demand)) min(final_demand, na.rm = TRUE) else NA,
    max = if (is.matrix(final_demand) && is.numeric(final_demand)) max(final_demand, na.rm = TRUE) else NA,
    mean = if (is.matrix(final_demand) && is.numeric(final_demand)) mean(final_demand, na.rm = TRUE) else NA,
    description = "Final demand matrix (Sector × Final Demand Components)",
    units = fd_units
  )
  details$final_demand <- fd_meta
  
  # 4. Added value
  av_meta <- list(
    class = class(added_value)[1],
    is_matrix = is.matrix(added_value),
    dim = if (is.matrix(added_value)) dim(added_value) else NA,
    rownames = if (is.matrix(added_value)) name_summary(rownames(added_value)) else NA,
    colnames = if (is.matrix(added_value)) name_summary(colnames(added_value)) else NA,
    n_na = if (is.matrix(added_value) && is.numeric(added_value)) sum(is.na(added_value)) else NA,
    min = if (is.matrix(added_value) && is.numeric(added_value)) min(added_value, na.rm = TRUE) else NA,
    max = if (is.matrix(added_value) && is.numeric(added_value)) max(added_value, na.rm = TRUE) else NA,
    mean = if (is.matrix(added_value) && is.numeric(added_value)) mean(added_value, na.rm = TRUE) else NA,
    description = "Value added matrix (Components × Sector)",
    units = av_units
  )
  details$added_value <- av_meta
  
  # 5. Labour
  labour_vec <- NULL
  if (is.matrix(labour) && ncol(labour) == 1) {
    labour_vec <- as.numeric(labour[, 1])
  } else if (is.data.frame(labour) && ncol(labour) == 1) {
    labour_vec <- as.numeric(labour[[1]])
  } else if (is.vector(labour)) {
    labour_vec <- as.numeric(labour)
  }
  labour_meta <- list(
    class = class(labour)[1],
    length = length(labour),
    is_numeric = is.numeric(labour_vec),
    n_na = if (!is.null(labour_vec)) sum(is.na(labour_vec)) else NA,
    min = if (!is.null(labour_vec)) min(labour_vec, na.rm = TRUE) else NA,
    max = if (!is.null(labour_vec)) max(labour_vec, na.rm = TRUE) else NA,
    mean = if (!is.null(labour_vec)) mean(labour_vec, na.rm = TRUE) else NA,
    sum = if (!is.null(labour_vec)) sum(labour_vec, na.rm = TRUE) else NA,
    description = "Labour employment per sector",
    units = labour_units
  )
  details$labour <- labour_meta
  
  # 6. Land proportion
  lp_meta <- list(
    class = class(land_proportion)[1],
    is_matrix = is.matrix(land_proportion),
    dim = if (is.matrix(land_proportion)) dim(land_proportion) else NA,
    rownames = if (is.matrix(land_proportion)) name_summary(rownames(land_proportion)) else NA,
    colnames = if (is.matrix(land_proportion)) name_summary(colnames(land_proportion)) else NA,
    n_na = if (is.matrix(land_proportion) && is.numeric(land_proportion)) sum(is.na(land_proportion)) else NA,
    min = if (is.matrix(land_proportion) && is.numeric(land_proportion)) min(land_proportion, na.rm = TRUE) else NA,
    max = if (is.matrix(land_proportion) && is.numeric(land_proportion)) max(land_proportion, na.rm = TRUE) else NA,
    mean = if (is.matrix(land_proportion) && is.numeric(land_proportion)) mean(land_proportion, na.rm = TRUE) else NA,
    description = "Land proportion matrix (Sector × Land Cover Class)",
    units = lp_units
  )
  details$land_proportion <- lp_meta
  
  # 7. Landuse table
  if (is.data.frame(landuse_table)) {
    landuse_meta <- list(
      class = class(landuse_table)[1],
      nrow = nrow(landuse_table),
      ncol = ncol(landuse_table),
      has_LC = "LC" %in% colnames(landuse_table),
      has_area = "area" %in% colnames(landuse_table),
      LC_summary = if ("LC" %in% colnames(landuse_table)) name_summary(as.character(landuse_table$LC)) else NA,
      area_min = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) min(landuse_table$area, na.rm = TRUE) else NA,
      area_max = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) max(landuse_table$area, na.rm = TRUE) else NA,
      area_sum = if ("area" %in% colnames(landuse_table) && is.numeric(landuse_table$area)) sum(landuse_table$area, na.rm = TRUE) else NA,
      n_na_area = if ("area" %in% colnames(landuse_table)) sum(is.na(landuse_table$area)) else NA,
      description = "Land use table (Land Cover Class × Area)",
      units = lu_units
    )
  } else {
    landuse_meta <- list(
      class = class(landuse_table)[1],
      error = "Not a data frame",
      description = "Land use table (Land Cover Class × Area)",
      units = lu_units
    )
  }
  details$landuse_table <- landuse_meta
  
  # 8. Growth rate
  growth_meta <- list(
    class = class(growth_rate)[1],
    length = length(growth_rate),
    is_numeric = is.numeric(growth_rate),
    n_na = if (is.numeric(growth_rate)) sum(is.na(growth_rate)) else NA,
    min = if (is.numeric(growth_rate)) min(growth_rate, na.rm = TRUE) else NA,
    max = if (is.numeric(growth_rate)) max(growth_rate, na.rm = TRUE) else NA,
    mean = if (is.numeric(growth_rate)) mean(growth_rate, na.rm = TRUE) else NA,
    sum = if (is.numeric(growth_rate)) sum(growth_rate, na.rm = TRUE) else NA,
    description = "Sectoral growth rates (annual, decimal)",
    units = gr_units
  )
  details$growth_rate <- growth_meta
  
  # Build summary table
  metadata_tbl <- tibble::tibble(
    Component = names(details),
    Class = sapply(details, function(x) if ("class" %in% names(x)) x$class else NA),
    Description = sapply(details, function(x) if ("description" %in% names(x)) x$description else NA),
    Units = sapply(details, function(x) if ("units" %in% names(x)) x$units else NA),
    Rows = sapply(details, function(x) {
      if ("dim" %in% names(x) && length(x$dim) == 2) x$dim[1]
      else if ("nrow" %in% names(x)) x$nrow
      else if ("length" %in% names(x)) x$length
      else NA
    }),
    Cols = sapply(details, function(x) {
      if ("dim" %in% names(x) && length(x$dim) == 2) x$dim[2]
      else if ("ncol" %in% names(x)) x$ncol
      else NA
    }),
    N_NA = sapply(details, function(x) if ("n_na" %in% names(x)) x$n_na else NA),
    Min = sapply(details, function(x) if ("min" %in% names(x)) x$min else NA),
    Max = sapply(details, function(x) if ("max" %in% names(x)) x$max else NA),
    Mean = sapply(details, function(x) if ("mean" %in% names(x)) x$mean else NA),
    Names_Summary = sapply(details, function(x) {
      if ("sector_names" %in% names(x)) x$sector_names
      else if ("rownames" %in% names(x)) x$rownames
      else if ("colnames" %in% names(x)) x$colnames
      else if ("LC_summary" %in% names(x)) x$LC_summary
      else NA
    })
  )
  
  list(
    metadata = metadata_tbl,
    details = details
  )
}

#' Save preprocessing data to RDS
#'
#' Saves the input and validation results into an RDS file so that the
#' preprocessing stage can be reloaded before running the core analysis.
#'
#' @param io_preprocessing List containing preprocessed input data.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the output directory.
#' @export
save_preprocessing_data <- function(io_preprocessing,
                                    output_dir = "inst/extdata/output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(io_preprocessing,
          file = file.path(output_dir, "descriptive_input_preprocessing.rds"))
  invisible(output_dir)
}

#' Check Input-Output balance
#'
#' Computes total input and total output per sector, the difference, and
#' balance status based on a given tolerance.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param added_value_m Numeric matrix of added value.
#' @param sector_names Character vector of sector names. Optional; if `NULL`,
#'   column names of `intermediate_consumption_m` are used.
#' @param tolerance_percent Numeric. Threshold for balance check in percent
#'   (default = 1).
#' @param tolerance_value Numeric. Not used; kept for backward compatibility.
#'
#' @return A list with aggregate values (`status`, `total_input`, `total_output`,
#'   `gap`, `gap_percent`) and per-sector details (`balance_detail`,
#'   `total_input_per_sector`, `total_output_per_sector`, `gap_per_sector`,
#'   `gap_percent_per_sector`).
#'
#' @examples
#' \dontrun{
#' bal <- check_io_balance(ic, fd, av, sector_names = sector$name)
#' }
#'
#' @export
check_io_balance <- function(intermediate_consumption_m,
                             final_demand_m,
                             added_value_m,
                             sector_names = NULL,
                             tolerance_percent = 1,
                             tolerance_value = 1) {
  if (!is.matrix(intermediate_consumption_m)) stop("intermediate_consumption_m must be a matrix")
  if (!is.matrix(final_demand_m)) stop("final_demand_m must be a matrix")
  if (!is.matrix(added_value_m)) stop("added_value_m must be a matrix")
  
  total_output <- rowSums(intermediate_consumption_m, na.rm = TRUE) +
    rowSums(final_demand_m, na.rm = TRUE)
  component <- get_added_value_components(intermediate_consumption_m, added_value_m)
  total_input <- component$total_input
  
  gap_per_sector <- total_input - total_output
  gap_percent_per_sector <- ifelse(total_output == 0, 0,
                                   abs(gap_per_sector / total_output) * 100)
  sector_status <- ifelse(gap_percent_per_sector <= tolerance_percent,
                          "BALANCED", "NOT BALANCED")
  
  if (is.null(sector_names)) {
    sector_names <- colnames(intermediate_consumption_m)
    if (is.null(sector_names))
      sector_names <- paste0("Sector ", seq_len(ncol(intermediate_consumption_m)))
  }
  
  balance_detail <- tibble::tibble(
    Sector = sector_names,
    Total_Input = total_input,
    Total_Output = total_output,
    Difference = gap_per_sector,
    Difference_Percent = gap_percent_per_sector,
    Status = sector_status
  )
  
  overall_input <- sum(total_input, na.rm = TRUE)
  overall_output <- sum(total_output, na.rm = TRUE)
  overall_gap <- overall_input - overall_output
  overall_gap_percent <- ifelse(overall_output == 0, 0,
                                round(abs(overall_gap / overall_output) * 100, 2))
  status <- if (overall_gap_percent <= tolerance_percent) "BALANCED" else "NOT BALANCED"
  
  list(status = status,
       total_input = overall_input,
       total_output = overall_output,
       gap = overall_gap,
       gap_percent = overall_gap_percent,
       tolerance_value = tolerance_value,
       tolerance_percent = tolerance_percent,
       balance_detail = balance_detail,
       total_input_per_sector = total_input,
       total_output_per_sector = total_output,
       gap_per_sector = gap_per_sector,
       gap_percent_per_sector = gap_percent_per_sector)
}

#' Compute technical coefficient matrix and Leontief inverse
#'
#' Computes the technical coefficient matrix (A) and the Leontief inverse
#' \eqn{(I - A)^{-1}} using validated added value components rather than
#' raw column sums.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param added_value_m Numeric matrix of added value (Component x Sector).
#'
#' @return A list with elements:
#'   \item{A}{Technical coefficient matrix}
#'   \item{Leontief}{Leontief inverse matrix}
#'   \item{inverse_total_input}{Vector of inverse total input per sector}
#'
#' @details
#' Total input is computed using [get_added_value_components()], which
#' identifies only the relevant primary-input rows (compensation, surplus,
#' taxes, imports) instead of summing all rows of the added value matrix.
#'
#' @references
#' Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and
#' Extensions*. Eurostat (2008). *Manual of Supply, Use and Input-Output Tables*.
#'
#' @examples
#' \dontrun{
#' leontief_result <- compute_leontief(ic, av)
#' }
#'
#' @export
compute_leontief <- function(intermediate_consumption_m, added_value_m) {
  
  if (!is.matrix(intermediate_consumption_m)) stop("intermediate_consumption_m must be a matrix.")
  if (!is.matrix(added_value_m)) stop("added_value_m must be a matrix.")
  
  # Total input from validated added value components
  comp <- get_added_value_components(intermediate_consumption_m, added_value_m)
  total_input <- comp$total_input
  
  inverse_total_input <- 1 / total_input
  inverse_total_input[is.infinite(inverse_total_input)] <- 0
  
  t_input_inverse <- diag(inverse_total_input)
  A <- intermediate_consumption_m %*% t_input_inverse
  I <- diag(ncol(intermediate_consumption_m))
  Leontief <- solve(I - A)
  
  list(
    A = A,
    Leontief = Leontief,
    inverse_total_input = inverse_total_input
  )
}

#' Compute linkage indices and Leontief inverse
#'
#' Computes technical coefficient matrix (A), Leontief inverse, and
#' backward/forward linkage indices (direct, total, and index).
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param added_value_m Numeric matrix of added value.
#'
#' @return A list with elements: `A`, `Leontief`, `DFL`, `TFL`, `ITFL`,
#'   `DBL`, `TBL`, `ITBL`, `DIF_TFL_DFL`, `DIF_TBL_DBL`, and
#'   `inverse_total_input`.
#'
#' @details
#' This function calls [compute_leontief()] to obtain A and Leontief,
#' then computes direct and total backward/forward linkage.
#'
#' @references
#' Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and
#' Extensions*. Rasmussen, P.N. (1956). *Studies in Inter-sectoral Relations*.
#'
#' @examples
#' \dontrun{
#' linkage <- compute_linkage(ic, av)
#' }
#'
#' @export
compute_linkage <- function(intermediate_consumption_m, added_value_m) {
  leontief_result <- compute_leontief(intermediate_consumption_m, added_value_m)
  
  A <- leontief_result$A
  Leontief <- leontief_result$Leontief
  inverse_total_input <- leontief_result$inverse_total_input
  
  dim_n <- ncol(A)
  
  dfl_value <- rowSums(A)
  tfl_value <- rowSums(Leontief)
  itfl_value <- (tfl_value * dim_n) / sum(tfl_value)
  dbl_value <- colSums(A)
  tbl_value <- colSums(Leontief)
  itbl_value <- (tbl_value * dim_n) / sum(tbl_value)
  
  list(
    A = A,
    Leontief = Leontief,
    DFL = dfl_value,
    TFL = tfl_value,
    ITFL = itfl_value,
    DIF_TFL_DFL = abs(tfl_value - dfl_value),
    DBL = dbl_value,
    TBL = tbl_value,
    ITBL = itbl_value,
    DIF_TBL_DBL = abs(tbl_value - dbl_value),
    inverse_total_input = inverse_total_input
  )
}

#' Create linkage table
#'
#' Creates a tibble containing direct, total, and index linkage values for
#' each sector.
#'
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param linkage_result A list returned by [compute_linkage()].
#'
#' @return A tibble with columns: ID, SECTOR, CATEGORY, DFL, TFL, ITFL,
#'   DBL, TBL, ITBL, |TFL-DFL|, |TBL-DBL|.
#'
#' @export
create_linkages_table <- function(sector, linkage_result) {
  tibble::tibble(
    ID = seq_len(nrow(sector)),
    SECTOR = sector[[1]],
    CATEGORY = sector[[2]],
    DFL = linkage_result$DFL,
    TFL = linkage_result$TFL,
    ITFL = linkage_result$ITFL,
    DBL = linkage_result$DBL,
    TBL = linkage_result$TBL,
    ITBL = linkage_result$ITBL,
    `|TFL-DFL|` = linkage_result$DIF_TFL_DFL,
    `|TBL-DBL|` = linkage_result$DIF_TBL_DBL
  )
}

#' Select key sectors (sektor unggulan)
#'
#' Selects sectors with `ITBL > 1` **and** `ITFL > 1`, following BPS practice:
#' "Suatu industri dikategorikan sebagai Industri Unggulan jika memiliki IBL
#' dan IFL lebih dari 1 (satu)."
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#'
#' @return A tibble containing key sectors.
#'
#' @details
#' The threshold of 1 belongs to the **normalised** indices ITBL and ITFL
#' (BPS: Indeks Daya Penyebaran and Indeks Derajat Kepekaan), which are
#' constructed so their mean is exactly 1. The direct linkages DBL and DFL
#' must not be tested against 1: `DBL = colSums(A)` is the intermediate input
#' share of total input, so it is strictly below 1 whenever value added is
#' non-negative, which makes the test unsatisfiable.
#'
#' @references
#' BPS, *Publikasi Tabel Input-Output 2016* (provincial series).
#' Rasmussen, P.N. (1956). *Studies in Inter-sectoral Relations*.
#'
#' @export
select_primary_sectors <- function(linkages_table) {
  primary_sector <- subset(linkages_table, ITBL > 1 & ITFL > 1)
  rownames(primary_sector) <- NULL
  primary_sector
}

#' Classify sectors into quadrants
#'
#' Classifies sectors into quadrants I-IV from the normalised linkage indices
#' ITBL (BPS: Indeks Daya Penyebaran) and ITFL (Indeks Derajat Kepekaan),
#' following the quadrant scheme published by BPS.
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#'
#' @return A tibble with an additional `Quadrant` column.
#'
#' @details
#' BPS quadrant scheme:
#' \itemize{
#'   \item Kuadran I   (ITBL > 1, ITFL > 1): industri unggulan
#'   \item Kuadran II  (ITBL > 1, ITFL < 1): potensi unggul
#'   \item Kuadran III (ITBL < 1, ITFL > 1): kurang diunggulkan
#'   \item Kuadran IV  (ITBL < 1, ITFL < 1): tertinggal
#' }
#'
#' @references
#' BPS Aceh, *Publikasi Tabel I-O 2016*, "Pemetaan Industri Menurut Kuadran".
#'
#' @export
classify_quadrant <- function(linkages_table) {
  linkages_table$Quadrant <- dplyr::case_when(
    linkages_table$ITBL > 1 & linkages_table$ITFL > 1 ~ "Kuadran I",
    linkages_table$ITBL > 1 & linkages_table$ITFL < 1 ~ "Kuadran II",
    linkages_table$ITBL < 1 & linkages_table$ITFL > 1 ~ "Kuadran III",
    linkages_table$ITBL < 1 & linkages_table$ITFL < 1 ~ "Kuadran IV",
    TRUE ~ "Undefined"
  )
  linkages_table
}

#' Calculate GDP per sector with land productivity and land requirement coefficients
#'
#' Computes GDP, output, land requirement, Land Requirement Coefficient (LRC),
#' and Land Productivity Coefficient (LPC) for each sector.
#'
#' @param added_value_m Numeric matrix of added value.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param land_proportion Numeric matrix of land proportions (Sector x Land Cover).
#' @param landuse_table Data frame or tibble with an `area` column
#'   representing land area in hectares.
#' @param leontief Numeric Leontief inverse matrix.
#'
#' @return A tibble with columns: SECTOR, CATEGORY, GDP, OUTPUT,
#'   Land_Requirement, LRC, LPC, GDP_PROP, FD, Prop_FD, Total_OUTPUT.
#'
#' @details
#' Land Requirement Coefficient (LRC) and Land Productivity Coefficient (LPC)
#' follow Hubacek & Sun (2001):
#'
#' \deqn{LRC_i = \frac{Land_i}{X_i}}
#' \deqn{LPC_i = \frac{1}{LRC_i} = \frac{X_i}{Land_i}}
#'
#' where \eqn{X_i} is total sectoral output and \eqn{Land_i} is total land
#' requirement of sector i.
#'
#' @references
#' Hubacek, K., & Sun, L. (2001). A scenario analysis of China's land use and
#' land cover change: incorporating biophysical information into input-output
#' modeling. Structural Change and Economic Dynamics, 12(4), 367–397.
#'
#' @export
calculate_gdp <- function(added_value_m,
                          intermediate_consumption_m,
                          final_demand_m,
                          sector,
                          land_proportion,
                          landuse_table,
                          leontief) {
  
  # Land requirement matrix
  landuse_area <- as.numeric(landuse_table$area)
  landuse_area_diag <- diag(landuse_area)
  land_requirement <- land_proportion %*% landuse_area_diag
  
  # Total output per sector
  output_val <- rowSums(final_demand_m) + rowSums(intermediate_consumption_m)
  
  # Added value / GDP
  component <- get_added_value_components(intermediate_consumption_m, added_value_m)
  gdp_val <- component$gdp_val
  
  # Land Requirement Coefficient (ha per unit of output)
  land_req_total <- rowSums(land_requirement)
  LRC <- ifelse(output_val == 0, 0, land_req_total / output_val)
  
  # Land Productivity Coefficient (output per ha)
  LPC <- ifelse(LRC == 0, 0, 1 / LRC)
  
  GDP <- tibble::tibble(
    SECTOR = sector[[1]],
    CATEGORY = sector[[2]],
    GDP = as.numeric(gdp_val),
    OUTPUT = as.numeric(output_val),
    Land_Requirement = land_req_total,
    LRC = LRC,
    LPC = LPC
  )
  
  GDP$GDP_PROP <- ifelse(GDP$OUTPUT == 0, 0, GDP$GDP / GDP$OUTPUT)
  GDP$FD <- rowSums(final_demand_m)
  GDP$Prop_FD <- ifelse(GDP$OUTPUT == 0, 0, GDP$FD / GDP$OUTPUT)
  GDP$Total_OUTPUT <- leontief %*% GDP$FD
  
  GDP
}

#' Output multiplier
#'
#' Output multiplier = column sums of Leontief inverse. Measures total output
#' generated in the economy per unit increase in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `table`, `top`, `plot`.
#'
#' @export
calc_output_multiplier <- function(Leontief, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  Out.multiplier <- colSums(Leontief)
  Out.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Out.multiplier = Out.multiplier
  )
  order_Out.multiplier <- Out.multiplier[order(-Out.multiplier$Out.multiplier), ]
  OMPL_graph <- ggplot(order_Out.multiplier,
                       aes(x = SECTOR, y = Out.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sectors") +
    ylab("Output multiplier")
  list(table = Out.multiplier, top = order_Out.multiplier, plot = OMPL_graph)
}

#' Income multiplier
#'
#' Income multiplier = `colSums(diag(income_coeff) %*% Leontief)`.
#' Measures total labour income generated economy-wide per unit increase
#' in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param total_output Numeric vector of total output per sector.
#' @param added_value Numeric matrix of added value.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `income`, `income_coeff`,
#'   `multiplier_matrix`, `table`, `top`, `plot`.
#'
#' @export
calc_income_multiplier <- function(Leontief, total_output, added_value, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  if (!"Kompensasi Tenaga Kerja" %in% rownames(added_value))
    stop("Row 'Kompensasi Tenaga Kerja' not found in added_value matrix.")
  
  income_value <- as.numeric(added_value["Kompensasi Tenaga Kerja", ])
  total_output <- as.numeric(total_output)
  income_coeff <- ifelse(total_output == 0, 0, income_value / total_output)
  
  income_multiplier_matrix <- diag(income_coeff) %*% Leontief
  income_multiplier <- colSums(income_multiplier_matrix, na.rm = TRUE)
  
  Inc.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Inc.multiplier = income_multiplier
  )
  order_Inc.multiplier <- head(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier), ], top_n)
  IMPL_graph <- ggplot(order_Inc.multiplier,
                       aes(x = reorder(SECTOR, Inc.multiplier),
                           y = Inc.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("Income Multiplier") +
    theme_minimal()
  
  list(income = income_value,
       income_coeff = income_coeff,
       multiplier_matrix = income_multiplier_matrix,
       table = Inc.multiplier,
       top = order_Inc.multiplier,
       plot = IMPL_graph)
}

#' Labour multiplier
#'
#' Labour multiplier = `colSums(diag(labour_coeff) %*% Leontief)`.
#' Measures total employment generated economy-wide per unit increase
#' in final demand of a sector.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param labour Numeric vector of labour per sector.
#' @param total_output Numeric vector of total output per sector.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `labour`, `labour_coeff`,
#'   `multiplier_matrix`, `table`, `top`, `plot`.
#'
#' @export
calc_labour_multiplier <- function(Leontief, labour, total_output, sector, top_n = 20) {
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  labour <- as.numeric(unlist(labour))
  total_output <- as.numeric(total_output)
  labour_coeff <- ifelse(total_output == 0, 0, labour / total_output)
  
  labour_multiplier_matrix <- diag(labour_coeff) %*% Leontief
  labour_multiplier <- colSums(labour_multiplier_matrix, na.rm = TRUE)
  
  Lab.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    Lab.multiplier = labour_multiplier
  )
  order_Lab.multiplier <- head(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier), ], top_n)
  LMPL_graph <- ggplot(order_Lab.multiplier,
                       aes(x = reorder(SECTOR, Lab.multiplier),
                           y = Lab.multiplier, fill = CATEGORY)) +
    geom_bar(stat = "identity", colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("Labour Multiplier") +
    theme_minimal()
  
  list(labour = labour,
       labour_coeff = labour_coeff,
       multiplier_matrix = labour_multiplier_matrix,
       table = Lab.multiplier,
       top = order_Lab.multiplier,
       plot = LMPL_graph)
}

#' Surplus Usaha Bruto (SUB) multiplier
#'
#' Computes the gross operating surplus multiplier using sectoral output
#' as the denominator, consistent with standard Input-Output multiplier
#' formulas.
#'
#' @param Leontief Numeric Leontief inverse matrix.
#' @param total_output Numeric vector of total output per sector.
#' @param added_value_m Numeric matrix of added value.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param sector A data frame or tibble with at least two columns:
#'   sector name and category.
#' @param top_n Integer. Number of top sectors to include in the plot.
#'
#' @return A list with elements: `coefficient`, `total_output`, `surplus`,
#'   `table`, `top`, `plot`, `compensation`.
#'
#' @details
#' The surplus coefficient is calculated as:
#' \deqn{sub\_coeff_i = \frac{S_i}{X_i}}
#' where \eqn{S_i} is gross operating surplus and \eqn{X_i} is total output.
#'
#' @references
#' Miller, R.E. & Blair, P.D. (2009). *Input-Output Analysis: Foundations and
#' Extensions*, Chapter 6.
#'
#' @export
calc_sub_multiplier <- function(Leontief,
                                total_output,
                                added_value_m,
                                intermediate_consumption_m,
                                sector,
                                top_n = 20) {
  
  sector_name <- sector[[1]]
  sector_category <- sector[[2]]
  
  comp <- get_added_value_components(intermediate_consumption_m, added_value_m)
  surplus <- comp$surplus
  compensation <- comp$compensation
  
  total_output <- as.numeric(total_output)
  sub_coeff <- ifelse(total_output == 0, 0, surplus / total_output)
  
  sub_multiplier_matrix <- diag(sub_coeff) %*% Leontief
  sub_multiplier <- colSums(sub_multiplier_matrix, na.rm = TRUE)
  
  SUB.multiplier <- tibble::tibble(
    SECTOR = sector_name,
    CATEGORY = sector_category,
    SUB.multiplier = sub_multiplier
  )
  
  top_SUB <- head(SUB.multiplier[order(-SUB.multiplier$SUB.multiplier), ], top_n)
  SUB_graph <- ggplot(top_SUB,
                      aes(x = reorder(SECTOR, SUB.multiplier),
                          y = SUB.multiplier, fill = CATEGORY)) +
    geom_col(colour = "black") +
    coord_flip() +
    xlab("Sector") +
    ylab("SUB Multiplier") +
    theme_minimal()
  
  list(coefficient = sub_coeff,
       total_output = total_output,
       surplus = surplus,
       table = SUB.multiplier,
       top = top_SUB,
       plot = SUB_graph,
       compensation = compensation)
}

#' Combine multiplier tables
#'
#' Combines output, income, labour, and SUB multiplier tables into a single
#' tibble.
#'
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#'
#' @return A tibble with columns: SECTOR, CATEGORY, Out.multiplier,
#'   Inc.multiplier, Lab.multiplier, SUB.multiplier.
#'
#' @export
combine_multipliers <- function(OM, IM, LM, SUBM) {
  multiplier <- OM$table |>
    dplyr::left_join(IM$table[, c("SECTOR", "Inc.multiplier")], by = "SECTOR") |>
    dplyr::left_join(LM$table[, c("SECTOR", "Lab.multiplier")], by = "SECTOR") |>
    dplyr::left_join(SUBM$table[, c("SECTOR", "SUB.multiplier")], by = "SECTOR")
  multiplier$Out.multiplier <- round(multiplier$Out.multiplier, 3)
  multiplier
}

#' Extract Added Value components from Input-Output table
#'
#' Identifies and calculates added value components (import, compensation,
#' surplus, taxes) from the added value matrix. Also computes total input
#' and GDP per sector.
#'
#' @param intermediate_consumption_m Numeric matrix of intermediate
#'   consumption (Sector x Sector).
#' @param added_value_m Numeric matrix of added value (Component x Sector).
#'
#' @return A list containing:
#'   \item{total_input}{Total input per sector}
#'   \item{intermediate_input}{Intermediate input per sector}
#'   \item{import_input}{Import input per sector}
#'   \item{compensation}{Compensation of employees per sector}
#'   \item{surplus}{Gross operating surplus per sector}
#'   \item{other_tax}{Other taxes less subsidies on production}
#'   \item{tax_product}{Taxes less subsidies on products}
#'   \item{primary_input}{Primary input (compensation + surplus + other taxes)}
#'   \item{gdp_val}{GDP per sector (same as primary input)}
#'   and row indices for each component.
#'
#' @details
#' GDP = Compensation of employees + Gross operating surplus + Other taxes
#' less subsidies on production. Total input = intermediate input + import +
#' taxes on products + primary input.
#'
#' @examples
#' \dontrun{
#' comp <- get_added_value_components(ic, av)
#' }
#'
#' @export
get_added_value_components <- function(intermediate_consumption_m, added_value_m) {
  row_mapping <- list(
    import_input   = c("Total Konsumsi Antara Impor", "Input Antara (Impor)"),
    compensation   = c("Kompensasi Tenaga Kerja"),
    surplus        = c("Surplus Usaha Bruto"),
    other_tax      = c("Pajak Dikurang Subsidi Lainnya Atas Produksi", "Pajak Neto Subsidi Atas Produksi"),
    tax_product    = c("Pajak Dikurang Subsidi Atas Produk", "Pajak Neto Subsidi Atas Produk")
  )
  
  import_row       <- find_row_alias(added_value_m, row_mapping$import_input)
  compensation_row <- find_row_alias(added_value_m, row_mapping$compensation)
  surplus_row      <- find_row_alias(added_value_m, row_mapping$surplus)
  other_tax_row    <- find_row_alias(added_value_m, row_mapping$other_tax)
  tax_product_row  <- find_row_alias(added_value_m, row_mapping$tax_product)
  
  missing_rows <- c()
  if (is.null(import_row))       missing_rows <- c(missing_rows, "200 - Total Konsumsi Antara Impor / Input Antara (Impor)")
  if (is.null(compensation_row)) missing_rows <- c(missing_rows, "201 - Kompensasi Tenaga Kerja")
  if (is.null(surplus_row))      missing_rows <- c(missing_rows, "202 - Surplus Usaha Bruto")
  if (is.null(other_tax_row))    missing_rows <- c(missing_rows, "203 - Pajak Dikurang Subsidi Lainnya Atas Produksi / Pajak Neto Subsidi Atas Produksi")
  if (is.null(tax_product_row))  missing_rows <- c(missing_rows, "195 - Pajak Dikurang Subsidi Atas Produk")
  if (length(missing_rows) > 0) {
    stop(paste("The following components were not found:\n", paste(missing_rows, collapse = "\n")),
         call. = FALSE)
  }
  
  intermediate_input <- colSums(intermediate_consumption_m, na.rm = TRUE)
  import_input    <- as.numeric(added_value_m[import_row, ])
  compensation    <- as.numeric(added_value_m[compensation_row, ])
  surplus         <- as.numeric(added_value_m[surplus_row, ])
  other_tax       <- as.numeric(added_value_m[other_tax_row, ])
  tax_product     <- as.numeric(added_value_m[tax_product_row, ])
  
  gdp_val       <- compensation + surplus + other_tax
  primary_input <- gdp_val
  total_input   <- intermediate_input + import_input + tax_product + primary_input
  
  list(
    total_input        = total_input,
    intermediate_input = intermediate_input,
    import_input       = import_input,
    compensation       = compensation,
    surplus            = surplus,
    other_tax          = other_tax,
    tax_product        = tax_product,
    primary_input      = primary_input,
    gdp_val            = gdp_val,
    import_row         = import_row,
    compensation_row   = compensation_row,
    surplus_row        = surplus_row,
    other_tax_row      = other_tax_row,
    tax_product_row    = tax_product_row
  )
}

#' Build full Input-Output table and code registry
#'
#' Combines Intermediate Consumption, Final Demand, and Added Value into a
#' single complete matrix, along with a Code Registry that maps each code to
#' label, type, and row/column position.
#'
#' @param sector A data frame or tibble with at least one column of sector names.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param final_demand_component Data frame or tibble with two columns:
#'   code and label for final demand components.
#' @param added_value_m Numeric matrix of added value.
#' @param added_value_lookup Data frame or tibble with two columns:
#'   code and label for added value components.
#'
#' @return A list with elements:
#'   \item{full_io_table}{Complete IO matrix}
#'   \item{code_registry}{Tibble with code mapping}
#'   \item{n_sector}{Number of sectors}
#'
#' @examples
#' \dontrun{
#' full <- build_full_io_table(sector, ic, fd, fd_comp, av, av_lookup)
#' }
#'
#' @export
build_full_io_table <- function(sector, intermediate_consumption_m, final_demand_m,
                                final_demand_component, added_value_m, added_value_lookup) {
  n_sector   <- nrow(sector)
  sector_codes <- as.character(seq_len(n_sector))
  fd_codes   <- as.character(final_demand_component[[1]])
  va_codes   <- as.character(added_value_lookup[[1]])
  
  if (anyNA(fd_codes) || any(trimws(fd_codes) == ""))
    stop("There are empty/NA codes in the first column of 'final_demand'.", call. = FALSE)
  if (anyNA(va_codes) || any(trimws(va_codes) == ""))
    stop("There are empty/NA codes in the first column of 'added_value'.", call. = FALSE)
  if (any(fd_codes %in% sector_codes))
    stop("Final Demand codes conflict with sector codes (1..N).")
  if (any(va_codes %in% sector_codes))
    stop("Added Value codes conflict with sector codes (1..N).")
  if (any(fd_codes %in% va_codes))
    stop("Final Demand codes conflict with Added Value codes.")
  
  row_codes <- c(sector_codes, va_codes)
  col_codes <- c(sector_codes, fd_codes)
  n_row <- length(row_codes)
  n_col <- length(col_codes)
  
  full_io <- matrix(NA_real_, nrow = n_row, ncol = n_col,
                    dimnames = list(row_codes, col_codes))
  full_io[seq_len(n_sector), seq_len(n_sector)] <- intermediate_consumption_m
  full_io[seq_len(n_sector), (n_sector + 1):n_col] <- final_demand_m
  full_io[(n_sector + 1):n_row, seq_len(n_sector)] <- added_value_m
  
  code_registry <- dplyr::bind_rows(
    tibble::tibble(Code = sector_codes, Label = as.character(sector[[1]]),
                   Type = "Sector", Axis = "Both",
                   Row_Index = seq_len(n_sector), Col_Index = seq_len(n_sector)),
    tibble::tibble(Code = fd_codes, Label = as.character(final_demand_component[[2]]),
                   Type = "Final_Demand", Axis = "Column",
                   Row_Index = NA_integer_, Col_Index = (n_sector + 1):n_col),
    tibble::tibble(Code = va_codes, Label = as.character(added_value_lookup[[2]]),
                   Type = "Added_Value", Axis = "Row",
                   Row_Index = (n_sector + 1):n_row, Col_Index = NA_integer_)
  )
  
  list(full_io_table = full_io, code_registry = code_registry, n_sector = n_sector)
}

#' Save core analysis results to RDS
#'
#' Saves the results of the core Input-Output analysis into an RDS file so
#' that visualisation can be performed without recalculating.
#'
#' @param core_analysis_results List containing core analysis outputs.
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#'
#' @return Invisibly returns the output directory.
#' @export
save_core_analysis_data <- function(core_analysis_results,
                                    output_dir = "inst/extdata/output") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(core_analysis_results,
          file = file.path(output_dir, "descriptive_core_analysis.rds"))
  invisible(output_dir)
}

#' Export descriptive analysis outputs to Excel
#'
#' Writes all main results of the descriptive Input-Output analysis into a
#' single Excel workbook. The workbook contains the following sheets:
#'
#' - `GDP`: GDP per sector table
#' - `Leontief_Matrix`: Leontief inverse matrix
#' - `Growth_Rate`: sectoral growth rates
#' - `Multiplier_Output`, `Multiplier_Income`, `Multiplier_Labour`,
#'   `Multiplier_SUB`: multiplier tables
#' - `Land_Proportion`: land proportion matrix
#' - `Linkage`: linkage table (DBL, DFL, TBL, TFL, ITBL, ITFL)
#' - `Full_IO_Table`: complete IO matrix (sectors + final demand + added value)
#' - `Code_Registry`: code registry for modules that need cell-level changes
#' - `Balance_Detail`: per-sector balance check table
#'
#' @param gdp_baseline Tibble returned by [calculate_gdp()].
#' @param linkage_result List returned by [compute_linkage()].
#' @param growth_rate Tibble or data frame of sectoral growth rates.
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#' @param land_proportion Numeric matrix of land proportions.
#' @param linkage_table Tibble of linkage results (from [create_linkages_table()]).
#' @param full_io_table Numeric matrix from [build_full_io_table()].
#' @param code_registry Tibble from [build_full_io_table()].
#' @param balance_detail Tibble from [check_io_balance()].
#' @param output_path Character. Path to the output Excel file.
#'   Default is `"data/IO_Analysis_Output.xlsx"`.
#'
#' @return Invisibly returns the full path to the saved workbook.
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook setColWidths
#'
#' @examples
#' \dontrun{
#' export_io_analysis(
#'   gdp_baseline = gdp_baseline,
#'   linkage_result = linkage_result,
#'   growth_rate = growth_rate,
#'   OM = OM, IM = IM, LM = LM, SUBM = SUBM,
#'   land_proportion = land_proportion,
#'   linkage_table = linkages_table,
#'   full_io_table = full_io_table,
#'   code_registry = code_registry,
#'   balance_detail = balance_result$balance_detail,
#'   output_path = "data/IO_Analysis_Output.xlsx"
#' )
#' }
#'
#' @export
export_io_analysis <- function(sector,
                               gdp_baseline,
                               linkage_result,
                               growth_rate,
                               labour,
                               OM, IM, LM, SUBM,
                               land_proportion,
                               landuse_table,
                               linkage_table,
                               full_io_table,
                               code_registry,
                               balance_detail,
                               output_path = "data/IO_Analysis_Output.xlsx") {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to export Excel files.")
  }
  
  # Ensure output directory exists
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  wb <- openxlsx::createWorkbook()
  
  # Sector
  if (!is.null(sector)) {
    openxlsx::addWorksheet(wb, "Sector")
    openxlsx::writeData(wb, sheet = "Sector", x = sector)
  } else {
    # Fallback: Buat sheet Sector dari gdp_baseline jika parameter tidak diisi
    sector_fallback <- gdp_baseline[, c("SECTOR", "CATEGORY")]
    openxlsx::addWorksheet(wb, "Sector")
    openxlsx::writeData(wb, sheet = "Sector", x = sector_fallback)
  }
  
  # GDP
  openxlsx::addWorksheet(wb, "GDP")
  openxlsx::writeData(wb, sheet = "GDP", x = gdp_baseline)
  
  # Leontief matrix
  openxlsx::addWorksheet(wb, "Leontief_Matrix")
  openxlsx::writeData(wb, sheet = "Leontief_Matrix",
                      x = as.data.frame(linkage_result$Leontief),
                      rowNames = FALSE, colNames = FALSE)
  
  # Growth rate
  openxlsx::addWorksheet(wb, "Growth_Rate")
  openxlsx::writeData(wb, sheet = "Growth_Rate", x = as.data.frame(growth_rate))
  
  # Multiplier tables
  openxlsx::addWorksheet(wb, "Multiplier_Output")
  openxlsx::writeData(wb, sheet = "Multiplier_Output", x = OM$table)
  
  openxlsx::addWorksheet(wb, "Multiplier_Income")
  openxlsx::writeData(wb, sheet = "Multiplier_Income", x = IM$table)
  
  labour_table <- LM$table |>
    dplyr::mutate(
      Labour_Compensation = SUBM$compensation,
      Labour = as.numeric(unlist(labour))
    )
  
  openxlsx::addWorksheet(wb, "Multiplier_Labour")
  openxlsx::writeData(wb, sheet = "Multiplier_Labour", x = labour_table)
  
  sub_table <- SUBM$table |>
    dplyr::mutate(Surplus = SUBM$surplus)
  
  openxlsx::addWorksheet(wb, "Multiplier_SUB")
  openxlsx::writeData(wb, sheet = "Multiplier_SUB", x = sub_table)
  
  # Land Cover
  if (!missing(landuse_table) && !is.null(landuse_table)) {
    openxlsx::addWorksheet(wb, "Land_Cover")
    openxlsx::writeData(wb, sheet = "Land_Cover", x = landuse_table)
  }
  
  # Land proportion
  openxlsx::addWorksheet(wb, "Land_Proportion")
  openxlsx::writeData(wb, sheet = "Land_Proportion",
                      x = land_proportion,
                      rowNames = TRUE, colNames = TRUE)
  
  # Linkage
  openxlsx::addWorksheet(wb, "Linkage")
  openxlsx::writeData(wb, sheet = "Linkage", x = linkage_table)
  
  # Full IO table & code registry
  openxlsx::addWorksheet(wb, "Full_IO_Table")
  openxlsx::writeData(wb, sheet = "Full_IO_Table",
                      x = full_io_table,
                      rowNames = TRUE, colNames = TRUE)
  
  openxlsx::addWorksheet(wb, "Code_Registry")
  openxlsx::writeData(wb, sheet = "Code_Registry", x = code_registry)
  
  # Balance detail
  if (!missing(balance_detail) && !is.null(balance_detail)) {
    openxlsx::addWorksheet(wb, "Balance_Detail")
    openxlsx::writeData(wb, sheet = "Balance_Detail", x = balance_detail)
  }
  
  # Auto width
  for (s in names(wb)) {
    openxlsx::setColWidths(wb, sheet = s, cols = 1:100, widths = "auto")
  }
  
  openxlsx::saveWorkbook(wb, file = output_path, overwrite = TRUE)
  invisible(output_path)
}

#' Save final descriptive analysis data to RDS or RData
#'
#' Saves a single combined object containing all inputs and results of the
#' descriptive Input-Output analysis. This file is intended to be loaded by
#' the projection module or other downstream modules.
#'
#' @param sector Tibble of sectors.
#' @param intermediate_consumption_m Numeric matrix of intermediate consumption.
#' @param final_demand_m Numeric matrix of final demand.
#' @param added_value_m Numeric matrix of added value.
#' @param labour Labour vector or one-column matrix/tibble.
#' @param land_proportion Numeric matrix of land proportions.
#' @param growth_rate Tibble or data frame of growth rates.
#' @param landuse_table Tibble with columns `id`, `lc`, `area`.
#' @param gdp_baseline Tibble returned by [calculate_gdp()].
#' @param linkage_result List returned by [compute_linkage()].
#' @param OM Output from [calc_output_multiplier()].
#' @param IM Output from [calc_income_multiplier()].
#' @param LM Output from [calc_labour_multiplier()].
#' @param SUBM Output from [calc_sub_multiplier()].
#' @param full_io_table Numeric matrix from [build_full_io_table()].
#' @param code_registry Tibble from [build_full_io_table()].
#' @param output_dir Character. Directory to save the RDS file.
#'   Default is `"inst/extdata/output"`.
#' @param combined_file Character. Filename for the combined object.
#'   Default is `"descriptive_analysis_module.rds"`.
#' @param format Character. Either `"rds"` (default) or `"rdata"`.
#'
#' @return Invisibly returns the output directory.
#'
#' @examples
#' \dontrun{
#' save_analysis_data(
#'   sector                   = sector,
#'   intermediate_consumption_m = intermediate_consumption.m,
#'   final_demand_m           = final_demand.m,
#'   added_value_m            = added_value.m,
#'   labour                   = labour,
#'   land_proportion          = land_proportion,
#'   growth_rate              = growth_rate,
#'   landuse_table            = landuse_table,
#'   gdp_baseline             = gdp_baseline,
#'   linkage_result           = linkage_result,
#'   OM = OM, IM = IM, LM = LM, SUBM = SUBM,
#'   full_io_table            = full_io_table,
#'   code_registry            = code_registry,
#'   output_dir               = "inst/extdata/output"
#' )
#' }
#'
#' @export
save_analysis_data <- function(sector,
                               intermediate_consumption_m,
                               final_demand_m,
                               added_value_m,
                               labour,
                               land_proportion,
                               growth_rate,
                               landuse_table,
                               gdp_baseline,
                               linkage_result,
                               OM, IM, LM, SUBM,
                               full_io_table,
                               code_registry,
                               output_dir = "inst/extdata/output",
                               combined_file = "descriptive_analysis_module.rds",
                               format = c("rds", "rdata")) {
  
  format <- match.arg(format)
  
  # -----------------------------------------------
  # Derive sector index
  # -----------------------------------------------
  berbasis_lahan <- tolower(trimws(as.character(sector$Berbasis_Lahan)))
  constrained    <- as.numeric(berbasis_lahan %in% c("ya", "yes", "true", "1"))
  unconstrained  <- 1 - constrained
  
  # -----------------------------------------------
  # Full combined list
  # -----------------------------------------------
  io <- list(
    sector                   = sector,
    constrained              = constrained,
    unconstrained            = unconstrained,
    intermediate_consumption = intermediate_consumption_m,
    final_demand             = final_demand_m,
    final_demand_component   = if (exists("final_demand_component")) final_demand_component else NULL,
    added_value              = added_value_m,
    added_value_lookup       = if (exists("added_value_lookup")) added_value_lookup else NULL,
    labour                   = labour,
    land_proportion          = land_proportion,
    P_default                = if (exists("P_default")) P_default else NULL,
    landuse_table            = landuse_table,
    growth_rate              = growth_rate,
    gdp_baseline             = gdp_baseline,
    linkage                  = linkage_result,
    multipliers = list(
      output = OM$table,
      income = IM$table,
      labour = LM$table  |> 
        dplyr::mutate(
          Labour_Compensation = SUBM$compensation,
          Labour = as.numeric(unlist(labour))
        ),
      sub    = SUBM$table |> 
        dplyr::mutate(Surplus = SUBM$surplus)
    ),
    full_io_table            = full_io_table,
    code_registry            = code_registry
  )
  
  # -----------------------------------------------
  # Create output directory
  # -----------------------------------------------
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # -----------------------------------------------
  # Save combined object
  # -----------------------------------------------
  if (format == "rds") {
    saveRDS(io, file = file.path(output_dir, combined_file))
  } else {
    save(io, file = file.path(output_dir, sub("\\.rds$", ".RData", combined_file)))
  }
  
  invisible(output_dir)
}

#' Generate linkage bubble plot by sector category
#'
#' Scatter plot of ITBL (Indeks Daya Penyebaran) against ITFL (Indeks Derajat
#' Kepekaan), with bubble size showing combined linkage strength and colour
#' showing sector category. Quadrant reference lines are drawn at 1.
#'
#' Use [generate_quadrant_plot()] for the companion view coloured by BPS
#' quadrant instead of category.
#'
#' @param linkages_table A tibble produced by [create_linkages_table()].
#' @param show_labels Logical. If `TRUE`, sector IDs are displayed.
#' @param base_size Numeric. Base font size for ggplot.
#'
#' @return A `ggplot` object.
#'
#' @export
generate_prs_plot <- function(linkages_table,
                              point_size = 5,
                              label_size = 3,
                              label_alpha = 0.6) {
  # ITBL and ITFL are normalised so their mean is exactly 1: the reference line
  # at 1 is therefore both the BPS threshold and the cross-sector average.
  linkages_table <- linkages_table %>%
    mutate(
      Quadrant = factor(
        Quadrant,
        levels = c("Kuadran I", "Kuadran II", "Kuadran III", "Kuadran IV")
      ),
      # Pastikan ID karakter (untuk label)
      ID = as.character(ID)
    )
  
  # Color code per kategori/kuadran
  quadrant_colors <- c(
    "Kuadran I"   = "#2E86AB",  # Biru - unggulan
    "Kuadran II"  = "#A23B72",  # Magenta - potensi unggul
    "Kuadran III" = "#F18F01",  # Orange - kurang diunggulkan
    "Kuadran IV"  = "#C73E1D"   # Merah - tertinggal
  )
  
  p <- ggplot(
    linkages_table,
    aes(
      x = ITBL,
      y = ITFL,
      color = Quadrant,
      text = paste0(
        "<b>", SECTOR, "</b><br>",
        "ITBL: ", round(ITBL, 3), "<br>",
        "ITFL: ", round(ITFL, 3), "<br>",
        "Kuadran: ", Quadrant
      )
    )
  ) +
    # Garis pembatas kuadran
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40", alpha = 0.7) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey40", alpha = 0.7) +
    # Bubble dengan ukuran SERAGAM + alpha transparan + jitter ringan
    geom_point(
      size = point_size,
      alpha = 0.6,
      position = position_jitter(width = 0.01, height = 0.01, seed = 42)
    ) +
    # Angka ID di tengah bubble, warna transparan
    geom_text(
      aes(label = ID),
      color = "white",
      alpha = label_alpha,
      size = label_size,
      fontface = "bold",
      position = position_jitter(width = 0.01, height = 0.01, seed = 42),
      show.legend = FALSE
    ) +
    scale_color_manual(values = quadrant_colors, name = "Kategori") +
    labs(
      title = "Peta Sektor Berdasarkan Backward & Forward Linkage",
      subtitle = "Garis putus-putus menunjukkan nilai rata-rata (indeks = 1)",
      x = "Indeks Total Backward Linkage (ITBL)",
      y = "Indeks Total Forward Linkage (ITFL)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

#' Color code kategori sektor
#' @export
category_colors <- c(
  "Pertanian"      = "#66BB6A",
  "Perkebunan"     = "#2E7D32",
  "Peternakan"     = "#9CCC65",
  "Kehutanan"      = "#1B5E20",
  "Perikanan"      = "#26C6DA",
  "Pertambangan"   = "#795548",
  "Industri"       = "#EF5350",
  "Utilitas"       = "#FFA726",
  "Konstruksi"     = "#8E24AA",
  "Perdagangan"    = "#FB8C00",
  "Transportasi"   = "#3949AB",
  "Jasa & Layanan" = "#29B6F6",
  "Pelayanan Umum" = "#546E7A",
  "Lainnya"        = "#BDBDBD"
)

#' Generate top-N multiplier bar chart (warna per kategori sektor)
#'
#' Horizontal bar chart of the N sectors with the highest value of one
#' multiplier. Bar colour follows the sector category via `category_colors`.
#'
#' @param multiplier_table A tibble produced by [combine_multipliers()].
#' @param sector A tibble of sector metadata with columns `Nama_Sektor` and
#'   `Kategori`.
#' @param column Character. Column to plot, e.g. `"Out.multiplier"`.
#' @param title Character. Plot title.
#' @param x_label Character. Axis label for the multiplier value.
#' @param top_n Integer. Number of sectors to show (default 15).
#' @param digits Integer. Rounding for the printed value.
#'
#' @return A `ggplot` object.
#'
#' @export
generate_multiplier_bar_chart <- function(multiplier_table,
                                          sector,
                                          column,
                                          title,
                                          x_label = "Multiplier",
                                          top_n = 15,
                                          digits = 4,
                                          label_width = 30) {
  if (!column %in% names(multiplier_table))
    stop(sprintf("Column '%s' not found in multiplier_table.", column), call. = FALSE)
  
  # Fungsi truncate: pakai stringr kalau ada, fallback ke base R
  truncate_label <- function(x, width) {
    if (requireNamespace("stringr", quietly = TRUE)) {
      stringr::str_trunc(x, width = width, side = "right")
    } else {
      ifelse(
        nchar(x) > width,
        paste0(substr(x, 1, width - 3), "..."),
        x
      )
    }
  }
  
  df <- multiplier_table %>%
    left_join(
      sector %>% dplyr::select(SECTOR = Nama_Sektor, Category = Kategori),
      by = "SECTOR"
    ) %>%
    mutate(VALUE = as.numeric(.data[[column]])) %>%
    filter(!is.na(VALUE)) %>%
    arrange(desc(VALUE)) %>%
    slice_head(n = top_n) %>%
    mutate(
      LABEL = format(round(VALUE, digits), trim = TRUE),
      # Potong label sumbu Y, simpan nama asli untuk tooltip
      SECTOR_SHORT = truncate_label(SECTOR, label_width),
      SECTOR_SHORT = factor(SECTOR_SHORT, levels = rev(SECTOR_SHORT))
    )
  
  ggplot(
    df,
    aes(
      x = SECTOR_SHORT,
      y = VALUE,
      fill = Category,
      text = paste0(
        "<b>", SECTOR, "</b><br>",       # tooltip pakai nama asli
        "Kategori: ", Category, "<br>",
        x_label, ": ", LABEL
      )
    )
  ) +
    geom_col(alpha = 0.9) +
    geom_text(aes(label = LABEL), hjust = -0.15, size = 3.2) +
    coord_flip() +
    scale_fill_manual(
      values = category_colors,
      name = "Kategori",
      na.value = "grey80"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(title = title, x = NULL, y = x_label) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}
#' Generate GDP bar chart
#'
#' Creates a horizontal bar chart for the top sectors by GDP.
#'
#' @param gdp_sector A tibble produced by [calculate_gdp()].
#' @param sector_info A tibble with sector names and categories.
#' @param top_n Integer. Number of top sectors to display.
#'
#' @return A list with elements `plot` (ggplot) and `data` (tibble).
#'
#' @export
generate_gdp_bar_chart <- function(gdp_sector, sector_info, top_n = 10) {
  plot_df <- merge(gdp_sector, sector_info,
                   by.x = "SECTOR", by.y = colnames(sector_info)[1], all.x = TRUE)
  total_gdp <- sum(plot_df$GDP, na.rm = TRUE)
  plot_df$PERCENTAGE <- round(100 * plot_df$GDP / total_gdp, 2)
  plot_df <- head(plot_df[order(-plot_df$GDP), ], top_n)
  category_col <- colnames(sector_info)[2]
  
  p <- ggplot(plot_df, aes(x = reorder(SECTOR, GDP), y = GDP,
                           fill = .data[[category_col]],
                           text = paste0("<b>Sector:</b> ", SECTOR,
                                         "<br><b>GDP:</b> ",
                                         format(round(GDP,0), big.mark = ","),
                                         " Million IDR",
                                         "<br><b>Contribution:</b> ", PERCENTAGE, "%"))) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "GDP (Million IDR)", fill = "Category") +
    theme_minimal()
  
  list(plot = p, data = plot_df)
}

#' Generate GDP donut chart
#'
#' Creates an interactive donut chart showing the share of top sectors in
#' total GDP.
#'
#' @param gdp_sector A tibble produced by [calculate_gdp()].
#' @param sector_info A tibble with sector names and categories.
#' @param top_n Integer. Number of top sectors to display.
#'
#' @return A list with elements `plot` (plotly) and `data` (tibble).
#'
#' @export
generate_gdp_donut_chart <- function(gdp_baseline, sector) {
  total_gdp <- sum(gdp_baseline$GDP, na.rm = TRUE)
  
  gdp_sorted <- gdp_baseline %>%
    mutate(GDP = as.numeric(GDP)) %>%
    arrange(desc(GDP))
  
  top10 <- gdp_sorted %>% slice_head(n = 10)
  others_gdp <- sum(gdp_sorted$GDP) - sum(top10$GDP)
  
  donut_data <- bind_rows(
    top10 %>% select(SECTOR, GDP),
    tibble(SECTOR = "Lainnya", GDP = others_gdp)
  ) %>%
    mutate(
      CONTRIBUTION = GDP / total_gdp * 100,
      SECTOR = factor(SECTOR, levels = c(rev(top10$SECTOR), "Lainnya"))
    )
  
  # Palet warna: 10 warna untuk top10 + abu untuk "Lainnya"
  cols <- c(
    scales::hue_pal()(10),
    "grey70"
  )
  
  p <- plot_ly(
    donut_data,
    labels = ~SECTOR,
    values = ~GDP,
    type = "pie",
    hole = 0.5,  # <- ini yang bikin bolong
    textposition = "inside",
    textinfo = "percent",
    hovertemplate = paste0(
      "<b>%{label}</b><br>",
      "PDRB: %{value:,.0f}<br>",
      "Kontribusi: %{percent}<extra></extra>"
    ),
    marker = list(
      colors = cols,
      line = list(color = "white", width = 2)
    )
  ) %>%
    layout(
      title = "Proporsi Kontribusi PDRB per Sektor",
      showlegend = TRUE,
      legend = list(orientation = "v", x = 1, y = 0.5)
    )
  
  list(plot = p, data = donut_data)
}