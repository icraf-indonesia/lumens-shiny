source('../../helper.R')

### Required Library ####
install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}

install_load(
  "bslib",
  "dplyr",
  "foreign",
  "ggplot2",
  "plotly",
  "purrr",
  "readr",
  "remote",
  "reshape",
  "reshape2",
  "rmarkdown",
  "sf",
  "shiny",
  "shinyFiles",
  "shinyvalidate",
  "splitstackshape",
  "terra", 
  "shinyjs",
  "shinyalert"
)

if (!("LUMENSR" %in% rownames(installed.packages()))) {
  install_github("icraf-indonesia/LUMENSR")
  do.call("library", list("LUMENSR"))
}
library(LUMENSR)

format_session_info_table <- function() {
  si <- sessionInfo()
  
  # Extract R version info
  r_version <- si$R.version[c("major", "minor", "year", "month", "day", "nickname")]
  r_version <- paste0(
    "R ", r_version$major, ".", r_version$minor,
    " (", r_version$year, "-", r_version$month, "-", r_version$day, ")",
    " '", r_version$nickname, "'"
  )
  
  # Extract platform and OS info
  platform_os <- paste(si$platform, "|", si$running)
  
  # Extract locale info, limit to first few locales if too many
  locale_info <- strsplit(si$locale, ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")
  
  # Extract .libPaths, limit the number of paths displayed
  lib_paths <- paste(.libPaths(), collapse = "<br>")
  
  # Combine all info into a single tibble
  session_summary <- tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  
  return(session_summary)
}

rename_uploaded_file <- function(input_file) {
  if (is.null(input_file)) return(NULL)
  
  old_path <- input_file$datapath
  new_path <- file.path(dirname(old_path), input_file$name)
  file.rename(old_path, new_path)
  return(new_path)
}

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

# Function to save raster files if they don't exist already
save_raster_if_needed <- function(input_file, save_path) {
  # Check if input_file is valid
  if (is.null(input_file) || !file.exists(input_file$datapath)) {
    stop("Error: Input file is missing or does not exist.")
  }
  
  # Check if save_path exists or create it
  if (!file.exists(save_path)) {
    message("Saving raster to: ", save_path)
    rast_data <- terra::rast(input_file$datapath)
    terra::writeRaster(rast_data, save_path, overwrite = TRUE)
  } else {
    message("Using existing raster: ", save_path)
  }
  
  # Return the raster object from the saved path
  return(terra::rast(save_path))
}

prepare_npv_lookup <- function(tbl_npv, quesc_tbl) {
  lookup_n<-tbl_npv
  colnames(lookup_n)[1] <- "ID_LC1"
  colnames(lookup_n)[2] <- "NPV1"
  dt_quesc_npv <- merge(quesc_tbl, lookup_n, by = "ID_LC1")
  colnames(lookup_n)[1] <- "ID_LC2"
  colnames(lookup_n)[2] <- "NPV2"
  dt_quesc_npv <- merge(dt_quesc_npv, lookup_n, by = "ID_LC2")
  
  tot_area <- sum(dt_quesc_npv$Ha)
  
  list(dt_quesc_npv = dt_quesc_npv, tot_area = tot_area)
}

build_opcost_table <- function(dt_quesc_npv, period, tot_area) {
  data_em_sel <- dt_quesc_npv[dt_quesc_npv$EM > 0, ]
  data_em_sel <- within(data_em_sel, {
    em_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (tot_area * period)
    em_tot <- (C_T1 - C_T2) * 3.67
    sq_rate <- ((C_T1 - C_T2) * (Ha * 3.67)) / (tot_area * period)
    sq_tot <- (C_T1 - C_T2) * 3.67
    opcost <- (NPV1 - NPV2) / em_tot
    opcost_sq <- (NPV1 - NPV2) / sq_tot
    cumsum_em <- cumsum(em_rate)
    cumsum_sq <- cumsum(sq_rate)
  })
  
  opcost_tab <- data.frame(
    luchg = data_em_sel$LU_CHG,
    zone = data_em_sel$PU,
    opcost = data_em_sel$opcost,
    emrate = data_em_sel$em_rate
  )
  
  #Build Positive Opcost Table
  opcost_tab_p<- opcost_tab[ which(opcost_tab$opcost >= 0),]
  opcost_tab_p<- opcost_tab_p[order(opcost_tab_p$opcost),]
  opcost_tab_p$cum_emrate<-cumsum(opcost_tab_p$emrate)
  opcost_tab_p$opcost_log<-log10(opcost_tab_p$opcost)
  is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
  opcost_tab_p[is.na(opcost_tab_p)] <- 0
  
  #Build Negative Opcost Table
  opcost_tab_n<- opcost_tab[ which(opcost_tab$opcost < 0),]
  opcost_tab_n<- opcost_tab_n[order(opcost_tab_n$opcost),]
  opcost_tab_n$cum_emrate<-cumsum(opcost_tab_n$emrate)
  opcost_tab_n$opcost_log<-opcost_tab_n$opcost*-1
  opcost_tab_n$opcost_log<-log10(opcost_tab_n$opcost_log)*-1
  
  opcost_all <- rbind(opcost_tab_n, opcost_tab_p)
  opcost_all$cum_emrate2 <- as.factor(opcost_all$cum_emrate)
  
  list(opcost_all = opcost_all)
}

carbon_accounting <- function(map1_rast, map2_rast, tbl_npv, tbl_carbon, raster_nodata) {
  # map1_rast <- rv$map1_rast
  # map2_rast <- rv$map2_rast
  # tbl_npv <- rv$tbl_npv
  # tbl_carbon <- rv$tbl_carbon
  
  NAflag(map1_rast) <- as.numeric(raster_nodata)
  NAflag(map2_rast) <- as.numeric(raster_nodata)
  
  names(tbl_carbon)[names(tbl_carbon) == "ID"] <- "ID_LC"
  merged_data <- merge(tbl_npv, tbl_carbon, by = "ID_LC")
  reclassify_matrix <- as.matrix(merged_data[, c("ID_LC", "Carbon")])
  
  map_carbon1 <- terra::classify(map1_rast, reclassify_matrix)
  map_carbon2 <- terra::classify(map2_rast, reclassify_matrix)
  
  chk_em <- map_carbon1 > map_carbon2
  emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em
  
  list(map_carbon1 = map_carbon1, map_carbon2 = map_carbon2, emission_map = emission_map)
}

npv_accounting <- function(map1_rast, map2_rast, tbl_npv) {
  npv_matrix <- as.matrix(tbl_npv[, c("ID_LC", "NPV")])
  
  map_npv1 <- terra::classify(map1_rast, npv_matrix)
  map_npv2 <- terra::classify(map2_rast, npv_matrix)
  
  npv_chg_map <- map_npv2 - map_npv1
  
  list(map_npv1 = map_npv1, map_npv2 = map_npv2, npv_chg_map = npv_chg_map)
}

calculate_opcost_map <- function(npv_chg_map, emission_map) {
  opcost_map <- npv_chg_map / emission_map
  opcost_map
}

generate_output_maps <- function(map_carbon1, map_carbon2, emission_map, opcost_map, wd) {
  writeRaster(map_carbon1, file.path(wd, "carbon_map_t1.tif"), overwrite = TRUE)
  writeRaster(map_carbon2, file.path(wd, "carbon_map_t2.tif"), overwrite = TRUE)
  writeRaster(emission_map, file.path(wd, "emission_map.tif"), overwrite = TRUE)
  writeRaster(opcost_map, file.path(wd, "opcost_map.tif"), overwrite = TRUE)
}

generate_opportunity_cost_curve <- function(opcost_table) {
  # Prepare data frame for the curve
  df_curve <- data.frame(
    emission = opcost_table$emrate,
    opportunity_cost = opcost_table$opcost,
    land_use_change = opcost_table$luchg
  )
  
  # Group data by land use change
  df_grouped <- df_curve %>%
    group_by(land_use_change) %>%
    summarise(emission = sum(emission),
              opportunity_cost = sum(opportunity_cost))
  
  # Filter and order data
  df_all <- df_grouped %>% filter(opportunity_cost != 0)
  df_order <- df_all[order(df_all$opportunity_cost),]
  df_order$order <- seq_len(nrow(df_order))
  
  # Create the Opportunity Cost Curve
  opcost_curve <- ggplot(df_order, aes(x = order, y = opportunity_cost)) +
    labs(x = NULL,
         y = "Opportunity Cost ($/ton CO2-eq)") +
    theme_classic() %+replace%
    theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_text(face = "bold", angle = 90)) +
    coord_cartesian(ylim = c(-5000, 5000)) +
    geom_bar(stat = "identity", width = 0.7, position = position_dodge(width = 0.4))
  
  return(opcost_curve)
}