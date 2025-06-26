source('../../helper.R')
### REQUIRED LIBRARY ######################

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
  "shiny",
  "devtools",
  "bslib",
  "bsicons",
  "htmltools",
  "reactable",
  "plotly",
  "leaflet",
  "stars",
  "mapview",
  "excelR",
  "RColorBrewer",
  "jsonlite",
  "dplyr",
  "reshape2",
  "leafem",
  "yaml",
  "paletteer",
  "rmarkdown",
  "shinyjs",
  "shinyvalidate",
  "shinyFiles",
  "shinyalert",
  "DT"
)

if (!("abacuslib" %in% rownames(installed.packages()))) {
  install_github("degi/abacuslib")
  do.call("library", list("abacuslib"))
}
library(abacuslib)

### TABLE DEFINITION #################################
table_file_df <- data.frame(
  var = c(
    "lc_list_df",
    "cstock_list",
    "zone_list_df",
    "other_emission_df"
  ),
  file = c(
    "landcover.csv",
    "carbonstock.csv",
    "zonation.csv",
    "other_emission.csv"
  ),
  label = c(
    "Land cover legend",
    "Carbon stock data",
    "Zonation legend",
    "Other emission data"
  )
)

scenario_file_df <- data.frame(
  var = c(
    "tpm",
    "baseline_tpm",
    "baseline_area",
    "landcover",
    "new_lc_id"
  ),
  file = c(
    "scenario_modif",
    "scenario_tpm",
    "scenario_area",
    "scenario_lc",
    "scenario_newlc"
  ),
  label = c(
    "Scenario modified projection rate",
    "Default projection rate",
    "Default land cover area",
    "Land cover list",
    "Added land cover"
  )
)


map_file_df <- data.frame(
  var = c("map1_stars", "map2_stars", "mapz_stars"),
  file = c("map1.tif", "map2.tif", "map_zone.tif"),
  label = c("Time series map #1", "Time series map #2", "Zonation map")
)

output_table_file_df <- data.frame(
  var = c(
    "abacus_baseline.projection.lc_area",
    "abacus_baseline.emission.lc_emission"
  ),
  file = c(
    "baseline_projection.csv",
    "baseline_emission_projection.csv"
  ),
  label = c(
    "Baseline land cover change projection",
    "Baseline emission projection"
  )
)

output_scenario_file_df <- data.frame(
  var = c(
    "abacus_scenario.projection.lc_area",
    "abacus_scenario.emission.lc_emission"
  ),
  file = c(
    "scenario_projection",
    "scenario_emission_projection"
  ),
  label = c(
    "Scenario land cover change projection",
    "Scenario emission projection"
  )
)

output_map_file_df <- data.frame(
  var = c("mapc_stars", "map_other_stars", "map_all_stars"),
  file = c(
    "map_c_emission.tif",
    "map_other_emission.tif",
    "map_all_emission.tif"
  ),
  label = c(
    "Map of carbon emission factor",
    "Map of additional emission factor",
    "Map of all combined emission factor"
  )
)

json_file_df <- data.frame(
  var = c("abacus_baseline", "abacus_scenario"),
  file = c("baseline_projection.json", "scenario_projection.json"),
  label = c("Baseline data", "Scenario data")
)

list_to_df <- function(x) {
  df <- dcast(melt(x), L1 ~ L2)
  df$L1 = NULL
  return(df)
}


### abacuslib handler
jslist_to_df <- function(x, session, inputname) {
  return(fromJSON(x))
}
removeInputHandler("js_to_df")
registerInputHandler("js_to_df", jslist_to_df)

### map
map_factor_to_numeric <- function(map) {
  v <- unlist(map[[1]])
  nr <- nrow(map[[1]])
  map[[1]] <- matrix(as.numeric(levels(v))[v], nr)
  return(map)
}

reclassify_map <- function(map, fromto_df) {
  fromto_df <- fromto_df[order(fromto_df[, 1]), ]
  from <- fromto_df[[1]]
  to <- fromto_df[[2]]
  map2 <- cut(map, c(min(from) - 1, from), labels = to)
  map2 <- map_factor_to_numeric(map2)
  return(map2)
}

co2_unit <- function(prefix = "", suffix = "") {
  tags$html(
    paste0(prefix, "CO"),
    tags$sub(2, .noWS = c("after", "before")),
    paste0("e", suffix),
    .noWS = c("after", "before")
  )
}

per_ha_unit <- function(prefix = "", suffix = "") {
  span(
    paste0(prefix, "ha"),
            tags$sup(-1, .noWS = c("after", "before")),
            suffix,
            .noWS = c("after", "before"))
}

quesc_transform <- function(quescdb) {
  df_lucdb_melt <- quescdb %>% 
    melt(
      id.vars = c('ID_LC2','ID_PU'), 
      measure.vars=c('Ha')
    )
  
  df_ha_pu_t2 <- df_lucdb_melt %>% dcast(formula = ID_LC2 + ID_PU ~ ., fun.aggregate = sum)
  names(df_ha_pu_t2)[3]<-"Ha.LC.PU.T2"
  
  df_lucdb_melt <- quescdb %>% 
    melt(
      id.vars = c('ID_LC1','ID_PU'), 
      measure.vars=c('Ha')
    )
  df_ha_pu_t1 <- df_lucdb_melt %>% dcast(formula = ID_LC1 + ID_PU ~ ., fun.aggregate = sum)
  names(df_ha_pu_t1)[3]<-"Ha.LC.PU.T1"
  
  df_lucdb_02 <- quescdb %>% 
    left_join(df_ha_pu_t1, by = c("ID_LC1", "ID_PU")) 
  
  df_lucdb_02 <- df_lucdb_02 %>% 
    merge(df_ha_pu_t2, 
          by.x=c("ID_LC1", "ID_PU"), 
          by.y=c("ID_LC2", "ID_PU"))
  
  # calculating the first iteration of TPM
  df_lucdb_02 <- df_lucdb_02 %>% mutate(
    TPM1 = Ha / Ha.LC.PU.T1
  )
  df_lucdb_02 <- df_lucdb_02 %>% replace(is.na(df_lucdb_02), 0)
  
  # Handling new emerging land use type in TPM
  # Get the total of TPM1 according to first land-cover period and zone
  df_temp <- df_lucdb_02 %>% 
    melt(
      id.vars = c('ID_LC1','ID_PU'),
      measure.vars = c('TPM1')
    ) %>% 
    dcast(
      formula = ID_LC1 + ID_PU ~ .,
      fun.aggregate = sum
    )
  
  # Check if the TPM value is...
  # equal to zero, then rename the column with 'fix'
  # it means the specific record-with-zero have to be revalued
  # more than zero, then rename the column with 'ignore'
  # it means the specific record must keep the value
  names(df_temp)[3] <- "CEK"
  df_temp <- df_temp %>% 
    mutate(ACT = case_when(
      CEK > 0 ~ "ignore",
      .default = "fix"
    ))
  df_temp$CEK <- NULL
  
  # merge df_lucdb_02 with data-bound table
  df_lutm <- merge(df_lucdb_02, df_temp, by = c("ID_LC1", "ID_PU"))
  df_lutm <- df_lutm %>% 
    mutate(TPM1 = case_when(
      ACT == "fix" & ID_LC1 == ID_LC2 ~ 1,
      .default = TPM1
    ))
  
  return(df_lutm)
}

generate_car_file <- function(df) {
  options(scipen=999)
  temp_car <- tempfile()
  t2 <- unlist(strsplit(names(df)[5], split = "X"))[2]
  t1 <- unlist(strsplit(names(df)[7], split = "X"))[2]
  
  general <- paste("file_version=1.2.0")
  write("#GENERAL", temp_car, append = TRUE, sep = "\t")
  write.table(general, temp_car, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE, sep = "\t")

  project <- c(
    "title=SCIENDO Abacus",
    "description=LUMENS project",
    paste0("baseyear0=", t1),
    paste0("baseyear1=", t2),
    paste0("n_iteration=", as.numeric(t2) - as.numeric(t1))
  )
  write("\n#PROJECT", temp_car, append = TRUE, sep = "\t")
  write.table(project, temp_car, append = TRUE, quote = FALSE, col.names = FALSE, row.names = FALSE,sep="\t")
  
  write("\n#LANDCOVER", temp_car, append=TRUE, sep="\t")
  lc <- df %>% melt(id.vars = c('ID_LC1', paste0('X', t1))) %>% 
    dplyr::select(-c(variable, value)) %>%
    distinct() %>%
    arrange(ID_LC1) %>% 
    dplyr::rename('//lc_id' = ID_LC1, label = paste0('X', t1)) %>% 
    mutate(description = "")
  write.table(lc, temp_car, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
  
  write("\n#ZONE", temp_car, append=TRUE, sep="\t")
  z <- df %>% 
    melt(id.vars=c('ID_PU', 'PU'), measure.vars=c('Ha')) %>%
    dcast(formula = ID_PU + PU ~ variable, fun.aggregate = sum ) %>% 
    dplyr::select(-Ha) %>%
    arrange(ID_PU) %>% 
    dplyr::rename('//zone_id' = ID_PU, label = PU) %>% 
    mutate(description = "")
  write.table(z, temp_car, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
  
  write("\n#LANDCOVER_CHANGE", temp_car, append=TRUE, sep="\t")
  lcc <- df %>% 
    mutate('//scenario_id' = 0, iteration_id = 0) %>%
    dplyr::select(c('//scenario_id', iteration_id, ID_PU, ID_LC1, ID_LC2, Ha)) %>%
    dplyr::rename(zone_id = ID_PU, lc1_id = ID_LC1, lc2_id = ID_LC2, area = Ha) %>%
    filter(area != 0)
  write.table(lcc, temp_car, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
  
  write("\n#CARBONSTOCK", temp_car, append=TRUE, sep="\t")
  carbon <- df %>%
    melt(id.vars = c('ID_LC1', 'ID_PU', 'C_T1')) %>% 
    dplyr::select(-c(variable, value)) %>%
    distinct(ID_LC1, ID_PU, C_T1) %>% 
    mutate('//scenario_id' = 0, iteration_id = 0) %>%
    dplyr::select(c('//scenario_id', iteration_id, ID_PU, ID_LC1, C_T1)) %>%
    dplyr::rename(zone_id = ID_PU, lc_id = ID_LC1, area = C_T1) 
  write.table(carbon, temp_car, append = TRUE, quote = FALSE, col.names = TRUE, row.names = FALSE, sep = "\t")
  
  write("\n#SCENARIO", temp_car, append=TRUE, sep="\t")
  return(temp_car)
}

generate_sciendo_scen_report <- function(output, dir) {
  report_params <- list(
    start_time = output$start_time,
    end_time = output$end_time,
    inputs = output$inputs
  )
  
  time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  output_file <- paste0("scen_builder_report_", time, ".html")
  
  rmarkdown::render(
    "../report_template/scen_builder_report_template.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
  
  return(output_file)
}

#' Prepare Transition Probability Matrix (TPM) data for display
#'
#' Processes raw TPM data by filtering, calculating differences, and formatting 
#' columns for display in interactive tables. The function prepares two key 
#' formatted columns: projected area (with percentage) and changes (with colored formatting).
#'
#' @param tpm_tbl A data frame containing transition probability matrix data with
#'   the following expected columns:
#'   \itemize{
#'     \item \code{area}: Projected area (numeric)
#'     \item \code{r}: Transition rate (numeric)
#'     \item \code{def_area}: Default/reference area (numeric)
#'     \item \code{def_r}: Default/reference transition rate (numeric)
#'     \item \code{zone}: Zone/region identifier
#'     \item \code{period}: Time period
#'     \item \code{lc1}: Original land cover class
#'     \item \code{lc2}: Converted land cover class
#'   }
#'
#' @return A data frame with the following columns:
#'   \itemize{
#'     \item \code{zone}: Zone/region identifier (grouping variable)
#'     \item \code{period}: Time period (grouping variable)
#'     \item \code{lc1}: Original land cover class
#'     \item \code{lc2}: Converted land cover class
#'     \item \code{projected_area}: Formatted string "X,XXX ha [YY%]"
#'     \item \code{changes}: Formatted string with difference from reference values
#'       ("±X,XXX ha [±YY%]") ready for color-coded display
#'   }
#'
#' @details The function:
#' \enumerate{
#'   \item Filters to include only rows with area > 0 (or alternatively lock == TRUE)
#'   \item Calculates differences between projected and default values
#'   \item Formats numeric values with proper rounding and thousand separators
#'   \item Creates display-ready strings with percentage values
#'   \item Prepares change values with +/- signs for color coding
#' }
#'
#' @examples
#' \dontrun{
#' # Sample usage:
#' tpm_data <- data.frame(
#'   area = c(5492.2, 2353.8, 1095.9),
#'   r = c(0.7, 0.3, 1),
#'   def_area = c(7846, 0, 141.7),
#'   def_r = c(1, 0, 0.1293),
#'   zone = c("Zone_A", "Zone_A", "Zone_B"),
#'   period = c("2000-2010", "2000-2010", "2000-2010"),
#'   lc1 = c("Forest", "Forest", "Wetland"),
#'   lc2 = c("Forest", "Shrub", "Wetland")
#' )
#' 
#' prepared_data <- tpm_tbl_prep(tpm_data)
#' DT::datatable(prepared_data)
#' }
#' 
#' @export
tpm_tbl_prep <- function(tpm_tbl) {
  tpm_tbl %>%
    filter(area > 0) %>%  # only include rows with value > 0, other option can be lock == TRUE
    mutate(
      across(c(area, r, def_area, def_r), as.numeric),
      dif_area = area - def_area,
      dif_r = r - def_r,
      
      # calculate percentages
      r_pct = round(r * 100, 0),
      def_r_pct = round(def_r * 100, 0),
      dif_r_pct = r_pct - def_r_pct,
      
      # set format for projected area column
      projected_area = paste0(
        format(round(area, 1), big.mark = ","), 
        " ha [", 
        r_pct, 
        "%]"
      ),
      
      # set format for changes column
      changes = ifelse(
        dif_area == 0,
        "0",
        paste0(
          ifelse(dif_area < 0, "-", "+"),
          format(abs(round(dif_area, 1)), big.mark = ","),
          " ha [",
          ifelse(dif_r_pct < 0, "-", "+"),
          abs(dif_r_pct),
          "%]"
        )
      )
    ) %>%
    select(zone, period, lc1, lc2, projected_area, changes)
}

#' Create Interactive Transition Probability Matrix Table
#' 
#' Generates an interactive DT table displaying land cover transition data with
#' grouped rows by zone and period, and color-coded change values.
#'
#' @param tpm_tbl_ready A data frame prepared by `tpm_tbl_prep()` containing:
#'   \itemize{
#'     \item zone (hidden grouping column)
#'     \item period (hidden grouping column) 
#'     \item lc1 (Original Land Cover)
#'     \item lc2 (Converted To)
#'     \item projected_area (Projected Area)
#'     \item changes (Changes)
#'   }
#'
#' @return An interactive DT datatable object with:
#'   \itemize{
#'     \item Rows grouped by zone and period
#'     \item Zone and period columns hidden
#'     \item Changes column color-coded (green=positive, red=negative)
#'     \item Clean column headers
#'   }
#' 
#' @examples
#' \dontrun{
#' prepared_data <- tpm_tbl_prep(raw_data)
#' tpm_table_viz(prepared_data)
#' }
#' 
#' @seealso \code{\link{tpm_tbl_prep}} for data preparation
#' @export
tpm_table_viz <- function(tpm_tbl_ready) {
  DT::datatable(
    tpm_tbl_ready,
    colnames = c('Zone', 'Time Period', 'Original Land Cover',
                 'Converted To', 'Projected Area', 'Changes'),
    rownames = FALSE,
    extensions = c('RowGroup'),
    options = list(
      rowGroup = list(dataSrc = c(0, 1)), # Group by zone and period
      dom = 'Bfrtip',
      pageLength = 10,
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 1)), # Hide zone and period columns
        list(
          targets = 5, # Apply coloring to Changes column
          render = DT::JS(
            "function(data, type, row) {
              if (data.startsWith('+')) {
                return '<span style=\"color:green;font-weight:bold\">' + data + '</span>';
              } else if (data.startsWith('-') || (data.length > 0 && !data.startsWith('0'))) {
                return '<span style=\"color:red;font-weight:bold\">' + data + '</span>';
              } else {
                return data;
              }
            }"
          )
        )
      )
    )
  )
}