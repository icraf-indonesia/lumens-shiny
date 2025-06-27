#' Check if a string is numeric
#'
#' This function checks if a given string can be converted to an integer.
#'
#' @param s A character string.
#' @return A boolean value, TRUE if the string can be converted to an integer, FALSE otherwise.
#' @examples
#' is_numeric_str("123") # returns TRUE
#' is_numeric_str("abc") # returns FALSE
is_numeric_str <- function(s) {
  return(!is.na(as.integer(as.character(s))))
}

#' English text for summary table
summary_text_en <- c("Period",
                     "Total area (ha)",
                     "Total emission (tonne CO2-eq)",
                     "Total sequestration (tonne CO2-eq)",
                     "Net emission (tonne CO2-eq)",
                     "Emission rate (tonne CO2-eq/ha.year)",
                     "Emission rate per-unit area (tonne CO2-eq/ha.year)")

#' Indonesian text for summary table
summary_text_id <- c("Periode", 
                     "Total area (ha)", 
                     "Total Emisi (Ton CO2-eq)", 
                     "Total Sekuestrasi (Ton CO2-eq)", 
                     "Emisi Bersih (Ton CO2-eq)", 
                     "Laju Emisi (Ton CO2-eq/tahun)",
                     "Laju emisi per-unit area (Ton CO2-eq/ha.tahun)")

#' English text for zonal summary table
summary_zonal_text_en <- list(ID = 1,
                              "Planning Unit" = 2, 
                              "Area (Ha)" = 3, 
                              "Carbon Avg. (Periode 1)" = 4, 
                              "Carbon Avg. (Periode 2)" = 5, 
                              "Net Emission" = 6, 
                              "Emission Rate" = 7
)

#' Indonesian text for zonal summary table
summary_zonal_text_id <- list(
  ID = 1,
  "Unit Perencanaan" = 2,
  "Luas (Ha)" = 3,
  "Rerata Karbon Periode 1" = 4,
  "Rerata Karbon Periode 2" = 5,
  "Emisi bersih" = 6,
  "Laju emisi" = 7
)
#' English text for zonal carbon summary table
summary_zona_carbon_text_en <- list(
  ID = 1,
  "Planning Unit" = 2,
  "Area (Ha)" = 3,
  "Total emission (tonne CO2-eq)" = 4,
  "Total sequestration (tonne CO2-eq)" = 5,
  "Net Emission (tonne CO2-eq)" = 6,
  "Emission Rate (tonne CO2-eq/ha.year)" = 7
)
#' Indonesian text for zonal carbon summary table
summary_zona_carbon_text_id <- list(
  ID = 1,
  "Unit perencanaan" = 2,
  "Luas (Ha)" = 3,
  "Total emisi (ton CO2-eq)" = 4,
  "Total sekuestrasi (ton CO2-eq)" = 5,
  "Emisi bersih (ton CO2-eq)" = 6,
  "Laju emisi (ton CO2-eq)" = 7
)

#' Format Session Information
#'
#' This function captures and formats the current R session information into a tibble.
#'
#' @return A tibble with session information, including R version, platform, OS, .libPaths, and locale.
#' @importFrom tibble tibble
#' @export
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
  
  # Extract locale info
  locale_info <- strsplit(si[[3]], ";")[[1]]
  locale_info <- paste(locale_info, collapse = "<br>")
  
  # Extract .libpaths, accomodate multiple library paths
  lib_paths <- .libPaths() |> paste(collapse = "<br>")
  
  # Combine all info into a single tibble
  session_summary <- tibble::tibble(
    Category = c("R Version", "Platform | OS", ".libPaths", "Locale"),
    Details = c(r_version, platform_os, lib_paths, locale_info)
  )
  return(session_summary)
}

#' Rasterize an sf MULTIPOLYGON object
#'
#' This function rasterizes an sf MULTIPOLYGON object to a SpatRaster object. The function also retains
#' an attribute table from the sf object, by assigning categorical ID values to the raster values.
#' The rasterized SpatRaster object will also contain a legend derived from the attribute table of the sf object.
#'
#' @param sf_object An sf MULTIPOLYGON object. It must contain an attribute table, with at least one categorical ID (numeric).
#' @param raster_res A numeric vector specifying the resolution of the raster. Default is c(100,100).
#' @param field A character string specifying the field name to be used for rasterization from the sf object. Default is "ID".
#' @return A SpatRaster object that is a rasterized version of the input sf object, with a legend derived from the attribute table of the sf object.
#' @importFrom sf st_drop_geometry st_geometry_type st_crs
#' @importFrom terra vect ext rast rasterize levels
#' @export
#' @examples
#' \dontrun{
#' rasterise_multipolygon_quesc(sf_object = ntt_admin, raster_res = c(100, 100), field = "ID")
#' }
rasterise_multipolygon_quesc <- function(sf_object, raster_res, field = "ID") {
  # Error checking
  if (!inherits(sf_object, "sf")) stop("sf_object must be an sf object.")
  if (!all(sf::st_geometry_type(sf_object) == "MULTIPOLYGON")) stop("All features in sf_object must be MULTIPOLYGONs.") # Check if sf_object has UTM projection
  if (!grepl("\\+units=m", sf::st_crs(sf_object)$proj4string)) stop("sf_object must have UTM projection system.")
  if (is.null(sf::st_drop_geometry(sf_object)) || !(field %in% names(sf::st_drop_geometry(sf_object)))) stop("sf_object must contain an attribute table with at least one numeric/factor column.")
  if (!is.numeric(sf_object[[field]]) && !is.factor(sf_object[[field]])) stop("The field must be numeric or a factor.")
  
  # Convert the sf object to a SpatVector
  spatvect <- terra::vect(sf_object)
  
  # Define the extent based on the SpatVector
  raster_extent <- terra::ext(spatvect)
  
  # Create an empty SpatRaster based on the extent, resolution, and CRS
  raster_template <- terra::rast(raster_extent, resolution = raster_res, crs = terra::crs(spatvect))
  
  # Rasterize the SpatVector based on the SpatRaster template
  # Specify the field in the rasterize function
  rasterised_spatraster <- terra::rasterize(spatvect, raster_template, field = field)
  
  # Convert the 'Kabupaten' column of the sf_object to a lookup_table
  lookup_table <- sf::st_drop_geometry(sf_object)
  
  # Add legend to the rasterized SpatRaster using the lookup_table
  terra::levels(rasterised_spatraster) <- lookup_table
  
  # Return the rasterized SpatRaster with legend
  return(rasterised_spatraster)
}

print_area <- function(x) {
  format(x, digits = 15, big.mark = ",")
}
#' Format Rate Values
#'
#' Formats a numeric value representing a rate with a big mark separator for thousands and two decimal places.
#'
#' @param x A numeric value.
#' @return A formatted character string.
#' @export
print_rate <- function(x) {
  format(x, digits = 15, nsmall = 2, decimal.mark = ".", big.mark = ",")
}


#' Plot QUES-C Results
#'
#' This function creates a ggplot for a SpatRaster object with a gradient fill.
#'
#' @param map A SpatRaster object to plot.
#' @param legend A character string for the legend title.
#' @param low A character string for the low end of the color gradient.
#' @param high A character string for the high end of the color gradient.
#' @param na_color A character string for the color of NA values. Default is "white".
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot theme_bw labs theme scale_fill_gradient element_text unit element_blank
#' @importFrom tidyterra geom_spatraster
#' @export
plot_quesc_results <- function(map, legend, low, high, na_color = "white") {
  # Determine plot title
  # plot_title <- if (!is.na(time(map))) time(map) else names(map)
  
  # Generate the plot
  plot_lc <- ggplot() +
    tidyterra::geom_spatraster(data = map) +
    ggplot2::scale_fill_gradient(
      low = low,
      high = high,
      na.value = na_color,
      name = if (!is.null(legend)) legend else NULL
    ) +
    ggplot2::theme_bw() +
    # labs(title = plot_title) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(1, "cm"),
      legend.key.width = ggplot2::unit(0.25, "cm"), 
      legend.position = "right",
      legend.justification = c(0, 0.5)
    )
  
  return(plot_lc)
}

#' Summarize Emission Calculation
#'
#' This function summarizes the emission and sequestration calculations from the QUES-C database.
#' It calculates total area, total emission, total sequestration, net emission, and emission rates
#' per planning unit and for the entire study area. It also generates an interactive bar plot
#' of average net emission rates per planning unit.
#'
#' @param quescdb A data frame containing the QUES-C database. Expected columns include `ID_PU`,
#'   `PU`, `Ha`, `EM` (emission), and `SQ` (sequestration).
#' @param period A list or data frame with `p1` and `p2` specifying the start and end year of the analysis period.
#' @return A list containing:
#'   \itemize{
#'     \item \code{area_zone}: Data frame with summarized area by planning unit.
#'     \item \code{zone_emission}: Data frame with total emission per planning unit.
#'     \item \code{zone_sequestration}: Data frame with total sequestration per planning unit.
#'     \item \code{zone_carbon}: Data frame with combined carbon metrics per planning unit, formatted.
#'     \item \code{plot_zone_carbon}: An interactive plotly bar plot of average net emission rates.
#'     \item \code{total_area}: Numeric, total area of the study region.
#'     \item \code{total_emission}: Numeric, total emission of the study region.
#'     \item \code{total_sequestration}: Numeric, total sequestration of the study region.
#'     \item \code{total_net_emission}: Numeric, total net emission of the study region.
#'     \item \code{total_rate_emission}: Numeric, total emission rate of the study region.
#'     \item \code{total_rate_emission_ha}: Numeric, total emission rate per hectare of the study region.
#'     \item \code{summary_df}: Data frame summarizing overall study region metrics.
#'   }
#' @importFrom dplyr %>% group_by summarise rename left_join mutate mutate_if
#' @importFrom stringr str_wrap
#' @importFrom plotly plot_ly layout
#' @export
summary_of_emission_calculation <- function(quescdb, period) {
  # Calculate the analysis period in years
  p <- as.numeric(period$p2) - as.numeric(period$p1)
  # Summarize area by planning unit
  az <- quescdb %>% 
    group_by(ID_PU, PU) %>% 
    summarise(Ha = sum(Ha, na.rm = TRUE), .groups = "drop") %>% 
    rename(ID = ID_PU)
  
  # Calculate total emission per planning unit from quescdb
  ze <- quescdb %>% 
    group_by(ID_PU, PU) %>% 
    summarise(TOTAL_EM = sum(EM, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::rename(ID = ID_PU)
  
  # Calculate total sequestration per planning unit from quescdb
  zs <- quescdb %>% 
    group_by(ID_PU, PU) %>% 
    summarise(TOTAL_SQ = sum(SQ, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::rename(ID = ID_PU)
  
  # Combine and calculate carbon metrics
  zc <- az %>% 
    left_join(ze, by = c("ID", "PU")) %>% 
    left_join(zs, by = c("ID", "PU")) %>% 
    mutate(
      NET_EM = TOTAL_EM - TOTAL_SQ,
      NET_EM_RATE = round(NET_EM / Ha / p, 2),
      TOTAL_EM = round(TOTAL_EM, 2),
      TOTAL_SQ = round(TOTAL_SQ, 2),
      NET_EM = round(NET_EM, 2)
    )
  
  # Prepare data for plotting
  zc_plot_data <- zc %>% 
    mutate(PU_wrapped = str_wrap(PU, width = 10))
  
  # Create interactive plot
  # Prepare data for plotting (moved outside the function for separation of concerns)
  zc_plot <- plotly::plot_ly(
    data = zc,
    x = ~reorder(stringr::str_wrap(PU, width = 40), -NET_EM_RATE),
    y = ~NET_EM_RATE,
    type = "bar",
    text = ~round(NET_EM_RATE, 1),
    hoverinfo = "text",
    hovertext = ~paste(
      "Planning Unit:", PU, "<br>", 
      "Average Net Emission Rate:", round(NET_EM_RATE, 1), "tonne CO<sub>2</sub>-eq/ha.yr" 
    ),
    marker = list(
      color = ~NET_EM_RATE,  
      colorscale = "Oranges",  
      reversescale = TRUE,
      showscale = FALSE, 
      colorbar = list(
        title = "Emission Rate",
        tickformat = ".1f"
      )
    )
  ) %>%
    plotly::layout(
      title = paste("Average of net emission rate", period$p1, "-", period$p2),
      xaxis = list(
        title = "",
        categoryorder = "total descending",
        tickangle = -270
      ),
      yaxis = list(
        title = "tonne CO<sub>2</sub>-eq/ha.yr",
        tickformat = ".1f"
      ),
      margin = list(b = 150),
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black"),
        align = "left" 
      ),
      showlegend = FALSE  
    )
  
  # Calculate total values
  total_area <- sum(az$Ha, na.rm = TRUE)
  total_emission <- sum(zc$TOTAL_EM, na.rm = TRUE)
  total_sequestration <- sum(zc$TOTAL_SQ, na.rm = TRUE)
  total_net_emission <- total_emission - total_sequestration
  total_rate_emission <- total_net_emission / p
  total_rate_emission_ha <- total_rate_emission / total_area
  
  # Format final table
  zc_final <- zc %>% 
    mutate(Ha = format(round(Ha, 2), nsmall = 2, big.mark = ",", decimal.mark = ".")) %>% 
    mutate_if(is.numeric, print_rate)
  
  names(zc_final)[1:length(summary_zona_carbon_text_en)] <- names(summary_zona_carbon_text_en)
  
  # Create summary data frame
  summary_df <- data.frame(
    ID = 1:7,
    Category = summary_text_en,
    Summary = as.character(
      c(
        paste0(period$p1, "-", period$p2),
        print_area(round(total_area, 2)),
        print_rate(round(total_emission, 2)),
        print_rate(round(total_sequestration, 2)),
        print_rate(round(total_net_emission, 2)),
        print_rate(round(total_rate_emission, 2)),
        print_rate(round(total_rate_emission_ha, 2))
      )
    )
  )
  
  # Consolidate outputs
  out <- list(
    area_zone = az,
    zone_emission = ze,
    zone_sequestration = zs,
    zone_carbon = zc_final,
    plot_zone_carbon = zc_plot,
    total_area = total_area,
    total_emission = total_emission,
    total_sequestration = total_sequestration,
    total_net_emission = total_net_emission,
    total_rate_emission = total_rate_emission,
    total_rate_emission_ha = total_rate_emission_ha,
    summary_df = summary_df
  )
  
  return(out)
}


#' Calculate Zonal Statistics Database
#'
#' This function calculates various zonal statistics from the QUES-C database,
#' including emission and sequestration rates by planning unit and land use change.
#' It also generates interactive bar plots for top 10 GHG emissions and sequestration
#' by land cover/use change.
#'
#' @param quescdb A data frame containing the QUES-C database. Expected columns include `ID_PU`,
#'   `PU`, `Ha`, `C_T1`, `C_T2`, `EM` (emission), `SQ` (sequestration), and `LU_CHG`.
#' @param period An integer representing the number of years in the analysis period.
#' @return A list containing various data frames and plots summarizing zonal statistics:
#'   \itemize{
#'     \item \code{data_zone}: Data frame with average carbon and emission/sequestration rates per planning unit.
#'     \item \code{data_zone_df}: Formatted version of `data_zone`.
#'     \item \code{tb_em_total_10}: Data frame with top 10 total emissions by land use change.
#'     \item \code{tb_em_total_10_summary}: Formatted version of `tb_em_total_10`.
#'     \item \code{largest_emission}: An interactive plotly bar plot of top 10 GHG emissions.
#'     \item \code{tb_em_zonal}: Data frame with zonal emissions by land use change.
#'     \item \code{tb_sq_total_10}: Data frame with top 10 total sequestration by land use change.
#'     \item \code{tb_sq_total_10_summary}: Formatted version of `tb_sq_total_10`.
#'     \item \code{largest_sequestration}: An interactive plotly bar plot of top 10 GHG sequestration.
#'     \item \code{tb_sq_zonal}: Data frame with zonal sequestration by land use change.
#'   }
#' @importFrom reshape2 melt dcast
#' @importFrom dplyr %>% rename select mutate arrange relocate filter
#' @importFrom stringr str_wrap
#' @importFrom plotly plot_ly layout
#' @export
zonal_statistic_database <- function(quescdb, period) {
  area_zone <- quescdb %>%
    reshape2::melt(id.vars = c("ID_PU", "PU"), measure.vars = c("Ha")) %>%
    reshape2::dcast(formula = ID_PU + PU ~ ., fun.aggregate = sum) %>%
    dplyr::rename(
      ID = 1,
      Ha = 3
    )
  
  data_zone <- area_zone
  data_zone$Z_CODE <- toupper(abbreviate(data_zone$PU))
  data_zone$Rate_seq <- data_zone$Rate_em <- data_zone$Avg_C_t2 <- data_zone$Avg_C_t1 <- 0
  for (a in 1:nrow(area_zone)) {
    i <- area_zone$PU[a]
    data_z <- quescdb[which(quescdb$PU == i), ]
    data_zone <- within(data_zone, {
      Avg_C_t1 <- ifelse(data_zone$PU == i,
                         sum(data_z$C_T1 * data_z$Ha) / sum(data_z$Ha),
                         Avg_C_t1
      )
    })
    data_zone <- within(data_zone, {
      Avg_C_t2 <- ifelse(data_zone$PU == i,
                         sum(data_z$C_T2 * data_z$Ha) / sum(data_z$Ha),
                         Avg_C_t2
      )
    })
    data_zone <- within(data_zone, {
      Rate_em <- ifelse(data_zone$PU == i,
                        sum(data_z$EM) / (sum(data_z$Ha) * period),
                        Rate_em
      )
    })
    data_zone <- within(data_zone, {
      Rate_seq <- ifelse(data_zone$PU == i,
                         sum(data_z$SQ) / (sum(data_z$Ha) * period),
                         Rate_seq
      )
    })
  }
  
  data_zone_ori <- data_zone %>%
    dplyr::select(-Z_CODE) %>%
    mutate(
      Avg_C_t1 = round(Avg_C_t1, 2),
      Avg_C_t2 = round(Avg_C_t2, 2),
      Rate_em = round(Rate_em, 2),
      Rate_seq = round(Rate_seq, 2)
    )
  data_zone_summary <- data_zone_ori %>%
    mutate(Ha = format(round(Ha, 2), nsmall = 2, big.mark = ",", decimal.mark = ".")) %>%
    mutate_if(is.numeric, print_rate) %>%
    dplyr::rename(
      unlist(summary_zonal_text_en)
    )
  
  # data_merge_sel <- quescdb[ which((quescdb$EM + quescdb$SQ) > 0), ]
  order_sq <- quescdb[order(-quescdb$SQ), ] %>% as.data.frame()
  order_em <- quescdb[order(-quescdb$EM), ] %>% as.data.frame()
  
  # total emission
  tb_em_total <- order_em$LU_CHG %>%
    cbind(as.data.frame(round(order_em$EM, digits = 3))) %>%
    as.data.frame() %>%
    dplyr::rename(
      LU_CHG = 1,
      EM = 2
    ) %>%
    aggregate(EM ~ LU_CHG, FUN = sum) %>%
    mutate(
      LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength = 5, strict = FALSE, method = "both")))
    ) %>%
    dplyr::arrange(desc(EM)) %>%
    dplyr::relocate(LU_CODE)
  
  tb_em_total_10 <- tb_em_total %>%
    mutate(
      PERCENTAGE = as.numeric(format(round((EM / sum(tb_em_total$EM) * 100), 2), nsmall = 2))
    ) %>%
    head(n = 10)
  tb_em_total_10_summary <- tb_em_total_10 %>%
    mutate(EM = print_rate(EM)) %>%
    dplyr::rename(
      "Land Use Code" = LU_CODE,
      "Land Use Change" = LU_CHG,
      "Total Emission" = EM,
      "Percentage" = PERCENTAGE
    )
  
  # Create the largest emission plot
  largest_em_bar <- plotly::plot_ly(
    data = tb_em_total_10,
    x =  ~stringr::str_wrap(LU_CHG, width = 25),
    y = ~EM,
    type = "bar",
    text = "", 
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover/Use Change:", LU_CHG, "<br>", 
      "Emission:", format(EM, big.mark = ",", scientific = FALSE), " ton CO<sub>2</sub>-eq"
    ),
    marker = list(
      color = ~EM,
      colorscale = "Magma",
      showscale = FALSE
    )
  ) %>%
    plotly::layout(
      title = "Top 10 GHG Emissions by Land Cover/Use Change",
      xaxis = list(
        title = "",
        categoryorder = "total descending", 
        tickangle = -270 
      ),
      yaxis = list(
        title = "GHG Emission (ton CO<sub>2</sub>-eq)",
        tickformat = ",.0f"
      ),
      margin = list(b = 150),
      hoverlabel = list(
        bgcolor = "white", 
        font = list(color = "black")
      )
    )
  
  # zonal emission
  tb_em_zonal <- as.data.frame(NULL)
  for (i in 1:nrow(area_zone)) {
    tryCatch(
      {
        tb_em <- order_em$PU %>%
          cbind(order_em$LU_CHG, as.data.frame(round(order_em$EM, digits = 3))) %>%
          as.data.frame() %>%
          dplyr::rename(
            PU = 1,
            LU_CHG = 2,
            EM = 3
          )
        
        a <- area_zone$PU[i]
        tb_em_z <- tb_em %>%
          dplyr::filter(PU == a) %>%
          as.data.frame() %>%
          aggregate(EM ~ PU + LU_CHG, FUN = sum) %>%
          mutate(
            LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength = 5, strict = FALSE, method = "both")))
          ) %>%
          dplyr::arrange(desc(EM)) %>%
          dplyr::relocate(LU_CODE, .before = LU_CHG)
        tb_em_z_10 <- tb_em_z %>%
          mutate(
            PERCENTAGE = as.numeric(format(round((EM / sum(tb_em_z$EM) * 100), 2), nsmall = 2))
          ) %>%
          head(n = 10)
        tb_em_zonal <- tb_em_zonal %>% rbind(tb_em_z_10)
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
  
  # total sequestration
  tb_sq_total <- order_sq$LU_CHG %>%
    cbind(as.data.frame(round(order_sq$SQ, digits = 3))) %>%
    as.data.frame() %>%
    dplyr::rename(
      LU_CHG = 1,
      SQ = 2
    ) %>%
    aggregate(SQ ~ LU_CHG, FUN = sum) %>%
    mutate(
      LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength = 5, strict = FALSE, method = "both")))
    ) %>%
    dplyr::arrange(desc(SQ)) %>%
    dplyr::relocate(LU_CODE)
  
  tb_sq_total_10 <- tb_sq_total %>%
    mutate(
      PERCENTAGE = as.numeric(format(round((SQ / sum(tb_sq_total$SQ) * 100), 2), nsmall = 2))
    ) %>%
    head(n = 10)
  tb_sq_total_10_summary <- tb_sq_total_10 %>%
    mutate(SQ = print_rate(SQ)) %>%
    dplyr::rename(
      "Land Use Code" = LU_CODE,
      "Land Use Change" = LU_CHG,
      "Total Sequestration" = SQ,
      "Percentage" = PERCENTAGE
    )
  # Create the sequestration plot
  largest_sq_bar <- plotly::plot_ly(
    data = tb_sq_total_10,
    x =  ~stringr::str_wrap(LU_CHG, width = 25),
    y = ~SQ,
    type = "bar",
    text = "",
    hoverinfo = "text",
    hovertext = ~paste(
      "Land Cover/Use Change:", LU_CHG, "<br>", 
      "Sequestration:", format(SQ, big.mark = ",", scientific = FALSE), " ton CO<sub>2</sub>-eq"
    ),
    marker = list(
      color = ~SQ,
      reversescale = TRUE,
      colorscale = "Greens",
      showscale = FALSE
    )
  ) %>%
    plotly::layout(
      title = "Top 10 GHG Sequestration by Land Cover/Use Change",
      xaxis = list(
        title = "",
        categoryorder = "total descending",
        tickangle = -270 
      ),
      yaxis = list(
        title = "GHG Sequestration (ton CO<sub>2</sub>-eq)",
        tickformat = ",.0f" 
      ),
      margin = list(b = 150), 
      hoverlabel = list(
        bgcolor = "white",
        font = list(color = "black")
      )
    )
  
  # zonal sequestration
  tb_sq_zonal <- as.data.frame(NULL)
  for (i in 1:nrow(area_zone)) {
    tryCatch(
      {
        tb_sq <- order_sq$PU %>%
          cbind(order_sq$LU_CHG, as.data.frame(round(order_sq$SQ, digits = 3))) %>%
          as.data.frame() %>%
          dplyr::rename(
            PU = 1,
            LU_CHG = 2,
            SQ = 3
          )
        
        a <- area_zone$PU[i]
        tb_sq_z <- tb_sq %>%
          dplyr::filter(PU == a) %>%
          as.data.frame() %>%
          aggregate(SQ ~ PU + LU_CHG, FUN = sum) %>%
          mutate(
            LU_CODE = as.factor(toupper(abbreviate(LU_CHG, minlength = 5, strict = FALSE, method = "both")))
          ) %>%
          dplyr::arrange(desc(SQ)) %>%
          dplyr::relocate(LU_CODE, .before = LU_CHG)
        tb_sq_z_10 <- tb_sq_z %>%
          mutate(
            PERCENTAGE = as.numeric(format(round((SQ / sum(tb_sq_z$SQ) * 100), 2), nsmall = 2))
          ) %>%
          head(n = 10)
        tb_sq_zonal <- tb_sq_zonal %>% rbind(tb_sq_z_10)
      },
      error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      }
    )
  }
  
  out <- list(
    data_zone = data_zone_ori,
    data_zone_df = data_zone_summary,
    tb_em_total_10 = tb_em_total_10,
    tb_em_total_10_summary = tb_em_total_10_summary,
    largest_emission = largest_em_bar,
    tb_em_zonal = tb_em_zonal,
    tb_sq_total_10 = tb_sq_total_10,
    tb_sq_total_10_summary = tb_sq_total_10_summary,
    largest_sequestration = largest_sq_bar,
    tb_sq_zonal = tb_sq_zonal
  )
}

### Required Library ####
#' Install and Load Required Libraries
#'
#' Checks if a list of required packages are installed, installs them if they are not, and then loads them.
#'
#' @param package1 A character string of the first package name.
#' @param ... Additional character strings of package names.
#'
#' @return None. This function is called for its side effects of installing and loading packages.
#' @export
#'
#' @examples
#' \dontrun{
#' install_load("dplyr", "ggplot2")
#' }
install_load <- function(package1, ...) {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages())) {
      do.call("library", list(package))
    } # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}


#' Check and Harmonise Geometry of a SpatRaster to a Reference
#'
#' This function checks if a raster map (SpatRaster) has the same geometry as a reference raster.
#' If not, it resamples the raster map to match the reference raster's geometry.
#'
#' @param raster_map SpatRaster. The raster map to be checked and harmonised.
#' @param reference_map SpatRaster. The reference raster map.
#'
#' @return A SpatRaster object with geometry identical to the reference_map.
#'
#' @importFrom terra compareGeom resample
#' @export
check_and_harmonise_geometry <- function(raster_map, reference_map) {
  if (!terra::compareGeom(raster_map, reference_map, stopOnError = FALSE)) {
    warning("Geometry mismatch detected. Resampling raster_map to match reference_map.")
    raster_map <- terra::resample(raster_map, reference_map, method = "near")
    message("Harmonization complete: raster_map now matches reference_map geometry.")
  } else {
    message("Geometries are already consistent.")
  }
  return(raster_map)
}


#' Generate Dummy Cross-tabulate
#'
#' Cross-tabulate two data.frame objects to create a contingency table.
#'
#' @param landcover List. Land cover lookup table input.
#' @param zone Data frame or list. Zone lookup table input.
#'
#' @importFrom splitstackshape expandRows
#' @return A table or data.frame
#' @export
#' Generate Dummy Cross-tabulate
#'
#' Cross-tabulate two data.frame objects to create a contingency table of all possible combinations.
#'
#' @param landcover A data frame for the land cover lookup table. The first column should be the ID.
#' @param zone A data frame for the zone lookup table. The first column should be the ID.
#'
#' @return A tibble with all possible combinations of land cover and zone IDs.
#' @importFrom splitstackshape expandRows
#' @importFrom tibble tibble
#' @export
generate_dummy_crosstab <- function(landcover, zone) {
  if (!is.data.frame(landcover)) {
    stop("Land cover is not a data frame")
  }
  
  if (!is.data.frame(zone)) {
    stop("Zone is not a data frame")
  }
  
  n_lc <- nrow(landcover)
  n_pu <- nrow(zone)
  
  dummy1 <- data.frame(nPU = zone[[1]], divider = n_lc * n_lc)
  dummy1 <- splitstackshape::expandRows(dummy1, "divider")
  
  dummy2 <- data.frame(nT1 = landcover[[1]], divider = n_lc)
  dummy2 <- splitstackshape::expandRows(dummy2, "divider")
  dummy2 <- data.frame(nT1 = rep(dummy2$nT1, n_pu))
  
  dummy3 <- data.frame(nT2 = rep(rep(landcover[[1]], n_lc), n_pu))
  
  lucDummy <- cbind(dummy1, dummy2, dummy3)
  colnames(lucDummy) <- c("ID_PU", "ID_LC1", "ID_LC2")
  return(tibble::tibble(lucDummy))
}

#' Plot a categorical raster map
#'
#' This function takes a raster object as input and produces a ggplot. If the raster
#' object includes a "color_pallete" column with hex color codes, these colors are
#' used for the fill scale. Otherwise, the default `scale_fill_hypso_d()` fill scale
#' from the tidyterra package is used.
#'
#' @param raster_object A raster object.
#'
#' @return A ggplot object.
#' @importFrom terra cats
#' @importFrom ggplot2 ggplot theme_bw labs theme scale_fill_manual element_text unit element_blank guides guide_legend
#' @importFrom tidyterra geom_spatraster scale_fill_hypso_d
#' @export
#' Plot a categorical raster map
#'
#' This function takes a raster object as input and produces a ggplot. If the raster
#' object includes a "color_pallete" column with hex color codes, these colors are
#' used for the fill scale. Otherwise, a default color palette is used.
#'
#' @param raster_object A SpatRaster object with categorical data.
#'
#' @return A ggplot object.
#' @importFrom terra cats time
#' @importFrom ggplot2 ggplot theme_bw labs theme scale_fill_manual element_text unit element_blank guides guide_legend
#' @importFrom tidyterra geom_spatraster
#' @export
plot_categorical_raster <- function(raster_object) {
  # Check if raster_object has a color_pallete column and it contains hex color codes
  if ("color_palette" %in% names(terra::cats(raster_object)[[1]]) && all(grepl("^#[0-9A-Fa-f]{6}$", terra::cats(raster_object)$color_pallete))) {
    fill_scale <- ggplot2::scale_fill_manual(values = terra::cats(raster_object)[[1]]$color_palette, na.value = "white")
  } else {
    # fill_scale <- ggplot2::scale_fill_manual(values = c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F", "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F","#BAB0AC"), na.value = "white")
    fill_scale <- ggplot2::scale_fill_manual(values = c(
      "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
      "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
      "#86BCB6", "#FFB84D", "#A5C1DC", "#D37295", "#C4AD66",
      "#7B8D8E", "#B17B62", "#8CD17D", "#DE9D9C", "#5A5A5A",
      "#A0A0A0", "#D7B5A6", "#6D9EEB", "#E69F00", "#56B4E9",
      "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
      "#999999", "#E51E10", "#FF7F00", "#FFFF33", "#A65628",
      "#F781BF", "#999933", "#8DD3C7", "#FFFFB3", "#BEBADA",
      "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5",
      "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F", "#E41A1C"
    ), na.value = "white")
  }
  if (!is.na(terra::time(raster_object))) {
    plot_title <- terra::time(raster_object)
  } else {
    plot_title <- names(raster_object)
  }
  # Generate the plot
  plot_lc <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = raster_object) +
    fill_scale +
    ggplot2::theme_bw() +
    ggplot2::labs(title = plot_title, fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(title.position = "top", ncol = 3)) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(0.25, "cm"),
      legend.key.width = ggplot2::unit(0.25, "cm"),
      legend.position = "bottom",
      legend.justification = c(0, 0.5)
    )
  return(plot_lc)
}

#' Generate QUES-C Report
#'
#' Generates a report for the Pre-QUES analysis using R Markdown.
#'
#' @param output_quesc List. Output from QUES-C analysis.
#' @param dir Character string. Directory to save the report.
#'
#' @importFrom rmarkdown render
#'
#' @export
#' Generate QUES-C Report
#'
#' Generates a report for the QUES-C analysis using R Markdown.
#'
#' @param output_quesc List. Output from `run_quesc_analysis`.
#' @param dir Character string. Directory to save the report.
#'
#' @importFrom rmarkdown render pandoc_available
#'
#' @export
generate_quesc_report <- function(output_quesc, dir) {
  report_params <- list(
    start_time = output_quesc$start_time,
    end_time = output_quesc$end_time,
    map_c1 = output_quesc$map_c1,
    map_c2 = output_quesc$map_c2,
    map_em = output_quesc$map_em,
    map_sq = output_quesc$map_sq,
    ques_db = output_quesc$ques_db,
    p1 = output_quesc$p1,
    p2 = output_quesc$p2,
    inputs = output_quesc$inputs,
    session_log = output_quesc$session_log
  )
  output_file <- paste0("quesc_report_", Sys.Date(), ".html")
  
  #template_path <- here::here("04_quesc", "report_template", "quesc_report_template.Rmd")
  
  #if (!file.exists(template_path)) {
  #  stop(paste("Template file not found at:", template_path))}
  #fun_path <- here::here("04_quesc", "rscript", "function_ques_c.R")
  #if (!file.exists(fun_path)) {
  #stop(paste("Template file not found at:", fun_path))}
  
  temp_dir <- tempdir()
  
  # Copy report template and functions to temporary directory
  if (file.exists("../report_template/quesc_report_template.Rmd")){
    quesc_report_path <- "../report_template/quesc_report_template.Rmd"
    helper_functions_path <- "../rscript/function_ques_c.R"
  } else if (file.exists("../../04_quesc/report_template/quesc_report_template.Rmd")){
    quesc_report_path <- "../../04_quesc/report_template/quesc_report_template.Rmd"
    helper_functions_path <- "../../04_quesc/rscript/function_ques_c.R"
  } else {
    quesc_report_path <- "04_quesc/report_template/quesc_report_template.Rmd"
    helper_functions_path <- "04_quesc/rscript/function_ques_c.R"
  }
  
  file.copy(quesc_report_path,
            to = file.path(temp_dir, "quesc_report_template.Rmd"), overwrite = TRUE)
  file.copy(helper_functions_path,
            to = file.path(temp_dir, "function_ques_c.R"), overwrite = TRUE)
  
  # Render the R Markdown report
  if (rmarkdown::pandoc_available()==FALSE){
    Sys.setenv(RSTUDIO_PANDOC=paste0(getwd(), "/pandoc"))
  }
  
  rmarkdown::render(
    input = file.path(temp_dir, "quesc_report_template.Rmd"),
    # "../report_template/quesc_report_template.Rmd",
    output_file = output_file,
    output_dir = dir,
    params = report_params
  )
}


#' Run QUES-C Analysis
#'
#' Perform LUMENS module QUES-C analysis
#'
#' @param lc_t1_input RasterLayer. Land cover data for time point 1.
#' @param lc_t2_input RasterLayer. Land cover data for time point 2.
#' @param admin_z_input RasterLayer Administrative zones data.
#' @param c_lookup_input Data frame. Land cover lookup table input.
#' @param time_points List. Time points for analysis (t1 and t2).
#' @param output_dir Character string. Directory to save output files.
#' @param progress_callback Function. Callback function to report progress (optional).
#'
#' @return List containing output_quesc
#'
#' @import LUMENSR add_legend_to_categorical_raster
#' @importFrom dplyr %>% rename rename_with right_join left_join mutate
#' @importFrom terra writeRaster resample compareGeom
#'
#' @export
#' Run QUES-C Analysis
#'
#' Perform LUMENS module QUES-C (Carbon) analysis. This function orchestrates the entire QUES-C workflow,
#' from loading and harmonizing input data to generating carbon, emission, and sequestration maps,
#' creating a QUES-C database, and finally generating a comprehensive report.
#'
#' @param lc_t1_path Character string. Absolute path to the land cover raster for time point 1.
#' @param lc_t2_path Character string. Absolute path to the land cover raster for time point 2.
#' @param admin_z_path Character string. Absolute path to the administrative zones vector file (e.g., shapefile).
#' @param c_lookup_path Character string. Absolute path to the carbon lookup table (CSV format).
#' @param time_points List. A list with two elements, `t1` and `t2`, specifying the years for the analysis.
#' @param output_dir Character string. Absolute path to the directory where output files (maps, database, report) will be saved.
#' @param progress_callback Function. An optional callback function to report progress during execution.
#'   It should accept two arguments: `progress_value` (numeric, 0 to 1) and `message` (character string).
#'
#' @return A list containing the results of the QUES-C analysis, including:
#'   \itemize{
#'     \item \code{start_time}: Character string, timestamp when the analysis started.
#'     \item \code{end_time}: Character string, timestamp when the analysis ended.
#'     \item \code{map_c1}: SpatRaster, carbon map for time point 1.
#'     \item \code{map_c2}: SpatRaster, carbon map for time point 2.
#'     \item \code{map_em}: SpatRaster, emission map.
#'     \item \code{map_sq}: SpatRaster, sequestration map.
#'     \item \code{ques_db}: Data frame, the QUES-C database containing detailed calculations.
#'     \item \code{p1}: Numeric, start year of the analysis period.
#'     \item \code{p2}: Numeric, end year of the analysis period.
#'     \item \code{inputs}: List, a record of all input file paths used.
#'     \item \code{session_log}: Data frame, R session information.
#'   }
#' @importFrom LUMENSR ques_pre add_legend_to_categorical_raster
#' @importFrom dplyr %>% rename rename_with right_join left_join mutate
#' @importFrom terra writeRaster resample compareGeom rast classify
#' @importFrom sf st_read st_cast st_drop_geometry
#' @importFrom readr read_csv
#' @export
run_quesc_analysis <- function(lc_t1_path, lc_t2_path, admin_z_path, c_lookup_path,
                               time_points, output_dir, progress_callback = NULL) {
  
  start_time <- Sys.time()
  cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # read table
  c_lookup_input <- readr::read_csv(c_lookup_path)
  
  
  if (!is.null(progress_callback)) progress_callback(0.2, "load maps")
  
  lc_t1 <- lc_t1_path %>%
    terra::rast() %>%
    LUMENSR::add_legend_to_categorical_raster(
      lookup_table = c_lookup_input,
      year = as.numeric(time_points$t1)
    )
  
  lc_t2 <- lc_t2_path %>%
    terra::rast() %>%
    LUMENSR::add_legend_to_categorical_raster(
      lookup_table = c_lookup_input,
      year = as.numeric(time_points$t2)
    ) %>%
    check_and_harmonise_geometry(reference_map = lc_t1)
  
  # read polygon
  zone_sf1 <- sf::st_read(admin_z_path)
  zone_sf <- sf::st_cast(zone_sf1, "MULTIPOLYGON")
  zone <- zone_sf %>%
    rasterise_multipolygon_quesc(
      raster_res = res(lc_t1), 
      field = paste0(colnames(sf::st_drop_geometry(zone_sf[1]))) 
    )
  zone_lookup_input <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
  
  zone <- zone %>%
    check_and_harmonise_geometry(reference_map = lc_t1)
  
  preques <- LUMENSR::ques_pre(lc_t1, lc_t2, zone)
  period_year <- as.numeric(time_points$t1) - as.numeric(time_points$t2)
  lucDummy <- generate_dummy_crosstab(c_lookup_input, zone_lookup_input)
  
  if (!is.null(progress_callback)) progress_callback(0.5, "create QUES-C database")
  
  # join table
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC1 = 1, C_T1 = 3) %>% dplyr::select(1:3) %>%
    dplyr::rename_with(.cols = 2, ~as.character(time_points$t1)) %>% dplyr::right_join(lucDummy, by="ID_LC1")
  df_lucdb <- c_lookup_input %>% dplyr::rename(ID_LC2 = 1, C_T2 = 3) %>% dplyr::select(1:3) %>%
    dplyr::rename_with(.cols = 2, ~as.character(time_points$t2)) %>% dplyr::right_join(df_lucdb, by="ID_LC2")
  df_lucdb <- zone_lookup_input %>% dplyr::rename(ID_PU = 1) %>%
    dplyr::rename_with(.cols = 2, ~names(zone)) %>% dplyr::right_join(df_lucdb, by="ID_PU") 
  df_lucdb <- df_lucdb %>%
    dplyr::left_join(
      preques[["landscape_level"]][["crosstab_long"]],
      by = c(names(zone), time_points$t1, time_points$t2)
    )
  # the full version of preques database from preques analysis combined with all possible landcover listed in the lookup table
  df_lucdb <- df_lucdb %>%
    replace(is.na(df_lucdb), 0) %>%
    dplyr::rename(PU = names(zone))
  
  # create new matrix reclassification
  reclassify_matrix <- as.matrix(c_lookup_input[, 1]) %>%
    cbind(., as.matrix(c_lookup_input[, 3]) ) %>%
    rbind(., c(0, NA))
  
  if (!is.null(progress_callback)) progress_callback(0.7, "generate carbon, emission, and sequestration maps")
  
  # create all maps
  map_carbon1 <- lc_t1 %>% terra::classify(reclassify_matrix)
  map_carbon2 <- lc_t2 %>% terra::classify(reclassify_matrix)
  map_emission <- ((map_carbon1 - map_carbon2) * 3.67) * (map_carbon1 > map_carbon2)
  map_sequestration <- ((map_carbon2 - map_carbon1) * 3.67) * (map_carbon1 < map_carbon2)
  
  # quescdatabase
  df_lucdb <- df_lucdb %>% dplyr::mutate(
    EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
    SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
    
    LU_CHG = do.call(paste, c(df_lucdb[c(as.character(time_points$t1), as.character(time_points$t2))], sep = " to "))
    
  )
  
  end_time <- Sys.time()
  cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  
  session_log <- format_session_info_table()
  
  out <- list(
    start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
    end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
    map_c1 = map_carbon1,
    map_c2 = map_carbon2,
    map_em = map_emission,
    map_sq = map_sequestration,
    ques_db = df_lucdb,
    p1 = time_points$t1,
    p2 = time_points$t2,
    inputs = list(
      lc_t1_path = normalizePath(lc_t1_path, mustWork = FALSE),
      lc_t2_path = normalizePath(lc_t2_path, mustWork = FALSE),
      admin_z_path = normalizePath(admin_z_path, mustWork = FALSE),
      c_lookup_path = normalizePath(c_lookup_path, mustWork = FALSE),
      output_dir = normalizePath(output_dir, mustWork = FALSE)
    ),
    session_log = session_log
  )
  
  if (!is.null(progress_callback)) progress_callback(0.9, "outputs generated and saved")
  write.table(df_lucdb,
              paste0(output_dir, "/quesc_database.csv"),
              quote = FALSE,
              row.names = FALSE,
              sep = ","
  )
  terra::writeRaster(map_carbon1,
              paste0(output_dir, "/carbon_map_t1.tif"),
              overwrite = T
  )
  terra::writeRaster(map_carbon2,
              paste0(output_dir, "/carbon_map_t2.tif"),
              overwrite = T
  )
  terra::writeRaster(map_emission,
              paste0(output_dir, "/emission_map.tif"),
              overwrite = T
  )
  terra::writeRaster(map_sequestration,
              paste0(output_dir, "/sequestration_map.tif"),
              overwrite = T
  )
  
  if (!is.null(progress_callback)) progress_callback(1, "generate report")
  generate_quesc_report(output_quesc = out, dir = output_dir)
  
  return(out)
}

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
        cat(paste0("\nAttempting to install package '", package, "'\n"))
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
