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
  "terra",
  "shiny",
  "shinyFiles",
  "bslib",
  "raster",
  "splitstackshape",
  "ggplot2",
  "foreign",
  "reshape2",
  "dplyr",
  "reshape",
  "purrr",
  "plotly",
  "sf",
  "shinyvalidate",
  "remote",
  "rmarkdown",
  "magick"
)

#### Helper Functions ####
create_linkages_table <- function(sector, DBL, DFL) {
  # Function to create the Linkages Table
  DBL <- as.data.frame(DBL)
  DFL <- as.data.frame(DFL)
  # BPD_temp <- DBL / mean(DBL)
  # FPD_temp <- DFL / mean(DFL)
  Linkages_table <- cbind(sector, DBL, DFL)
  colnames(Linkages_table) <- c("SECTOR", "CATEGORY", "DBL", "DFL")
  return(Linkages_table)
}

calculate_land_requirements <- function(land_distribution, land_use, fin_dem, int_con, sector) {
  # Function to calculate land requirements
  lc_freq <- freq(land_use)
  lc_freq <- as.data.frame(na.omit(lc_freq))
  landuse_area <- as.matrix((lc_freq$count))
  land_distribution_t <- as.matrix(land_distribution)
  landuse_area_diag <- diag(as.numeric(landuse_area))
  land_distribution_val <- land_distribution_t %*% landuse_area_diag
  
  land_requirement <- rowSums(land_distribution_val)
  fin_dem_rtot <- rowSums(fin_dem)
  int_con_rtot <- rowSums(int_con)
  demand <- fin_dem_rtot + int_con_rtot
  land_requirement_coeff <- land_requirement / demand
  land_requirement_coeff[is.infinite(land_requirement_coeff)] <- 0
  
  land_productivity_coeff <- land_requirement / fin_dem_rtot
  land_productivity_coeff[is.infinite(land_productivity_coeff)] <- 0
  
  land_requirement_table <- cbind(
    sector,
    LR = round(land_requirement),
    LR_PROP = round(land_requirement / sum(land_requirement), 2),
    OUTPUT = round(demand),
    DEMAND = round(demand),
    LRC = round(land_requirement_coeff, 2),
    LPC = round(land_productivity_coeff, 2)
  )
  
  colnames(land_requirement_table) <- c("SECTOR", "CATEGORY", "LR", "LR_PROP", "OUTPUT", "DEMAND", "LRC", "LPC")
  return(as.data.frame(land_requirement_table))
}

create_graph <- function(sector, data, y_label, graph_title) {
  sector <- as.data.frame(sector$SECTOR)
  colnames(sector) <- "SECTOR"
  # Function to create a graph
  ggplot(data = data.frame(SECTOR = sector, VALUE = data), aes(x = SECTOR, y = VALUE, fill = SECTOR)) +
    geom_bar(colour = "black", stat = "identity") +
    coord_flip() +
    guides(fill = FALSE) +
    xlab("Sectors") +
    ylab(y_label) +
    ggtitle(graph_title)
}