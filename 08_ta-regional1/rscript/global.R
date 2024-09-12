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
  "magick",
  "plotly",
  "purrr",
  "raster",
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
  "DT"
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
  # land.distribution.ctot<-colSums(land.distribution.val)
  # land.distribution.prop<-land.distribution.val %*% diag(1/land.distribution.ctot)
  # land.distribution.prop[is.na(land.distribution.prop)]<-0
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
  # sector <- as.data.frame(sector$SECTOR)
  sector <- as.data.frame(sector)
  colnames(sector) <- c("SECTOR","CATEGORY")
  # Function to create a graph
  ggplot(data = data.frame(SECTOR = sector, VALUE = data), aes(x = SECTOR.SECTOR, y = VALUE, fill = SECTOR.CATEGORY)) +
    geom_bar(colour = "black", stat = "identity") +
    coord_flip() +
    xlab("Sectors") +
    ylab(y_label) +
    ggtitle(graph_title)
}

generate_land_distribution_prop <- function(land_use, land_distribution) {
  lc_freq <- freq(land_use)
  lc_freq <- as.data.frame(na.omit(lc_freq))
  landuse_area <- as.matrix(lc_freq$count)
  land_distribution_t <- as.matrix(land_distribution)
  landuse_area_diag <- diag(as.numeric(landuse_area))
  land_distribution_val <- land_distribution_t %*% landuse_area_diag
  
  land_requirement <- rowSums(land_distribution_val)
  land_distribution_ctot <- colSums(land_distribution_val)
  land_distribution_prop <- land_distribution_val %*% diag(1 / land_distribution_ctot)
  land_distribution_prop[is.na(land_distribution_prop)] <- 0
  
  list(
    land_distribution_prop = land_distribution_prop,
    land_requirement = land_requirement,
    land_distribution_ctot = land_distribution_ctot
  )
}

generate_gdp_table <- function(add_val_m, sector, int_con_ctot) {
  GDP_val <- as.data.frame(add_val_m[2,] + add_val_m[3,])
  GDP_val_m <- as.numeric(as.matrix(GDP_val))
  
  OUTPUT_val <- as.data.frame(add_val_m[2,] + add_val_m[3,] + add_val_m[1,] + int_con_ctot)
  OUTPUT_val_m <- as.numeric(as.matrix(OUTPUT_val))
  
  GDP <- cbind(sector, GDP_val, OUTPUT_val)
  colnames(GDP) <- c("SECTOR", "CATEGORY", "GDP", "OUTPUT")
  GDP$GDP_PROP <- GDP$GDP / GDP$OUTPUT
  GDP[is.na(GDP)] <- 0
  colnames(GDP)[5] <- "P_OUTPUT"
  
  GDP_tot <- colSums(as.matrix(GDP$GDP))
  
  list(GDP = GDP, GDP_tot = GDP_tot)
}

generate_multipliers <- function(Leontief, sector, GDP_val, fin_con, labour) {
  # Output Multiplier
  Out_multiplier <- colSums(Leontief)
  Out_multiplier <- cbind(sector, Out_multiplier)
  
  # Income Multiplier
  V_income <- as.matrix(GDP_val * fin_con)
  Inc_multiplier <- Leontief %*% V_income
  
  # Labour Multiplier
  labour_m <- as.matrix(labour * fin_con) / 1000000
  Lab_multiplier <- Leontief %*% labour_m
  
  multiplier <- cbind(Out_multiplier, Inc_multiplier, Lab_multiplier)
  colnames(multiplier) <- c("SECTOR", "CATEGORY", "Out.multiplier", "Inc.multiplier", "Lab.multiplier")
  multiplier$Out.multiplier <- round(multiplier$Out.multiplier, digits = 3)
  
  list(multiplier = multiplier)
}
