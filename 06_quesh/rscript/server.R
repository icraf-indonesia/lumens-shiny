library(shiny)

server <- function(input, output, session) {
  rv <- reactiveValues(
    wd = "",
    report_file = NULL,
    rainfall_file = NULL,
    dem_file = NULL,
    sand_file = NULL,
    silt_file = NULL,
    clay_file = NULL,
    orgc_file = NULL,
    lc_dir = NULL,
    pu_file = NULL,
    c_ref_file = NULL
  )
  
  volumes <- c(getVolumes()())
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  } 
  
  #### File Inputs ####
  observeEvent(input$rainfall_file, {
    rv$rainfall_file <- input$rainfall_file
  })
  
  observeEvent(input$dem_file, {
    rv$dem_file <- input$dem_file
  })
  
  observeEvent(input$sand_file, {
    rv$sand_file <- input$sand_file
  })
  
  observeEvent(input$silt_file, {
    rv$silt_file <- input$silt_file
  })
  
  observeEvent(input$clay_file, {
    rv$clay_file <- input$clay_file
  })
  
  observeEvent(input$orgc_file, {
    rv$orgc_file <- input$orgc_file
  })
  
  observeEvent(input$lc_dir, {
    rv$lc_dir <- input$lc_dir
  })
  
  observeEvent(input$pu_file, {
    rv$pu_file <- input$pu_file
  })
  
  observeEvent(input$c_ref_file, {
    rv$c_ref_file <- input$c_ref_file
  })
  
  
  #### Process data ####
  observeEvent(input$process, {
    tryCatch({
      start_time <- Sys.time()
      
      # 1. Validate data inputs
      req(rv$rainfall_file, message = "Please upload the total annual precipitation map.")
      req(rv$dem_file, message = "Please upload the digital elevation model (DEM).")
      req(rv$sand_file, message = "Please upload the sand content map.")
      req(rv$silt_file, message = "Please upload the silt content map.")
      req(rv$clay_file, message = "Please upload the clay content map.")
      req(rv$orgc_file, message = "Please upload the organic content map.")
      req(rv$lc_dir, message = "Please upload the land cover map.")
      req(rv$pu_file, message = "Please upload the planning unit map.")
      req(rv$c_ref_file, message = "Please upload the c factor attribute table.")
      
      # 2. R - Rainfall erosivity preparation -----------------------------------------------------
      
      # Define R factor data
      rainfall_annual <- syncGeom(input = rv$rainfall_file, ref = rv$pu_file)
      
      # Calculate R factor - Rainfall Erosivity (Moore, 1979)
      r_factor <- calculate_r_moore(p = rainfall_annual)
      
      writeRaster(r_factor, paste0(rv$wd, "r_factor.tif"), overwrite = TRUE)
      
      # 3. K - Soil erodibility preparation -------------------------------------
      
      # Define K factor data
      sand <- syncGeom(input = rv$sand_file, ref = rv$pu_file)
      silt <- syncGeom(input = rv$silt_file, ref = rv$pu_file)
      clay <- syncGeom(input = rv$clay_file, ref = rv$pu_file)
      orgc <- syncGeom(input = rv$orgc_file, ref = rv$pu_file)
      
      soil_stack <- c(sand, silt, clay, orgc)
      
      # Calculate K factor - Soil Erodibility (Williams, 1995)
      k_factor <- calculate_k_williams(
        sndprc = sand, 
        sltprc = silt, 
        clyprc = clay, 
        orcprc = orgc
      )
      
      writeRaster(k_factor, paste0(rv$wd, "k_factor.tif"), overwrite = TRUE)
      
      # 4. LS - Length and steepnes preparation --------------------------------
      
      # Define LS factor data
      dem <- syncGeom(input = rv$dem_file, ref = rv$pu_file)
      slope_deg <- terrain(dem, v = "slope", unit="degree")  
      slope_rad <- terrain(dem, v = "slope", unit="radians") 
      aspect <- terrain(dem, v = "aspect", unit="radians") 
      flow_acc <- terrain(dem, v = "flowdir")
      
      # Calculate LS factor by Previous LUMENS Script
      # ls_factor <- calculate_ls(
      #   slope = slope_rad,
      #   aspect = aspect
      # )
      
      # Calculate LS factor by Moore & Burch (1986) - BRIN 
      # ls_factor <- calculate_ls_moore(
      #   dem = dem,
      #   slope = slope_rad,
      #   flow_acc = flow_acc
      # )
      
      # Calculate LS Factor by Moore and Burch (1986) and Moore and Wilson (1992)
      # m <- 0.4  # value can range between 0.4 and 0.6
      # n <- 1.3  # value can range between 1.22 and 1.3
      # 
      # ls_factor <- ((flow_acc / 22.13)^m) * ((sin(slope) / 0.0896)^n) # use radians slope
      
      # Calculate LS factor by Wischmeier and Smith (1978)
      # Function to classify 'm' based on the slope angle (β)
      calculate_m <- function(slope_val) {
        if (is.na(slope_val)) {
          return(NA)
        } else if (slope_val > 0.05) {
          return(0.5)
        } else if (slope_val > 0.03) {
          return(0.4)
        } else if (slope_val > 0.01) {
          return(0.3)
        } else {
          return(0.2)
        }
      }
      
      m <- app(slope_rad, fun = function(x) sapply(x, calculate_m))
      ls_factor <- ((flow_acc / 22.13)^m) * (65.4 * (sin(slope_rad)^2) + 4.5 * sin(slope_rad) + 0.0654) # use radians slope
      
      writeRaster(ls_factor, paste0(rv$wd, "ls_factor.tif"), overwrite = TRUE)
      
      # 5. C - Cover management preparation -------------------------------------
      
      # Define C factor data
      c_ref <- read.csv(rv$c_ref_file)
      landcover <- syncGeom(input = rv$lc_dir, ref = rv$pu_file)
      
      lc <- as.factor(landcover)
      reclass_matrix <- as.matrix(c_ref[, c("ID", "C_factor")])
      c_factor <- classify(landcover, reclass_matrix, others = 1)
      
      # Method 1
      # c_factor_lookup <- setNames(c_ref$C_factor, c_ref$ID)
      # 
      # replace_with_c_factor <- function(x) {
      #   c_factor_lookup[as.character(x)] 
      # }
      # 
      # c_factor_raster <- app(landcover, replace_with_c_factor)
      
      # Method 2
      # landcover_c <- landcover
      # c_ref2 <- as.matrix(c_ref[,1])
      # c_ref3 <- as.matrix(c_ref[,3])
      # c_ref4 <- cbind(c_ref2, c_ref3)
      # c_ref4 <- rbind(c_ref4, c(0, NA))
      # c_factor <- classify(landcover_c, c_ref4)
      
      # Calculate C factor
      # calculate_c_lc(
      #   landcover = landcover,
      #   c_ref = c_ref
      # )
      
      writeRaster(c_factor, paste0(rv$wd, "c_factor.tif"), overwrite = TRUE)
      
      
      # 6. P - Practice factor preparation --------------------------------------
      
      slope_pct <- tan(slope_deg * pi / 180) * 100
      
      # There are 3 option for practice management according to Shin (1999)
      # You can choose the applied practices: Contouring; Strip Cropping; Terracing
      # change the parameter by the following order p_user <- c([contouring], [strip cropping], [terracing])
      # The value of 1 means the corresponding practice applied and 0 means not applied
      # If the value all 0, it means the P factor will be define as 1 (no practice applied)
      
      p_user <- c(1, 0, 0) # change the value with 0 or 1 by this order: c([contouring], [strip cropping], [terracing])
      
      p_factor <- calculate_p_shin(
        slope_pct = slope_pct, 
        p_user = p_user
      )
      
      writeRaster(p_factor, paste0(rv$wd, "p_factor.tif"), overwrite = TRUE)
      
      # 7. Calculate soil erosion RUSLE ----------------------------------------------------
      
      # Redefine input parameters
      
      r <- r_factor
      k <- k_factor
      ls <- ls_factor
      c <- c_factor
      p <- p_factor
      
      # Calculate RUSLE
      a <- r*k*ls*c*p
      
      writeRaster(a, filename = paste0(rv$wd, "soil_erosion.tif"), overwrite = TRUE)
      
      # End of the script
      end_time <- Sys.time()
      cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
      
      # 8. 8. Prepare parameters for report -------------------------------------
      
      report_params <- list(
        start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
        end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
        output_dir = rv$wd,
        dem = rv$dem,
        pu = rv$pu,
        rainfall = rv$rainfall_annual,
        soil = soil_stack,
        landcover = landcover,
        r = r,
        k = k,
        ls = ls,
        c = c,
        p = p,
        a = a
      )
      
      # Render the R markdown report
      if (!rmarkdown::pandoc_available()) {
        Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
      }
      
      rmarkdown::render(
        input = "06_quesh/report_template/quesh_report.Rmd",
        output_file = "QUES-H_report.html",
        output_dir = rv$wd,
        params = report_params
      )
      
    }, error = function(e) {
      cat("An error occurred:\n")
      print(e)
    }, finally = {
      cat("Script execution completed.\n")
    })
  })

  #### Set working directory ####
  shinyDirChoose(
    input, 
    'wd',
    roots = volumes,
    session = session
  )
  
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  observe({
    if(!is.null(rv$report_file)){
      file.show(rv$report_file)
      js$closeWindow()
      shinyjs::delay(5000, stopApp())
    }
  })
}