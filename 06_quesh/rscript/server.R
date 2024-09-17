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
    c_ref_file = NULL,
    map_resolution = NULL
  )
  
  volumes <- c(getVolumes()())
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  } 
  
  #### File Inputs ####
  observeEvent(input$rainfall_file, {
    rv$rainfall_file <- input$rainfall_file$datapath
  })
  
  observeEvent(input$dem_file, {
    rv$dem_file <- input$dem_file$datapath
  })
  
  observeEvent(input$sand_file, {
    rv$sand_file <- input$sand_file$datapath
  })
  
  observeEvent(input$silt_file, {
    rv$silt_file <- input$silt_file$datapath
  })
  
  observeEvent(input$clay_file, {
    rv$clay_file <- input$clay_file$datapath
  })
  
  observeEvent(input$orgc_file, {
    rv$orgc_file <- input$orgc_file$datapath
  })
  
  observeEvent(input$lc_dir, {
    rv$lc_dir <- input$lc_dir$datapath
  })
  
  # observeEvent(input$pu_file, {
  #   rv$pu_file <- input$pu_file$datapath
  # })
  
  observeEvent(input$pu_file, {
    shp <- input$pu_file
    if (is.null(shp)) return()
    
    required_files <- c("shp", "shx", "dbf") # Optionally include "prj"
    
    # Ensure all necessary shapefile components are uploaded
    uploaded_files <- tools::file_ext(shp$name)
    if (!all(required_files %in% uploaded_files)) {
      showNotification("Incomplete shapefile. Please upload all required components: .shp, .shx, .dbf", type = "error")
      return()
    }
    
    prev_wd <- getwd()
    uploaded_dir <- dirname(shp$datapath[1])
    setwd(uploaded_dir)
    
    for (i in 1:nrow(shp)) {
      file.rename(shp$datapath[i], shp$name[i])
    }
    
    setwd(prev_wd)
    
    rv$pu_file <- paste(uploaded_dir, shp$name[grep(pattern = "*.shp$", shp$name)], sep = "/")
    
    if (is.null(rv$pu_file) || !file.exists(rv$pu_file)) {
      showNotification("Shapefile not found or is invalid", type = "error")
      return()
    }
    
    # Further processing with the shapefile
  })
  
  
  observeEvent(input$c_ref_file, {
    rv$c_ref_file <- input$c_ref_file$datapath
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

      # Prepare the planning unit
      pu1 <- st_read(rv$pu_file)
      pu <- rasterise_multipolygon(
        sf_object = pu1, 
        raster_res = c(rv$map_resolution, rv$map_resolution), 
        field = paste0(colnames(st_drop_geometry(pu1[1])))
      )
      
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
      
      dem <- syncGeom(input = rv$dem_file, ref = pu)
      
      # Calculate LS factor by Moore & Burch (1986) - BRIN 
      ls_factor <- calculate_ls_moore(dem = dem)
      
      writeRaster(ls_factor, paste0(rv$wd, "ls_factor.tif"), overwrite = TRUE)
      
      # 5. C - Cover management preparation -------------------------------------
      
      # Define C factor data
      c_ref <- readr::read_csv(rv$c_ref_file)
      landcover <- rast(rv$lc_dir)
      landcover_c <- landcover
      lookup_lc <-landcover_c %>% freq() %>%
        select(ID=value) %>%
        left_join(c_ref, by="ID") %>% select(-LC)
      
      levels(landcover_c)[[1]] <- lookup_lc
      
      c_factor <- landcover_c %>% as.numeric(1) %>% resample(pu, method="near")
      
      writeRaster(c_factor, paste0(rv$wd, "c_factor.tif"), overwrite = TRUE)
      
      
      # 6. P - Practice factor preparation --------------------------------------
      slope_deg <- terrain(dem, v = "slope", unit="degree") 
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
      
      # 8. Data Visualization ---------------------------------------------------
      
      # Landcover preparation
      lc_class <-landcover %>% freq() %>%
        select(ID=value) %>%
        left_join(c_ref, by="ID") %>% select(-C_factor)
      levels(landcover)[[1]] <- lc_class
      
      # Reclassify erosion rates based on China National Standard (2008)
      breaks <- c(-Inf, 5, 25, 50, 80, 150, Inf)
      labels <- c("Slight (< 5 ton/ha/yr)", 
                  "Mild (5-25 ton/ha/yr)", 
                  "Moderate (25-50 ton/ha/yr)", 
                  "Strong (50-80 ton/ha/yr)", 
                  "Very strong (80-150 ton/ha/yr)", 
                  "Severe (> 150 ton/ha/yr)")
      rcl_matrix <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks)-1))
      
      erosion_classified <- classify(a, rcl = rcl_matrix)
      levels(erosion_classified) <- data.frame(id=1:6, category=labels)
      plot(erosion_classified)
      
      writeRaster(erosion_classified, filename = paste0(output_dir, "soil_erosion_reclass.tif"), overwrite = TRUE)
      
      # Create dataset
      erosion_db <- data.frame(erosion_classified) %>%
        group_by(across(everything())) %>% 
        summarise(count = n())
      colnames(erosion_db, do.NULL = FALSE)
      colnames(erosion_db) <- c("Soil Erosion Rates","Area (Ha)")
      erosion_db$`Area (Ha)`*(10000/(path$map_resolution^2))
      erosion_db$`Percentage (%)` <- (erosion_db$`Area (Ha)`/sum(erosion_db$`Area (Ha)`))*100
      
      # hist_erosion(df = erosion_db)
      
      write.csv(erosion_db, file = paste0(output_dir, "soil_erosion.csv"))
      
      # End of the script
      end_time <- Sys.time()
      cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
      
      # 9. Prepare parameters for report -------------------------------------
      
      report_params <- list(
        start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
        end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
        output_dir = output_dir,
        dem = dem,
        pu = pu,
        rainfall = rainfall_annual,
        soil = soil_stack,
        landcover = landcover,
        r = r,
        k = k,
        ls = ls,
        c = c,
        p = p,
        a = erosion_classified,
        df = erosion_db, 
        map_resolution = map_resolution
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