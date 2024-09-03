server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = "",
    report_file = NULL,
    area_name = NULL,
    recon_file = NULL,
    unresolved_table = NULL,
    map_resolution = NULL,
    raster_temp = NULL,
    summary_PUR = NULL,
    u = NULL,
    ref = NULL,
    sa = NULL,
    pur_sa = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }  
  
  #### File Inputs ####
  observeEvent(input$area_name, {
    rv$area_name <- as.character(input$area_name)
  })
  
  observeEvent(input$recon_file, {
    shp <- input$recon_file
    if (is.null(shp))
      return()
    
    prev_wd <- getwd()
    uploaded_dir <- dirname(shp$datapath[1])
    setwd(uploaded_dir)
    for (i in 1:nrow(shp)) {
      file.rename(shp$datapath[i], shp$name[i])
    }
    setwd(prev_wd)
    
    rv$recon_file <- paste(uploaded_dir, shp$name[grep(pattern = "*.shp$", shp$name)], sep = "/")
    
    if (is.null(rv$recon_file) || !file.exists(rv$recon_file)) {
      showNotification("Recon file not found or is invalid", type = "error")
      return()
    }
    
    # Reading shapefile and handling errors
    rv$pur_sa <- tryCatch({
      rv$recon_file %>% st_read() %>% st_as_sf() %>% st_drop_geometry()
    }, error = function(e) {
      showNotification("Failed to read shapefile: check the file format", type = "error")
      return(NULL)
    })
    
    rv$sa <- tryCatch({
      rv$recon_file %>% st_read()
    }, error = function(e) {
      showNotification("Failed to read shapefile geometry: check the file format", type = "error")
      return(NULL)
    })
    
    if (is.null(rv$sa) || is.null(rv$pur_sa)) {
      showNotification("Failed to load shapefile data", type = "error")
      return()
    }
    
    rv$ref <- rasterise_multipolygon(sf_object = rv$sa, raster_res = c(100, 100), field = "ID")
  })
  
  observeEvent(input$unresolved_table, {
    rv$u <- input$unresolved_table
  })
  
  observeEvent(input$map_resolution, {
    rv$map_resolution <- as.numeric(input$map_resolution)
  })
  
  #### Process data ####
  observeEvent(input$process, {
    # Check if required data is available
    tryCatch({
    # Start running PUR
    start_time <- Sys.time()
    
    if (is.null(rv$pur_sa)) {
      showNotification("No data available for processing. Please upload a valid shapefile.", type = "error")
      return()
    }
    
    # 1. Define input parameters ------------------------------------
    # Load unresolved cases and join with attribute table
    unresolved_edit <- readxl::read_xlsx(rv$u$datapath)
    unresolved_edit.c1 <- as.data.frame(unresolved_edit[["ID"]]) 
    unresolved_edit.c2 <- as.data.frame(unresolved_edit[["Reconcile Action"]])
    colnames(unresolved_edit.c1)[1] <- "ID"
    colnames(unresolved_edit.c2)[1] <- "resolved"
    unresolved_edit.join <- cbind(unresolved_edit.c1, unresolved_edit.c2)
    
    # Define attribute ori from shp
    attribute_ori <- rv$pur_sa %>% 
      rename(Rec_phase1b = Rec_phase2) %>% 
      select(ID, Rec_phase1b)
    
    # Define attribute from shp
    pur_attribute_top <- rv$pur_sa %>% 
      rename(Rec_phase1b = Rec_phase2) %>% 
      select(ID_rec, Rec_phase1b) %>% 
      filter(!Rec_phase1b %in% "unresolved_case") %>% 
      distinct(Rec_phase1b) %>% 
      tibble::rownames_to_column("ID") %>%
      mutate(ID = as.numeric(ID))
    
    pur_attribute_mid <- rv$pur_sa %>%
      rename(Rec_phase1b = Rec_phase2) %>% 
      select(ID, Rec_phase1b) %>% 
      filter(Rec_phase1b %in% "unresolved_case") %>% 
      arrange(ID)
    
    pur_attribute_top <- bind_rows(pur_attribute_top, pur_attribute_mid)
    attribute <- pur_attribute_top
    
    # 2. Data preparation -------------------------------------------
    # Load original attribute data and merge with unresolved cases
    attribute.edit <- merge(attribute_ori, unresolved_edit.join, by = "ID", all = TRUE)
    
    # 3. Resolve any unresolved cases --------------------------------
    test <- as.data.frame(unique(unresolved_edit[["Reconcile Action"]]))
    test2 <- as.data.frame(unique(attribute$Rec_phase1b))
    colnames(test)[1] <- "add"
    colnames(test2)[1] <- "add"
    test3 <- rbind(test, test2)
    levels(attribute.edit$resolved) <- levels(test3$add)
    colnames(attribute.edit)[1] <- "PU_name"
    
    len <- nrow(attribute.edit)
    for (s in 1:len) {
      if (is.na(attribute.edit$resolved[s])) {
        attribute.edit$resolved[s] <- attribute.edit$Rec_phase1b[s]
        attribute.edit$res_id[s] <- attribute.edit$PU_name[s]
      }
    }
    
    # 4. Create Unique Class IDs for Resolved Cases -----------------
    unique_class <- as.data.frame(unique(attribute.edit$resolved))
    colnames(unique_class)[1] <- "resolved"
    countrow <- nrow(unique_class)
    unique_class$PU_ID <- seq(countrow)
    attribute.edit <- merge(attribute.edit, unique_class, by = "resolved")
    attribute.edit <- attribute.edit |> select(ID = PU_name, resolved, PU_ID)
    
    # 5. Save final reconciliation data ----------------------------
    sa0 <- rv$sa %>% select(-Referenc_1)
    rv$sa  <- merge(sa0, attribute.edit, by = "ID", all = TRUE)
    st_write(rv$sa, paste0(rv$wd, "/PUR_reconciliation_result.shp"), driver = "ESRI Shapefile", append = FALSE)
    
    ref0 <- rv$sa %>% select(ID, REFERENCE)
    ref <- rasterise_multipolygon(sf_object = ref0, raster_res = c(rv$map_resolution, rv$map_resolution), field = "ID")
    
    # Check and handle the projection of the map
    if (grepl("+units=m", as.character(st_crs(ref)$proj4string))){
      print("Raster maps have projection in meter unit")
      Spat_res<-res(ref)[1]*res(ref)[2]/10000
      paste("Raster maps have ", Spat_res, " Ha spatial resolution, PUR will automatically generate data in Ha unit")
    } else if (grepl("+proj=longlat", as.character(st_crs(ref)$proj4string))){
      print("Raster maps have projection in degree unit")
      Spat_res<-res(ref)[1]*res(ref)[2]*(111319.9^2)/10000
      paste("Raster maps have ", Spat_res, " Ha spatial resolution, PUR will automatically generate data in Ha unit")
    } else{
      statuscode<-0
      statusmessage<-"Raster map projection is unknown"
      statusoutput<-data.frame(statuscode=statuscode, statusmessage=statusmessage)
      quit()
    }
    
    pur_final_recon_rast <- terra::rasterize(vect(rv$sa), rast(ref), field = "PU_ID", res = res(ref)[1], background = NA)
    pur_final_recon_rast2 <- terra::rasterize(vect(rv$sa), rast(ref), field = "resolved", res = res(ref)[1], background = NA)
    
    # Create summary of final reconciliation
    test4 <- raster(pur_final_recon_rast)
    test4 <- ratify(test4, filename = paste0(rv$wd, '/PUR.grd'), count = TRUE, overwrite = TRUE)
    rv$summary_PUR <- as.data.frame(levels(test4))
    colnames(rv$summary_PUR)[1] <- "PU_ID"
    rv$summary_PUR <- merge(rv$summary_PUR, unique_class, by = "PU_ID")
    
    raster_temp <- reclassify(test4, cbind(255, NA))
    raster_temp_name <- paste0(rv$wd, "/PUR_reconciliation_result.tif")
    writeRaster(raster_temp, filename = raster_temp_name, format = "GTiff", overwrite = TRUE)
    
    # pur_attribute_table <- rv$summary_PUR
    # colnames(pur_attribute_table) <- c("ID", "COUNT", "Legend")
    # csv_file <- paste0(rv$wd, "/csv_planning_unit.csv")
    # write.table(pur_attribute_table, file = csv_file, quote = FALSE, row.names = FALSE, sep = ",")
    
    saveRDS(pur_final_recon_rast, paste0(rv$wd, "/PUR_final_recon_rast.rds"))
    saveRDS(rv$summary_PUR, paste0(rv$wd, "/summary_PUR.rds"))
    
    colnames(rv$summary_PUR) <- c("ID", "Ha", "Final Resolved Area")
    write.table(rv$summary_PUR, paste0(rv$wd, "PUR_final_lookup_table.csv"), quote = FALSE, row.names = FALSE, sep = ",")
    
    # End of the script
    end_time <- Sys.time()
    cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # 6. Generate report -------------------------
    report_params <- list(
      start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
      end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
      output_dir = rv$wd,
      raster_temp = rv$raster_temp,
      summary_PUR = rv$summary_PUR,
      area_name = rv$area_name,
      sa = rv$sa,
      dir_raster_temp = paste0(rv$wd, "/PUR_reconciliation_result.tif"),
      dir_sa = paste0(rv$wd, "/PUR_reconciliation_result.shp"),
      dir_summary_PUR = paste0(rv$wd, "/PUR_final_lookup_table.csv")
    )
    
    # Prepare summary data for the report
    summary_data <- list(
      total_area = sum(rv$summary_PUR$Ha) * Spat_res
      # resolved_area = sum(rv$summary_PUR$Ha[attribute_ori$Rec_phase1b != "unresolved_case"]) * Spat_res,
      # unresolved_area = sum(rv$summary_PUR$Ha[attribute_ori$Rec_phase1b == "unresolved_case"]) * Spat_res,
      # resolved_percentage = (sum(rv$summary_PUR$Ha[attribute_ori$Rec_phase1b != "unresolved_case"]) / sum(pur_attribute_table$COUNT)) * 100,
      # unresolved_percentage = (sum(rv$summary_PUR$Ha[attribute_ori$Rec_phase1b == "unresolved_case"]) / sum(pur_attribute_table$COUNT)) * 100
    )
    
    report_params$summary_data <- summary_data
    
    # Render the R markdown report
    if (!rmarkdown::pandoc_available()) {
      Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
    }
    
    rmarkdown::render(
      input = "D:/OneDrive - CIFOR-ICRAF/Documents/GitHub/lumens-shiny/02_pur2/report_template/PUR2_report.Rmd",
      output_file = "PUR_reconcile_report.html",
      output_dir = rv$wd,
      params = report_params
    )
   
    rv$report_file <- paste(rv$wd, "PUR_reconcile_report.html", sep = "/")
    
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
    if (length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ", rv$wd)
    }
  })
  
  # Final checks and showing the report
  observe({
    if (!is.null(rv$report_file)) {
      file.show(rv$report_file)
      shinyjs::delay(5000, stopApp())
    }
  })
}