server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = "",
    report_file = NULL,
    #area_name = NULL,
    recon_file = NULL,
    unresolved_table = NULL,
    #map_resolution = NULL,
    #raster_temp = NULL,
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
  # observeEvent(input$area_name, {
  #   rv$area_name <- as.character(input$area_name)
  # })
  
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
  
  # observeEvent(input$map_resolution, {
  #   rv$map_resolution <- as.numeric(input$map_resolution)
  # })
  
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
    
  
    
    # 2. Data preparation -------------------------------------------
    
    # select ID and Reconcile action column
    reconcile_scenario <- unresolved_edit %>% 
      dplyr::rename("ID"=1, 
                    "Reconcile Action"=9) %>% 
      dplyr::select(any_of(c("ID", "Reconcile Action")))
    
    
    # Unresolved cases
    unresolved_cases <- rv$pur_sa %>%
      st_drop_geometry() %>%
      rename(ID_rec=6) %>%
      filter( Rec_phase2  %in% "unresolved_case") 
    
    # check if the number of unresolved cases is identical/matched
    if (!nrow(unresolved_edit) == nrow(reconcile_scenario)){
      errorCondition("The number of unresolved cases between the planning unit and attribute table is not matched.")
    } else {
      message("The number of unresolved cases between the planning unit and attribute table is  matched.")
    }
    
    # check if the IDs of unresolved cases are identical/matched with the IDs from reconcile_scenario
    ID_shapefile <- sort(as.numeric(pull(unresolved_cases , ID)))
    ID_xlsx <- sort(as.numeric(pull(reconcile_scenario, ID)))
    
    if (!identical(ID_shapefile, ID_xlsx)){
      errorCondition("The planning unit IDs and attribute table IDs are not matched.")
    } else {
      message("The planning unit IDs and attribute table IDs are matched.")
    }
    
    # 3. Resolve any unresolved cases --------------------------------
    reconciled_map <- rv$sa %>%
      dplyr::select(-ID_rec) %>% 
      left_join(reconcile_scenario, by = "ID") %>%
      mutate(Rec_phase2 = ifelse(!is.na(`Reconcile Action`), `Reconcile Action`, Rec_phase2)) %>%
      select(-`Reconcile Action`)
    
    
    # 3. Calculate area --------------------------------
    
    # Check the CRS and verify if the units are in meters
    crs_info <- terra::crs(reconciled_map, proj = TRUE)
    units_in_meters <- grepl("units=m", crs_info)
    
    if (units_in_meters) {
      # Calculate the area in hectares
      reconciled_map <- reconciled_map %>%
        mutate(area = units::set_units(st_area(.), "ha"))
    } else {
      cat("The CRS units are not in meters. Cannot calculate area in hectares.\n")
    }
    
    
    # 4. Export results --------------------------------
    st_write(reconciled_map, paste0(rv$wd, "/PUR_reconciliation_result.shp"),
             driver = "ESRI Shapefile",
             append = FALSE)
    
    reconciled_map %>% 
      st_drop_geometry() %>% 
      write.csv(paste0(rv$wd, "/PUR_reconciliation_lookup_table.csv"))
      
    
    # 5. Disolve unique cases after reconciliation
    reconciled_map_dissolved  <- reconciled_map %>%
      group_by(across(c("Rec_phase2"))) %>%
      summarise() %>%
      tibble::rowid_to_column("ID") %>% 
      mutate(Area = units::set_units(st_area(.), "ha"), .after=2)

    # 6. Export results --------------------------------
    st_write(reconciled_map_dissolved, paste0(rv$wd, "/PUR_reconciliation_result_dissolved.shp"),
             driver = "ESRI Shapefile",
             append = FALSE)
    
    rv$summary_PUR <- reconciled_map_dissolved %>% 
      st_drop_geometry()
    
    
    write.csv(rv$summary_PUR, paste0(rv$wd, "/PUR_final_lookup_table.csv"))
    
    # End of the script
    end_time <- Sys.time()
    cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
    
    # 6. Generate report -------------------------
    report_params <- list(
      start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
      end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
      output_dir = rv$wd,
      summary_PUR = rv$summary_PUR,
      sa = reconciled_map_dissolved,
      dir_sa = paste0(rv$wd, "/PUR_reconciliation_result.shp"),
      dir_summary_PUR = paste0(rv$wd, "/PUR_reconciliation_lookup_table.csv")
    )
 
    report_params$total_area <- 
      rv$summary_PUR %>% pull(Area) %>% sum()
    
    # Render the R markdown report
    if (!rmarkdown::pandoc_available()) {
      Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
    }
    
    if (file.exists("02_pur2/report_template/PUR2_report.Rmd")) {
      path_report <- "02_pur2/report_template/PUR2_report.Rmd"
    } else if (file.exists("../report_template/PUR2_report.Rmd")) {
      path_report <- "../report_template/PUR2_report.Rmd"
    } else {
      error("No template file for PUR reconcile module is found.")
    }
    
    rmarkdown::render(
      input = path_report,
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