server <- function(input, output, session) {
  # Directory selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'output_dir', roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Update reactive value when output directory is selected
  observe({
    if (!is.null(rv$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  output$selected_dir <- renderText({
    if (!is.null(selected_output_dir())) {
      paste("Selected output directory:", selected_output_dir())
    } else {
      "No output directory selected"
    }
  })
  
  output$print_output_dir <- renderPrint({
    if (!is.null(selected_output_dir())) {
      cat(paste(selected_output_dir()))
    } else {
      cat("No output directory selected")
    }
  })
  
  output$user_guide <- renderUI({
    guide_paths <- c(
      "02_pur2/helpfile/help.Rmd",
      "../helpfile/help.Rmd"
    )
    
    for (path in guide_paths) {
      if (file.exists(path)) {
        html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
        return(HTML(readLines(html_content)))
      }
    }
    
    HTML("<p>User guide file not found.</p>")
  })
  
  # Create reactive values for inputs
  rv <- reactiveValues(
    output_dir = NULL,
    report_file = NULL,
    recon_file = NULL,
    unresolved_table = NULL,
    map_resolution = NULL,
    summary_PUR = NULL,
    u = NULL,
    ref = NULL,
    sa = NULL,
    pur_sa = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$output_dir <- parseDirPath(volumes, input$output_dir)
    rv$recon_file <- input$recon_file
    rv$unresolved_table <- input$unresolved_table
    rv$map_resolution <- input$map_resolution
  })
  
  # Set working directory
  wd <- getwd()
  wd_lumens <- sub("(.*lumens-shiny).*", "\\1", wd)
  
  if (wd != wd_lumens) {
    setwd(wd_lumens)
  }
  
  #### Run analysis ####
  observeEvent(input$run_analysis, {
    shinyjs::disable("run_analysis")
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    withProgress(message = 'Processing PUR', value = 0, {
      tryCatch({
      start_time <- Sys.time()

      # 1. Data preparation -------------------------------------------
      
      incProgress(0.2, detail = "Preparing data inputs")
      rv$u <- input$unresolved_table
      rv$map_resolution <- as.numeric(rv$map_resolution)
      rv$sa <- read_shapefile(shp_input = rv$recon_file)
      rv$pur_sa <- rv$sa %>% st_as_sf() %>% st_drop_geometry()
      rv$ref <- rasterise_multipolygon(sf_object = rv$sa, raster_res = c(rv$map_resolution, rv$map_resolution), field = paste0(colnames(st_drop_geometry(rv$sa[1]))))
  
      unresolved_edit <- readxl::read_xlsx(rv$u$datapath)
      
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
      
      # 2. Resolve any unresolved cases --------------------------------
      incProgress(0.6, detail = "Resolve Any Unresolved Cases")
      
      reconciled_map <- rv$sa %>%
        dplyr::select(-ID_rec) %>% 
        left_join(reconcile_scenario, by = "ID") %>%
        mutate(Rec_phase2 = ifelse(!is.na(`Reconcile Action`), `Reconcile Action`, Rec_phase2)) %>%
        select(-`Reconcile Action`)
      
      # 3. Calculate area --------------------------------
      incProgress(0.7, detail = "Calculating Area")
      
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
      incProgress(0.8, detail = "Exporting Results")
      
      st_write(reconciled_map, paste0(rv$output_dir, "/PUR_reconciliation_result.shp"),
               driver = "ESRI Shapefile",
               append = FALSE)
      
      reconciled_map %>% 
        st_drop_geometry() %>% 
        write.csv(paste0(rv$output_dir, "/PUR_reconciliation_lookup_table.csv"))
        
      # 5. Disolve unique cases after reconciliation --------------------------------
      reconciled_map_dissolved  <- reconciled_map %>%
        group_by(across(c("Rec_phase2"))) %>%
        summarise() %>%
        tibble::rowid_to_column("ID") %>% 
        mutate(Area = units::set_units(st_area(.), "ha"), .after=2)
  
      # 6. Export results --------------------------------
      st_write(reconciled_map_dissolved, paste0(rv$output_dir, "/PUR_reconciliation_result_dissolved.shp"),
               driver = "ESRI Shapefile",
               append = FALSE)
      
      rv$summary_PUR <- reconciled_map_dissolved %>% 
        st_drop_geometry()
      
      write.csv(rv$summary_PUR, paste0(rv$output_dir, "/PUR_final_lookup_table.csv"))
      
      # End of the script
      end_time <- Sys.time()
      cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
      cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
      
      # 6. Generate report -------------------------
      incProgress(1, detail = "Preparing Report")
      
      report_params <- list(
        start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
        end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
        output_dir = rv$output_dir,
        summary_PUR = rv$summary_PUR,
        sa = reconciled_map_dissolved
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
        output_dir = rv$output_dir,
        params = report_params
      )
     
      rv$report_file <- paste(rv$output_dir, "PUR_reconcile_report.html", sep = "/")
      
      showNotification("Analysis completed successfully!", type = "message")
      # After successful completion
      shinyjs::show("open_output_folder")
      shinyjs::show("open_report")
      
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error")
      }, finally = {
        shinyjs::enable("process")
      })
    })
  })
  
  output$selected_directory <- renderText({
    rv$output_dir <- parseDirPath(volumes, input$output_dir)
    if (length(rv$output_dir) == 0) {
      return()
    } else {
      paste0("Selected output directory: ", rv$output_dir)
    }
  })
  
  # Open Output Folder button observer
  observeEvent(input$open_output_folder, {
    if (!is.null(rv$output_dir) && dir.exists(rv$output_dir)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$output_dir)
      } else {
        system2("open", args = rv$output_dir)
      }
    } else {
      showNotification("Output directory not found", type = "error")
    }
  })
  
  # Open Report button observer
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$report_file)
      } else {
        system2("open", args = rv$report_file)
      }
    } else {
      showNotification("Report file not found", type = "error")
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # shinyjs::delay(1000, stopApp())
  })
}