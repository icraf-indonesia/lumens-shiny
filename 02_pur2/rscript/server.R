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
    path <- "../helpfile/help.Rmd"
    if (file.exists(path)) {
      html_content <- rmarkdown::render(path, output_format = "html_fragment", quiet = TRUE)
      HTML(readLines(html_content))
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
  # Create reactive values for inputs
  rv <- reactiveValues(
    output_dir = NULL,
    report_file = NULL,
    recon_file = NULL,
    unresolved_table = NULL,
    map_resolution = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$output_dir <- parseDirPath(volumes, input$output_dir)
    rv$recon_file <- input$recon_file
    rv$unresolved_table <- input$unresolved_table
    rv$map_resolution <- input$map_resolution
  })
  
  #### Run analysis ####
  observeEvent(input$run_analysis, {
    # Check if any input is missing
    missing_inputs <- c()
    
    if (is.null(input$output_dir) || is.null(rv$output_dir) || is.null(selected_output_dir())) {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    if (is.null(rv$recon_file)) {
      missing_inputs <- c(missing_inputs, "Unresolved Case Map")
    }
    if (is.null(rv$unresolved_table)) {
      missing_inputs <- c(missing_inputs, "Unresolved Case Table")
    }
    if (is.null(rv$map_resolution)) {
      missing_inputs <- c(missing_inputs, "Map Resolution")
    }
    
    # If there are missing inputs, show a notification and stop
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Please upload the following inputs:", paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    withProgress(message = 'Processing PUR', value = 0, {
      tryCatch({
        start_time <- Sys.time()
        
        # Required data input
        req(rv$output_dir)
        req(rv$recon_file)
        req(rv$unresolved_table)
        req(rv$map_resolution)
        
        # 1. Data preparation -------------------------------------------
        shinyjs::disable("run_analysis")
        incProgress(0.1, detail = "Preparing data inputs")
        u <- rename_uploaded_file(input_file = input$unresolved_table)
        rv$map_resolution <- as.numeric(rv$map_resolution)
        sa <- read_shapefile(shp_input = rv$recon_file)
        pur_sa <- sa %>% st_as_sf() %>% st_drop_geometry()
        ref <- rasterise_multipolygon(sf_object = sa, raster_res = c(rv$map_resolution, rv$map_resolution), field = paste0(colnames(st_drop_geometry(sa[1]))))
        
        unresolved_edit <- readxl::read_xlsx(u)
        
        # select ID and Reconcile action column
        reconcile_scenario <- unresolved_edit %>% 
          dplyr::rename("ID"=1, 
                        "Reconcile Action"= which(colnames(unresolved_edit) == 'Reconcile Action')) %>% 
          dplyr::select(any_of(c("ID", "Reconcile Action")))
        
        # Unresolved cases
        unresolved_cases <- pur_sa %>%
          st_drop_geometry() %>%
          rename(ID_rec = which(colnames(pur_sa) == 'ID_rec')) %>%
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
        incProgress(0.2, detail = "Resolve Any Unresolved Cases")
        
        reconciled_map <- sa %>%
          dplyr::select(-ID_rec) %>% 
          left_join(reconcile_scenario, by = "ID") %>%
          mutate(Rec_phase2 = ifelse(!is.na(`Reconcile Action`), `Reconcile Action`, Rec_phase2)) %>%
          dplyr::select(-`Reconcile Action`)
        
        # 3. Calculate area --------------------------------
        incProgress(0.3, detail = "Calculating Area")
        
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
        incProgress(0.4, detail = "Exporting Results")
        
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
        
        # Write raster output
        template_raster <- rast(ext(reconciled_map_dissolved), resolution = rv$map_resolution, 
                                crs = crs(reconciled_map_dissolved))
        reconciled_map_raster <- rasterize(reconciled_map_dissolved, template_raster, 
                                           field = "ID")
        writeRaster(reconciled_map_raster, 
                    filename = paste0(rv$output_dir, "/PUR_reconciliation_result.tif"),  
                    overwrite = TRUE) 
        
        # 6. Export results --------------------------------
        st_write(reconciled_map_dissolved, paste0(rv$output_dir, "/PUR_reconciliation_result_dissolved.shp"),
                 driver = "ESRI Shapefile",
                 append = FALSE)
        
        summary_PUR <- reconciled_map_dissolved %>% 
          st_drop_geometry()
        
        write.csv(summary_PUR, paste0(rv$output_dir, "/PUR_final_lookup_table.csv"))
        
        # End of the script
        end_time <- Sys.time()
        cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
        cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
        
        # 6. Generate report -------------------------
        incProgress(0.5, detail = "Preparing Report")
        unresolved_shp_path <- rename_uploaded_file(input_file = input$recon_file)
        
        report_params <- list(
          session_log = format_session_info_table(),
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          output_dir = rv$output_dir,
          summary_PUR = summary_PUR,
          sa = reconciled_map_dissolved,
          unresolved_shp_path = unresolved_shp_path,
          unresolved_table_path = u, #unresolved_table_path,
          recon_result_shp_path = "PUR_reconciliation_result.shp",
          recon_result_table_path = "PUR_reconciliation_lookup_table.csv",
          recon_result_shp_dis_path = "PUR_reconciliation_result_dissolved.shp",
          recon_result_raster_path = "PUR_reconciliation_result.tif",
          final_lookup_table_path = "PUR_final_lookup_table.csv"
        )
        
        report_params$total_area <- 
          summary_PUR %>% pull(Area) %>% sum()
        
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
        
        # Post Analysis
        output$status_messages <- renderText("Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder or view the report.")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification(id = "running_notification")
        shinyjs::enable("run_analysis")
        showNotification("Analysis completed successfully!", type = "message")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        output$success_message <- renderText(NULL)
        removeNotification(id = "running_notification")
        shinyjs::enable("run_analysis")
        showNotification("Error in analysis. Please check the error messages.", type = "error")
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