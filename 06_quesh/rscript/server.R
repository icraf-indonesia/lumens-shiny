server <- function(input, output, session) {
  # Directory selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Update reactive value when output directory is selected
  observe({
    if (!is.null(rv$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  # Display selected output directory
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
  
  # Dynamically check which user guide file exists
  output$dynamic_guide <- renderUI({
    guide_paths <- c("06_quesh/helpfile/quesh_quick_user_guide.Rmd",
                     "../helpfile/quesh_quick_user_guide.Rmd")
    
    # Find the first path that exists
    valid_path <- NULL
    for (path in guide_paths) {
      if (file.exists(path)) {
        valid_path <- path
        break
      }
    }
    
    # If a valid path is found, include it as markdown
    if (!is.null(valid_path)) {
      withMathJax(includeMarkdown(valid_path))
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
  # Create reactive values for inputs
  rv <- reactiveValues(
    rainfall_file = NULL,
    dem_file = NULL,
    sand_file = NULL,
    silt_file = NULL,
    clay_file = NULL,
    orgc_file = NULL,
    pu_file = NULL,
    c_ref_file = NULL,
    map_resolution = NULL,
    practice = NULL,
    practice_file = NULL,
    lc_t1_file = NULL,
    lc_t2_file = NULL,
    t1 = NULL,
    t2 = NULL,
    output_dir = NULL,
    report_file = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$rainfall_file <- input$rainfall_file
    rv$dem_file <- input$dem_file
    rv$sand_file <- input$sand_file
    rv$silt_file <- input$silt_file
    rv$clay_file <- input$clay_file
    rv$orgc_file <- input$orgc_file
    rv$pu_file <- input$pu_file
    rv$c_ref_file <- input$c_ref_file
    rv$map_resolution <- input$map_resolution
    if (input$practice == "yes"){
      rv$practice_file <- input$practice_file
    }
    if (input$multiseries == "two_step"){
      rv$lc_t1_file <- input$lc_t1_file
      rv$lc_t2_file <- input$lc_t2_file
      rv$t1 <- input$t1
      rv$t2 <- input$t2
    } else {
      rv$lc_t1_file <- input$lc_t1_file
      rv$t1 <- input$t1
    }
    rv$output_dir <- parseDirPath(volumes, input$output_dir)
  })
  
  # Run analysis
  observeEvent(input$run_analysis, {
    # Check if any input is missing
    missing_inputs <- c()

    if (is.null(input$output_dir) || is.null(rv$output_dir) || is.null(selected_output_dir())) {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    if (is.null(rv$rainfall_file)) {
      missing_inputs <- c(missing_inputs, "Rainfall Map")
    }
    if (is.null(rv$dem_file)) {
      missing_inputs <- c(missing_inputs, "DEM Map")
    }
    if (is.null(rv$sand_file)) {
      missing_inputs <- c(missing_inputs, "Sand Map")
    }
    if (is.null(rv$silt_file)) {
      missing_inputs <- c(missing_inputs, "Silt Map")
    }
    if (is.null(rv$clay_file)) {
      missing_inputs <- c(missing_inputs, "Clay Map")
    }
    if (is.null(rv$orgc_file)) {
      missing_inputs <- c(missing_inputs, "Soil Organic Content Map")
    }
    if (is.null(rv$pu_file)) {
      missing_inputs <- c(missing_inputs, "Planning Unit Map")
    }
    if (is.null(rv$c_ref_file)) {
      missing_inputs <- c(missing_inputs, "C Factor Table")
    }
    if (is.null(rv$map_resolution)) {
      missing_inputs <- c(missing_inputs, "Map Resolution")
    }
    if (input$practice == "yes"){
      if (is.null(rv$practice_file)) {
        missing_inputs <- c(missing_inputs, "P Factor Map")
      }
    }
    if (input$multiseries == "two_step"){
      if (is.null(rv$lc_t1_file)) {
        missing_inputs <- c(missing_inputs, "Initial Land Cover/Use Map")
      }
      if (is.null(rv$t1)) {
        missing_inputs <- c(missing_inputs, "Initial Year")
      }
      if (is.null(rv$lc_t2_file)) {
        missing_inputs <- c(missing_inputs, "Final Land Cover/Use Map")
      }
      if (is.null(rv$t2)) {
        missing_inputs <- c(missing_inputs, "Final Year")
      }

    } else {
      if (is.null(rv$lc_t1_file)) {
        missing_inputs <- c(missing_inputs, "Land Cover/Use Map")
      }
      if (is.null(rv$t1)) {
        missing_inputs <- c(missing_inputs, "Year")
      }
    }

    # If there are missing inputs, show a notification and stop
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Please upload the following inputs:", paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    withProgress(message = 'Running QuES-H Analysis', value = 0,{
      tryCatch({
        start_time <- Sys.time()
        showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
        shinyjs::disable("run_analysis")
        incProgress(0.1, detail = "Preparing data inputs")
      
        # Rename uploaded file
        lapply(list(rv$output_dir, rv$rainfall_file, rv$dem_file, rv$sand_file, rv$silt_file, rv$clay_file, rv$orgc_file, rv$pu_file, rv$c_ref_file, rv$map_resolution), req)
        rainfall_path <- rename_uploaded_file(input_file = rv$rainfall_file)
        dem_path <- rename_uploaded_file(input_file = rv$dem_file)
        sand_path <- rename_uploaded_file(input_file = rv$sand_file)
        silt_path <- rename_uploaded_file(input_file = rv$silt_file)
        clay_path <- rename_uploaded_file(input_file = rv$clay_file)
        orgc_path <- rename_uploaded_file(input_file = rv$orgc_file)
        pu_path <- rename_uploaded_file(input_file = rv$pu_file)
        c_factor_path <- rename_uploaded_file(input_file = rv$c_ref_file)

        if (input$practice == "yes"){
          req(rv$practice_file)
          p_factor_path <- rename_uploaded_file(input_file = rv$practice_file)
        }
        if (input$multiseries == "two_step"){
          req(rv$lc_t1_file)
          req(rv$t1)
          req(rv$lc_t2_file)
          req(rv$t2)
          lc1_path <- rename_uploaded_file(input_file = rv$lc_t1_file)
          lc2_path <- rename_uploaded_file(input_file = rv$lc_t2_file)
        } else {
          req(rv$lc_t1_file)
          req(rv$t1)
          lc1_path <- rename_uploaded_file(input_file = rv$lc_t1_file)
        }

        # Prepare the planning unit
        pu1 <- read_shapefile(shp_input = rv$pu_file)
        pu <- rasterise_multipolygon(sf_object = pu1, raster_res = c(rv$map_resolution, rv$map_resolution), field = paste0(colnames(st_drop_geometry(pu1[1]))))
        names(pu) <- tools::file_path_sans_ext(rv$pu_file$name[1])
        
        # Prepare R factor input
        rainfall <- rast(rainfall_path)
        rainfall <- terra::resample(rainfall, pu, method = "near")

        # Prepare K factor input
        soil_files <- list(sand_path, silt_path, clay_path, orgc_path)
        soil_stack <- lapply(soil_files, function(file) terra::resample(rast(file), pu))
        soil_stack <- do.call(c, soil_stack)
        
        # Prepare LS factor input
        dem <- rast(dem_path)
        dem <- terra::resample(dem, pu)
        
        # Prepare C factor input
        c_ref <- readr::read_csv(c_factor_path)
        
        if (input$multiseries == "two_step"){
          landcover_t1 <- rast(lc1_path)
          landcover_t2 <- rast(lc2_path)
          landcover_t1_viz <- prepare_lc_data(lc_input = rv$lc_t1_file, lookup_table = c_ref, time_point = rv$t1)
          landcover_t2_viz <- prepare_lc_data(lc_input = rv$lc_t2_file, lookup_table = c_ref, time_point = rv$t2)
          landcover_stack <- c(landcover_t1_viz, landcover_t2_viz)
        } else {
          landcover_t1 <- rast(lc1_path)
          landcover_t1_viz <- prepare_lc_data(lc_input = rv$lc_t1_file, lookup_table = c_ref, time_point = rv$t1)
        }
        
        # Prepare P factor input parameters
        if (input$practice == "yes"){
          p <- rast(p_factor_path)
          p_factor <- terra::resample(p, pu)
        } else {
          p_factor <- pu %>% classify(cbind(1:nrow(unique(pu)), 1))
        }
        
        # Run RUSLE analysis
        incProgress(0.3, detail = "Processing QuES-H analysis")
        a <- quesh_rusle_calc(rainfall = rainfall, 
                              sand = soil_stack[[1]], 
                              silt = soil_stack[[2]], 
                              clay = soil_stack[[3]], 
                              orgc = soil_stack[[4]], 
                              dem = dem, 
                              landcover_t1 = landcover_t1,
                              landcover_t2 = landcover_t2,
                              c_ref = c_ref, 
                              p_factor = p_factor,
                              multiseries = input$multiseries,
                              pu = pu)
        
        incProgress(0.5, detail = "Post processing analysis")
        # Reclassify erosion rates based on China National Standard (2008)
        breaks <- c(-Inf, 5, 25, 50, 80, 150, Inf)
        labels <- c("Slight (< 5 ton/ha/yr)", 
                    "Mild (5-25 ton/ha/yr)", 
                    "Moderate (25-50 ton/ha/yr)", 
                    "Strong (50-80 ton/ha/yr)", 
                    "Very strong (80-150 ton/ha/yr)", 
                    "Severe (> 150 ton/ha/yr)")
        rcl_matrix <- cbind(breaks[-length(breaks)], breaks[-1], 1:(length(breaks)-1))
        
        # Post Analysis Work
        if (input$multiseries == "two_step"){
          
          # Redefined the results
          erosion_t1 <- a[[1]]
          erosion_t2 <- a[[2]]
          r_factor <- a[[3]]
          k_factor <- a[[4]]
          ls_factor <- a[[5]]
          c_factor_t1 <- a[[6]]
          c_factor_t2 <- a[[7]]
          c_factor_stack <- c(c_factor_t1, c_factor_t2)
          
          # Reclassify erosion rate
          erosion_classified_t1 <- classify(erosion_t1, rcl = rcl_matrix)
          erosion_classified_t2 <- classify(erosion_t2, rcl = rcl_matrix)
          levels(erosion_classified_t1) <- data.frame(id=1:6, category=labels)
          levels(erosion_classified_t2) <- data.frame(id=1:6, category=labels)
          erosion_stack <- c(erosion_classified_t1, erosion_classified_t2)
          
          # Create dataset of erosion estimation
          erosion_db_t1 <- erosion_dataset(erosion_classified = erosion_classified_t1, map_resolution = rv$map_resolution)
          erosion_db_t2 <- erosion_dataset(erosion_classified = erosion_classified_t2, map_resolution = rv$map_resolution)
          
          erosion_db_t2 <- erosion_db_t2 %>%
            rename(
              `Area (Ha) T2` = `Area (Ha)`,
              `Percentage (%) T2` = `Percentage (%)`
            )
          
          erosion_db <- erosion_db_t1 %>%
            rename(
              `Area (Ha) T1` = `Area (Ha)`,
              `Percentage (%) T1` = `Percentage (%)`
            ) %>%
            inner_join(erosion_db_t2, by = "Class")
          
          # Calculate erosion difference between two series of time
          e_diff <- erosion_classified_t2 - erosion_classified_t1
          e_rcl_matrix <- matrix(c(-Inf, -0.0001, 1,  # Class 1: Erosion risk decrease
                                   -0.0001, 0.0001, 2,  # Class 2: No erosion risk changes
                                   0.0001, Inf, 3),   # Class 3: Erosion risk increase
                                 ncol = 3, byrow = TRUE)
          e_diff_classified <- classify(e_diff, rcl = e_rcl_matrix)
          e_labels <- c("Erosion risk decrease", "No erosion risk changes", "Erosion risk increase")
          levels(e_diff_classified) <- data.frame(id=1:3, category=e_labels)
          
          # Create erosion difference data table
          erosion_diff_db <- erosion_dataset(erosion_classified = e_diff_classified, map_resolution = rv$map_resolution)
          
          # Calculate erosion for each planning unit class
          summary_e_pu_df_t1 <- compute_erosion_per_pu(erosion_classified = erosion_classified_t1, pu = pu)
          summary_e_pu_df_t2 <- compute_erosion_per_pu(erosion_classified = erosion_classified_t2, pu = pu)
          summary_e_pu_df <- list(summary_e_pu_df_t1, summary_e_pu_df_t2)
          
          # Calculate erosion difference for each planning unit class
          summary_e_diff_pu_df <- compute_erosion_per_pu(erosion_classified = e_diff_classified, pu = pu)
          
          # Export the results
          incProgress(0.6, detail = "Exporting results")
          writeRaster(r_factor, paste0(rv$output_dir, "/r_factor.tif"), overwrite = TRUE)
          writeRaster(k_factor, paste0(rv$output_dir, "/k_factor.tif"), overwrite = TRUE)
          writeRaster(ls_factor, paste0(rv$output_dir, "/ls_factor.tif"), overwrite = TRUE)
          writeRaster(c_factor_t1, paste0(rv$output_dir, "/c_factor", paste0(rv$t1), ".tif"), overwrite = TRUE)
          writeRaster(c_factor_t2, paste0(rv$output_dir, "/c_factor", paste0(rv$t2), ".tif"), overwrite = TRUE)
          writeRaster(erosion_t1, filename = paste0(rv$output_dir, "/soil_erosion", paste0(rv$t1), ".tif"), overwrite = TRUE)
          writeRaster(erosion_t2, filename = paste0(rv$output_dir, "/soil_erosion", paste0(rv$t2), ".tif"), overwrite = TRUE)
          writeRaster(erosion_classified_t1, filename = paste0(rv$output_dir, "/soil_erosion_reclass", paste0(rv$t1), ".tif"), overwrite = TRUE)
          writeRaster(erosion_classified_t2, filename = paste0(rv$output_dir, "/soil_erosion_reclass", paste0(rv$t2), ".tif"), overwrite = TRUE)
          write.csv(erosion_db, file = paste0(rv$output_dir, "/soil_erosion", paste0(rv$t1), "-", paste0(rv$t2), ".csv"))
          write.csv(summary_e_pu_df, file = paste0(rv$output_dir, "/soil_erosion_per_planning_unit", paste0(rv$t1), "-", paste0(rv$t2), ".csv"))
          write.csv(summary_e_diff_pu_df, file = paste0(rv$output_dir, "/soil_erosion_changes_per_planning_unit", paste0(rv$t1), "-", paste0(rv$t2), ".csv"))
          
          # End of the script
          end_time <- Sys.time()
          cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
          cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
          
          # Prepare parameters for report
          report_params <- list(
            session_log = format_session_info_table(),
            start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
            end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
            output_dir = rv$output_dir,
            dem = dem,
            pu = pu,
            rainfall = rainfall,
            soil = soil_stack,
            landcover = landcover_stack,
            t1 = rv$t1,
            t2 = rv$t2,
            r = r_factor,
            k = k_factor,
            ls = ls_factor,
            c = c_factor_stack,
            p = p_factor,
            a = erosion_stack,
            e_diff = e_diff_classified,
            e_table = erosion_db,
            e_pu_df = summary_e_pu_df,
            e_diff_table = erosion_diff_db,
            e_pu_diff_table = summary_e_diff_pu_df,
            multiseries = input$multiseries,
            map_resolution = rv$map_resolution,
            rainfall_path = rainfall_path,
            dem_path = dem_path,
            sand_path = sand_path,
            silt_path = silt_path,
            clay_path = clay_path,
            orgc_path = orgc_path,
            pu_path = pu_path,
            c_factor_path = c_factor_path,
            input_lc = list(
              lc1_path = lc1_path, 
              lc2_path = lc2_path
            )
          )
          summary_data <- list(
            total_area = sum(erosion_db_t1[[2]]),
            min_erosion_t1 = minmax(erosion_t1)[1],
            max_erosion_t1 = minmax(erosion_t1)[2],
            min_erosion_t2 = minmax(erosion_t2)[1],
            max_erosion_t2 = minmax(erosion_t2)[2],
            highest_erosion_class_name_t1 = erosion_db_t1 %>% slice_max(erosion_db_t1[[2]]) %>% pull(Class) %>% as.character(),
            highest_erosion_class_name_t2 = erosion_db_t2 %>% slice_max(erosion_db_t2[[2]]) %>% pull(Class) %>% as.character(),
            highest_erosion_class_area_t1 = max(erosion_db_t1[[2]]),
            highest_erosion_class_area_t2 = max(erosion_db_t2[[2]]),
            highest_erosion_class_percentage_t1 = max(erosion_db_t1[[3]]),
            highest_erosion_class_percentage_t2 = max(erosion_db_t1[[3]]),
            severe_area_t1 = erosion_db_t1 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(2),
            severe_percentage_t1 = erosion_db_t1 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(3),
            severe_area_t2 = erosion_db_t2 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(2),
            severe_percentage_t2 = erosion_db_t2 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(3),
            erosion_increase = erosion_diff_db %>% filter(Class == "Erosion risk increase") %>% pull(2),
            erosion_decrease = erosion_diff_db %>% filter(Class == "Erosion risk decrease") %>% pull(2),
            erosion_stable = erosion_diff_db %>% filter(Class == "No erosion risk changes") %>% pull(2)
          )
          report_params$summary_data <- summary_data
        } else {
          
          # Redefined the results
          erosion_t1 <- a[[1]]
          r_factor <- a[[2]]
          k_factor <- a[[3]]
          ls_factor <- a[[4]]
          c_factor <- a[[5]]
          
          # Reclassify erosion rate
          erosion_classified_t1 <- classify(erosion_t1, rcl = rcl_matrix)
          levels(erosion_classified_t1) <- data.frame(id=1:6, category=labels)
          
          # Create dataset of erosion estimation
          erosion_db_t1 <- erosion_dataset(erosion_classified = erosion_classified_t1, map_resolution = rv$map_resolution)
          
          # Calculate erosion for each planning unit class
          summary_e_pu_df <- compute_erosion_per_pu(erosion_classified = erosion_classified_t1, pu = pu)
          
          # Export the results
          incProgress(0.6, detail = "Exporting results")
          writeRaster(r_factor, paste0(rv$output_dir, "/r_factor.tif"), overwrite = TRUE)
          writeRaster(k_factor, paste0(rv$output_dir, "/k_factor.tif"), overwrite = TRUE)
          writeRaster(ls_factor, paste0(rv$output_dir, "/ls_factor.tif"), overwrite = TRUE)
          writeRaster(c_factor, paste0(rv$output_dir, "/c_factor", paste0(rv$t1), ".tif"), overwrite = TRUE)
          writeRaster(erosion_t1, filename = paste0(rv$output_dir, "/soil_erosion", paste0(rv$t1), ".tif"), overwrite = TRUE)
          writeRaster(erosion_classified_t1, filename = paste0(rv$output_dir, "/soil_erosion_reclass", paste0(rv$t1), ".tif"), overwrite = TRUE)
          write.csv(erosion_db_t1, file = paste0(rv$output_dir, "/soil_erosion", paste0(rv$t1), ".csv"))
          write.csv(summary_e_pu_df, file = paste0(rv$output_dir, "/soil_erosion_per_planning_unit", paste0(rv$t1), ".csv"))
          
          # End of the script
          end_time <- Sys.time()
          cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
          cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
          
          # Prepare parameters for report
          report_params <- list(
            session_log = format_session_info_table(),
            start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
            end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
            output_dir = rv$output_dir,
            dem = dem,
            pu = pu,
            rainfall = rainfall,
            soil = soil_stack,
            landcover = landcover_t1_viz,
            t1 = rv$t1,
            t2 = rv$t2,
            r = r_factor,
            k = k_factor,
            ls = ls_factor,
            c = c_factor,
            p = p_factor,
            a = erosion_classified_t1,
            e_table = erosion_db_t1,
            e_pu_df = summary_e_pu_df,
            multiseries = input$multiseries,
            map_resolution = rv$map_resolution,
            rainfall_path = rainfall_path,
            dem_path = dem_path,
            sand_path = sand_path,
            silt_path = silt_path,
            clay_path = clay_path,
            orgc_path = orgc_path,
            pu_path = pu_path,
            c_factor_path = c_factor_path,
            input_lc = lc1_path
          )
          summary_data <- list(
            total_area = sum(erosion_db_t1[[2]]),
            min_erosion = minmax(erosion_t1)[1],
            max_erosion = minmax(erosion_t1)[2],
            highest_erosion_class_name = erosion_db_t1 %>% slice_max(erosion_db_t1[[3]]) %>% pull(Class) %>% as.character(),
            highest_erosion_class_area = max(erosion_db_t1[[2]]),
            highest_erosion_class_percentage = max(erosion_db_t1[[3]]),
            severe_area = erosion_db_t1 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(2),
            severe_percentage = erosion_db_t1 %>% filter(Class == "Severe (> 150 ton/ha/yr)") %>% pull(3)
          )
          report_params$summary_data <- summary_data
        }
        
        # Render the R markdown report
        incProgress(0.7, detail = "Preparing report")
        
        if (rmarkdown::pandoc_available() == FALSE) {
          Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
        }
        
        output_file <- paste0("QUES-H_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")

        if (file.exists("06_quesh/report_template/quesh_report.Rmd")) {
          path_report <- "06_quesh/report_template/quesh_report.Rmd"
        } else if (file.exists("../report_template/quesh_report.Rmd")) {
          path_report <- "../report_template/quesh_report.Rmd"
        } else {
          error("No template file for QuES-H module is found.")
        }

        rmarkdown::render(
          input = path_report,
          output_file = output_file,
          output_dir = rv$output_dir,
          params = report_params
        )
        
        rv$report_file <- paste(rv$output_dir, output_file, sep = "/")
        
      }, error = function(e) {
        cat("An error occurred:\n")
        print(e)
      }, finally = {
        cat("Script execution completed.\n")
        
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
  
  # Open output folder
  observeEvent(input$open_output_folder, {
    if (!is.null(selected_output_dir())) {
      if (.Platform$OS.type == "windows") {
        shell.exec(selected_output_dir())
      } else {
        system2("open", selected_output_dir())
      }
    }
  })
  
  # Open report
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") {
        showNotification("Opening report...", type = "message")
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