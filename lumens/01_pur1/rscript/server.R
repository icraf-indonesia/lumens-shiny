server <- function(input, output, session) {
  # Directory selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, 'output_dir', roots = volumes, session = session)
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
      "01_pur1/helpfile/help.Rmd",
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
    ref_map = NULL,
    ref_class = NULL,
    ref_mapping = NULL,
    pu_units = NULL,
    map_resolution = NULL,
    database_unresolved_out = NULL,
    pur_unresolved_vector = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$output_dir <- parseDirPath(volumes, input$output_dir)
    rv$ref_map <- input$ref_map
    rv$ref_class <- input$ref_class
    rv$ref_mapping <- input$ref_mapping
    rv$pu_units <- input$pu_units
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
    # Check if any input is missing
    missing_inputs <- c()
    
    if (is.null(input$output_dir) || is.null(rv$output_dir) || is.null(selected_output_dir())) {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    if (is.null(rv$ref_map)) {
      missing_inputs <- c(missing_inputs, "Reference Map")
    }
    if (is.null(rv$ref_class)) {
      missing_inputs <- c(missing_inputs, "Reference Class")
    }
    if (is.null(rv$ref_mapping)) {
      missing_inputs <- c(missing_inputs, "Reference Class of Reference Map")
    }
    if (is.null(rv$pu_units)) {
      missing_inputs <- c(missing_inputs, "Planning Unit List Data")
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
        
        # Require each input
        req(rv$output_dir,)
        req(rv$ref_map)
        req(rv$ref_class)
        req(rv$ref_mapping)
        req(rv$pu_units)
        req(rv$map_resolution)
        
        # Data preparation
        incProgress(0.1, detail = "Preparing data")
        
        shinyjs::disable("run_analysis")
        rv$map_resolution <- as.numeric(rv$map_resolution)
        ref_data <- read_shapefile(shp_input = rv$ref_map)
        ref <- rasterise_multipolygon(sf_object = ref_data, raster_res = c(rv$map_resolution, rv$map_resolution), field = paste0(colnames(st_drop_geometry(ref_data[1]))))
        
        # Determine Spatial Resolution
        if (grepl("\\+units=m", as.character(st_crs(ref)$proj4string))) {
          Spat_res <- res(ref)[1] * res(ref)[2] / 10000
          message <- paste("Raster maps have", Spat_res, "Ha spatial resolution, PUR will automatically generate data in Ha unit"
          )
        } else if (grepl("\\+proj=longlat", as.character(st_crs(ref)$proj4string))) {
          Spat_res <- res(ref)[1] * res(ref)[2] * (111319.9 ^ 2) / 10000
          message <- paste("Raster maps have", Spat_res, "Ha spatial resolution, PUR will automatically generate data in Ha unit"
          )
        } else {
          statuscode <- 0
          statusmessage <- "Raster map projection is unknown"
          statusoutput <- data.frame(statuscode = statuscode, statusmessage = statusmessage)
          stop(statusmessage)
        }
        
        ref.name <- names(ref)
        
        # Table preparation
        lookup_ref <- ref_data %>% st_drop_geometry()
        colnames(lookup_ref)[ncol(lookup_ref)] <- "REFERENCE"
        ref.name <- names(ref)
        
        tabel_acuan <- read.table(rv$ref_class$datapath, header = FALSE, sep = ",", skip = 1) %>%
          setNames(c("acuan_kelas", "acuan_kode"))
        
        tabel_mapping <- read.table(rv$ref_mapping$datapath, header = FALSE, sep = ",", skip = 1) %>%
          setNames(c("REFERENCE", "IDS")) %>% left_join(lookup_ref, by = "REFERENCE")
        
        if ("COUNT" %in% colnames(tabel_mapping)) {
          tabel_mapping <- tabel_mapping %>% select(-COUNT)
        }
        
        tabel_mapping <- tabel_mapping %>%rename(IDO = ID)
        
        # Planning unit data preparation
        pu_list <- read.table(rv$pu_units$datapath, header = FALSE, sep = ",", skip = 1)
        n_pu_list <- nrow(pu_list)
        pu_lut_list <- list()
        cmd <- paste()
        command1 <- vector("list", n_pu_list)
        central_attr <- NULL
        
        # Iterate over each planning unit in the list
        for (i in 1:n_pu_list) {
          data_name <- as.character(pu_list[i, 1])
          lut_table <- pu_list[i, 5]
          pu_vector <- st_read(file.path(dirname(lut_table), paste0(data_name, ".shp")))
          pu_raster <- rasterise_multipolygon(sf_object = pu_vector, raster_res = c(rv$map_resolution, rv$map_resolution), paste0(colnames(st_drop_geometry(pu_vector[1]))))
          print(pu_raster)
          pu_lut_list[[i]] <- lut_table
          central_attr <- append(central_attr, data_name)
          pu_raster[is.na(pu_raster)] <- 0
          # pu_raster <- reclassify(pu_raster, cbind(255, 0))  # Reclassify 255 values to 0
          names(pu_raster) <- data_name
          j <- n_pu_list + 1 - i
          assign(paste0("R", i), pu_raster * (100 ^ (j)))
          # Build a command for further calculations
          cmd <- paste0(cmd, "R", i, "+")
          command1[[i]] <- pu_raster
        }
        
        # Calculate a reference value and store in an 'R' variable
        ref.number <- n_pu_list + 1
        R_ref <- ref * 1
        assign(paste0("R", ref.number), R_ref)
        
        cmd <- paste0(cmd, "R", ref.number)
        command1[[ref.number]] <- R_ref
        PUR_stack <- rast(command1)
        
        # 3. Create raster attribute table -------------------------
        incProgress(0.4, detail = "Creating raster attribute table")
        # Create and process the raster attribute table
        eval(parse(text = (paste("PUR<-", cmd, sep = ""))))
        PUR_raster <- raster(PUR)
        PUR <- ratify(PUR_raster, count = TRUE)
        PUR_db <- levels(PUR)[[1]]
        
        # reclassify attribute ID
        ORI_ID <- PUR_db$ID
        NEW_ID <- seq(nrow(PUR_db))
        rclmat <- cbind(as.matrix(ORI_ID), as.matrix(NEW_ID))
        PUR <- reclassify(PUR, rclmat)
        PUR <- ratify(PUR, count = TRUE)
        
        # extract all ids
        PUR_db$NEW_ID <- NEW_ID
        PUR_db$TEMP_ID <- PUR_db[, 1]
        k <- 0
        
        # Loop to extract variables from the TEMP_ID using modular arithmetic
        while (k < ref.number) {
          eval(parse(text = (paste("PUR_db$Var", n_pu_list - k, "<-PUR_db$TEMP_ID %% 100", sep = ""))))
          # Extract the last two digits of TEMP_ID and store in the respective column
          PUR_db$TEMP_ID <- floor(PUR_db$TEMP_ID / 100)  # Divide TEMP_ID by 100 and discard the remainder
          k = k + 1
        }
        
        PUR_db$TEMP_ID <- NULL
        
        # 4. Conduct reconciliation ---------------------------------
        incProgress(0.5, detail = "Conducting reconciliation")
        colnames(PUR_db)[1] <- "unique_id"
        colnames(PUR_db)[2] <- "Freq"
        colnames(PUR_db)[4] <- ref.name
        
        m <- 0
        for (l in 1:n_pu_list) {
          pu_data <- as.character(pu_list[l, 2])
          var_num <- n_pu_list + 4 - m
          # Check if the column exists in PUR_db
          if (var_num <= ncol(PUR_db)) {
            # Use the actual raster object stored in command1 to get the name
            colnames(PUR_db)[var_num] <- names(command1[[l]])
          } else {
            warning(paste("Column", var_num, "does not exist in PUR_db"))
          }
          m <- m + 1
        }
        
        # combining tabel mapping and pur attribute table
        colnames(tabel_mapping)[3] <- ref.name
        
        # Prepare data for reconciliation
        PUR_dbmod <- merge(PUR_db, tabel_mapping, by = ref.name)
        
        for (j in 1:(n_pu_list)) {
          data_name <- as.character(pu_list[j, 1])
          data_value <- pu_list[j, 3]
          Type <- as.character(pu_list[j, 4])
          
          # Update the column
          idx_non_zero <- PUR_dbmod[[data_name]] != 0
          PUR_dbmod[idx_non_zero, data_name] <- data_value
          
          # Create new column
          new_col_name <- paste("cek", j, sep = "")
          
          # Reconciliation scenario:
          if (1 == Type) {
            # Additional Scenario
            PUR_dbmod[[new_col_name]] <- as.numeric(PUR_dbmod[[data_name]] != 0)
          } else {
            # Reconcile scenario
            PUR_dbmod[[new_col_name]] <- as.numeric(PUR_dbmod[[data_name]] == PUR_dbmod$IDS)
          }
          
        }
        
        # check planning unit which is overlapped refer to reference data
        #   if there is no overlapping data then attribute equals to reference,
        #   else attribute would become unresolved
        command4 <- paste()
        # Iterate over each planning unit in the list
        for (p in 1:n_pu_list) {
          if (p != n_pu_list) {
            # If not the last planning unit
            eval(parse(text = (paste("command4 <- paste(command4, ", '"cek', p, '+', '")', sep = ""))))
            # Append "cekX+" to the command4 string, where X is the planning unit index
          } else {
            # If the last planning unit
            eval(parse(text = (paste("command4 <- paste(command4, ", '"cek', p, '")', sep = ""))))
            # Append "cekX" to the command4 string for the last planning unit
          }
        }
        
        # Calculate the value of 'reconcile1' column based on 'command4' string
        PUR_dbmod <- within(PUR_dbmod, {
          reconcile1 <- eval(parse(text = (command4)))
        })
        # This evaluates the concatenated command4 string as R code and assigns the result to the 'reconcile1' column
        
        # Calculate the 'reconcile_attr' column based on 'reconcile1' values
        PUR_dbmod <- within(PUR_dbmod, {
          reconcile_attr <- ifelse(reconcile1 == 0, as.character(REFERENCE), "unresolved")
        })
        # If 'reconcile1' is 0, set 'reconcile_attr' to the value of 'REFERENCE', otherwise set it to "unresolved"
        command5 <- paste()  # Initialize the command5 variable as an empty character string
        
        # Iterate over each planning unit in the list
        for (r in 1:n_pu_list) {
          if (r != n_pu_list) {
            # If not the last planning unit
            eval(parse(text = (paste("command5 <- paste(command5, ", '"(cek",', r, ',"*",', r, ', ")+", sep="")', sep = ""))))
            # Append "(cekX*X)+" to the command5 string, where X is the planning unit index
          } else {
            # If the last planning unit
            eval(parse(text = (paste( "command5 <- paste(command5, ", '"(cek",', r, ',"*",', r, ', ")", sep="")', sep = ""))))
            # Append "(cekX*X)" to the command5 string for the last planning unit
          }
        }
        
        # Calculate the 'reconcile_attr2' column based on conditions and 'command5' string
        PUR_dbmod <- within(PUR_dbmod, {
          reconcile_attr2 <- ifelse(reconcile1 == 1, eval(parse(text = (command5))), 100)
        })
        
        # 5. Create and process central attributes -----------------
        incProgress(0.6, detail = "Processing central attributes")
        central_attr <- central_attr %>%
          as_tibble() %>%
          mutate(numb_ca = row_number()) %>%
          dplyr::select(numb_ca, everything())
        central_attrmod <- central_attr %>%
          rename(Rec_phase1 = 2, reconcile_attr2 = 1)
        add_22 <- tibble(Rec_phase1 = "none", reconcile_attr2 = 100)
        
        # Convert Rec_phase1 column to character in central_attrmod
        central_attrmod <- central_attrmod %>%
          mutate(Rec_phase1 = as.character(Rec_phase1))
        
        # Combine the data frames and convert reconcile_attr2 column to numeric
        central_attrmod <- bind_rows(central_attrmod, add_22) %>%
          mutate(reconcile_attr2 = as.numeric(reconcile_attr2))
        
        # 6. Finalize and save result ----------------
        incProgress(0.7, detail = "Finalizing results")
        
        # Finalize reconciliation results
        PUR_dbfinal <- PUR_dbmod %>%
          inner_join(central_attrmod, by = "reconcile_attr2") %>%
          mutate(Rec_phase1 = ifelse(Rec_phase1 == "none", as.character(reconcile_attr), as.character(Rec_phase1))) %>%
          mutate(Rec_phase1b = ifelse(reconcile_attr == "unresolved", "unresolved_case", as.character(Rec_phase1))
          )
        PUR_dbfinal2 <- PUR_dbfinal %>%
          dplyr::select(NEW_ID, Rec_phase1b) %>%
          setNames(c("ID", "Rec_phase1b"))
        levels(PUR) <- merge(levels(PUR), PUR_dbfinal2 %>% distinct(ID, Rec_phase1b), by = "ID")
        
        # Merge classes based on Rec_phase 1b column
        # 1. Extract levels of the raster into a data frame
        df_levels <- as.data.frame(levels(PUR)[[1]])
        # 2. Assign new unique IDs for each category in Rec_phase1b
        # We exclude 'unresolved_case' here as mentioned
        unique_categories <- unique(df_levels$Rec_phase1b[df_levels$Rec_phase1b != "unresolved_case"])
        category_mapping <- setNames(1:length(unique_categories), unique_categories)
        df_levels$ID_rec <- ifelse(df_levels$Rec_phase1b == "unresolved_case", df_levels$ID, category_mapping[df_levels$Rec_phase1b])
        PUR_dbfinal <- bind_cols(PUR_dbfinal, df_levels["ID_rec"])
        
        # Prepare a new variable as a PUR vector output
        pur_unresolved <- rast(PUR)
        pur_reconciled <- PUR_dbfinal %>%
          as_tibble() %>%
          select(ID = NEW_ID, REFERENCE, Rec_phase1, Rec_phase1b, ID_rec) %>%
          arrange(ID)
        joined_data <- levels(pur_unresolved)[[1]] %>%
          left_join(pur_reconciled, by = "ID") %>%
          select(ID, REFERENCE, Rec_phase1, Rec_phase1b, ID_rec)
        levels(pur_unresolved)[[1]] <- joined_data
        
        # Convert the raster to polygons
        rv$pur_unresolved_vector <- as.polygons(pur_unresolved)
        rv$pur_unresolved_vector$ID <- joined_data$ID
        col_order <- c("ID", "REFERENCE")
        rv$pur_unresolved_vector <- rv$pur_unresolved_vector[, col_order]
        rv$pur_unresolved_vector$Reference <- joined_data$REFERENCE
        rv$pur_unresolved_vector$Rec_phase1 <- joined_data$Rec_phase1
        rv$pur_unresolved_vector$Rec_phase2 <- joined_data$Rec_phase1b
        rv$pur_unresolved_vector$ID_rec <- joined_data$ID_rec
        
        # Filter out 'unresolved_case'
        filtered_df <- df_levels |>
          subset(Rec_phase1b != "unresolved_case")
        
        # Group by 'Rec_phase1b' and summarise
        merged_df <- filtered_df |>
          group_by(ID_rec, Rec_phase1b) |>
          summarise(Total_COUNT = sum(COUNT)) |>
          ungroup() |>
          mutate(Rec_phase1b = as.character(Rec_phase1b))
        # Add back 'unresolved_case' with its COUNT
        unresolved_cases <- df_levels |>
          filter(Rec_phase1b == "unresolved_case") |>
          group_by(ID_rec) |>
          summarise(Rec_phase1b = "unresolved_case",
                    Total_COUNT = sum(COUNT)) |>
          ungroup() |>
          mutate(Rec_phase1b = as.character(Rec_phase1b))
        
        db_final2 <- rbind(merged_df, unresolved_cases)
        colnames(db_final2) <- c("ID", "Rec_phase1b" , "COUNT")
        
        # write PUR reconciliation phase 1 raster
        write.dbf(PUR_dbfinal, paste0(rv$output_dir, "PUR-build_database.dbf"))
        crs(PUR) <- crs(ref_data)
        writeRaster(PUR, filename = paste0(rv$output_dir, "PUR_first_phase_result"), format = "GTiff", overwrite = TRUE)
        
        #=Save PUR final database and unresolved case(s)
        database_unresolved <- subset(PUR_dbfinal, Rec_phase1b == "unresolved_case") |> dplyr::select(-ID_rec)
        
        # save final database for viz
        #database_final <- as.data.frame(levels(PUR))
        #database_final$COUNT_ha <- database_final$COUNT * Spat_res
        data_attribute <- db_final2 %>% dplyr::select(-COUNT)
        missing_strings <- setdiff(unique(PUR_dbfinal[["Rec_phase1"]]), data_attribute$Rec_phase1b)
        
        # Append missing strings to the data_attribute tibble
        if (length(missing_strings) > 0) {
          new_IDs <- max(data_attribute$ID) + seq_along(missing_strings)
          new_rows <- tibble(ID = new_IDs, Rec_phase1b = missing_strings)
          data_attribute <- bind_rows(data_attribute, new_rows)
        }
        
        # Export 
        crs(rv$pur_unresolved_vector) <- crs(ref_data)
        writeVector(rv$pur_unresolved_vector, filename = file.path(rv$output_dir, "/PUR_first_phase_result.shp"), overwrite = TRUE)
        write.table(data_attribute, paste0(rv$output_dir, "/PUR_attribute.csv"), quote = FALSE, row.names = FALSE, sep = ",")
        #PUR_dbfinal <- PUR_dbfinal |> select(-ID_rec)
        write.table(PUR_dbfinal, paste0(rv$output_dir, "/PUR_dbfinal.csv"), quote = FALSE, row.names = FALSE, sep = ",")
        
        # 7. Handle unresolved case ------------------
        incProgress(0.8, detail = "Handling unresolved cases")
        # Process and save unresolved cases if any
        if (nrow(unresolved_cases) != 0) {
          len <- nrow(database_unresolved)
          for (r in 1:n_pu_list) {
            pu_data <- as.character(pu_list[r, 2])
            word1 <- paste0("cek", r)
            word2 <- paste0("PU_", r)
            database_unresolved[[word2]] <- "NULL"
            database_unresolved[[word2]] <- ifelse(database_unresolved[[word1]] > 0, names(command1[[r]]), "-")
          }
          
          numberx <- ncol(database_unresolved)
          numbery <- numberx - n_pu_list
          rv$database_unresolved_out <- database_unresolved[, c(numbery:numberx)]
          dat1 <- data.frame(ID = database_unresolved$NEW_ID)
          dat2 <- data.frame(COUNT = database_unresolved$Freq)
          dat3 <- data.frame(REFERENCE = database_unresolved$REFERENCE)
          rv$database_unresolved_out <- cbind(dat1, rv$database_unresolved_out, dat3, dat2)
          database_unresolved_out1 <- rv$database_unresolved_out
          database_unresolved_out1$'Reconcile Action' <- "unresolved_case"
          
          # Create the workbook
          database_unresolved_out_wb = createWorkbook()
          
          # Add worksheets
          addWorksheet(database_unresolved_out_wb, "PUR_unresolved_case")
          writeData(database_unresolved_out_wb, sheet = "PUR_unresolved_case", x = as_tibble(database_unresolved_out1), startCol = 1)
          addWorksheet(database_unresolved_out_wb, "drop-down_attribute", visible = FALSE)
          pur_attribute_df <- data_attribute[data_attribute$Rec_phase1b != "unresolved_case", -1]
          names(pur_attribute_df)[1] <- "Reconcile Action"
          new_row <- as.data.frame(matrix("unresolved_case",nrow = 1,ncol = ncol(pur_attribute_df)))
          colnames(new_row) <- colnames(pur_attribute_df)
          pur_attribute_df <- rbind(pur_attribute_df, new_row) %>% mutate(`Reconcile Action` = as.factor(`Reconcile Action`))
          
          #pur_attribute_df$`Reconcile Action`[1] <- "unresolved_case"
          writeData(database_unresolved_out_wb,sheet = "drop-down_attribute", x = as_tibble(pur_attribute_df), startCol = 1)
          
          # Add dropdown to Excel workbook
          dataValidation(
            wb = database_unresolved_out_wb,
            sheet = "PUR_unresolved_case",
            cols = 9,
            rows = 2:(1 + nrow(database_unresolved_out1)),
            type = "list",
            value = paste0(
              "'drop-down_attribute'!$A$2:$A$",
              nrow(pur_attribute_df) + 1),
            allowBlank = TRUE
          )
          # Save the workbook
          saveWorkbook(database_unresolved_out_wb, paste0(rv$output_dir, "/PUR_unresolved_case.xlsx"), overwrite = TRUE)
        } else {
          database_unresolved_out1 <- tibble("Reconciliation result" = "There are no unresolved areas in this analysis session")
        }
        
        # End of the script
        end_time <- Sys.time()
        cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
        cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
        
        # 8. Prepare parameters for report -------------------------
        incProgress(0.9, detail = "Preparing report")
        
        # Rename file path for report
        ref_path <- rename_uploaded_file(input$ref_map)
        ref_class_path <- rename_uploaded_file(input$ref_class)
        ref_mapping_path <- rename_uploaded_file(input$ref_mapping)
        pu_list_path <- rename_uploaded_file(input$pu_units)
        
        report_params <- list(
          session_log = format_session_info_table(),
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          output_dir = rv$output_dir,
          PUR_stack = PUR_stack,
          db_final2 = db_final2,
          database_unresolved_out = rv$database_unresolved_out,
          pur_unresolved_vector = rv$pur_unresolved_vector,
          inputs = list(
            ref = ref,
            ref_class = tabel_acuan,
            ref_mapping = lookup_ref,
            pu_lut_list = pu_lut_list
          ),
          ref_path = ref_path,
          ref_class_path = ref_class_path,
          ref_mapping_path =  ref_mapping_path,
          pu_list_path = pu_list_path,
          dir_PURdbfinal = "PUR-build_database.dbf",
          dir_UnresolvedCase = "PUR_unresolved_case.xlsx",
          dir_PUR1shp = "PUR_first_phase_result.shp"
        )
        
        # Prepare summary data for the report
        summary_data <- list(
          total_area = sum(db_final2$COUNT) * Spat_res,
          resolved_area = sum(db_final2$COUNT[db_final2$Rec_phase1b != "unresolved_case"]) * Spat_res,
          unresolved_area = sum(db_final2$COUNT[db_final2$Rec_phase1b == "unresolved_case"]) * Spat_res,
          resolved_percentage = (sum(db_final2$COUNT[db_final2$Rec_phase1b != "unresolved_case"]) / sum(db_final2$COUNT)) * 100,
          unresolved_percentage = (sum(db_final2$COUNT[db_final2$Rec_phase1b == "unresolved_case"]) / sum(db_final2$COUNT)) * 100
        )
        
        report_params$summary_data <- summary_data
        
        # Render the R markdown report
        if (!rmarkdown::pandoc_available()) {
          Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
        }
        
        output_file <- "PUR_build_report.html"
        
        if (file.exists("01_pur1/report_template/PUR1_report.Rmd")) {
          path_report <- "01_pur1/report_template/PUR1_report.Rmd"
        } else if (file.exists("../report_template/PUR1_report.Rmd")) {
          path_report <- "../report_template/PUR1_report.Rmd"
        } else {
          error("No template file for PUR build module is found.")
        }
        
        rmarkdown::render(
          input = path_report,
          output_file = output_file,
          output_dir = rv$output_dir,
          params = report_params
        )
        
        rv$report_file <- paste(rv$output_dir, output_file, sep = "/")
        
        showNotification("Analysis completed successfully!", type = "message")
        # After successful completion
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification(id = "running_notification")
        shinyjs::enable("run_analysis")
        
      }, error = function(e) {
        showNotification(paste("An error occurred:", e$message), type = "error")
      }, finally = {
        removeNotification(id = "running_notification")
        shinyjs::enable("run_analysis")
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