# source('../../helper.R')
source('function_ques_c.R')

install_load(
  "terra",
  "shiny",
  "shinyFiles",
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
  "remotes",
  "shinyjs",
  "rmarkdown",
  "bslib",
  "shinyalert",
  "data.table",
  "magrittr"
)

if (!("LUMENSR" %in% rownames(installed.packages()))) {
  install_github("icraf-indonesia/LUMENSR", force = T)
  do.call("library", list("LUMENSR"))
}

library(LUMENSR)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  # extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  titlePanel("QuES-C Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lc_t1_path", "Initial Land Cover/Use", accept = c("image/tiff"), placeholder = "input raster (.tif)"),
      textInput("t1", "Initial Year", value = "2000"),
      fileInput("lc_t2_path", "Final Land Cover/Use", accept = c("image/tiff"), placeholder = "input raster (.tif)"),
      textInput("t2", "Final Year", value = "2020"),
      fileInput("c_lookup_path", "Land Cover/Use Carbon Stock Table", accept = c(".csv"), placeholder = "input table (.csv)"),
      fileInput("admin_z_path",
                "Planning Unit",
                accept = c(".shp", ".dbf", ".shx", ".prj"),
                multiple = T,
                placeholder = "input shapefiles (.shp, .dbf, .shx, .prj)"),

      # Select Peat Decomposition Option
      radioButtons("peat_decomposition", "Include Peat Decomposition?",
                   choices = c("Yes" =  "Yes", "No" = "No"),
                   selected = "No"),
      conditionalPanel(
        condition = "input.peat_decomposition == 'Yes'",
        fileInput("peat_map_path",
                  "Peatland Map",
                  accept = c(".shp", ".dbf", ".shx", ".prj"),
                  multiple = T,
                  placeholder = "input shapefiles (.shp, .dbf, .shx, .prj)"),
        fileInput("peat_emission_factor_table_path", "Peat Emission Factor Table", accept = c(".csv"), placeholder = "input table (.csv)")
      ),
      conditionalPanel(
        condition = "input.peat_decomposition == 'No'",
        HTML("<p><i>Calculate emission from mineral land only</i></p>")
      ),

      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select output directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("processQUESC", "Run QuES-C Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
          actionButton("open_output_folder", "Open Output Folder",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          actionButton("returnButton", "Return to Main Menu",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", includeMarkdown("../helpfile/quesc_help.md")),
        tabPanel("Log",
                 textOutput("selected_directory"),         
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  
  # Directory selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Update reactive value when output directory is selected
  observe({
    if (!is.null(input$output_dir)) {
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
    guide_paths <- c("04_quesc/helpfile/quesc_help.md",
                     "../helpfile/quesc_help.md")
    
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
    output_dir = NULL,
    report_file = NULL,
    lc_t1_path = NULL,
    lc_t2_path = NULL,
    admin_z_path = NULL,
    c_lookup_path = NULL,
    peat_map_path = NULL,
    peat_emission_factor_table_path = NULL,
    peat_decomposition = NULL,
    t1 = NULL,
    t2 = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$output_dir = input$output_dir
    rv$report_file = input$report_file
    rv$lc_t1_path = input$lc_t1_path
    rv$lc_t2_path = input$lc_t2_path
    rv$admin_z_path = input$admin_z_path
    rv$c_lookup_path = input$c_lookup_path
    rv$t1 = input$t1
    rv$t2 = input$t2
    if (input$peat_decomposition == "Yes"){
      rv$peat_map_path = input$peat_map_path
      rv$peat_emission_factor_table_path = input$peat_emission_factor_table_path
    }
    rv$output_dir = parseDirPath(volumes, input$output_dir)
  })
  
  #### Do the calculation and store it to the markdown content ####
  observeEvent(input$processQUESC, {
    # Check if any input is missing
    missing_inputs <- c()
    if (is.null(input$output_dir) || is.null(rv$output_dir) || is.null(selected_output_dir())) {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    if (is.null(rv$lc_t1_path)) {
      missing_inputs <- c(missing_inputs, "Initial Land Cover/Use Map")
    }
    if (is.null(rv$lc_t2_path)) {
      missing_inputs <- c(missing_inputs, "Final Land Cover/Use Map")
    }
    if (is.null(rv$admin_z_path)) {
      missing_inputs <- c(missing_inputs, "Planning Unit Map")
    }
    if (is.null(rv$c_lookup_path)) {
      missing_inputs <- c(missing_inputs, "Land Cover/Use Carbon Stock Table")
    }
    if (is.null(rv$t1)) {
      missing_inputs <- c(missing_inputs, "Initial Year")
    }
    if (is.null(rv$t2)) {
      missing_inputs <- c(missing_inputs, "Final Year")
    }
    if (input$peat_decomposition == "Yes"){
      if (is.null(rv$peat_map_path)) {
        missing_inputs <- c(missing_inputs, "Peat Land Map")
      }
      if (is.null(rv$peat_emission_factor_table_path)) {
        missing_inputs <- c(missing_inputs, "Peat Emission Factor Table")
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
    
    withProgress(message = 'Running QuES-C Analysis', value = 0,{
      tryCatch({
        start_time <- Sys.time()
        showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
        shinyjs::disable("processQUESC")
        incProgress(0.1, detail = "Preparing data inputs")

        # Data preparation
        lc_t1_path <- rename_uploaded_file(input_file = rv$lc_t1_path)
        lc_t2_path <- rename_uploaded_file(input_file = rv$lc_t2_path)
        c_lookup_path <- rename_uploaded_file(input_file = rv$c_lookup_path)
        if (input$peat_decomposition == "Yes"){
          peat_emission_factor_table_path <- rename_uploaded_file(input_file = rv$peat_emission_factor_table_path)
        }
        
        incProgress(0.2, detail = "Calculating Land Based Carbon Emission")
        
        # Run QuES-C analysis
        results <- run_quesc_analysis(
          lc_t1_path = lc_t1_path,
          lc_t2_path = lc_t2_path,
          admin_z_path = rv$admin_z_path,
          c_lookup_path = c_lookup_path,
          time_points = list(t1 = rv$t1, t2 = rv$t2),
          output_dir = rv$output_dir,
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          }
        )
        
        map_c1 <- results$map_c1 
        map_c2 <- results$map_c2 
        map_e <- results$map_em 
        map_s <- results$map_sq 
        tbl_quesc <- results$ques_db
        lc_t1 <- results$lc_t1
        lc_t2 <- results$lc_t2 
        zone <- results$zone
        df_pu <- results$zone_lookup_input
        df_c <- results$df_c
        
        # Run QuES-C peat analysis (if any)
        
        if (input$peat_decomposition == "Yes"){
          incProgress(0.4, detail = "Calculating Peat Decomposition Carbon Emission")
          
          results_peat <- run_quesc_peat_analysis(
            output_dir = rv$output_dir,
            lc_t1_path = lc_t1_path,
            lc_t2_path = lc_t2_path,
            admin_z_path = rv$admin_z_path,
            peat_map_path = rv$peat_map_path,
            peat_emission_factor_table_path = peat_emission_factor_table_path,
            t1 = rv$t1,
            t2 = rv$t2
          )
          
          # Access peat emission results
          tbl_quesc_peat <- results_peat$chg_ptable
          tbl_quesc_peat_sum <- results_peat$chg_pdtable
          map_e_peat <- results_peat$em_map
          peatmap <- results_peat$peatmap
          lookup_c.pt <- results_peat$lookup_c.pt
        }
        
        # Combine result (if any) -------------------------------------------------------
        
        if (input$peat_decomposition == "Yes"){
          
          incProgress(0.5, detail = "Calculating Total Carbon Emission From Mineral and Peat")
          # Calculate total emission of mineral and peat
          sum_with_na <- function(x) {
            if (is.na(x[1])) {
              return(x[2])
            } else if (is.na(x[2])) {
              return(x[1])
            } else {
              return(x[1] + x[2])
            }
          }
          map_e_peat_res <- resample(map_e_peat, map_e)
          combined_map_e <- c(map_e, map_e_peat_res)
          map_e_mineral_peat <- app(combined_map_e, fun = sum_with_na)
          
          # Create mineral and peatland emission database
          tbl_quesc_filtered <- tbl_quesc %>% filter(!(Freq == 0 & Ha == 0))
          options(scipen = 999)
          
          tbl_quesc_new <- tbl_quesc_filtered %>%
            rename(em_mineral = EM) %>%
            mutate(matching_key = paste(ID_PU, get(as.character(rv$t1)), get(as.character(rv$t2)), sep = "_")
            )
          
          # Prepare the new peat emission table
          peat_matching <- tbl_quesc_peat %>%
            mutate(matching_key = paste(ID_PU, get(as.character(rv$t1)), get(as.character(rv$t2)), sep = "_")) %>%
            select(matching_key, em_calc)
          
          # Merge the tables and reorganize columns
          quesc_database_mineral_peat <- tbl_quesc_new %>%
            left_join(peat_matching, by = "matching_key") %>%
            mutate(em_peat = ifelse(is.na(em_calc), 0, em_calc)) %>%
            select(-matching_key, -em_calc) %>%
            mutate(EM = em_mineral + em_peat, Type = ifelse(em_peat == 0, "mineral", "peat")) %>%
            # Relocate columns
            relocate(em_peat, .after = em_mineral) %>%
            relocate(EM, .after = em_peat) %>%
            relocate(Type, .after = SQ)
        }
        
        # Export results -------------------------------------------------------
        
        incProgress(0.7, detail = "Exporting Results")
        
        if (input$peat_decomposition == "Yes"){
          write.table(quesc_database_mineral_peat, paste0(rv$output_dir, "/quesc_database_mineral&peat_", rv$t1, "-", rv$t2, ".csv"), quote=FALSE, row.names=FALSE, sep=",")
          writeRaster(map_c1, paste0(rv$output_dir, "/carbon_map_", rv$t1, ".tif"), overwrite = T)
          writeRaster(map_c2, paste0(rv$output_dir, "/carbon_map_", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_e, paste0(rv$output_dir, "/mineral-land_emission_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_e_peat, paste0(rv$output_dir, "/peat-land_emission_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_e_mineral_peat, paste0(rv$output_dir, "/total_emission_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_s, paste0(rv$output_dir, "/sequestration_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
        } else {
          write.table(tbl_quesc, paste0(rv$output_dir, "/quesc_database_", rv$t1, "-", rv$t2, ".csv"), quote=FALSE, row.names=FALSE, sep=",")
          writeRaster(map_c1, paste0(rv$output_dir, "/carbon_map_", rv$t1, ".tif"), overwrite = T)
          writeRaster(map_c2, paste0(rv$output_dir, "/carbon_map_", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_e, paste0(rv$output_dir, "/emission_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
          writeRaster(map_s, paste0(rv$output_dir, "/sequestration_map_", rv$t1, "-", rv$t2, ".tif"), overwrite = T)
        }
        
        session_log <- format_session_info_table()
        
        # End of the script
        end_time <- Sys.time()
        cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
        cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
        
        incProgress(0.8, detail = "Preparing Report")
        # Prepare report parameters based on input
        report_params <- list(
          start_time = start_time,
          end_time = end_time,
          map_c1 = map_c1,
          map_c2 = map_c2,
          map_em = map_e,
          map_sq = map_s,
          ques_db = tbl_quesc,
          p1 = rv$t1,
          p2 = rv$t2,
          lc_t1 = lc_t1,
          lc_t2 = lc_t2,
          zone = zone,
          df_pu = df_pu,
          df_c = df_c,
          
          # Conditional peat parameters
          tbl_quesc_peat = if (input$peat_decomposition == "Yes") tbl_quesc_peat else NULL,
          tbl_quesc_peat_sum = if (input$peat_decomposition == "Yes") tbl_quesc_peat_sum else NULL,
          map_e_peat = if (input$peat_decomposition == "Yes") map_e_peat else NULL,
          map_e_mineral_peat = if (input$peat_decomposition == "Yes") map_e_mineral_peat else NULL,
          quesc_database_mineral_peat = if (input$peat_decomposition == "Yes") quesc_database_mineral_peat else NULL,
          peatmap = if (input$peat_decomposition == "Yes") peatmap else NULL,
          lookup_c.pt = if (input$peat_decomposition == "Yes") lookup_c.pt else NULL,
          
          inputs = list(
            lc_t1_path = lc_t1_path,
            lc_t2_path = lc_t2_path,
            admin_z_path = rv$admin_z_path,
            c_lookup_path = c_lookup_path,
            peat_emission_factor_table_path = if (input$peat_decomposition == "Yes") peat_emission_factor_table_path else NULL,
            output_dir = rv$output_dir
          ),
          session_log = session_log,
          peat_decomposition =input$peat_decomposition
        )
        
        # Render the R markdown report
        if (rmarkdown::pandoc_available() == FALSE) {
          Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
        }
        
        output_file <- paste0("QuES-C_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        
        rmarkdown::render(
          "../report_template/quesc_report_template.Rmd",
          output_file = output_file,
          output_dir = rv$output_dir,
          params = report_params
        )
        
        rv$report_file <- paste(rv$output_dir, output_file, sep = "/")
        
        # Post Analysis
        output$status_messages <- renderText("Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder or view the report.")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification(id = "running_notification")
        shinyjs::enable("processQUESC")
        showNotification("Analysis completed successfully!", type = "message")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        output$success_message <- renderText(NULL)
        removeNotification(id = "running_notification")
        shinyjs::enable("processQUESC")
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
    shinyalert(
      title = "Confirmation",
      text =  "Do you want to return to main menu?",
      showCancelButton = TRUE,
      size = "xs",
      type = "warning",
      inputId = "alert"
    )
  })
  
  observeEvent(input$alert, {
    if(input$alert) {
      js$closeWindow()
      message("Return to main menu!")  
      # shinyjs::delay(1000, stopApp())
    }
  })
}

shinyApp(ui = ui, server = server)
