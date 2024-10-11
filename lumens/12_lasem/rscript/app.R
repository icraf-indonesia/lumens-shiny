source('../../helper.R')

# Define the list of required packages
required_packages <- c(
  "terra",
  "dplyr",
  "sf",
  "purrr",
  "tidyr",
  "rlang",
  "tibble",
  "stringr",
  "readr",
  "magrittr",
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyFiles",
  "bslib",
  "rmarkdown",
  "kableExtra",
  "htmlTable",
  "knitr",
  "ggplot2",
  "patchwork",
  "tidyterra"
)

# Function to check and install required packages
check_and_install_packages <- function(packages) {
  invisible(lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)  
      library(pkg, character.only = TRUE, quietly = TRUE)  
    }
  }))
}

# Check and install packages
check_and_install_packages(required_packages)

# Source LaSEM functions
if (file.exists("LaSEM_functions.R")){
  source("LaSEM_functions.R")
} else if (file.exists("12_lasem/rscript/LaSEM_functions.R")){
  source("12_lasem/rscript/LaSEM_functions.R")
} else {
  stop("LaSEM_functions.R file not found.")
}

# UI
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("Land Suitability Evaluation Module (LaSEM)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("raster_inputs_csv", "Raster Inputs CSV", accept = c(".csv"), placeholder = "Upload raster inputs CSV"),
      fileInput("crop_suitability_csv", "Crop Suitability CSV", accept = c(".csv"), placeholder = "Upload crop suitability CSV"),
      fileInput("intervention_csv", "Intervention Lookup CSV", accept = c(".csv"), placeholder = "Upload intervention lookup CSV"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("run_analysis", "Run LaSEM Analysis",
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
        tabPanel("User Guide",
                 div(
                   style = "height: 800px; overflow-y: scroll; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                   uiOutput("user_guide")
                 )
        ),
        tabPanel("Log",
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Define volumes first
  volumes <- c(
    Home = fs::path_home(),
    "R Installation" = R.home(),
    shinyFiles::getVolumes()()
  )
  
  # Initialize shinyDirChoose
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Check if the app is running in test mode
  is_testing <- isTRUE(getOption("shiny.testmode"))
  
  # Single observer to set selected_output_dir based on testing mode
  observe({
    if (is_testing) {
      # Set to the test output directory
      selected_output_dir("../../tests/testthat/output/shinytest2_lasem")
    } else {
      if (!is.null(input$output_dir)) {
        selected_output_dir(parseDirPath(volumes, input$output_dir))
      }
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
  
  # User Guide
  output$user_guide <- renderUI({
    guide_paths <- c(
      "12_lasem/helpfile/lasem_quick_user_guide.Rmd",
      "../helpfile/lasem_quick_user_guide.Rmd"
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
    raster_inputs_csv = NULL,
    crop_suitability_csv = NULL,
    intervention_csv = NULL,
    report_file = NULL
  )
  
  # Update reactive values when inputs change
  observe({
    rv$raster_inputs_csv <- input$raster_inputs_csv
    rv$crop_suitability_csv <- input$crop_suitability_csv
    rv$intervention_csv <- input$intervention_csv
  })
  
  # Input validation
  validate_inputs <- reactive({
    validate(
      need(rv$raster_inputs_csv, "Please upload Raster Inputs CSV file"),
      need(rv$crop_suitability_csv, "Please upload Crop Suitability CSV file"),
      need(rv$intervention_csv, "Please upload Intervention Lookup CSV file"),
      need(selected_output_dir(), "Please select an output directory")
    )
    return(TRUE)
  })
  
  # Run analysis
  observeEvent(input$run_analysis, {
    req(validate_inputs())
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    withProgress(message = 'Running LaSEM Analysis', value = 0, {
      tryCatch({
        
        # Read inputs
        path_lookup_raster_inputs <- rv$raster_inputs_csv$datapath
        path_lookup_crop_suitability <- rv$crop_suitability_csv$datapath
        path_lookup_intervention <- rv$intervention_csv$datapath
        path_output <- selected_output_dir()
        
        # Handle path_report_template based on testing mode
        if (is_testing) {
          path_report_template <- "../../tests/testthat/report_template/LaSEM_report.Rmd"
        } else {
          if (file.exists(normalizePath("12_lasem/report_template/LaSEM_report.Rmd"))) {
            path_report_template <- normalizePath("12_lasem/report_template/LaSEM_report.Rmd")
          } else if (file.exists(normalizePath("../report_template/LaSEM_report.Rmd"))) {
            path_report_template <- normalizePath("../report_template/LaSEM_report.Rmd")
          } else {
            stop("Report template file is not found.")
          }
        }
        
        # Check if the report template exists
        if (!file.exists(path_report_template)) {
          stop(paste("Report template file does not exist at:", path_report_template))
        }
        
        # Load Biophysical Raster Inputs -------------------------------------------
        start_time <- Sys.time()
        cat("Started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
        incProgress(0.1, detail = "Loading raster inputs")
        input_paths <-
          read_csv(path_lookup_raster_inputs) %>%
          dplyr::filter(availability %in% "Yes")
        
        input_suit_factors <- input_paths %>% read_raster_files()
        
        stackedRasters <- stack_raster_layers(input_suit_factors,
                                              input_paths[["name_parameter"]])
        
        # Load Crop Suitability Table ---------------------------------------------
        incProgress(0.2, detail = "Loading crop suitability data")
        cropSuitabilityData <- read_csv(path_lookup_crop_suitability)
        
        # Load Intervention Table -------------------------------------------------
        incProgress(0.3, detail = "Loading intervention data")
        interventionTable <- read_csv(path_lookup_intervention)
        
        # Run suitability analysis ------------------------------------------------
        incProgress(0.5, detail = "Performing suitability analysis")
        suitability_results <- perform_suitability_analysis(
          harmonised_rasters = stackedRasters,
          suitability_parameter = cropSuitabilityData,
          lookup_intervention = interventionTable
        )
        
        # 4. Export files ------------------------------------------------------------
        incProgress(0.7, detail = "Exporting results")
        dir.create(path_output, recursive = TRUE, showWarnings = FALSE)
        
        # a. harmonised raster for report generating reports
        file_name_soil_climate_factors_rds <- paste0(path_output,"/soil_climatic_factors.rds")
        stackedRasters |>
          terra::wrap() |>
          saveRDS(file_name_soil_climate_factors_rds)
        
        # b. actual and potential suitability in rds format
        file_name_land_suit_rds <- paste0(path_output,"/land_suitability.rds")
        suitability_results[["suitability_polygon"]] |>
          saveRDS(file_name_land_suit_rds)
        
        # c. Actual and potential suitability polygon (should be checked and tidied up)
        file_name_land_suit_shp <- paste0(path_output,"/land_suitability.shp")
        suitability_results[["suitability_polygon"]] %>%
          mutate(across(where(is.list) & !geometry, ~sapply(., function(x) paste(x, collapse = ", ")))) %>%
          sf::st_make_valid() %>%
          rename(
            cat = categories,
            suit = suitability,
            lmt_fact_a = limiting_factor_actual,
            lmt_fact_p = limiting_factor_potential,
            suit_pot_l = suitability_potential_low,
            suit_pot_m = suitability_potential_med,
            suit_pot_h = suitability_potential_high
          ) %>%
          rename_with(~substr(gsub("[^a-zA-Z0-9]", "", .), 1, 10)) %>%
          sf::st_write(.,
                       file_name_land_suit_shp,
                       append = FALSE,
                       driver = "ESRI Shapefile")
        
        # d. Actual suitability raster map
        file_name_land_suit_tif  <- paste0(path_output, "/land_suitability.tif")
        writeRaster(suitability_results[["suitability_raster"]], file_name_land_suit_tif, overwrite = TRUE)
        
        # e. Actual suitability raster lookup table
        file_name_land_suit_lookup_csv  <- paste0(path_output,"/land_suitability_lookup.csv")
        write_csv(suitability_results[["lookup_suitability_factors"]], file_name_land_suit_lookup_csv)
        
        # f. suitability layers for a certain crop for each soil and climatic factors
        file_name_suit_factors_tif  <- paste0(path_output, "/land_suitability_factors.tif")
        writeRaster(suitability_results[["suitability_by_factors"]], file_name_suit_factors_tif, overwrite = TRUE)
        
        # End of the script
        end_time <- Sys.time()
        cat("Ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
        
        # 5. Reporting ---------------------------------------------------------------
        incProgress(0.9, detail = "Generating report")
        session_log <- format_session_info_table()
        
        report_params <- list(
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          file_name_soil_climate_factors_rds =file_name_soil_climate_factors_rds,
          file_name_land_suit_rds =  file_name_land_suit_rds,
          file_name_land_suit_tif = file_name_land_suit_tif,
          file_name_land_suit_shp = file_name_land_suit_shp,
          file_name_land_suit_lookup_csv = file_name_land_suit_lookup_csv,
          file_name_suit_factors_tif = file_name_suit_factors_tif,
          path_lookup_raster_inputs = path_lookup_raster_inputs,
          path_lookup_crop_suitability = path_lookup_crop_suitability,
          path_lookup_intervention = path_lookup_intervention,
          path_output = path_output,
          session_log = session_log
        )
        
        # Render the R Markdown report
        if (!rmarkdown::pandoc_available()) {
          Sys.setenv(RSTUDIO_PANDOC = paste0(getwd(), "/pandoc"))
        }
        
        output_file <- paste0("LaSEM_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        
        rmarkdown::render(
          input = path_report_template,
          output_file = output_file,
          output_dir = path_output,
          params = report_params
        )
        
        rv$report_file <- file.path(path_output, output_file)
        
        # Set success messages
        output$status_messages <- renderText("Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder or view the report.")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification("running_notification")
        showNotification("Analysis completed successfully!", type = "message")
        
      }, error = function(e) {
        # Handle errors by setting error messages
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        output$success_message <- renderText(NULL)
        removeNotification("running_notification")
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
      showNotification("Opening report...", type = "message")
      utils::browseURL(rv$report_file)
    } else {
      showNotification("Report file not found.", type = "error")
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Return to main menu
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # You can add additional code here to handle returning to the main menu if needed
  })
}


# Run the app
shinyApp(ui, server)


