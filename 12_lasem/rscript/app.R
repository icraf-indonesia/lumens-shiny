# LaSEM Shiny App - Refactored UI with 3-step guided workflow

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
  "shiny",
  "shinyjs",
  "shinyFiles",
  "bslib",
  "rmarkdown",
  "knitr",
  "DT",
  "leaflet"
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

check_and_install_packages(required_packages)

# Source LaSEM functions
if (file.exists("LaSEM_functions.R")) {
  source("LaSEM_functions.R")
} else if (file.exists("12_lasem/rscript/LaSEM_functions.R")) {
  source("12_lasem/rscript/LaSEM_functions.R")
} else {
  stop("LaSEM_functions.R file not found.")
}

# Source refactored function modules
if (file.exists("functions_all.R")) {
  source("functions_all.R")
} else if (file.exists("12_lasem/rscript/functions_all.R")) {
  source("12_lasem/rscript/functions_all.R")
}

# JavaScript code for closing window
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# UI ---------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 5,
    primary = "#1B5E45",
    secondary = "#E8A838",
    base_font = font_google("Inter")
  ),
  extendShinyjs(text = jscode, functions = c("closeWindow")),

  # Header with logo
  tags$div(
    class = "d-flex align-items-center mb-3 mt-2",
    tags$img(src = "lumens_logo/logo.png", height = "40px", class = "me-3"),
    tags$h4("Land Suitability Evaluation Module (LaSEM)", class = "mb-0 text-primary")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Step 1: Upload Inputs", class = "text-primary"),
      fileInput("raster_inputs_csv", "Raster Inputs CSV",
        accept = c(".csv"), placeholder = "Upload CSV"
      ),
      fileInput("crop_suitability_csv", "Crop Suitability CSV",
        accept = c(".csv"), placeholder = "Upload CSV"
      ),
      fileInput("intervention_csv", "Intervention Lookup CSV",
        accept = c(".csv"), placeholder = "Upload CSV"
      ),
      hr(),
      h5("Output", class = "text-primary"),
      shinyDirButton(
        "output_dir", "Select Output Directory",
        "Please select a directory"
      ),
      verbatimTextOutput("print_output_dir", placeholder = TRUE),
      hr(),
      div(
        id = "run_section",
        actionButton("run_analysis", "Run LaSEM Analysis",
          class = "btn btn-primary w-100",
          icon = icon("play")
        ),
        hidden(
          actionButton("open_report", "Open Report",
            class = "btn btn-secondary w-100 mt-2",
            icon = icon("file-alt")
          )
        ),
        hidden(
          actionButton("open_output_folder", "Open Output Folder",
            class = "btn btn-secondary w-100 mt-2",
            icon = icon("folder-open")
          )
        )
      ),
      hr(),
      actionButton("returnButton", "Return to Main Menu",
        class = "btn btn-outline-danger w-100",
        icon = icon("arrow-left")
      )
    ),
    mainPanel(
      width = 9,
      navset_card_tab(
        id = "main_tabs",

        # Tab 1: Upload & Preview
        nav_panel(
          title = tagList(icon("upload"), " Upload & Preview"),
          card(
            card_header("Input Validation"),
            uiOutput("validation_panel")
          ),
          card(
            card_header("Raster Inputs Preview"),
            DTOutput("preview_raster_inputs")
          ),
          card(
            card_header("Crop Suitability Preview"),
            DTOutput("preview_crop_suitability")
          ),
          card(
            card_header("Intervention Lookup Preview"),
            DTOutput("preview_intervention")
          )
        ),

        # Tab 2: Inspect Factors
        nav_panel(
          title = tagList(icon("search"), " Inspect Factors"),
          value = "inspect_tab",
          card(
            card_header("Factor Inspection"),
            p("Optionally review input rasters before running analysis. Flag issues to document them in the report."),
            selectInput("inspect_factor", "Select Factor to Inspect:",
              choices = NULL, width = "300px"
            ),
            uiOutput("factor_inspection_status"),
            plotOutput("factor_map", height = "400px"),
            uiOutput("factor_criteria"),
            radioButtons("factor_verdict", "Verification:",
              choices = c("Looks Good" = "good", "Flag Issue" = "issue"),
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.factor_verdict == 'issue'",
              textInput("factor_note", "Note:", placeholder = "Describe the issue...")
            ),
            actionButton("save_verdict", "Save Verdict",
              class = "btn btn-primary mt-2"
            )
          ),
          card(
            card_header("Inspection Summary"),
            tableOutput("inspection_summary")
          )
        ),

        # Tab 3: Results
        nav_panel(
          title = tagList(icon("chart-bar"), " Results"),
          value = "results_tab",
          card(
            card_header("Suitability Map"),
            leafletOutput("results_map", height = "500px")
          ),
          card(
            card_header("Area by Suitability Class"),
            tableOutput("area_summary")
          )
        )
      )
    )
  )
)

# Server -----------------------------------------------------------------
server <- function(input, output, session) {
  # Add resource path for LUMENS logo
  logo_dirs <- c(
    file.path(getwd(), "..", "..", "www", "images"),
    file.path(getwd(), "..", "..", "..", "www", "images"),
    file.path(getwd(), "www", "images")
  )
  logo_found <- FALSE
  for (logo_dir in logo_dirs) {
    if (dir.exists(logo_dir)) {
      shiny::addResourcePath("lumens_logo", logo_dir)
      logo_found <- TRUE
      break
    }
  }
  if (!logo_found) {
    warning("LUMENS logo directory not found")
  }

  # Volumes for directory chooser
  volumes <- c(
    Home = fs::path_home(),
    "R Installation" = R.home(),
    shinyFiles::getVolumes()()
  )
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)

  selected_output_dir <- reactiveVal(value = NULL)
  is_testing <- isTRUE(getOption("shiny.testmode"))

  observe({
    if (is_testing) {
      selected_output_dir("../../tests/testthat/output/shinytest2_lasem")
    } else if (!is.null(input$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })

  output$print_output_dir <- renderPrint({
    if (!is.null(selected_output_dir())) {
      cat(selected_output_dir())
    } else {
      cat("No output directory selected")
    }
  })

  # Reactive values
  rv <- reactiveValues(
    raster_inputs = NULL,
    crop_suitability = NULL,
    intervention = NULL,
    validation = list(valid = FALSE, errors = dplyr::tibble(field = character(), message = character())),
    inspection_status = list(), # Named list: factor -> list(status, note)
    report_file = NULL,
    analysis_results = NULL
  )

  # Helper to validate raster paths - does NOT auto-resolve, just reports errors
  validate_raster_paths <- function(raster_inputs, csv_dir) {
    # Check each raster path that should be available
    available_rows <- raster_inputs |>
      dplyr::filter(availability %in% c("Yes", "yes"))

    invalid_paths <- c()

    for (i in seq_len(nrow(available_rows))) {
      path <- available_rows$raster_path[i]

      # Check if path exists as-is
      if (!file.exists(path)) {
        # Check relative to CSV directory
        rel_path <- file.path(csv_dir, path)
        if (!file.exists(rel_path)) {
          invalid_paths <- c(invalid_paths, available_rows$parameter[i])
        }
      }
    }

    invalid_paths
  }

  # Load and validate CSVs when uploaded
  observeEvent(input$raster_inputs_csv, {
    req(input$raster_inputs_csv)
    rv$raster_inputs <- readr::read_csv(input$raster_inputs_csv$datapath)
    updateValidation()
  })

  observeEvent(input$crop_suitability_csv, {
    req(input$crop_suitability_csv)
    rv$crop_suitability <- readr::read_csv(input$crop_suitability_csv$datapath)
    updateValidation()
  })

  observeEvent(input$intervention_csv, {
    req(input$intervention_csv)
    rv$intervention <- readr::read_csv(input$intervention_csv$datapath)
    updateValidation()
  })

  # Validation logic
  updateValidation <- function() {
    errors <- dplyr::tibble(field = character(), message = character())
    valid <- TRUE

    # Check all files uploaded
    if (is.null(rv$raster_inputs)) {
      valid <- FALSE
      errors <- dplyr::bind_rows(errors, dplyr::tibble(
        field = "raster_inputs", message = "Raster inputs CSV not uploaded"
      ))
    } else {
      # Check for either parameter_name or name_parameter column
      raster_cols <- names(rv$raster_inputs)
      has_param_col <- "parameter_name" %in% raster_cols || "name_parameter" %in% raster_cols

      if (!has_param_col) {
        valid <- FALSE
        errors <- dplyr::bind_rows(errors, dplyr::tibble(
          field = "raster_inputs",
          message = "Missing required column: parameter_name (or name_parameter)"
        ))
      }

      schema <- validate_csv_schema(
        rv$raster_inputs,
        c("ID", "parameter", "availability", "raster_path")
      )
      if (!schema$valid) {
        valid <- FALSE
        errors <- dplyr::bind_rows(errors, schema$errors)
      }

      # Validate raster file paths
      if ("raster_path" %in% names(rv$raster_inputs)) {
        csv_dir <- dirname(input$raster_inputs_csv$datapath)
        invalid_rasters <- validate_raster_paths(rv$raster_inputs, csv_dir)

        if (length(invalid_rasters) > 0) {
          valid <- FALSE
          errors <- dplyr::bind_rows(errors, dplyr::tibble(
            field = "raster_files",
            message = paste0(
              "Invalid raster paths for: ", paste(invalid_rasters, collapse = ", "),
              ". Please check that raster_path values in your CSV are correct ",
              "relative to the CSV file location, or use absolute paths."
            )
          ))
        }
      }
    }

    if (is.null(rv$crop_suitability)) {
      valid <- FALSE
      errors <- dplyr::bind_rows(errors, dplyr::tibble(
        field = "crop_suitability", message = "Crop suitability CSV not uploaded"
      ))
    } else {
      schema <- validate_csv_schema(
        rv$crop_suitability,
        c("name_common", "name_sp", "class", "name_parameter", "value", "unit")
      )
      if (!schema$valid) {
        valid <- FALSE
        errors <- dplyr::bind_rows(errors, schema$errors)
      }
    }

    if (is.null(rv$intervention)) {
      valid <- FALSE
      errors <- dplyr::bind_rows(errors, dplyr::tibble(
        field = "intervention", message = "Intervention CSV not uploaded"
      ))
    }

    if (is.null(selected_output_dir())) {
      valid <- FALSE
      errors <- dplyr::bind_rows(errors, dplyr::tibble(
        field = "output", message = "Output directory not selected"
      ))
    }

    rv$validation <- list(valid = valid, errors = errors)
  }

  # Validation panel display
  output$validation_panel <- renderUI({
    val <- rv$validation

    if (val$valid) {
      div(
        class = "alert alert-success",
        icon("check-circle"), " All inputs valid. Ready to inspect factors or run analysis."
      )
    } else {
      div(
        lapply(seq_len(nrow(val$errors)), function(i) {
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            strong(val$errors$field[i]), ": ", val$errors$message[i]
          )
        })
      )
    }
  })

  # CSV Previews
  output$preview_raster_inputs <- renderDT({
    req(rv$raster_inputs)
    datatable(rv$raster_inputs, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$preview_crop_suitability <- renderDT({
    req(rv$crop_suitability)
    datatable(rv$crop_suitability, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$preview_intervention <- renderDT({
    req(rv$intervention)
    datatable(rv$intervention, options = list(pageLength = 5, scrollX = TRUE))
  })

  # Factor inspection - populate dropdown when crop suitability loaded
  observeEvent(rv$crop_suitability, {
    req(rv$crop_suitability)
    factors <- rv$crop_suitability |>
      dplyr::pull(name_parameter) |>
      unique()
    updateSelectInput(session, "inspect_factor", choices = factors)
  })

  # Factor inspection status
  output$factor_inspection_status <- renderUI({
    req(input$inspect_factor)
    status <- rv$inspection_status[[input$inspect_factor]]

    if (is.null(status)) {
      div(class = "alert alert-warning", "Not yet inspected")
    } else if (status$status == "good") {
      div(class = "alert alert-success", icon("check"), " Looks Good")
    } else {
      div(class = "alert alert-danger", icon("flag"), " Issue flagged: ", status$note)
    }
  })

  # Factor map preview - show actual raster data
  output$factor_map <- renderPlot({
    req(input$inspect_factor, rv$raster_inputs)

    tryCatch(
      {
        # Find the raster path for this factor - handle either column name
        raster_cols <- names(rv$raster_inputs)
        has_param_name <- "parameter_name" %in% raster_cols
        has_name_param <- "name_parameter" %in% raster_cols

        if (has_param_name && has_name_param) {
          factor_row <- rv$raster_inputs |>
            dplyr::filter(parameter_name == input$inspect_factor | name_parameter == input$inspect_factor)
        } else if (has_param_name) {
          factor_row <- rv$raster_inputs |>
            dplyr::filter(parameter_name == input$inspect_factor)
        } else if (has_name_param) {
          factor_row <- rv$raster_inputs |>
            dplyr::filter(name_parameter == input$inspect_factor)
        } else {
          plot.new()
          text(0.5, 0.5, "No parameter name column found in raster inputs CSV",
            cex = 1.2, col = "red"
          )
          return()
        }

        if (nrow(factor_row) == 0) {
          plot.new()
          text(0.5, 0.5, paste("No raster found for factor:", input$inspect_factor),
            cex = 1.2, col = "red"
          )
          return()
        }

        # Get the raster path and check if it exists
        raster_path <- factor_row$raster_path[1]

        # If not absolute, try relative to CSV directory
        if (!file.exists(raster_path) && !is.null(input$raster_inputs_csv)) {
          csv_dir <- dirname(input$raster_inputs_csv$datapath)
          rel_path <- file.path(csv_dir, raster_path)
          if (file.exists(rel_path)) {
            raster_path <- rel_path
          }
        }

        if (!file.exists(raster_path)) {
          plot.new()
          text(0.5, 0.5, paste("Raster file not found:\n", basename(raster_path)),
            cex = 1.2, col = "red"
          )
          return()
        }

        # Read and plot the raster
        r <- terra::rast(raster_path)
        terra::plot(r,
          main = paste("Input raster:", input$inspect_factor),
          col = terrain.colors(20)
        )
      },
      error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error loading raster:\n", conditionMessage(e)),
          cex = 1.0, col = "red"
        )
      }
    )
  })

  # Factor criteria display
  output$factor_criteria <- renderUI({
    req(rv$crop_suitability, input$inspect_factor)

    criteria <- rv$crop_suitability |>
      dplyr::filter(name_parameter == input$inspect_factor) |>
      dplyr::select(class, value, unit)

    if (nrow(criteria) == 0) {
      return(NULL)
    }

    tagList(
      h6("Classification Criteria:"),
      tableOutput("factor_criteria_table")
    )
  })

  output$factor_criteria_table <- renderTable({
    req(rv$crop_suitability, input$inspect_factor)
    rv$crop_suitability |>
      dplyr::filter(name_parameter == input$inspect_factor) |>
      dplyr::select(Class = class, Range = value, Unit = unit)
  })

  # Save verdict
  observeEvent(input$save_verdict, {
    req(input$inspect_factor)
    rv$inspection_status[[input$inspect_factor]] <- list(
      status = input$factor_verdict,
      note = ifelse(input$factor_verdict == "issue", input$factor_note, NA)
    )
    showNotification("Verdict saved", type = "message")
  })

  # Inspection summary
  output$inspection_summary <- renderTable({
    if (length(rv$inspection_status) == 0) {
      return(data.frame(Message = "No factors inspected yet"))
    }

    data.frame(
      Factor = names(rv$inspection_status),
      Status = sapply(rv$inspection_status, function(x) x$status),
      Note = sapply(rv$inspection_status, function(x) ifelse(is.na(x$note), "", x$note))
    )
  })

  # Run analysis
  observeEvent(input$run_analysis, {
    if (!rv$validation$valid) {
      showNotification("Please fix validation errors first.", type = "error")
      return()
    }

    showNotification("Analysis is running...",
      type = "message", duration = NULL,
      id = "running_notification"
    )
    start_time <- Sys.time()

    withProgress(message = "Running LaSEM Analysis", value = 0, {
      tryCatch(
        {
          path_output <- selected_output_dir()

          # Load raster inputs
          incProgress(0.1, detail = "Loading raster inputs")
          input_paths <- rv$raster_inputs |>
            dplyr::filter(availability %in% c("Yes", "yes"))

          # Determine parameter column name
          param_col <- if ("parameter_name" %in% names(input_paths)) {
            "parameter_name"
          } else if ("name_parameter" %in% names(input_paths)) {
            "name_parameter"
          } else {
            stop("Raster inputs CSV must have 'parameter_name' or 'name_parameter' column")
          }

          # Validate paths exist before proceeding
          csv_dir <- dirname(input$raster_inputs_csv$datapath)
          invalid <- validate_raster_paths(input_paths, csv_dir)
          if (length(invalid) > 0) {
            stop("Invalid raster paths detected. Please fix the CSV file before running analysis.")
          }

          input_suit_factors <- input_paths |>
            read_raster_files()

          stacked_rasters <- stack_raster_layers(
            input_suit_factors,
            input_paths[[param_col]]
          )

          # Load crop suitability
          incProgress(0.2, detail = "Loading crop suitability data")
          crop_suit_data <- rv$crop_suitability

          # Load intervention
          incProgress(0.3, detail = "Loading intervention data")
          intervention_table <- rv$intervention

          # Run analysis
          incProgress(0.5, detail = "Performing suitability analysis")
          results <- perform_suitability_analysis(
            harmonised_rasters = stacked_rasters,
            suitability_parameter = crop_suit_data,
            lookup_intervention = intervention_table
          )

          rv$analysis_results <- results

          # Export files
          incProgress(0.7, detail = "Exporting results")
          dir.create(path_output, recursive = TRUE, showWarnings = FALSE)

          # Save input CSVs to output dir
          file.copy(input$raster_inputs_csv$datapath,
            file.path(path_output, "raster_inputs.csv"),
            overwrite = TRUE
          )
          file.copy(input$crop_suitability_csv$datapath,
            file.path(path_output, "crop_suitability.csv"),
            overwrite = TRUE
          )
          file.copy(input$intervention_csv$datapath,
            file.path(path_output, "intervention.csv"),
            overwrite = TRUE
          )

          # Save harmonised rasters (wrapped for RDS)
          file_name_soil_climate_factors_rds <- file.path(path_output, "soil_climate_factors.rds")
          saveRDS(terra::wrap(stacked_rasters), file_name_soil_climate_factors_rds)

          # Save suitability raster as TIFF
          file_name_land_suit_tif <- file.path(path_output, "land_suitability.tif")
          terra::writeRaster(results[["suitability_raster"]], file_name_land_suit_tif,
            overwrite = TRUE
          )

          # Save suitability polygon as RDS
          file_name_land_suit_rds <- file.path(path_output, "land_suitability.rds")
          saveRDS(results[["suitability_polygon"]], file_name_land_suit_rds)

          # Save factor rasters as TIFF
          file_name_suit_factors_tif <- file.path(path_output, "suitability_factors.tif")
          terra::writeRaster(results[["suitability_by_factors"]], file_name_suit_factors_tif,
            overwrite = TRUE
          )

          # Save lookup
          file_name_land_suit_lookup_csv <- file.path(path_output, "suitability_lookup.csv")
          readr::write_csv(results[["lookup_suitability_factors"]], file_name_land_suit_lookup_csv)

          # Generate report
          incProgress(0.85, detail = "Generating report")
          path_report <- NULL
          if (file.exists("12_lasem/report_template/LaSEM_report.Rmd")) {
            path_report <- "12_lasem/report_template/LaSEM_report.Rmd"
          } else if (file.exists("../report_template/LaSEM_report.Rmd")) {
            path_report <- "../report_template/LaSEM_report.Rmd"
          } else if (file.exists("report_template/LaSEM_report.Rmd")) {
            path_report <- "report_template/LaSEM_report.Rmd"
          }

          if (!is.null(path_report)) {
            report_params <- list(
              start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
              end_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              file_name_soil_climate_factors_rds = file_name_soil_climate_factors_rds,
              file_name_land_suit_rds = file_name_land_suit_rds,
              file_name_land_suit_tif = file_name_land_suit_tif,
              file_name_land_suit_shp = NA,
              file_name_land_suit_lookup_csv = file_name_land_suit_lookup_csv,
              file_name_suit_factors_tif = file_name_suit_factors_tif,
              path_lookup_raster_inputs = file.path(path_output, "raster_inputs.csv"),
              path_lookup_crop_suitability = file.path(path_output, "crop_suitability.csv"),
              path_lookup_intervention = file.path(path_output, "intervention.csv"),
              path_output = path_output,
              session_log = format_session_info_table(),
              validation_results = rv$validation,
              inspection_log = rv$inspection_status
            )

            output_file <- "LaSEM_report.html"
            rmarkdown::render(
              input = path_report,
              output_file = output_file,
              output_dir = path_output,
              params = report_params,
              quiet = TRUE
            )
            rv$report_file <- file.path(path_output, output_file)
          } else {
            warning("Report template not found")
          }

          incProgress(0.95, detail = "Complete")
          shinyjs::show("open_output_folder")
          shinyjs::show("open_report")
          removeNotification("running_notification")
          showNotification("Analysis completed successfully!", type = "message", duration = NULL)
        },
        error = function(e) {
          err_msg <- conditionMessage(e)
          message("LaSEM Analysis Error: ", err_msg)
          removeNotification("running_notification")
          showNotification(paste("Analysis failed:", err_msg), type = "error", duration = NULL)
        }
      )
    })
  })

  # Results map
  output$results_map <- renderLeaflet({
    req(rv$analysis_results)
    polygon <- rv$analysis_results$suitability_polygon
    if (is.null(polygon) || nrow(polygon) == 0) {
      return(leaflet() |> addTiles())
    }

    # Leaflet tiles and markers require WGS84 (EPSG:4326).
    # The analysis pipeline preserves the raster's original CRS, so we
    # reproject the polygon on the fly before rendering.
    if (sf::st_crs(polygon)$epsg != 4326) {
      polygon <- sf::st_transform(polygon, crs = 4326)
    }

    suit_colors <- c(
      "S1" = "#228B22",
      "S2" = "#90EE90",
      "S3" = "#FFA500",
      "N"  = "#DC143C"
    )

    # Use the same rendering style as the report's interactive map:
    # nearly-invisible grey borders (weight 0.1) with full fill opacity
    # so polygons blend together smoothly instead of appearing speckled.
    leaflet(polygon) |>
      addTiles() |>
      addPolygons(
        fillColor = ~ suit_colors[suitability],
        fillOpacity = 1,
        color = "grey",
        weight = 0.1,
        smoothFactor = 1,
        popup = ~ paste(
          "<strong>Suitability:</strong>", suitability,
          "<br><strong>Limiting factor:</strong>",
          ifelse(is.na(limiting_factor_actual), "None", limiting_factor_actual)
        )
      ) |>
      addLegend(
        position = "bottomright",
        colors = suit_colors,
        labels = c(
          "S1 - Highly Suitable", "S2 - Moderately Suitable",
          "S3 - Marginally Suitable", "N - Not Suitable"
        ),
        title = "Suitability Class"
      )
  })

  # Area summary
  output$area_summary <- renderTable({
    req(rv$analysis_results)
    polygon <- rv$analysis_results$suitability_polygon
    if (is.null(polygon)) {
      return(data.frame(Message = "No results available"))
    }

    polygon |>
      sf::st_drop_geometry() |>
      dplyr::group_by(suitability) |>
      dplyr::summarise(
        pixel_count = sum(count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        percentage = round(pixel_count / sum(pixel_count) * 100, 1)
      ) |>
      dplyr::arrange(match(suitability, c("S1", "S2", "S3", "N")))
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
      utils::browseURL(rv$report_file)
    } else {
      showNotification("Report file not found.", type = "error")
    }
  })

  # Return to main menu
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

# Run the app
shinyApp(ui, server)
