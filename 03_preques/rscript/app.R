# Define a list of required packages for the Pre-QuES analysis and Shiny app
required_packages <- c(
  "terra", "dplyr", "tidyterra", "ggplot2", "forcats", "stringr",
  "cowplot", "networkD3", "scales", "purrr", "rmarkdown",
  "kableExtra", "htmlTable", "knitr", "magrittr", "tidyr",
  "rlang", "stats", "utils", "methods", "sf", "ggrepel",
  "viridis", "textclean", "shiny", "shinydashboard", "shinyjs",
  "shinyFiles", "bslib"
)


if (file.exists("functions_ques_pre.R")){
  source("functions_ques_pre.R")
} else {
  source("03_preques/rscript/functions_ques_pre.R")
}

# Check if required packages are installed, and install them if not
check_and_install_packages(required_packages)


ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("Pre-QuES Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("lc_t1", "Land Use/Cover T1", accept = c(".tif", ".tiff")),
      numericInput("t1_year", "T1 Year", value = 1990),
      fileInput("lc_t2", "Land Use/Cover T2", accept = c(".tif", ".tiff")),
      numericInput("t2_year", "T2 Year", value = 2010),
      fileInput("lookup_lc", "Land Use/Cover Lookup Table (CSV)", accept = c(".csv")),
      fileInput("lookup_trajectory", "Trajectory Lookup Table (CSV)", accept = c(".csv")),
      fileInput("zone_shapefile", "Planning Units (Shapefile)",
                accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE, placeholder = "Upload the .shp, .dbf, .prj, and .shx files."),
      # radioButtons("zone_type", "Planning Units Input Type",
      #              choices = c("Raster" = "raster", "Shapefile" = "shapefile")),
      # conditionalPanel(
      #   condition = "input.zone_type == 'raster'",
      #   fileInput("zone_raster", "Planning Units (Raster)", accept = c(".tif", ".tiff")),
      #   fileInput("lookup_zone", "Planning Units Lookup (CSV)", accept = c(".csv"))
      # ),
      # conditionalPanel(
      #   condition = "input.zone_type == 'shapefile'",
      #   fileInput("zone_shapefile", "Planning Units (Shapefile)",
      #             accept = c(".shp", ".dbf", ".prj", ".shx"), multiple = TRUE)
      # ),

      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("run_analysis", "Run Pre-QuES Analysis",
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
        tabPanel("User Guide", uiOutput("user_guide")),
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

  output$user_guide <- renderUI({
    guide_paths <- c(
      "03_preques/helpfile/preques_quick_user_guide.Rmd",
      "../helpfile/preques_quick_user_guide.Rmd"
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
    lc_t1 = NULL,
    lc_t2 = NULL,
    lookup_lc = NULL,
    zone_input = NULL,
    lookup_zone = NULL,
    lookup_trajectory = NULL
  )

  # Update reactive values when inputs change
  observe({
    rv$lc_t1 <- input$lc_t1
    rv$lc_t2 <- input$lc_t2
    rv$lookup_lc <- input$lookup_lc
    rv$lookup_trajectory <- input$lookup_trajectory

    # if (input$zone_type == "raster") {
    #   rv$zone_input <- input$zone_raster
    #   rv$lookup_zone <- input$lookup_zone
    # } else {
    #   rv$zone_input <- input$zone_shapefile
    #   rv$lookup_zone <- NULL  # Will be created from shapefile
    # }
    
    rv$zone_input <- input$zone_shapefile
    rv$lookup_zone <- NULL  # Will be created from shapefile
  })

  # Input validation
  validate_inputs <- reactive({
    validate(
      need(rv$lc_t1, "Please upload Land Use/Cover T1 file"),
      need(rv$lc_t2, "Please upload Land Use/Cover T2 file"),
      need(rv$lookup_lc, "Please upload Land Use/Cover Lookup Table (CSV) file"),
      #need(input$zone_type, "Please select Planning Units Input Type"),
      need(rv$zone_input, "Please upload Planning Units file"),
      #need(if(input$zone_type == "raster") rv$lookup_zone else TRUE, "Please upload Planning Units Lookup (CSV) file for raster input"),
      need(rv$lookup_trajectory, "Please upload Land Use/Cover Trajectory Lookup Table (CSV) file"),
      need(selected_output_dir(), "Please select an output directory")
    )
    return(TRUE)
  })

  # Run analysis
  observeEvent(input$run_analysis, {
    req(validate_inputs())

    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    withProgress(message = 'Running Pre-QuES Analysis', value = 0, {
      tryCatch({
       
        # Load LC T1 raster
        lc_t1_raster <- terra::rast(rv$lc_t1$datapath)

        # Process planning unit input
        zone_data <- process_planning_unit(
          #zone_type = input$zone_type,
          zone_type = "shapefile",
          zone_input = rv$zone_input,
          lc_t1_raster = lc_t1_raster
        )

        results <- run_preques_analysis(
          lc_t1_input = rv$lc_t1,
          lc_t2_input = rv$lc_t2,
          admin_z_input = zone_data$zone_raster,
          lc_lookup_input = rv$lookup_lc,
          zone_lookup_input = zone_data$lookup_zone,
          #zone_lookup_input = if(input$zone_type == "raster") rv$lookup_zone else zone_data$lookup_zone,
          trajectory_lookup_input = rv$lookup_trajectory,
          time_points = list(t1 = input$t1_year, t2 = input$t2_year),
          output_dir = selected_output_dir(),
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          }
        )

        output$status_messages <- renderText("Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder or view the report.")
        output$error_messages <- renderText(NULL)
        shinyjs::show("open_output_folder")
        shinyjs::show("open_report")
        removeNotification("running_notification")
        showNotification("Analysis completed successfully!", type = "message")
      }, error = function(e) {
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
    report_path <- file.path(selected_output_dir(), "PreQUES_report.html")
    if (file.exists(report_path)) {
      showNotification("Opening report...", type = "message")
      utils::browseURL(report_path)
    } else {
      showNotification("Report file not found.", type = "error")
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

shinyApp(ui, server)
