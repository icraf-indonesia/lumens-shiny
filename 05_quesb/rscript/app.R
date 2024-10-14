source('../../helper.R')
source('ques_biodiv_functions.r')

required_packages <- c(
  "terra",
  "dplyr",
  "ggplot2",
  "shiny",
  "shinyjs",
  "shinyFiles",
  "caTools",
  "sf",
  "DBI",
  "RSQLite",
  "rmarkdown",
  "bslib"
)

# Check if required packages are installed, and install them if not
check_and_install_packages(required_packages)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("QuES-B Module"),
  sidebarLayout(
    sidebarPanel(
      # Select Multiple Time Series Option
      radioButtons(
        "multiseries",
        "Multiple Time Series Analysis",
        choices = c("Single Step" = "single_step", "Two Step" = "two_step")
      ),
      conditionalPanel(
        condition = "input.multiseries == 'two_step'",
        fileInput(
          "lc_t1_file",
          "Initial Land Cover/Use Map",
          accept = c(".tif", ".tiff"),
          placeholder = "input raster (.tif)"
        ),
        numericInput("t1", "Initial Year", value = 2005),
        fileInput(
          "lc_t2_file",
          "Final Land Cover/Use Map",
          accept = c(".tif", ".tiff"),
          placeholder = "input raster (.tif)"
        ),
        numericInput("t2", "Final Year", value = 2010)
      ),
      conditionalPanel(
        condition = "input.multiseries == 'single_step'",
        fileInput(
          "lc_t1_file",
          "Land Cover/Use Map",
          accept = c(".tif", ".tiff"),
          placeholder = "input raster (.tif)"
        ),
        numericInput("t1", "Year", value = 2005)
      ),
      numericInput("nodata_class", "No Data Class", value = 0),
      fileInput(
        "lulc_lut",
        "Land Use/Cover & Focal Area Lookup Table (CSV)",
        accept = c(".csv")
      ),
      fileInput("contab", "Edge Contrast Table (FSQ)", accept = c(".fsq")),
      numericInput("sampling_points", "Sampling Points", value = 1000),
      numericInput("window_size", "Window Size", value = 1000),
      selectInput(
        "window_shape",
        "Window Shape",
        choices = c("Square" = 0, "Circle" = 1),
        selected = 1
      ),
      fileInput(
        "fca_path",
        "FRAGSTATS Configuration ",
        accept = c(".fca"),
        placeholder = "(Optional)"
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 10px;",
        shinyDirButton("fragstats_path", "FRAGSTATS Path (Optional)", "(Optional)"),
        shinyDirButton(
          "output_dir",
          "Select Output Directory",
          "Please select a directory"
        ),
        verbatimTextOutput("print_output_dir", placeholder = TRUE),
        actionButton("run_analysis", "Run QuES-B Analysis", style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
        hidden(
          actionButton("open_report", "Open Report", style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
        ),
        hidden(
          actionButton("open_output_folder", "Open Output Folder",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
        ),
        actionButton("returnButton", "Return to Main Menu", style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", uiOutput("user_guide")),
        tabPanel(
          "Log",
          textOutput("selected_dir"),
          verbatimTextOutput("status_messages"),
          verbatimTextOutput("error_messages")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Directory selection
  volumes <- c(Home = fs::path_home(),
               "R Installation" = R.home(),
               getVolumes()())
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  # Reactive value to store selected output directory
  selected_output_dir <- reactiveVal(value = NULL)
  
  # Update reactive value when output directory is selected
  observe({
    if (!is.null(input$output_dir)) {
      selected_output_dir(parseDirPath(volumes, input$output_dir))
    }
  })
  
  output$print_output_dir <- renderPrint({
    if (!is.null(selected_output_dir())) {
      cat(paste(selected_output_dir()))
    } else {
      cat("No output directory selected")
    }
  })
  
  # Display selected output directory
  output$selected_dir <- renderText({
    if (!is.null(input$output_dir)) {
      paste("Selected output directory:",
            parseDirPath(volumes, input$output_dir))
    } else {
      "No output directory selected"
    }
  })
  
  # Directory selection for FRAGSTATS
  shinyDirChoose(input,
                 "fragstats_path",
                 roots = volumes,
                 session = session)
  
  # Display selected FRAGSTATS directory
  output$selected_fragstats_dir <- renderText({
    if (!is.null(input$fragstats_path)) {
      paste("Selected FRAGSTATS directory:",
            parseDirPath(volumes, input$fragstats_path))
    } else {
      "No FRAGSTATS directory selected (Optional)"
    }
  })
  
  # Render user guide
  output$user_guide <- renderUI({
    guide_path <- "../helpfile/quesb_quick_user_guide.Rmd"
    if (file.exists(guide_path)) {
      html_content <- rmarkdown::render(guide_path, output_format = "html_fragment", quiet = TRUE)
      HTML(readLines(html_content))
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
  # Function to rename uploaded file
  rename_uploaded_file <- function(input_file) {
    if (is.null(input_file))
      return(NULL)
    
    old_path <- input_file$datapath
    new_path <- file.path(dirname(old_path), input_file$name)
    file.rename(old_path, new_path)
    return(new_path)
  }
  
  observeEvent(input$run_analysis, {
    req(input$output_dir)
    output_dir <- parseDirPath(volumes, input$output_dir)
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = 'Running QuES-B Analysis', value = 0, {
      tryCatch({
        # Rename uploaded files
        lc_t1_path <- rename_uploaded_file(input$lc_t1_file)
        lulc_lut_path <- rename_uploaded_file(input$lulc_lut)
        contab_path <- rename_uploaded_file(input$contab)
        fca_path <- if (!is.null(input$fca_path)) rename_uploaded_file(input$fca_path) else NULL
        
        if (input$multiseries == "single_step") {
          result <- run_ques_b(
            lc_t1_path = lc_t1_path,
            t1 = input$t1,
            nodata_class = input$nodata_class,
            lulc_lut_path = lulc_lut_path,
            contab_path = contab_path,
            sampling_points = input$sampling_points,
            window_size = input$window_size,
            window.shape = as.numeric(input$window_shape),
            fca_path = fca_path,
            fragstats_path = if (!is.null(input$fragstats_path)) parseDirPath(volumes, input$fragstats_path) else NULL,
            output_dir = output_dir,
            report_template_path = "../report_template/quesb_report_template.Rmd"
          )
          
          
        } else if (input$multiseries == "two_step") {
          # Rename second land cover file
          lc_t2_path <- rename_uploaded_file(input$lc_t2_file)
          result <- run_ques_b(
            lc_t1_path = lc_t1_path,
            t1 = input$t1,
            lc_t2_path = lc_t2_path,
            t2 = input$t2,
            nodata_class = input$nodata_class,
            lulc_lut_path = lulc_lut_path,
            contab_path = contab_path,
            sampling_points = input$sampling_points,
            window_size = input$window_size,
            window.shape = as.numeric(input$window_shape),
            fca_path = fca_path,
            fragstats_path = if (!is.null(input$fragstats_path)) parseDirPath(volumes, input$fragstats_path) else NULL,
            output_dir = output_dir,
            report_template_path = "../report_template/quesb_report_template.Rmd"
          )
        }
        
        
        
        shinyjs::show("open_report")
        shinyjs::show("open_output_folder")
        removeNotification("running_notification")
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        
      }, error = function(e) {
        removeNotification("running_notification")
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  observeEvent(input$open_report, {
    report_path <- paste0(selected_output_dir(), "/QuES_B_report.html")
    if (file.exists(report_path)) {
      showNotification("Opening report...", type = "message")
      utils::browseURL(report_path)
    } else {
      showNotification("Report file not found.", type = "error")
    }
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