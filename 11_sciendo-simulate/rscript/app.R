source('function_sciendo_simulate.R')
source('../../helper.R')
options(shiny.maxRequestSize=30*1024^2)

install_load(
  "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert"
)

if (!("LUMENSR" %in% rownames(installed.packages()))) {
  install_github("icraf-indonesia/LUMENSR", force = T)
  do.call("library", list("LUMENSR"))
}
library(LUMENSR)

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  titlePanel("SCIENDO Simulate"),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file", "Land cover map initial", accept = c("image/tiff")),
      fileInput("mapz_file", "Planning Unit", accept = c("image/tiff")),
      fileInput("lc_file", "Land Use/Cover Lookup Table (CSV)", accept = c(".csv")),
      fileInput("rc_file", "Raster Cube", multiple=T),
      numericInput("repetition", "Repetition", value = 2),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("tm_path", "Transition Matrix Folder Path", "Choose a folder contains CSV files"),
          verbatimTextOutput("print_tm_dir", placeholder = TRUE),
          shinyDirButton("dcf_path", "Weights of Evidence Folder Path", "Choose a folder contains DCF files"),
          verbatimTextOutput("print_dcf_dir", placeholder = TRUE),
          shinyDirButton("wd", "Select output directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          shinyDirButton("dinamica_path", "DINAMICA EGO Path (Optional)", "(Optional)"),
          actionButton("processSimulate", "Run Analysis", 
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("openReport", "Open Report",
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
        tabPanel("User Guide", includeMarkdown("../helpfile/sciendo_simulate_quick_user_guide.md")),
        tabPanel("Log",
                 textOutput("selected_directory"),
                 textOutput("dinamica_path"),
                 textOutput("tm_directory"),
                 textOutput("dcf_directory"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = NULL,
    factors_path = NULL,
    dinamica_path = NULL,
    report_file = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    mapz_df = NULL,
    lc_path = NULL,
    lc_df = NULL
  )
  
  volumes <- c(
    Home = fs::path_home(), "R Installation" = R.home(), 
    getVolumes()()
  )
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  #### Read file inputs ####
  observeEvent(input$map1_file, {
    map1 <- input$map1_file
    if(is.null(map1))
      return()
    
    rv$map1_file <- rename_uploaded_file(map1)
  })
  
  observeEvent(input$mapz_file, {
    mapz <- input$mapz_file
    if(is.null(mapz))
      return()
    
    rv$mapz_file <- rename_uploaded_file(mapz)
  })
  
  #### Read file inputs ####
  observeEvent(input$rc_file, {
    rc <- input$rc_file
    if(is.null(rc))
      return()
    
    if(nrow(rc) == 1){
      if(substrRight(rc$name, 3) == "tif") {
        rv$rc <- rename_uploaded_file(rc)
      } else {
        return()
      }
    } else {
      prev_wd <- getwd()
      uploaded_dir <- dirname(rc$datapath[1])
      setwd(uploaded_dir)
      for(i in 1:nrow(rc)){
        print(rc$name[i])
        file.rename(rc$datapath[i], rc$name[i])
      }
      setwd(prev_wd)
      
      rv$rc <- paste(uploaded_dir, rc$name[grep(pattern="*.ers$", rc$name)], sep = "/")
    } 
    
  })
  
  #### Read lc lookup table ####
  observeEvent(input$lc_file, {
    f <- input$lc_file
    rv$lc_path <- rename_uploaded_file(f)
    df_c <- read.csv(rv$lc_path)
    
    if(nrow(df_c) == 0)
      return()
    if(nrow(df_c) < 2)
      return()
    if(!is_numeric_str(df_c[1, 1]))
      return()
    
    df <- data.frame("ID_LC" = as.integer((as.character(df_c[, 1]))))
    df$LC <- df_c[, 2]
    rv$lc_df <- df
  })
  
  #### Set working directory ####
  shinyDirChoose(
    input, 
    'wd',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$wd)) {
      rv$wd <- parseDirPath(volumes, input$wd)
    }
  })
  
  output$selected_directory <- renderText({
    if(!is.null(rv$wd)) {
      paste0("Selected output directory: ",  rv$wd)
    } else {
      "No output directory selected"
    }
  })
  
  output$print_output_dir <- renderPrint({
    if(!is.null(rv$wd)) {
      cat(paste(rv$wd))
    } else {
      cat("No output directory selected")
    }
  })
  
  #### Set transition matrix directory ####
  shinyDirChoose(
    input, 
    'tm_path',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$tm_path)) {
      rv$tm_path <- parseDirPath(volumes, input$tm_path)
    }
  })
  
  output$print_tm_dir <- renderPrint({
    if(!is.null(rv$tm_path)) {
      cat(paste(rv$tm_path))
    } else {
      cat("No transition matrix directory selected")
    }
  })
  
  #### Set dcf directory ####
  shinyDirChoose(
    input, 
    'dcf_path',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$dcf_path)) {
      rv$dcf_path <- parseDirPath(volumes, input$dcf_path)
    }
  })
  
  output$print_dcf_dir <- renderPrint({
    if(!is.null(rv$dcf_path)) {
      cat(paste(rv$dcf_path))
    } else {
      cat("No woe dcf directory selected")
    }
  })
  
  #### Set DINAMICA Path ####
  shinyDirChoose(
    input, 
    'dinamica_path',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$dinamica_path)) {
      rv$dinamica_path <- parseDirPath(volumes, input$dinamica_path)
    } else {
      rv$dinamica_path <- NULL
    }
  })
  
  output$dinamica_path <- renderText({
    if(!is.null(rv$dinamica_path)) {
      paste0("Selected DINAMICA path: ",  rv$dinamica_path)
    } else {
      "No DINAMICA EGO path selected (Optional)"
    }
  })
  
  # Function to rename uploaded file
  rename_uploaded_file <- function(input_file) {
    if (is.null(input_file)) return(NULL)
    
    old_path <- input_file$datapath
    new_path <- file.path(dirname(old_path), input_file$name)
    file.rename(old_path, new_path)
    return(new_path)
  }
  
  # Input validation
  iv <- InputValidator$new()
  iv$add_rule("map1_file", sv_required(message = "Please upload land cover map at T1"))
  iv$add_rule("mapz_file", sv_required(message = "Please upload planning unit"))
  iv$add_rule("rc_file", sv_required(message = "Please upload raster cube"))
  iv$add_rule("lc_file", sv_required(message = "Please upload land cover lookup table"))
  iv$add_rule("tm_path", sv_required(message = "Please select a directory of transition matrix"))
  iv$add_rule("dcf_path", sv_required(message = "Please select a directory of woe dcf"))
  iv$add_rule("wd", sv_required(message = "Please select an output directory"))
  
  #### Do the calculation and store it to the markdown content ####
  observeEvent(input$processSimulate, {
    if(!iv$is_valid()) {
      iv$enable()
      showNotification(
        "Please correct the errors in the form and try again",
        id = "submit_message", type = "error")
      return()
    }
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = "Running SCIENDO Simulate", value = 0, {
      tryCatch({
        result <- run_sciendo_simulate_process(
          lc_t1_path = rv$map1_file,
          lc_lookup_table_path = rv$lc_path,
          lc_lookup_table = rv$lc_df,
          zone_path = rv$mapz_file,
          ers_path = rv$rc, 
          n_rep = input$repetition,
          tm_path = rv$tm_path,
          dcf_path = rv$dcf_path,
          dinamica_path = rv$dinamica_path,
          output_dir = rv$wd,
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          }
        )
        
        output$status_messages <- renderText("STATUS: Analysis completed successfully!")
        output$success_message <- renderText("Analysis completed successfully! You can now open the output folder.")
        output$error_messages <- renderText(NULL)
        removeNotification("running_notification")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("openReport")
        shinyjs::show("open_output_folder")
      }, error = function(e) {
        output$status_messages <- renderText(paste("Error in analysis:", e$message))
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        output$success_message <- renderText(NULL)
        removeNotification("running_notification")
        showNotification("Error in analysis. Please check the error messages.", type = "error")
      })
    })
    
  })
  
  observeEvent(input$openReport, {
    report_path <- paste0(rv$wd, "/sciendo_simulate_report_", Sys.Date(), ".html")
    if (file.exists(report_path)) {
      showNotification("Opening report...", type = "message")
      utils::browseURL(report_path)
    } else {
      showNotification("Report file not found.", type = "error")
    }
  })
  
  # Open output folder
  observeEvent(input$open_output_folder, {
    if (!is.null(rv$wd)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$wd)
      } else {
        system2("open", rv$wd)
      }
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
      shinyjs::delay(1000, stopApp())
    }
  })
}

shinyApp(ui = ui, server = server)