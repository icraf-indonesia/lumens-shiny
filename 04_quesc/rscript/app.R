source('function_ques_c.R')
source('../../helper.R')

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
  "shinyalert"
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
  titlePanel("QUES-C Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
      textInput("map1_year", "Year of T1", value = "1990"),
      fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
      textInput("map2_year", "Year of T2", value = "2000"),
      fileInput("carbon_file", "Carbon stock lookup table", accept = c(".csv")),
      fileInput("mapz_file", 
                "Planning Unit", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                multiple = T, 
                placeholder = "All related shapefiles"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select output directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("processQUESC", "Run", 
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
  options(shiny.maxRequestSize = 1024^3)
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = NULL,
    report_file = NULL,
    map1_year = NULL,
    map2_year = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    map1_rast = NULL,
    map2_rast = NULL,
    mapz_rast = NULL,
    mapc_spat = NULL,
    maps_spat = NULL,
    mapz_df = NULL,
    map_c1 = NULL,
    map_c2 = NULL,
    map_e = NULL,
    map_s = NULL,
    tbl_c = NULL,
    tbl_quesc = NULL,
    period_year = NULL
  )
  
  lc_list <- c("map1", "map2")
  map_list <- c("mapz", "map1", "map2")
  
  volumes <- c(
    Home = fs::path_home(), "R Installation" = R.home(), 
    getVolumes()()
  )
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  # Function to rename uploaded file
  rename_uploaded_file <- function(input_file) {
    if (is.null(input_file)) return(NULL)
    
    old_path <- input_file$datapath
    new_path <- file.path(dirname(old_path), input_file$name)
    file.rename(old_path, new_path)
    return(new_path)
  }
  
  #### Read file inputs ####
  lapply(lc_list, function(id) {
    inputs <- paste0(id, "_file")
    files <- paste0(id, "_file")
    rasters <- paste0(id, "_rast")
    df <- paste0(id, "_df")
    
    observeEvent(input[[inputs]], {
      rv[[files]] <- rename_uploaded_file(input[[inputs]])
      rv[[rasters]] <- rv[[files]] %>% raster() 
    })
  })
  
  observeEvent(input$mapz_file, {
    shp <- input$mapz_file
    if(is.null(shp))
      return()
    
    prev_wd <- getwd()
    uploaded_dir <- dirname(shp$datapath[1])
    setwd(uploaded_dir)
    for(i in 1:nrow(shp)){
      file.rename(shp$datapath[i], shp$name[i])
    }
    setwd(prev_wd)
    
    rv$mapz_file <- paste(uploaded_dir, shp$name[grep(pattern="*.shp$", shp$name)], sep = "/")
    
    zone_sf <- rv$mapz_file %>% st_read()
    zone <- zone_sf %>% 
      rasterise_multipolygon_quesc(
        raster_res = res(rv$map1_rast), 
        field = "IDS" 
      )
    rv$mapz_df <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
    rv$mapz_rast <- zone %>% raster()
  })
  
  #### Read year input ####
  lapply(lc_list, function(x) {
    id <- paste0(x, "_year")
    observeEvent(input[[id]], {
      rv[[id]] <- input[[id]]
    })
  })
  
  #### Read carbon lookup table ####
  observeEvent(input$carbon_file, {
    f <- input$carbon_file
    df_c <- read.csv(f$datapath)
    
    if(nrow(df_c) == 0)
      return()
    if(nrow(df_c) < 2)
      return()
    if(!is_numeric_str(df_c[1, 1]))
      return()
    
    df <- data.frame("ID_LC" = as.integer((as.character(df_c[, 1]))))
    df$LC <- df_c[, 2]
    df$CARBON <- df_c[, 3]
    rv$tbl_c <- df
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
  
  # Input validation
  iv <- InputValidator$new()
  iv$add_rule("map1_file", sv_required(message = "Please upload land cover map at T1"))
  iv$add_rule("map2_file", sv_required(message = "Please upload land cover map at T2"))
  iv$add_rule("mapz_file", sv_required(message = "Please upload planning unit"))
  iv$add_rule("carbon_file", sv_required(message = "Please upload carbon stock lookup table"))
  iv$add_rule("map1_year", sv_required(message = "Please define the year of T1"))
  iv$add_rule("map2_year", sv_required(message = "Please define the year of T2"))
  iv$add_rule("wd", sv_required(message = "Please select an output directory"))
  
  #### Do the calculation and store it to the markdown content ####
  observeEvent(input$processQUESC, {
    if(!iv$is_valid()) {
      iv$enable()
      showNotification(
        "Please correct the errors in the form and try again",
        id = "submit_message", type = "error")
      return()
    }
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    withProgress(message = "Running QUES-C Analysis", value = 0, {
      tryCatch({
        c_lookup_path <- rename_uploaded_file(input$carbon_file)
        results <- run_quesc_analysis(
          lc_t1_path = rv$map1_file,
          lc_t2_path = rv$map2_file,
          admin_z_path = rv$mapz_file,
          c_lookup_path = c_lookup_path,
          lc_t1_input = rv$map1_rast,
          lc_t2_input = rv$map2_rast,
          admin_z_input = rv$mapz_rast,
          c_lookup_input = rv$tbl_c,
          zone_lookup_input = rv$mapz_df,
          time_points = list(t1 = rv$map1_year, t2 = rv$map2_year),
          output_dir = rv$wd,
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          }
        )
        
        # save to reactive
        rv$map_c1 <- results$map_c1 
        rv$map_c2 <- results$map_c2 
        rv$map_e <- results$map_em 
        rv$map_s <- results$map_sq 
        rv$tbl_quesc <- results$ques_db
        rv$period_year <- as.numeric(results$p1) - as.numeric(results$p2)
        
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
    report_path <- paste0(rv$wd, "/quesc_report_", Sys.Date(), ".html")
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