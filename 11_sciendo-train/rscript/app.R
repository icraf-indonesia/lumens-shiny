source('function_sciendo_train.R')
source('../../helper.R')

install_load(
  "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert",
  "usdm", "corrplot", "tidyr", "openxlsx", "openxlsx2", "RDCOMClient", "XML", "xml2"
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
  titlePanel("SCIENDO Train"),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
      textInput("map1_year", "Year of T1"),
      fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
      textInput("map2_year", "Year of T2"),
      fileInput("lc_file", "Land Use/Cover Lookup Table (CSV)", accept = c(".csv")),
      radioButtons("zone_type", "Planning Unit Input Type", 
                   choices = c("Raster" = "raster", "Shapefiles" = "shapefile"), inline = T),
      conditionalPanel(
        condition = "input.zone_type == 'shapefile'",
        fileInput("mapz_file", 
                  "Planning Unit", 
                  accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                  multiple = T, 
                  placeholder = "All related shapefiles (.shp, .dbf, .prj, .shx)")
      ),
      conditionalPanel(
        condition = "input.zone_type == 'raster'",
        fileInput("mapz_file", "Planning Unit", accept = c("image/tiff"))
      ),
      fileInput("z_file", "Planning Unit Lookup Table (CSV)", accept = c(".csv")),
      
      # selectInput("memory_allocation", "Choose Memory Allocation",
      #   choices = c("Balanced" = 1, "Prefer Memory" = 0, "Prefer Disk" = 2, "Memory Only" = 3, "Aggressive" = 4)
      # ),
      
      tags$head(
        tags$style(HTML("
      .selectize-dropdown-content .option {
        padding-left: 20px;
      }
    "))
      ),
      
      selectizeInput("memory_allocation", "Choose Memory Allocation",
                     choices = c("Balanced" = 1, "Prefer Memory" = 0, "Prefer Disk" = 2, 
                                 "Memory Only" = 3, "Aggressive" = 4),
                     options = list(render = I("
                   {
                     option: function(item, escape) {
                       var explanations = {
                         '1': 'Balanced: This memory management policy keeps input maps in memory and tries to keep resulting maps in memory whenever possible. However, resulting maps will be kept in disk if there is not enough free memory available to store them, or if Dinamica EGO suspects that by storing them in memory, it will hurt its ability of allocating memory to perform other types of operations in the future.',
                         '0': 'Prefer Memory: This memory management policy keeps input maps and resulting maps in memory whenever possible. However, maps will be kept in disk and brought back to memory piece by piece if there is not enough free memory available to store them. Beware that a model execution might fail if there is not enough free memory left in the system to perform other types of operations.',
                         '2': 'Prefer Disk: This memory management policy keeps all maps in disk, including the resulting ones, and brings them back to memory piece by piece, as necessary.',
                         '3': 'Memory Only: This memory management policy keeps input maps and resulting maps only in memory. However, if there is not enough free memory available to store the maps, an ERROR will be reported and the model script execution will be aborted.',
                         '4': 'Aggressive: This memory management policy keeps all input maps in disk and keeps resulting maps in memory whenever possible. However, maps will be kept in disk and brought back to memory piece by piece if there is not enough free memory available to store them. Beware that a model execution might fail if there is not enough free memory left in the system to perform other types of operations.'
                       };
                       var tooltip = explanations[item.value] || 'No explanation available';
                       return '<div style=\"padding-left: 20px;\" title=\"' + tooltip + '\">' + escape(item.label) + '</div>';
                     }
                   }
                 "))),
      
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("factors_path", "Factor(s) Folder Path", "Choose a folder contains factor files"),
          verbatimTextOutput("print_factor_dir", placeholder = TRUE),
          shinyDirButton("wd", "Select output directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          shinyDirButton("dinamica_path", "DINAMICA EGO Path (Optional)", "(Optional)"),
          actionButton("processTrain", "Run Analysis", 
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
        tabPanel("User Guide", includeMarkdown("../helpfile/sciendo_train_quick_user_guide.md")),
        tabPanel("Log",
                 textOutput("selected_directory"),
                 textOutput("dinamica_path"),
                 textOutput("factor_directory"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100 * 1024^2)
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = NULL,
    factors_path = NULL,
    dinamica_path = NULL,
    report_file = NULL,
    map1_year = NULL,
    map2_year = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    mapz_df = NULL,
    lc_path = NULL,
    lc_df = NULL,
    z_path = NULL,
    period_year = NULL,
    memory_allocation = NULL
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
  
  #### Read file inputs ####
  lapply(lc_list, function(id) {
    inputs <- paste0(id, "_file")
    files <- paste0(id, "_file")
    rasters <- paste0(id, "_rast")
    df <- paste0(id, "_df")
    
    observeEvent(input[[inputs]], {
      rv[[files]] <- rename_uploaded_file(input[[inputs]])
      # rv[[rasters]] <- rv[[files]] %>% raster() 
    })
  })
  
  observeEvent(input$mapz_file, {
    mapz <- input$mapz_file
    if(is.null(mapz))
      return()
    
    if(input$zone_type == "raster") {
      rv$mapz_file <- rename_uploaded_file(mapz)
    } else {
      shp <- mapz
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
        rasterise_multipolygon(
          raster_res = c(100, 100), 
          field = "IDS" 
        )
      rv$mapz_df <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
      rv$mapz_rast <- zone %>% raster()
    }
  })
  
  #### Read year input ####
  lapply(lc_list, function(x) {
    id <- paste0(x, "_year")
    observeEvent(input[[id]], {
      rv[[id]] <- input[[id]]
    })
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
  
  #### Read zone lookup table ####
  observeEvent(input$z_file, {
    f <- input$z_file
    rv$z_path <- rename_uploaded_file(f)
    df_z <- read.csv(rv$z_path)
    
    if(nrow(df_z) == 0)
      return()
    if(nrow(df_z) < 2)
      return()
    if(!is_numeric_str(df_z[1, 1]))
      return()
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
  
  #### Set factors directory ####
  shinyDirChoose(
    input, 
    'factors_path',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$factors_path)) {
      rv$factors_path <- parseDirPath(volumes, input$factors_path)
    }
  })
  
  output$factor_directory <- renderText({
    if(!is.null(rv$factors_path)) {
      paste0("Selected factors directory: ",  rv$factors_path)
    } else {
      "No factors directory selected"
    }
  })
  
  output$print_factor_dir <- renderPrint({
    if(!is.null(rv$factors_path)) {
      cat(paste(rv$factors_path))
    } else {
      cat("No factors directory selected")
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
  iv$add_rule("map2_file", sv_required(message = "Please upload land cover map at T2"))
  iv$add_rule("mapz_file", sv_required(message = "Please upload planning unit"))
  iv$add_rule("map1_year", sv_required(message = "Please define the year of T1"))
  iv$add_rule("map2_year", sv_required(message = "Please define the year of T2"))
  iv$add_rule("lc_file", sv_required(message = "Please upload land cover lookup table"))
  iv$add_rule("z_file", sv_required(message = "Please upload planning unit lookup table"))
  iv$add_rule("factors_path", sv_required(message = "Please select a directory of factors"))
  iv$add_rule("wd", sv_required(message = "Please select an output directory"))
  iv$add_rule("memory_allocation", sv_required(message = "Please select memory allocation option"))
  
  #### Do the calculation and store it to the markdown content ####
  observeEvent(input$processTrain, {
    if(!iv$is_valid()) {
      iv$enable()
      showNotification(
        "Please correct the errors in the form and try again",
        id = "submit_message", type = "error")
      return()
    }
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    if(input$zone_type == "raster") {
      zone_path <- rv$mapz_file
    } else {
      zone_path <- paste0(rv$wd, "/zone.tif")
      writeRaster(rv$mapz_rast, zone_path, datatype = "INT1U", overwrite = T)
    }
    
    withProgress(message = "Running SCIENDO Train", value = 0, {
      tryCatch({
        result <- run_sciendo_train_process(
          lc_t1_path = rv$map1_file,
          lc_t2_path = rv$map2_file,
          zone_path = zone_path,
          lc_lookup_table_path = rv$lc_path,
          lc_lookup_table = rv$lc_df,
          z_lookup_table_path = rv$z_path,
          factor_path = rv$factors_path,
          time_points = list(t1 = rv$map1_year, t2 = rv$map2_year),
          dinamica_path = rv$dinamica_path,
          output_dir = rv$wd,
          memory_allocation = input$memory_allocation,
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
    report_path <- paste0(rv$wd, "/sciendo_train_report_", Sys.Date(), ".html")
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