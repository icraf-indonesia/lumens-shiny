source('function_sciendo_train.R')

install_load(
  "shinyFiles", "shinyvalidate", "shinyjs", "bslib",
  "dplyr", "remotes", "rmarkdown", "XML"
)


jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("SCIENDO Train"),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
      textInput("map1_year", "Year of T1"),
      fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
      textInput("map2_year", "Year of T2"),
      fileInput("lc_file", "Land Use/Cover Lookup Table (CSV)", accept = c(".csv")),
      fileInput("mapz_file", 
                "Planning Unit", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                multiple = T, 
                placeholder = "All related shapefiles (.shp, .dbf, .prj, .shx)"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("factors_path", "Factor(s) Folder Path", "Choose a folder contains factor files"),
          shinyDirButton("dinamica_path", "DINAMICA EGO Path (Optional)", "(Optional)"),
          shinyDirButton("wd", "Select output directory", "Please select a directory"),
          actionButton("processTrain", "Run Analysis", 
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("openReport", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", includeMarkdown("../helpfile/sciendo_train_help.md")),
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
    map1_rast = NULL,
    map2_rast = NULL,
    mapz_rast = NULL,
    mapz_df = NULL,
    tbl_lc = NULL,
    tbl_quesc = NULL,
    period_year = NULL
  )
  
  lc_list <- c("map1", "map2")
  map_list <- c("mapz", "map1", "map2")
  
  volumes <- c(
    getVolumes()()
  )
  
  #### Read file inputs ####
  lapply(lc_list, function(id) {
    inputs <- paste0(id, "_file")
    files <- paste0(id, "_file")
    rasters <- paste0(id, "_rast")
    df <- paste0(id, "_df")
    
    observeEvent(input[[inputs]], {
      rv[[files]] <- input[[inputs]]
      rv[[rasters]] <- rv[[files]]$datapath %>% raster() 
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
      rasterise_multipolygon(
        raster_res = c(100, 100), 
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
  
  output$selected_directory <- renderText({
    if(!is.null(rv$factors_path)) {
      paste0("Selected factors directory: ",  rv$factors_path)
    } else {
      "No factors directory selected"
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
    }
  })
  
  output$selected_directory <- renderText({
    if(!is.null(rv$dinamica_path)) {
      paste0("Selected DINAMICA path: ",  rv$dinamica_path)
    } else {
      "No DINAMICA EGO path selected (Optional)"
    }
  })
  
  # Input validation
  iv <- InputValidator$new()
  iv$add_rule("map1_file", sv_required(message = "Please upload land cover map at T1"))
  iv$add_rule("map2_file", sv_required(message = "Please upload land cover map at T2"))
  iv$add_rule("mapz_file", sv_required(message = "Please upload planning unit"))
  iv$add_rule("map1_year", sv_required(message = "Please define the year of T1"))
  iv$add_rule("map2_year", sv_required(message = "Please define the year of T2"))
  iv$add_rule("factors_path", sv_required(message = "Please select a directory of factors"))
  iv$add_rule("wd", sv_required(message = "Please select an output directory"))
  
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
    
    # preparing factors
    listFactors <- rv$factors_path %>% list.files(full.names=TRUE, pattern=".tif$") %>%
      data.frame(file=., select=1)
    
    factors <- as.character(listFactors$file)
    nFactors <- length(factors)
    
    aliasFactor<-NULL
    for (a in 1:nFactors) {
      temp <- substr(basename(factors[a]), 1, nchar(basename(factors[a])) - 4)
      aliasFactor <- c(aliasFactor, temp)
    }
    
    dinamica_path <- rv$dinamica_path
    if (is.null(dinamica_path)) {
      program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
      dinamica_dirs <- list.files(program_files, pattern = "^Dinamica EGO", full.names = TRUE)
      
      if (length(dinamica_dirs) == 0) {
        stop("No DINAMICA EGO installation found.")
      }
      
      # Sort directories to use the latest version if multiple are found
      dinamica_path <- sort(dinamica_dirs, decreasing = TRUE)[1]
    } 
    dinamica_exe <- dinamica_path %>% 
      list.files(pattern = "^DinamicaConsole", full.names = TRUE) %>%
      nth(2)
    
    # create raster cube egoml
    # begin writing tag
    con <- xmlOutputDOM(tag="script")
    # add property
    con$addTag("property", attrs=c(key="dff.date", value="2016-Oct-17 12:02:15"))
    con$addTag("property", attrs=c(key="dff.version", value="3.0.17.20160922"))
    
    # begin.
    # add functor = SaveMap
    con$addTag("functor", attrs=c(name="SaveMap"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value="saveMap1680"))
    con$addTag("inputport", attrs=c(name="map", peerid=paste("v", nFactors+1,sep="")))
    con$addTag("inputport", attrs=c(name="filename"), paste('"', rv$wd, '/sciendo_factor.ers"', sep=''))
    con$addTag("inputport", attrs=c(name="suffixDigits"), 2)
    con$addTag("inputport", attrs=c(name="step"), ".none")
    con$addTag("inputport", attrs=c(name="useCompression"), ".yes")
    con$addTag("inputport", attrs=c(name="workdir"), ".none")
    con$closeTag("functor")
    # end.
    
    # begin.
    # add functor = LoadMap
    for (b in 1:nFactors){
      con$addTag("functor", attrs=c(name="LoadMap"), close=FALSE)
      con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[b]))
      con$addTag("inputport", attrs=c(name="filename"), paste('"', factors[b], '"', sep=""))
      con$addTag("inputport", attrs=c(name="nullValue"), ".none")
      con$addTag("inputport", attrs=c(name="loadAsSparse"), ".no")
      con$addTag("inputport", attrs=c(name="suffixDigits"), 0)
      con$addTag("inputport", attrs=c(name="step"), ".none")
      con$addTag("inputport", attrs=c(name="workdir"), ".none")
      con$addTag("outputport", attrs=c(name="map", id=paste("v",b,sep="")))
      con$closeTag("functor") 
    }
    # end.
    
    # begin.
    # add containerfunctor = CreateCubeMap
    con$addTag("containerfunctor", attrs=c(name="CreateCubeMap"), close=FALSE)
    con$addTag("property", attrs=c(key="dff.functor.alias", value="createCubeMap1678"))
    con$addTag("inputport", attrs=c(name="cellType"), ".float32")
    con$addTag("inputport", attrs=c(name="nullValue"), "-9999")
    con$addTag("outputport", attrs=c(name="map", id=paste("v", nFactors+1, sep="")))
    # add subtag functor for CreateCubeMap
    for (c in 1:nFactors) {
      con$addTag("functor", attrs=c(name="NumberAndNameMap"), close=FALSE)
      con$addTag("property", attrs=c(key="dff.functor.alias", value=aliasFactor[c]))
      con$addTag("inputport", attrs=c(name="map", peerid=paste("v", c, sep="")))
      con$addTag("inputport", attrs=c(name="mapName"), paste('"', aliasFactor[c], '"', sep=""))
      con$addTag("inputport", attrs=c(name="mapNumber"), 0)
      con$closeTag("functor")
    }
    con$closeTag("containerfunctor")
    # end.
    
    saveXML(con$value(), file=paste(rv$wd, "/01_sciendo_train_raster_cube.egoml", sep=''))
    
    # Prepare DINAMICA Console command
    command <- paste('"', dinamica_exe, '" -processors 0 -log-level 4 "', rv$wd, '/01_sciendo_train_raster_cube.egoml"', sep="")
  
    # Execute DINAMICA
    result <- system(command)
    
    if(result != 0) {
      stop("DINAMICA EGO execution failed. Check installation and parameters.")
    } else {
      message("DINAMICA EGO training process completed successfully.")
    }
    
    
    
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
}

shinyApp(ui = ui, server = server)