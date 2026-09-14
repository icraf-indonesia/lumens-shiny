library(conflicted)

conflict_prefer("xml_find_all", "xml2")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("extract", "tidyr")

source('function_sciendo_simulate.R')
source('../../helper.R')
options(repos = c(CRAN = "https://cloud.r-project.org"))

install_load(
  "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert",
  "terra", "tibble", "ggplot2", "magrittr", "tidyr", "tidyterra", "DT",
  "readr", "plotly", "scales", "ggthemes", "rlang", "xml2", "tidyverse",
  "readxl", "openxlsx", "openxlsx2", "conflicted", "stringr", "mapview",
  "tools", "leafsync", "leaflet"
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
      fileInput("map1_file",
                label = label_with_help("Initial Land Cover/Use Map", "map1_file"),
                accept = c("image/tiff"), placeholder = "Upload raster file (.tif)"),
      numericInput("init_year",
                   label = label_with_help("Initial Year", "init_year"),
                   value = 2025),
      fileInput("mapz_file",
                label = label_with_help("Planning Unit Map", "mapz_file"),
                accept = c("image/tiff"), placeholder = "Upload raster file (.tif)"),
      fileInput("lc_file",
                label = label_with_help("Land Use/Cover Lookup Table", "lc_file"),
                accept = c(".csv", ".xlsx"), placeholder = "Upload table file (.csv/.xlsx)"),
      fileInput("rc_file",
                label = label_with_help("Raster Cube Map", "rc_file"),
                accept = c(".tif", ".xml"), multiple = TRUE, placeholder = "Upload raster file (.tif & .xml)"),
      numericInput("repetition",
                   label = label_with_help("Simulation Periods", "repetition"),
                   value = 2),
      
      tags$head(
        tags$style(HTML("
      .selectize-dropdown-content .option {
        padding-left: 20px;
      }
      .alloc-disabled {
        opacity: 0.5;
        pointer-events: none;
        user-select: none;
        filter: grayscale(0.3);
      }
      .alloc-info-row {
        display: flex;
        align-items: center;
        gap: 6px;
        margin-bottom: 6px;
      }
      .alloc-info-row .form-group {
        margin-bottom: 0;
        flex: 1;
      }
      .alloc-label {
        display: flex;
        justify-content: space-between;
        align-items: flex-start;
        gap: 4px;
        width: 100%;
      }
      .alloc-help-icon {
        display: inline-flex;
        align-items: center;
        color: #0d6efd;
        cursor: pointer;
        flex-shrink: 0;
        margin-top: 2px;
        line-height: 1;
      }
      .alloc-help-icon a,
      .alloc-help-icon .action-button {
        color: inherit !important;
        text-decoration: none !important;
        background: none !important;
        border: none !important;
        padding: 0 !important;
        cursor: pointer !important;
      }
      .alloc-help-icon:hover,
      .alloc-help-icon:hover a {
        color: #0a58ca !important;
      }
      .alloc-3col .form-group > label {
        min-height: 3em;
        display: block;
      }
      .alloc-tmpl-row .btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 6px;
      }
      .alloc-tmpl-row .form-group {
        margin-bottom: 0;
      }
      .dir-btn-wrapper {
        position: relative;
        width: 100%;
      }
      .dir-btn-wrapper .btn {
        width: 100%;
        text-align: center;
      }
      .dir-help-overlay {
        position: absolute;
        right: 12px;
        top: 50%;
        transform: translateY(-50%);
        z-index: 10;
      }
      /.form-group > label.control-label {
      /  display: block;
      /  width: 100%;
      /}
    "))
      ),
      
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          dir_button_with_help(
            "tm_path",
            "Transition Probability Matrix Folder Path",
            "Choose a folder contains CSV files",
            "tm_path"
          ),
          verbatimTextOutput("print_tm_dir", placeholder = TRUE),
          
          dir_button_with_help(
            "dcf_path",
            "Weights of Evidence Folder Path",
            "Choose a folder contains DCF files",
            "dcf_path"
          ),
          verbatimTextOutput("print_dcf_dir", placeholder = TRUE),
          
          dir_button_with_help(
            "wd",
            "Select output directory",
            "Please select a directory",
            "wd"
          ),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          
          dir_button_with_help(
            "dinamica_path",
            "DINAMICA EGO Path (Optional)",
            "(Optional)",
            "dinamica_path"
          ),
          verbatimTextOutput("print_dinamica_path", placeholder = TRUE),
          
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Advanced Settings",
              icon = icon("gear"),
              value = "advanced_settings",
              
              selectizeInput("memory_allocation",
                             label = label_with_help("Choose Memory Allocation", "memory_allocation"),
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
              
              tags$hr(),
              
              tags$div(
                class = "alloc-info-row",
                checkboxInput("alloc_custom",
                              "Parameterize Allocate Transitions",
                              value = FALSE),
                alloc_help_icon("custom")
              ),
              tags$div(
                id = "alloc_params_wrapper",
                class = "alloc-disabled",
                
                numericInput("alloc_percent",
                             label = tags$div(
                               class = "alloc-label",
                               tags$span(HTML("Percent of Transitions by Expansion (0\u20131)")),
                               alloc_help_icon("percent")
                             ),
                             value = 0.5, min = 0, max = 1, step = 0.05),
                
                fluidRow(
                  class = "alloc-3col",
                  column(4, numericInput("alloc_exp_mean",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Expansion Mean<br>Patch Size (ha)")),
                                           alloc_help_icon("exp_mean")
                                         ),
                                         value = 2, min = 0, step = 0.1)),
                  column(4, numericInput("alloc_exp_var",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Expansion Patch<br>Size Variance (ha)")),
                                           alloc_help_icon("exp_var")
                                         ),
                                         value = 1, min = 0, step = 0.1)),
                  column(4, numericInput("alloc_exp_iso",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Expansion Patch<br>Isometry (0\u20132)")),
                                           alloc_help_icon("exp_iso")
                                         ),
                                         value = 1, min = 0, max = 2, step = 0.1))
                ),
                
                fluidRow(
                  class = "alloc-3col",
                  column(4, numericInput("alloc_gen_mean",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Generation Mean<br>Patch Size (ha)")),
                                           alloc_help_icon("gen_mean")
                                         ),
                                         value = 1, min = 0, step = 0.1)),
                  column(4, numericInput("alloc_gen_var",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Generation Patch<br>Size Variance (ha)")),
                                           alloc_help_icon("gen_var")
                                         ),
                                         value = 1, min = 0, step = 0.1)),
                  column(4, numericInput("alloc_gen_iso",
                                         label = tags$div(
                                           class = "alloc-label",
                                           tags$span(HTML("Generation Patch<br>Isometry (0\u20132)")),
                                           alloc_help_icon("gen_iso")
                                         ),
                                         value = 1, min = 0, max = 2, step = 0.1))
                ),
                
                tags$hr(),
                
                tags$p(
                  style = "margin-bottom: 8px; display: flex; align-items: center; gap: 6px;",
                  tags$span("Per-Transition Allocation (optional)"),
                  alloc_help_icon("override")
                ),
                
                tags$div(
                  class = "alloc-tmpl-row",
                  style = "display: flex; gap: 10px; margin-bottom: 10px;",
                  tags$div(style = "flex: 1;",
                           actionButton("generate_alloc_template", "Generate Template",
                                        icon = icon("file-excel"),
                                        style = "font-size: 14px; padding: 6px 10px; width: 100%; height: 38px;")
                  ),
                  tags$div(style = "flex: 1;",
                           downloadButton("download_alloc_template", "Download Template",
                                          icon = icon("download"),
                                          style = "font-size: 14px; padding: 6px 10px; width: 100%; height: 38px;")
                  )
                ),
                
                fileInput("alloc_override_file",
                          "Upload Filled Template (.csv or .xlsx)",
                          accept = c(".csv", ".xlsx"), placeholder = "Upload table file (.csv/.xlsx)",
                          width = "100%")
              )
            )
          ),
          
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
        tabPanel("User Guide", 
                 div(
                   style = "height: 800px; overflow-y: auto; padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #fff;",
                   includeMarkdown("../helpfile/sciendo_simulate_quick_user_guide_ID.md")
                 )
        ),
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
  options(shiny.maxRequestSize = 100 * 1024^2)
  rv <- reactiveValues(
    wd = NULL,
    dinamica_path = NULL,
    report_file = NULL,
    map1_file = NULL,
    mapz_file = NULL,
    mapz_df = NULL,
    lc_path = NULL,
    lc_df = NULL,
    zone_df = NULL,
    dcf_path = NULL,
    tm_path = NULL,
    period_value = NULL,
    memory_allocation = NULL,
    rc = NULL,
    rc_xml = NULL,
    alloc_override_df = NULL,
    alloc_template    = NULL
  )
  
  volumes <- c(
    Home = fs::path_home(), "R Installation" = R.home(), 
    getVolumes()()
  )
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  # Read file inputs
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
  
  # Read file inputs
  observeEvent(input$rc_file, {
    rc <- input$rc_file
    if (is.null(rc)) return()
    
    has_tif <- any(grepl("\\.tif$", rc$name, ignore.case = TRUE))
    has_xml <- any(grepl("\\.xml$", rc$name, ignore.case = TRUE))
    
    if (!has_tif || !has_xml) {
      showNotification(
        "Please upload both a .tif and a .xml file.",
        type = "error",
        duration = 5
      )
      return() 
    }
    
    prev_wd <- getwd()
    uploaded_dir <- dirname(rc$datapath[1])
    setwd(uploaded_dir)
    
    # rename uploaded files to their original names
    for (i in 1:nrow(rc)) {
      file.rename(rc$datapath[i], rc$name[i])
    }
    
    # save paths
    tif_file <- rc$name[grep("\\.tif$", rc$name, ignore.case = TRUE)]
    xml_file <- rc$name[grep("\\.xml$", rc$name, ignore.case = TRUE)]
    rv$rc <- file.path(uploaded_dir, tif_file)
    rv$rc_xml <- file.path(uploaded_dir, xml_file)
    
    xml_data <- xml2::read_xml(rv$rc_xml)  
    
    # Extract PU classes
    pu_classes <- xml_data %>%
      xml2::xml_find_all(".//PUClasses/Class") %>% 
      purrr::map_df(~ tibble(
        ID = xml2::xml_attr(., "ID"),
        PU = xml2::xml_attr(., "PU"))
      )
    
    # Extract period value
    period_value <- xml_data %>%
      xml2::xml_find_first(".//Period/Value") %>%
      xml2::xml_text() %>%
      as.numeric()
    
    rv$zone_df <- pu_classes
    rv$period_value <- period_value
    
    setwd(prev_wd)
  })
  
  
  
  # Read lc lookup table
  observeEvent(input$lc_file, {
    f <- input$lc_file
    if (is.null(f)) {
      rv$lc_path <- NULL
      return()
    }
    
    ext <- tolower(tools::file_ext(f$name))
    df_c <- tryCatch({
      if (ext == "csv") {
        read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "xlsx") {
        as.data.frame(openxlsx::read.xlsx(f$datapath, check.names = FALSE))
      } else {
        stop("Unsupported file format. Please upload a .csv or .xlsx file.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message),
                       type = "error", duration = 6)
      NULL
    })
    
    if (is.null(df)) {
      rv$lc_path <- NULL
      return()
    }
    
    
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
  
  # Set working directory 
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
  
  # Set transition matrix directory
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
  
  # Set dcf directory
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
  
  # Set DINAMICA Path
  shinyDirChoose(
    input, 
    'dinamica_path',
    roots = volumes,
    session = session
  )
  
  observe({
    if (!is.null(input$dinamica_path) && !identical(parseDirPath(volumes, input$dinamica_path), character(0))) {
      rv$dinamica_path <- parseDirPath(volumes, input$dinamica_path)
    } else {
      program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
      dinamica_dirs <- list.files(program_files, pattern = "^Dinamica EGO", full.names = TRUE, recursive = FALSE)
      
      if (length(dinamica_dirs) == 0) {
        showNotification("No DINAMICA EGO installation found.", type = "error")
        rv$dinamica_path <- NULL
      } else {
        rv$dinamica_path <- sort(dinamica_dirs, decreasing = TRUE)[1]
      }
    }
  })
  
  output$dinamica_path <- renderText({
    if(!is.null(rv$dinamica_path)) {
      paste0("Selected DINAMICA path: ",  rv$dinamica_path)
    } else {
      "No DINAMICA EGO path selected (Optional)"
    }
  })
  
  output$print_dinamica_path <- renderPrint({
    if(!is.null(rv$dinamica_path)) {
      cat(paste(rv$dinamica_path))
    } else {
      cat("No DINAMICA EGO path selected")
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
  
  #### Click-to-open help modals ####
  lapply(names(alloc_help_texts), function(id) {
    observeEvent(input[[paste0("alloc_help_", id)]], {
      info <- alloc_help_texts[[id]]
      showModal(modalDialog(
        title = tagList(icon("circle-info"), " ", info$title),
        HTML(info$body),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m"
      ))
    })
  })
  
  # Allocate Transitions
  observe({
    if (isTRUE(input$alloc_custom)) {
      shinyjs::removeClass("alloc_params_wrapper", "alloc-disabled")
    } else {
      shinyjs::addClass("alloc_params_wrapper", "alloc-disabled")
    }
  })
  
  # Generate template
  observeEvent(input$generate_alloc_template, {
    if (is.null(rv$lc_df)) {
      showNotification(
        "Please upload the Land Use/Cover Lookup Table (CSV) first.",
        type = "warning", duration = 5
      )
      return()
    }
    rv$alloc_template <- make_alloc_trans_template(rv$lc_df)
    showNotification(
      "Template generated. Click 'Download Template' to save it.",
      type = "message", duration = 5
    )
  })
  
  # Download template (xlsx)
  output$download_alloc_template <- downloadHandler(
    filename = function() {
      paste0("allocate_transitions_template_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tmpl <- rv$alloc_template
      if (is.null(tmpl)) {
        if (is.null(rv$lc_df)) {
          showNotification(
            "Please upload the Land Use/Cover Lookup Table (CSV) first.",
            type = "warning", duration = 5
          )
          openxlsx::write.xlsx(data.frame(), file)
          return()
        }
        tmpl <- make_alloc_trans_template(rv$lc_df)
        rv$alloc_template <- tmpl
      }
      openxlsx::write.xlsx(tmpl, file)
    }
  )
  
  # Parse override upload 
  observeEvent(input$alloc_override_file, {
    f <- input$alloc_override_file
    if (is.null(f)) {
      rv$alloc_override_df <- NULL
      return()
    }
    
    ext <- tolower(tools::file_ext(f$name))
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(f$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "xlsx") {
        as.data.frame(openxlsx::read.xlsx(f$datapath, check.names = FALSE))
      } else {
        stop("Unsupported file format. Please upload a .csv or .xlsx file.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message),
                       type = "error", duration = 6)
      NULL
    })
    
    if (is.null(df)) {
      rv$alloc_override_df <- NULL
      return()
    }
    
    required <- c("from_id", "to_id",
                  "percent", "exp_mean", "exp_var", "exp_iso",
                  "gen_mean", "gen_var", "gen_iso")
    missing_cols <- setdiff(required, names(df))
    if (length(missing_cols) > 0) {
      showNotification(
        paste("Template is missing required columns:",
              paste(missing_cols, collapse = ", ")),
        type = "error", duration = 6
      )
      rv$alloc_override_df <- NULL
      return()
    }
    
    # Coerce id columns to integer
    df$from_id <- as.integer(df$from_id)
    df$to_id   <- as.integer(df$to_id)
    
    rv$alloc_override_df <- df
    showNotification(
      "Override template loaded. Non-empty cells will override inputs and defaults.",
      type = "message", duration = 5
    )
  })
  
  # Input validation
  iv <- InputValidator$new()
  
  iv$add_rule("map1_file", sv_required(message = "Please upload land cover map at T1"))
  iv$add_rule("mapz_file", sv_required(message = "Please upload planning unit"))
  iv$add_rule("rc_file", sv_required(message = "Please upload raster cube"))
  iv$add_rule("lc_file", sv_required(message = "Please upload land cover lookup table"))
  iv$add_rule("tm_path", sv_required(message = "Please select a directory of transition matrix"))
  iv$add_rule("dcf_path", sv_required(message = "Please select a directory of woe dcf"))
  iv$add_rule("wd", sv_required(message = "Please select an output directory"))
  iv$add_rule("memory_allocation", sv_required(message = "Please select memory allocation option"))
  
  # Raster cube input validation
  iv$add_rule("rc_file", function(value) {
    if (is.null(value)) return(NULL)  
    
    has_tif <- any(grepl("\\.tif$", value$name, ignore.case = TRUE))
    has_xml <- any(grepl("\\.xml$", value$name, ignore.case = TRUE))
    
    if (!has_tif || !has_xml) {
      return("Please upload both a .tif and a .xml file for the raster cube")
    }
    
    NULL
  })
  
  # Only validate allocate transition parameters when the checkbox is enabled
  iv$add_rule("alloc_percent", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value < 0 || value > 1) return("Must be between 0 and 1 (inclusive)")
    NULL
  })
  iv$add_rule("alloc_exp_mean", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value <= 0) return("Must be greater than 0")
    NULL
  })
  iv$add_rule("alloc_exp_var", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value < 0) return("Must be 0 or greater")
    NULL
  })
  iv$add_rule("alloc_exp_iso", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value < 0 || value > 2) return("Must be between 0 and 2 (inclusive)")
    NULL
  })
  iv$add_rule("alloc_gen_mean", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value <= 0) return("Must be greater than 0")
    NULL
  })
  iv$add_rule("alloc_gen_var", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value < 0) return("Must be 0 or greater")
    NULL
  })
  iv$add_rule("alloc_gen_iso", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return("Please provide a value")
    if (value < 0 || value > 2) return("Must be between 0 and 2 (inclusive)")
    NULL
  })
  
 # Process simulate
  observeEvent(input$processSimulate, {
    if(!iv$is_valid()) {
      iv$enable()
      showNotification(
        "Please correct the errors in the form and try again",
        id = "submit_message", type = "error")
      return()
    }
    
    showNotification("Analysis is running. Please wait...", type = "message", duration = NULL, id = "running_notification")
    
    # Prepare allocate-transition parameters
    alloc_params <- NULL
    alloc_override <- NULL
    if (isTRUE(input$alloc_custom)) {
      alloc_params <- list(
        percent  = input$alloc_percent,
        exp_mean = input$alloc_exp_mean,
        exp_var  = input$alloc_exp_var,
        exp_iso  = input$alloc_exp_iso,
        gen_mean = input$alloc_gen_mean,
        gen_var  = input$alloc_gen_var,
        gen_iso  = input$alloc_gen_iso
      )
      alloc_override <- rv$alloc_override_df
    }
    
    withProgress(message = "Running SCIENDO Simulate", value = 0, {
      tryCatch({
        result <- run_sciendo_simulate_process(
          lc_t1_path = rv$map1_file,
          initial_year = input$init_year,
          period_value = rv$period_value,
          lc_lookup_table_path = rv$lc_path,
          lc_lookup_table = rv$lc_df,
          zone_lookup_table = rv$zone_df,
          zone_path = rv$mapz_file,
          ers_path = rv$rc, 
          n_rep = input$repetition,
          tm_path = rv$tm_path,
          dcf_path = rv$dcf_path,
          dinamica_path = rv$dinamica_path,
          output_dir = rv$wd,
          memory_allocation = input$memory_allocation,
          alloc_params = alloc_params,
          alloc_override_df = alloc_override,
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