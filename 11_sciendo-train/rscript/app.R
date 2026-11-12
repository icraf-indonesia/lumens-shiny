source('function_sciendo_train.R')
source('../../helper.R')

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_load(
  "shiny", "shiny.i18n", "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert",
  "usdm", "corrplot", "tidyr", "openxlsx", "openxlsx2", "readxl", "XML", "xml2"
)

if (!("LUMENSR" %in% rownames(installed.packages()))) {
  install_github("icraf-indonesia/LUMENSR", force = T)
  do.call("library", list("LUMENSR"))
}
library(LUMENSR)

i18n <- Translator$new(
  translation_csvs_path = "translations",
  translation_csv_config = "translations/config.yaml"
)
i18n$set_translation_language("id")

tr <- function(key, lang = i18n$get_translation_language()) {
  df <- i18n$get_translations()
  if (key %in% rownames(df) && lang %in% colnames(df)) {
    val <- df[key, lang]
    if (!is.null(val) && !is.na(val) && length(val) > 0) return(val)
  }
  key
}

ui <- fluidPage(
  useShinyjs(),
  usei18n(i18n),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  div(
    class = "d-flex justify-content-between align-items-center mb-3 mt-2 pb-2 border-bottom",
    h2("SCIENDO Train", style = "margin: 0; font-weight: normal;"),
    div(
      class = "dropdown lang-dropdown",
      tags$button(
        class = "btn btn-lang-toggle dropdown-toggle d-flex align-items-center",
        type = "button",
        id = "langDropdownBtn",
        `data-bs-toggle` = "dropdown",
        `aria-expanded` = "false",
        title = "Bahasa Indonesia",
        tags$span(id = "current_lang_flag", HTML('<svg class="flag-icon" viewBox="0 0 20 14" width="22" height="15"><rect width="20" height="7" fill="#dc3545"/><rect y="7" width="20" height="7" fill="#ffffff"/></svg>'))
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end shadow-sm",
        `aria-labelledby` = "langDropdownBtn",
        tags$li(
          tags$a(
            id = "opt_lang_id",
            class = "dropdown-item active",
            href = "#",
            title = "Bahasa Indonesia",
            onclick = "Shiny.setInputValue('selected_language', 'id', {priority: 'event'}); return false;",
            HTML('<svg class="flag-icon" viewBox="0 0 20 14" width="22" height="15"><rect width="20" height="7" fill="#dc3545"/><rect y="7" width="20" height="7" fill="#ffffff"/></svg>')
          )
        ),
        tags$li(
          tags$a(
            id = "opt_lang_en",
            class = "dropdown-item",
            href = "#",
            title = "English",
            onclick = "Shiny.setInputValue('selected_language', 'en', {priority: 'event'}); return false;",
            HTML('<svg class="flag-icon" viewBox="0 0 60 30" width="22" height="15"><clipPath id="trn_dd_uk1"><path d="M0,0 v30 h60 v-30 z"/></clipPath><clipPath id="trn_dd_uk2"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath><g clip-path="url(#trn_dd_uk1)"><path d="M0,0 v30 h60 v-30 z" fill="#012169"/><path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/><path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#trn_dd_uk2)" stroke="#C8102E" stroke-width="4"/><path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/><path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g></svg>')
          )
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
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
      
      .lang-dropdown .btn-lang-toggle {
        background-color: #ffffff;
        border: 1px solid #ced4da;
        padding: 5px 8px;
        border-radius: 6px;
        cursor: pointer;
        box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        transition: all 0.15s ease-in-out;
      }
      .lang-dropdown .btn-lang-toggle:hover,
      .lang-dropdown .btn-lang-toggle:focus {
        background-color: #f1f3f5;
        border-color: #adb5bd;
      }
      .lang-dropdown .btn-lang-toggle::after {
        margin-left: 5px;
        vertical-align: middle;
        font-size: 11px;
        color: #6c757d;
      }
      .lang-dropdown .dropdown-menu {
        min-width: unset !important;
        width: auto !important;
        padding: 4px !important;
        border-radius: 6px !important;
        border: 1px solid rgba(0,0,0,0.12) !important;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
      }
      .lang-dropdown .dropdown-item {
        padding: 6px 10px !important;
        border-radius: 4px !important;
        cursor: pointer !important;
        display: flex !important;
        align-items: center !important;
        justify-content: center !important;
        transition: background-color 0.15s !important;
      }
      .lang-dropdown .dropdown-item:hover {
        background-color: #f1f3f5 !important;
      }
      .lang-dropdown .dropdown-item.active {
        background-color: #e7f1ff !important;
      }
      .flag-icon {
        width: 22px !important;
        height: 15px !important;
        border-radius: 2px !important;
        box-shadow: 0 0 1px rgba(0,0,0,0.4) !important;
        display: inline-block !important;
        vertical-align: middle !important;
        flex-shrink: 0 !important;
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
      .selectize-dropdown-content .option {
        padding-left: 20px;
      }
        "))
      ),
      fileInput("map1_file", label = label_with_help(i18n$t("lbl_map1_file"), "map1_file"), accept = c("image/tiff"), placeholder = tr("ph_map1_file", "id")),
      numericInput("map1_year", label = label_with_help(i18n$t("lbl_map1_year"), "map1_year"), value = 2015),
      fileInput("map2_file", label = label_with_help(i18n$t("lbl_map2_file"), "map2_file"), accept = c("image/tiff"), placeholder = tr("ph_map1_file", "id")),
      numericInput("map2_year", label = label_with_help(i18n$t("lbl_map2_year"), "map2_year"), value = 2020),
      fileInput("lc_file", label = label_with_help(i18n$t("lbl_lc_file"), "lc_file"), accept = c(".csv", ".xlsx", ".xls"), placeholder = tr("ph_lc_file", "id")),
      radioButtons("zone_type", label = label_with_help(i18n$t("lbl_zone_type"), "zone_type"),
                   choiceNames = list(i18n$t("choice_raster"), i18n$t("choice_shapefiles")), choiceValues = list("raster", "shapefile"), inline = T),
      conditionalPanel(
        condition = "input.zone_type == 'shapefile'",
        fileInput("mapz_file",
                  label = label_with_help(i18n$t("lbl_mapz_file_shp"), "mapz_file"),
                  accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                  multiple = T,
                  placeholder = tr("ph_mapz_file_shp", "id"))
      ),
      conditionalPanel(
        condition = "input.zone_type == 'raster'",
        fileInput("mapz_file", label = label_with_help(i18n$t("lbl_mapz_file_rast"), "mapz_file"), accept = c("image/tiff"), placeholder = tr("ph_mapz_file_rast", "id")),
        fileInput("z_file", label = label_with_help(i18n$t("lbl_z_file"), "z_file"), accept = c(".csv", ".xlsx", ".xls"), placeholder = tr("ph_z_file", "id"))
      ),
      
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          dir_button_with_help("wd", tr("lbl_wd", "id"), tr("tip_wd", "id"), "wd", label_key = "lbl_wd"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          
          tags$div(
            style = "margin-top: 10px; margin-bottom: 2px; border-top: 1px solid #e0e0e0; padding-top: 10px;",
            tags$h5(i18n$t("hdr_predictors"), style = "font-weight: 600; margin: 0; color: #333;")
          ),
          
          dir_button_with_help("factors_path", tr("lbl_factors_path", "id"), tr("tip_factors_path", "id"), "factors_path", label_key = "lbl_factors_path"),
          verbatimTextOutput("print_factor_dir", placeholder = TRUE),
          
          tags$div(
            id = "eval_buttons_container",
            style = "display: flex; gap: 8px; width: 100%; margin-top: 2px; margin-bottom: 2px;",
            tags$div(
              id = "eval_btn_wrapper",
              style = "flex: 1 1 0px; width: 100%; min-width: 0;",
              actionButton("evaluate_factors", i18n$t("btn_eval_factors"),
                           style = "width: 100%; font-size: 13px; padding: 6px 4px; white-space: normal; background-color: #4CAF50; color: white;")
            ),
            hidden(
              tags$div(
                id = "open_eval_wrapper",
                style = "flex: 1 1 0px; width: 100%; min-width: 0;",
                actionButton("open_eval_report", i18n$t("btn_open_eval_report"),
                             style = "width: 100%; font-size: 13px; padding: 6px 4px; white-space: normal; background-color: #008CBA; color: white;")
              )
            )
          ),
          
          accordion(
            open = FALSE,
            accordion_panel(
              title = i18n$t("accordion_adv_settings"),
              icon = icon("gear"),
              value = "advanced_settings",
              
              selectizeInput("memory_allocation", label = label_with_help(i18n$t("lbl_memory_allocation"), "memory_allocation"),
                             choices = c("Seimbang" = 1, "Utamakan Memori" = 0, "Utamakan Disk" = 2,
                                         "Hanya Memori" = 3, "Agresif" = 4),
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
              
              dir_button_with_help("dinamica_path", tr("lbl_dinamica_path", "id"), tr("tip_dinamica_path", "id"), "dinamica_path", label_key = "lbl_dinamica_path"),
              verbatimTextOutput("print_dinamica_path", placeholder = TRUE)
            )
          ),
          
          actionButton("processTrain", i18n$t("btn_run_analysis"),
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("openReport", i18n$t("btn_open_report"),
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
            actionButton("open_output_folder", i18n$t("btn_open_output_folder"),
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          actionButton("returnButton", i18n$t("btn_return_main"),
                       style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(i18n$t("tab_user_guide"), 
                 uiOutput("user_guide_content")
        ),
        tabPanel(i18n$t("tab_log"),
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
  current_lang <- reactiveVal("id")
  
  update_file_input_placeholders <- function(lang) {
    ph_map1      <- tr("ph_map1_file", lang)
    ph_lc        <- tr("ph_lc_file", lang)
    ph_mapz_shp  <- tr("ph_mapz_file_shp", lang)
    ph_mapz_rast <- tr("ph_mapz_file_rast", lang)
    ph_z         <- tr("ph_z_file", lang)
    js <- sprintf("
      $('#map1_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#map2_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#lc_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('.shiny-panel-conditional[data-display-if*=\"shapefile\"] input[type=text]').attr('placeholder', '%s');
      $('.shiny-panel-conditional[data-display-if*=\"raster\"] #mapz_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#z_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
    ", ph_map1, ph_map1, ph_lc, ph_mapz_shp, ph_mapz_rast, ph_z)
    shinyjs::runjs(js)
  }
  
  update_memory_choices <- function(lang) {
    curr <- input$memory_allocation
    choices <- if (lang == "en") {
      c("Balanced" = 1, "Prefer Memory" = 0, "Prefer Disk" = 2, "Memory Only" = 3, "Aggressive" = 4)
    } else {
      c("Seimbang" = 1, "Utamakan Memori" = 0, "Utamakan Disk" = 2, "Hanya Memori" = 3, "Agresif" = 4)
    }
    updateSelectizeInput(session, "memory_allocation", choices = choices, selected = curr)
  }
  
  svg_flag_id <- '<svg class="flag-icon" viewBox="0 0 20 14" width="22" height="15"><rect width="20" height="7" fill="#dc3545"/><rect y="7" width="20" height="7" fill="#ffffff"/></svg>'
  svg_flag_en <- '<svg class="flag-icon" viewBox="0 0 60 30" width="22" height="15"><clipPath id="trn_dd_uk1"><path d="M0,0 v30 h60 v-30 z"/></clipPath><clipPath id="trn_dd_uk2"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath><g clip-path="url(#trn_dd_uk1)"><path d="M0,0 v30 h60 v-30 z" fill="#012169"/><path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/><path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#trn_dd_uk2)" stroke="#C8102E" stroke-width="4"/><path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/><path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g></svg>'
  
  observeEvent(input$selected_language, {
    lang <- input$selected_language
    req(lang)
    current_lang(lang)
    shiny.i18n::update_lang(lang)
    i18n$set_translation_language(lang)
    
    if (lang == "id") {
      shinyjs::html("current_lang_flag", svg_flag_id)
      shinyjs::runjs("$('#langDropdownBtn').attr('title', 'Bahasa Indonesia');")
      shinyjs::addClass("opt_lang_id", "active")
      shinyjs::removeClass("opt_lang_en", "active")
    } else {
      shinyjs::html("current_lang_flag", svg_flag_en)
      shinyjs::runjs("$('#langDropdownBtn').attr('title', 'English');")
      shinyjs::addClass("opt_lang_en", "active")
      shinyjs::removeClass("opt_lang_id", "active")
    }
    
    update_file_input_placeholders(lang)
    update_memory_choices(lang)
  }, ignoreInit = TRUE)
  
  output$user_guide_content <- renderUI({
    div(
      style = "height: 800px; overflow-y: auto; padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #fff;",
      if (current_lang() == "en" && file.exists("../helpfile/sciendo_train_quick_user_guide.md")) {
        includeMarkdown("../helpfile/sciendo_train_quick_user_guide.md")
      } else {
        includeMarkdown("../helpfile/sciendo_train_quick_user_guide_ID.md")
      },
      uiOutput("dynamic_guide", inline = TRUE)
    )
  })
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = NULL,
    factor_eval_report_file = NULL,
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
    zone_type = NULL,
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

  #### Click-to-open help modals ####
  lapply(names(train_help_texts), function(id) {
    observeEvent(input[[paste0("train_help_", id)]], {
      lang <- current_lang()
      title_key <- paste0("help_", id, "_title")
      body_key  <- paste0("help_", id, "_body")
      
      modal_title <- tr(title_key, lang)
      modal_body  <- tr(body_key, lang)
      
      if (modal_title == title_key && id %in% names(train_help_texts)) {
        modal_title <- train_help_texts[[id]]$title
        modal_body  <- train_help_texts[[id]]$body
      }
      
      showModal(modalDialog(
        title = tagList(icon("circle-info"), " ", modal_title),
        HTML(modal_body),
        easyClose = TRUE,
        footer = modalButton(tr("btn_close", lang)),
        size = "m"
      ))
    })
  })
  
  #### Read file inputs ####
  lapply(lc_list, function(id) {
    inputs <- paste0(id, "_file")
    files <- paste0(id, "_file")
    rasters <- paste0(id, "_rast")
    df <- paste0(id, "_df")
    
    observeEvent(input[[inputs]], {
      f <- input[[inputs]]
      rv[[files]] <- rename_uploaded_file(f)
      
      # Auto-detect year from filename and prefill year input
      if (!is.null(f$name)) {
        detected_yr <- extract_year_from_filename(f$name[1])
        if (!is.na(detected_yr)) {
          year_input_id <- paste0(id, "_year")
          updateNumericInput(session, year_input_id, value = detected_yr)
        }
      }
      # rv[[rasters]] <- rv[[files]] %>% raster() 
    })
  })
  
  # Prepare planning unit data
  observeEvent(input$mapz_file, {
    rv$zone_type <- input$zone_type
    mapz <- input$mapz_file
    if(is.null(mapz))
      return()
    
    if(input$zone_type == "raster") {
      rv$mapz_file <- rename_uploaded_file(mapz)
    } else {
      
      # check if all shapefile components are present
      has_shp <- any(grepl("\\.shp$", mapz$name, ignore.case = TRUE))
      has_shx <- any(grepl("\\.shx$", mapz$name, ignore.case = TRUE))
      has_dbf <- any(grepl("\\.dbf$", mapz$name, ignore.case = TRUE))
      has_prj <- any(grepl("\\.prj$", mapz$name, ignore.case = TRUE))
      
      if (!has_shp || !has_shx || !has_dbf || !has_prj) {
        showNotification(
          "Please upload all shapefile components (.shp, .shx, .dbf, .prj)",
          type = "error",
          duration = 8
        )
        return() 
      }
      
      # Check if map1_file is uploaded first
      if (is.null(rv$map1_file)) {
        showModal(modalDialog(
          title = "Peta Tutupan Lahan T1 Belum Diunggah",
          div(
            p("Silakan unggah peta tutupan lahan pada T1 terlebih dahulu sebelum mengunggah berkas Shapefile unit perencanaan."),
            p("Peta T1 diperlukan sebagai acuan batas spasial (extent) dan resolusi sel untuk merasterisasi shapefile.")
          ),
          easyClose = TRUE,
          footer = modalButton("Tutup")
        ))
        showNotification(
          "Silakan unggah peta tutupan lahan (T1) terlebih dahulu untuk menentukan batas dan resolusi rasterisasi shapefile.",
          type = "error",
          duration = 10
        )
        return()
      }
      
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
      
      # Check if the shapefile has appropriate columns for rasterization
      numeric_cols <- sapply(zone_sf, function(x) is.numeric(x) || is.factor(x) || is.character(x))
      
      if (sum(numeric_cols) == 0) {
        showNotification(
          "Shapefile must contain at least one numeric, factor, or character column for rasterization.",
          type = "error",
          duration = 10
        )
        return()
      }
      
      id_field <- names(zone_sf)[numeric_cols][1]
      
      # Create a unique numeric ID if no suitable column exists
      if (is.null(id_field) || length(id_field) == 0) {
        zone_sf$IDS <- seq_len(nrow(zone_sf))
        id_field <- "IDS"
      }
      
      tryCatch({
        map1_raster <- raster(rv$map1_file)
        raster_res <- res(map1_raster)  
        
        zone <- zone_sf %>% 
          rasterise_multipolygon(
            raster_res = raster_res,
            field = id_field
          )
        
        # Store the lookup table with ID and name
        name_field <- if (ncol(zone_sf) > 1) names(zone_sf)[2] else id_field
        rv$mapz_df <- data.frame(ID_PU = zone_sf[[id_field]], PU = zone_sf[[name_field]])
        rv$mapz_rast <- zone %>% raster()
        
      }, error = function(e) {
        showNotification(
          paste("Error processing raster resolution:", e$message),
          type = "error",
          duration = 10
        )
      })
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
    df_c <- tryCatch({
      read_table_flexible(rv$lc_path)
    }, error = function(e) {
      showNotification(paste("Error reading land cover table:", e$message), type = "error", duration = 6)
      NULL
    })
    
    if(is.null(df_c) || nrow(df_c) == 0)
      return()
    if(nrow(df_c) < 2)
      return()
    if(!is_numeric_str(df_c[1, 1]))
      return()
    
    df <- data.frame("ID_LC" = as.integer((as.character(df_c[, 1]))))
    df$LC <- as.character(df_c[, 2])
    rv$lc_df <- df
  })
  
  #### Read zone lookup table ####
  observeEvent(input$z_file, {
    if (input$zone_type != "raster") {
      return()
    }
    
    f <- input$z_file
    rv$z_path <- rename_uploaded_file(f)
    df_z <- tryCatch({
      read_table_flexible(rv$z_path)
    }, error = function(e) {
      showNotification(paste("Error reading zone table:", e$message), type = "error", duration = 6)
      NULL
    })
    
    if(is.null(df_z) || nrow(df_z) == 0)
      return()
    if(nrow(df_z) < 2)
      return()
    if(!is_numeric_str(df_z[1, 1]))
      return()
      
    if (ncol(df_z) >= 2) {
      colnames(df_z)[1:2] <- c("ID_PU", "PU")
      df_z$ID_PU <- suppressWarnings(as.numeric(as.character(df_z$ID_PU)))
      df_z$PU <- as.character(df_z$PU)
    }
    rv$mapz_df <- df_z
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
      cat(tr("ph_print_wd", current_lang()))
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
      shinyjs::hide("open_eval_wrapper")
    }
  })
  
  output$factor_directory <- renderText({
    if(!is.null(rv$factors_path)) {
      paste0("Selected predictors directory: ",  rv$factors_path)
    } else {
      "No predictors directory selected"
    }
  })
  
  output$print_factor_dir <- renderPrint({
    if(!is.null(rv$factors_path)) {
      cat(paste(rv$factors_path))
    } else {
      cat("No predictors directory selected")
    }
  })
  
  #### Evaluate Predictors Handler ####
  observeEvent(input$evaluate_factors, {
    if (is.null(rv$factors_path) || identical(rv$factors_path, character(0)) || !dir.exists(rv$factors_path)) {
      showModal(modalDialog(
        title = "Direktori Prediktor Belum Dipilih",
        "Silakan pilih direktori yang berisi berkas raster prediktor (.tif) terlebih dahulu.",
        easyClose = TRUE,
        footer = modalButton("Tutup")
      ))
      showNotification("Please select a valid predictors folder first.", type = "warning", duration = 8)
      return()
    }
    
    tif_files <- list.files(rv$factors_path, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)
    if (length(tif_files) < 2) {
      showModal(modalDialog(
        title = "Berkas Prediktor Tidak Mencukupi",
        paste0("Direktori prediktor minimal harus memuat 2 berkas GeoTIFF (.tif) untuk evaluasi multikolinearitas. Ditemukan: ", length(tif_files), " berkas."),
        easyClose = TRUE,
        footer = modalButton("Tutup")
      ))
      showNotification("Predictors folder must contain at least 2 .tif files.", type = "error", duration = 8)
      return()
    }
    
    # Determine output folder for report
    eval_out_dir <- if (!is.null(rv$wd) && !identical(rv$wd, character(0)) && dir.exists(rv$wd)) {
      rv$wd
    } else {
      file.path(rv$factors_path, "factor_evaluation")
    }
    
    if (!dir.exists(eval_out_dir)) {
      dir.create(eval_out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    withProgress(message = "Mengevaluasi variabel prediktor...", value = 0.2, {
      incProgress(0.3, detail = "Menghitung VIF dan matriks korelasi...")
      
      tryCatch({
        report_path <- generate_factor_evaluation_report(
          factor_path = rv$factors_path,
          output_dir = eval_out_dir,
          vif_threshold = 5,
          sample_size = 10000
        )
        
        incProgress(0.5, detail = "Menyusun laporan HTML...")
        rv$factor_eval_report_file <- report_path
        
        shinyjs::show("open_eval_wrapper")
        showNotification(paste0("Evaluasi prediktor selesai. Laporan tersimpan di: ", report_path), type = "message", duration = 10)
        
      }, error = function(e) {
        showModal(modalDialog(
          title = "Evaluasi Prediktor Gagal",
          div(
            p("Terjadi kesalahan saat mengevaluasi variabel prediktor:"),
            p(tags$code(e$message))
          ),
          easyClose = TRUE,
          footer = modalButton("Tutup")
        ))
        showNotification(paste("Error evaluating predictors:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  observeEvent(input$open_eval_report, {
    if (!is.null(rv$factor_eval_report_file) && file.exists(rv$factor_eval_report_file)) {
      utils::browseURL(rv$factor_eval_report_file)
    } else {
      showNotification("Report file not found.", type = "error")
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
    if (!is.null(input$dinamica_path) && !identical(parseDirPath(volumes, input$dinamica_path), character(0))) {
      rv$dinamica_path <- parseDirPath(volumes, input$dinamica_path)
    } else {
      # Find DINAMICA directory if not provided
      program_files <- c("C:/Program Files/", "C:/Program Files (x86)/")
      dinamica_dirs <- list.files(program_files, pattern = "^Dinamica EGO", full.names = TRUE, recursive = FALSE)
      
      if (length(dinamica_dirs) == 0) {
        showNotification("No DINAMICA EGO installation found.", type = "error")
        rv$dinamica_path <- NULL
      } else {
        # Sort directories to use the latest version if multiple are found
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
      cat(tr("ph_print_dinamica", current_lang()))
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
  
  # Input validation - create validators but don't enable them initially
  iv <- InputValidator$new()
  iv$add_rule("map1_file", function(value) if (is.null(value)) tr("val_map1_req", current_lang()) else NULL)
  iv$add_rule("map2_file", function(value) if (is.null(value)) tr("val_map2_req", current_lang()) else NULL)
  iv$add_rule("map1_year", function(value) if (is.null(value)) tr("val_map1_year_req", current_lang()) else NULL)
  iv$add_rule("map2_year", function(value) if (is.null(value)) tr("val_map2_year_req", current_lang()) else NULL)
  iv$add_rule("lc_file", function(value) if (is.null(value)) tr("val_lc_req", current_lang()) else NULL)
  iv$add_rule("memory_allocation", function(value) if (is.null(value)) tr("val_memory_req", current_lang()) else NULL)
  iv$add_rule("factors_path", function(value) if (is.null(value)) tr("val_factors_req", current_lang()) else NULL)
  iv$add_rule("wd", function(value) if (is.null(value)) tr("val_wd_req", current_lang()) else NULL)
  
  # Zone-specific validator (initially disabled)
  zone_validator <- reactiveVal(NULL)
  
  # Track if we've shown validation errors before
  validation_triggered <- reactiveVal(FALSE)
  
  # Reactive validation for zone-specific inputs
  observeEvent(input$zone_type, {
    zone_type <- input$zone_type
    
    # Remove any existing zone-specific validator
    if (!is.null(zone_validator())) {
      zone_validator()$disable()
    }
    
    # Create new validator based on zone type (but don't enable it yet)
    if (zone_type == "raster") {
      new_validator <- InputValidator$new()
      new_validator$add_rule("mapz_file", function(value) if (is.null(value)) tr("val_mapz_req", current_lang()) else NULL)
      new_validator$add_rule("z_file", function(value) if (is.null(value)) tr("val_z_file_req", current_lang()) else NULL)
      zone_validator(new_validator)
    } else {
      new_validator <- InputValidator$new()
      new_validator$add_rule("mapz_file", function(value) {
        if (is.null(value)) return(tr("val_mapz_req", current_lang()))
        
        has_shp <- any(grepl("\\.shp$", value$name, ignore.case = TRUE))
        has_shx <- any(grepl("\\.shx$", value$name, ignore.case = TRUE))
        has_dbf <- any(grepl("\\.dbf$", value$name, ignore.case = TRUE))
        has_prj <- any(grepl("\\.prj$", value$name, ignore.case = TRUE))
        
        if (!has_shp || !has_shx || !has_dbf || !has_prj) {
          return(tr("notif_shp_all_parts", current_lang()))
        }
        
        NULL
      })
      zone_validator(new_validator)
    }
  })
  
  # Run process
  observeEvent(input$processTrain, {
    # Enable validators for validation check
    iv$enable()
    if (!is.null(zone_validator())) {
      zone_validator()$enable()
    }
    
    # Set flag that validation has been triggered
    validation_triggered(TRUE)
    
    # Check main validation
    if (!iv$is_valid()) {
      iv$validate()
      showNotification(
        "Please correct the errors in the form and try again.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # Check zone-specific validation
    if (!is.null(zone_validator()) && !zone_validator()$is_valid()) {
      zone_validator()$validate()
      showNotification(
        "Please correct the zone-specific errors in the form and try again.",
        type = "error",
        duration = 5
      )
      return()
    }
    
    # If both validations pass, proceed with processing
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
          z_lookup_table = rv$mapz_df,
          factor_path = rv$factors_path,
          time_points = list(t1 = rv$map1_year, t2 = rv$map2_year),
          dinamica_path = rv$dinamica_path,
          output_dir = rv$wd,
          memory_allocation = input$memory_allocation,
          progress_callback = function(value, detail) {
            setProgress(value = value, message = detail)
          },
          lang = current_lang()
        )
        
        output$status_messages <- renderText(tr("status_success", current_lang()))
        output$success_message <- renderText(paste(
          tr("success_detail", current_lang()),
          tr("notif_macro_unblock_hint", current_lang())
        ))
        output$error_messages <- renderText(NULL)
        removeNotification("running_notification")
        showNotification(tr("notif_success", current_lang()), type = "message")
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
  
  # Only enable validation display after the first run attempt
  observe({
    if (validation_triggered()) {
      iv$enable()
      if (!is.null(zone_validator())) {
        zone_validator()$enable()
      }
    } else {
      iv$disable()
      if (!is.null(zone_validator())) {
        zone_validator()$disable()
      }
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
    lang <- current_lang()
    shinyalert(
      title = tr("confirm_title", lang),
      text =  tr("confirm_text", lang),
      showCancelButton = TRUE,
      cancelButtonText = tr("btn_close", lang),
      confirmButtonText = "OK",
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
