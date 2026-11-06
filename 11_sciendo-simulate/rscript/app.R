library(conflicted)

conflict_prefer("xml_find_all", "xml2")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("extract", "tidyr")

source('function_sciendo_simulate.R')
source('../../helper.R')
options(repos = c(CRAN = "https://cloud.r-project.org"))

install_load(
  "shiny", "shinyFiles", "shinyvalidate", "shinyjs", "bslib", "sf", "raster",
  "dplyr", "remotes", "rmarkdown", "XML", "splitstackshape", "shinyalert",
  "terra", "tibble", "ggplot2", "magrittr", "tidyr", "tidyterra", "DT",
  "readr", "plotly", "scales", "ggthemes", "rlang", "xml2", "tidyverse",
  "readxl", "openxlsx", "openxlsx2", "conflicted", "stringr", "mapview",
  "tools", "leafsync", "leaflet", "shiny.i18n"
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
    h2("SCIENDO Simulate", style = "margin: 0; font-weight: normal;"),
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
            HTML('<svg class="flag-icon" viewBox="0 0 60 30" width="22" height="15"><clipPath id="sim_dd_uk1"><path d="M0,0 v30 h60 v-30 z"/></clipPath><clipPath id="sim_dd_uk2"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath><g clip-path="url(#sim_dd_uk1)"><path d="M0,0 v30 h60 v-30 z" fill="#012169"/><path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/><path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#sim_dd_uk2)" stroke="#C8102E" stroke-width="4"/><path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/><path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g></svg>')
          )
        )
      )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file",
                label = label_with_help(i18n$t("lbl_map1_file"), "map1_file"),
                accept = c("image/tiff"), placeholder = tr("ph_map1_file", "id")),
      numericInput("init_year",
                   label = label_with_help(i18n$t("lbl_init_year"), "init_year"),
                   value = 2025),
      radioButtons("zone_type", label = label_with_help(i18n$t("lbl_zone_type"), "zone_type"),
                   choiceNames = list(i18n$t("choice_raster"), i18n$t("choice_shapefiles")), choiceValues = list("raster", "shapefile"), inline = TRUE),
      conditionalPanel(
        condition = "input.zone_type == 'shapefile'",
        fileInput("mapz_file",
                  label = label_with_help(i18n$t("lbl_mapz_file_shp"), "mapz_file"),
                  accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
                  multiple = TRUE,
                  placeholder = tr("ph_mapz_file_shp", "id"))
      ),
      conditionalPanel(
        condition = "input.zone_type == 'raster'",
        fileInput("mapz_file", label = label_with_help(i18n$t("lbl_mapz_file_rast"), "mapz_file"), accept = c("image/tiff"), placeholder = tr("ph_mapz_file_rast", "id"))
      ),
      fileInput("lc_file",
                label = label_with_help(i18n$t("lbl_lc_file"), "lc_file"),
                accept = c(".csv", ".xlsx"), placeholder = tr("ph_lc_file", "id")),
      fileInput("rc_file",
                label = label_with_help(i18n$t("lbl_rc_file"), "rc_file"),
                accept = c(".tif", ".xml"), multiple = TRUE, placeholder = tr("ph_rc_file", "id")),
      numericInput("repetition",
                   label = label_with_help(i18n$t("lbl_repetition"), "repetition"),
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
            tr("lbl_tm_path", "id"),
            tr("tip_tm_path", "id"),
            "tm_path",
            label_key = "lbl_tm_path"
          ),
          verbatimTextOutput("print_tm_dir", placeholder = TRUE),
          
          dir_button_with_help(
            "dcf_path",
            tr("lbl_dcf_path", "id"),
            tr("tip_dcf_path", "id"),
            "dcf_path",
            label_key = "lbl_dcf_path"
          ),
          verbatimTextOutput("print_dcf_dir", placeholder = TRUE),
          
          dir_button_with_help(
            "wd",
            tr("lbl_wd", "id"),
            tr("tip_wd", "id"),
            "wd",
            label_key = "lbl_wd"
          ),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          
          
          accordion(
            open = FALSE,
            accordion_panel(
              title = i18n$t("accordion_adv_settings"),
              icon = icon("gear"),
              value = "advanced_settings",
              
              selectizeInput("memory_allocation",
                             label = label_with_help(i18n$t("lbl_memory_allocation"), "memory_allocation"),
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
              
              dir_button_with_help(
                "dinamica_path",
                tr("lbl_dinamica_path", "id"),
                tr("tip_dinamica_path", "id"),
                "dinamica_path",
                label_key = "lbl_dinamica_path"
              ),
              verbatimTextOutput("print_dinamica_path", placeholder = TRUE),
              tags$hr(),
              
              tags$div(
                class = "alloc-info-row",
                checkboxInput("alloc_custom",
                              i18n$t("chk_alloc_custom"),
                              value = FALSE),
                alloc_help_icon("custom")
              ),
              tags$div(
                id = "alloc_params_wrapper",
                class = "alloc-disabled",
                
                uiOutput("alloc_pixel_info"),
                
                numericInput("alloc_percent",
                             label = tags$div(
                               class = "alloc-label",
                               tags$span(HTML("Percent of Transitions by Expansion (0\u20131)")),
                               alloc_help_icon("percent")
                             ),
                             value = 0.5, min = 0, max = 1, step = 0.05),
                
                fluidRow(
                  class = "alloc-3col",
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(i18n$t("lbl_exp_mean")),
                           alloc_help_icon("exp_mean")
                         ),
                         numericInput("alloc_exp_mean", label = NULL, value = 2, min = 0, step = 0.1),
                         uiOutput("feedback_exp_mean")
                  ),
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(i18n$t("lbl_exp_var")),
                           alloc_help_icon("exp_var")
                         ),
                         numericInput("alloc_exp_var", label = NULL, value = 1, min = 0, step = 0.1),
                         uiOutput("feedback_exp_var")
                  ),
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(HTML("Expansion Patch<br>Isometry (0\u20132)")),
                           alloc_help_icon("exp_iso")
                         ),
                         numericInput("alloc_exp_iso", label = NULL, value = 1, min = 0, max = 2, step = 0.1)
                  )
                ),
                
                fluidRow(
                  class = "alloc-3col",
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(i18n$t("lbl_gen_mean")),
                           alloc_help_icon("gen_mean")
                         ),
                         numericInput("alloc_gen_mean", label = NULL, value = 1, min = 0, step = 0.1),
                         uiOutput("feedback_gen_mean")
                  ),
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(i18n$t("lbl_gen_var")),
                           alloc_help_icon("gen_var")
                         ),
                         numericInput("alloc_gen_var", label = NULL, value = 1, min = 0, step = 0.1),
                         uiOutput("feedback_gen_var")
                  ),
                  column(4, 
                         tags$div(
                           class = "alloc-label",
                           tags$span(HTML("Generation Patch<br>Isometry (0\u20132)")),
                           alloc_help_icon("gen_iso")
                         ),
                         numericInput("alloc_gen_iso", label = NULL, value = 1, min = 0, max = 2, step = 0.1)
                  )
                ),
                
                tags$hr(),
                
                tags$p(
                  style = "margin-bottom: 8px; display: flex; align-items: center; gap: 6px;",
                  tags$span(i18n$t("lbl_override")),
                  alloc_help_icon("override")
                ),
                
                tags$div(
                  class = "alloc-tmpl-row",
                  style = "display: flex; gap: 10px; margin-bottom: 10px;",
                  tags$div(style = "flex: 1;",
                           actionButton("generate_alloc_template", i18n$t("btn_gen_template"),
                                        icon = icon("file-excel"),
                                        style = "font-size: 14px; padding: 6px 10px; width: 100%; height: 38px;")
                  ),
                  tags$div(style = "flex: 1;",
                           downloadButton("download_alloc_template", i18n$t("btn_download_template"),
                                          icon = icon("download"),
                                          style = "font-size: 14px; padding: 6px 10px; width: 100%; height: 38px;")
                  )
                ),
                
                fileInput("alloc_override_file",
                          label = i18n$t("lbl_upload_override"),
                          accept = c(".csv", ".xlsx"), placeholder = tr("ph_upload_override", "id"),
                          width = "100%")
              )
            )
          ),
          
          actionButton("processSimulate", i18n$t("btn_run_analysis"), 
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
  current_lang <- reactiveVal("id")
  
  update_file_input_placeholders <- function(lang) {
    ph_map1  <- tr("ph_map1_file", lang)
    ph_mapz  <- if (isTRUE(input$zone_type == "raster")) tr("ph_mapz_file_rast", lang) else tr("ph_mapz_file_shp", lang)
    ph_lc    <- tr("ph_lc_file", lang)
    ph_rc    <- tr("ph_rc_file", lang)
    ph_ov    <- tr("ph_upload_override", lang)
    js <- sprintf("
      $('#map1_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#mapz_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#lc_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#rc_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
      $('#alloc_override_file').closest('.form-group').find('input[type=text]').attr('placeholder', '%s');
    ", ph_map1, ph_mapz, ph_lc, ph_rc, ph_ov)
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
  svg_flag_en <- '<svg class="flag-icon" viewBox="0 0 60 30" width="22" height="15"><clipPath id="sim_dd_uk1"><path d="M0,0 v30 h60 v-30 z"/></clipPath><clipPath id="sim_dd_uk2"><path d="M30,15 h30 v15 z v15 h-30 z h-30 v-15 z v-15 h30 z"/></clipPath><g clip-path="url(#sim_dd_uk1)"><path d="M0,0 v30 h60 v-30 z" fill="#012169"/><path d="M0,0 L60,30 M60,0 L0,30" stroke="#fff" stroke-width="6"/><path d="M0,0 L60,30 M60,0 L0,30" clip-path="url(#sim_dd_uk2)" stroke="#C8102E" stroke-width="4"/><path d="M30,0 v30 M0,15 h60" stroke="#fff" stroke-width="10"/><path d="M30,0 v30 M0,15 h60" stroke="#C8102E" stroke-width="6"/></g></svg>'
  
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
      if (current_lang() == "en" && file.exists("../helpfile/sciendo_simulate_quick_user_guide.md")) {
        includeMarkdown("../helpfile/sciendo_simulate_quick_user_guide.md")
      } else {
        includeMarkdown("../helpfile/sciendo_simulate_quick_user_guide_ID.md")
      }
    )
  })
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
    alloc_template    = NULL,
    cell_area_ha      = 0.09,
    pixel_resolution_m = 30.0
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
    if (!is.null(map1$name)) {
      detected_yr <- extract_year_from_filename(map1$name[1])
      if (!is.na(detected_yr)) {
        updateNumericInput(session, "init_year", value = detected_yr)
      }
    }
    tryCatch({
      r_tmp <- terra::rast(rv$map1_file)
      res_xy <- terra::res(r_tmp)
      area_ha <- (res_xy[1] * res_xy[2]) / 10000
      if (!is.na(area_ha) && area_ha > 0) {
        rv$cell_area_ha <- area_ha
        rv$pixel_resolution_m <- round(mean(res_xy), 1)
      }
    }, error = function(e) {
      rv$cell_area_ha <- 0.09
      rv$pixel_resolution_m <- 30.0
    })
  })
  
  observeEvent(input$mapz_file, {
    mapz <- input$mapz_file
    if (is.null(mapz)) return()
    
    if (is.null(input$zone_type) || input$zone_type == "raster") {
      rv$mapz_file <- rename_uploaded_file(mapz)
    } else {
      # Check if map1_file is uploaded first
      if (is.null(rv$map1_file)) {
        lang <- current_lang()
        showModal(modalDialog(
          title = tr("modal_map1_missing_title", lang),
          div(
            p(tr("modal_map1_missing_p1", lang)),
            p(tr("modal_map1_missing_p2", lang))
          ),
          easyClose = TRUE,
          footer = modalButton(tr("btn_close", lang))
        ))
        showNotification(
          tr("notif_map1_missing_pu", lang),
          type = "error",
          duration = 10
        )
        return()
      }
      
      # Check if all shapefile components are present
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
      
      shp <- mapz
      prev_wd <- getwd()
      uploaded_dir <- dirname(shp$datapath[1])
      setwd(uploaded_dir)
      for (i in 1:nrow(shp)) {
        file.rename(shp$datapath[i], shp$name[i])
      }
      setwd(prev_wd)
      
      shp_path <- paste(uploaded_dir, shp$name[grep(pattern="*\\.shp$", shp$name, ignore.case = TRUE)][1], sep = "/")
      zone_sf <- tryCatch({
        sf::st_read(shp_path, quiet = TRUE)
      }, error = function(e) {
        showNotification(paste("Error reading shapefile:", e$message), type = "error", duration = 8)
        NULL
      })
      if (is.null(zone_sf)) return()
      
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
      if (is.null(id_field) || length(id_field) == 0) {
        zone_sf$IDS <- seq_len(nrow(zone_sf))
        id_field <- "IDS"
      }
      
      tryCatch({
        map1_raster <- raster::raster(rv$map1_file)
        raster_res <- raster::res(map1_raster)
        
        zone <- zone_sf %>%
          rasterise_multipolygon(
            raster_res = raster_res,
            field = id_field
          )
        
        zone_tif_path <- file.path(uploaded_dir, "planning_unit_rasterized.tif")
        raster::writeRaster(zone %>% raster::raster(), zone_tif_path, datatype = "INT1U", overwrite = TRUE)
        
        rv$mapz_file <- zone_tif_path
        rv$mapz_rast <- zone %>% raster::raster()
        
        showNotification("Shapefile planning unit successfully rasterized!", type = "message", duration = 6)
      }, error = function(e) {
        showNotification(
          paste("Error processing raster resolution:", e$message),
          type = "error",
          duration = 10
        )
      })
    }
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
    
    rv$lc_path <- rename_uploaded_file(f)
    
    ext <- tolower(tools::file_ext(f$name))
    df_c <- tryCatch({
      if (ext == "csv") {
        read.csv(rv$lc_path, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext == "xlsx") {
        as.data.frame(openxlsx::read.xlsx(rv$lc_path, check.names = FALSE))
      } else {
        stop("Unsupported file format. Please upload a .csv or .xlsx file.")
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message),
                       type = "error", duration = 6)
      NULL
    })
    
    if (is.null(df_c)) {
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
      cat(tr("ph_print_wd", current_lang()))
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
      cat(tr("ph_print_tm_dir", current_lang()))
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
      cat(tr("ph_print_dcf_dir", current_lang()))
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
  
  #### Click-to-open help modals ####
  lapply(names(alloc_help_texts), function(id) {
    observeEvent(input[[paste0("alloc_help_", id)]], {
      lang <- current_lang()
      title_key <- paste0("help_", id, "_title")
      body_key  <- paste0("help_", id, "_body")
      
      modal_title <- tr(title_key, lang)
      modal_body  <- tr(body_key, lang)
      
      if (modal_title == title_key && id %in% names(alloc_help_texts)) {
        modal_title <- alloc_help_texts[[id]]$title
        modal_body  <- alloc_help_texts[[id]]$body
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
  
  # Allocate Transitions
  observe({
    if (isTRUE(input$alloc_custom)) {
      shinyjs::removeClass("alloc_params_wrapper", "alloc-disabled")
    } else {
      shinyjs::addClass("alloc_params_wrapper", "alloc-disabled")
    }
  })
  
  # Dynamic feedback for patch size ha -> pixel conversion
  output$alloc_pixel_info <- renderUI({
    lang <- current_lang()
    if (!is.null(rv$map1_file)) {
      msg <- sprintf(tr("info_pixel_res_detected", lang), rv$pixel_resolution_m, rv$cell_area_ha)
      tags$div(
        class = "alert alert-info py-1 px-2 mb-2", style = "font-size: 12px; margin-top: 5px;",
        msg
      )
    } else {
      tags$div(
        class = "alert alert-secondary py-1 px-2 mb-2", style = "font-size: 12px; margin-top: 5px;",
        tr("info_pixel_res_default", lang)
      )
    }
  })
  
  .calc_px_badge <- function(val_ha, cell_ha, is_variance = FALSE) {
    if (is.null(val_ha) || is.na(val_ha) || val_ha < 0) return(NULL)
    if (!is_variance && val_ha <= 0) return(NULL)
    px <- if (is_variance) max(0, round(val_ha / cell_ha)) else max(1, round(val_ha / cell_ha))
    tags$div(style = "font-size: 11px; color: #0d6efd; font-weight: 600; margin-top: -2px; margin-bottom: 6px;",
             sprintf("\u2248 %s px", format(px, big.mark = ",")))
  }
  
  output$feedback_exp_mean <- renderUI({ .calc_px_badge(input$alloc_exp_mean, rv$cell_area_ha, is_variance = FALSE) })
  output$feedback_exp_var  <- renderUI({ .calc_px_badge(input$alloc_exp_var, rv$cell_area_ha, is_variance = TRUE) })
  output$feedback_gen_mean <- renderUI({ .calc_px_badge(input$alloc_gen_mean, rv$cell_area_ha, is_variance = FALSE) })
  output$feedback_gen_var  <- renderUI({ .calc_px_badge(input$alloc_gen_var, rv$cell_area_ha, is_variance = TRUE) })

  
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
  
  iv$add_rule("map1_file", function(value) if (is.null(value)) tr("val_map1_req", current_lang()) else NULL)
  iv$add_rule("mapz_file", function(value) if (is.null(value)) tr("val_mapz_req", current_lang()) else NULL)
  iv$add_rule("rc_file", function(value) if (is.null(value)) tr("val_rc_req", current_lang()) else NULL)
  iv$add_rule("lc_file", function(value) if (is.null(value)) tr("val_lc_req", current_lang()) else NULL)
  iv$add_rule("tm_path", function(value) if (is.null(value)) tr("val_tm_req", current_lang()) else NULL)
  iv$add_rule("dcf_path", function(value) if (is.null(value)) tr("val_dcf_req", current_lang()) else NULL)
  iv$add_rule("wd", function(value) if (is.null(value)) tr("val_wd_req", current_lang()) else NULL)
  iv$add_rule("memory_allocation", function(value) if (is.null(value)) tr("val_memory_req", current_lang()) else NULL)
  
  # Raster cube input validation
  iv$add_rule("rc_file", function(value) {
    if (is.null(value)) return(NULL)  
    
    has_tif <- any(grepl("\\.tif$", value$name, ignore.case = TRUE))
    has_xml <- any(grepl("\\.xml$", value$name, ignore.case = TRUE))
    
    if (!has_tif || !has_xml) {
      return(tr("val_rc_both", current_lang()))
    }
    
    NULL
  })
  
  # Only validate allocate transition parameters when the checkbox is enabled
  iv$add_rule("alloc_percent", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value < 0 || value > 1) return(tr("val_between_0_1", current_lang()))
    NULL
  })
  iv$add_rule("alloc_exp_mean", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value <= 0) return(tr("val_gt_0", current_lang()))
    NULL
  })
  iv$add_rule("alloc_exp_var", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value < 0) return(tr("val_gte_0", current_lang()))
    NULL
  })
  iv$add_rule("alloc_exp_iso", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value < 0 || value > 2) return(tr("val_between_0_2", current_lang()))
    NULL
  })
  iv$add_rule("alloc_gen_mean", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value <= 0) return(tr("val_gt_0", current_lang()))
    NULL
  })
  iv$add_rule("alloc_gen_var", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value < 0) return(tr("val_gte_0", current_lang()))
    NULL
  })
  iv$add_rule("alloc_gen_iso", function(value) {
    if (!isTRUE(input$alloc_custom)) return(NULL)
    if (is.null(value) || is.na(value)) return(tr("val_provide_val", current_lang()))
    if (value < 0 || value > 2) return(tr("val_between_0_2", current_lang()))
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
      cell_ha <- if (!is.null(rv$cell_area_ha) && rv$cell_area_ha > 0) rv$cell_area_ha else 0.09
      alloc_params <- list(
        percent  = input$alloc_percent,
        exp_mean = max(1, round(input$alloc_exp_mean / cell_ha)),
        exp_var  = max(0, round(input$alloc_exp_var / cell_ha)),
        exp_iso  = input$alloc_exp_iso,
        gen_mean = max(1, round(input$alloc_gen_mean / cell_ha)),
        gen_var  = max(0, round(input$alloc_gen_var / cell_ha)),
        gen_iso  = input$alloc_gen_iso
      )
      alloc_override <- rv$alloc_override_df
      if (!is.null(alloc_override)) {
        for (col in c("exp_mean", "gen_mean")) {
          if (col %in% names(alloc_override)) {
            val <- alloc_override[[col]]
            alloc_override[[col]] <- ifelse(!is.na(val) & val > 0, pmax(1, round(val / cell_ha)), val)
          }
        }
        for (col in c("exp_var", "gen_var")) {
          if (col %in% names(alloc_override)) {
            val <- alloc_override[[col]]
            alloc_override[[col]] <- ifelse(!is.na(val) & val >= 0, pmax(0, round(val / cell_ha)), val)
          }
        }
      }
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
