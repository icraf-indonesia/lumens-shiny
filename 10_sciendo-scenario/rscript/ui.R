
generate_download_link <- function(df, title) {
  p(p(tags$b(title)), apply(df, 1, function(x) {
    downloadLink(paste0(x[["var"]], "_id"), (x[["label"]]))
  }))
}

upload_file_button <- function(prefix_id, title) {
  span(icon("upload"), "Upload") |>
    popover(
      title = div(icon("upload"), title),
      id = paste0(prefix_id, "_pop"),
      fileInput(
        paste0(prefix_id, "_upload"),
        NULL,
        accept = c(".csv"),
        placeholder = "Upload data file"
      )
    )
}

pop_inp_map <- function(..., inp_map_id) {
  popover(
    ...,
    title = div(icon("upload"), "Upload map file"),
    id = paste0(inp_map_id, "_pop"),
    fileInput(
      inp_map_id,
      NULL,
      accept = c("image/tiff"),
      placeholder = "Upload map"
    )
  )
}

pop_delete <- function(..., inp_map_id) {
  popover(
    ...,
    title = div(
      bs_icon("exclamation-circle", size = "3em"),
      "Delete zonation map",
      style = "color: #E09F3E;"
    ),
    id = "zone_delete_pop",
    p(h5("Remove this zonation map?")),
    span(
      actionButton("zone_delete_button", "Yes")
    )
  )
}

if_element <- function(cond, element, element_else = NULL) {
  if (cond)
    return(element)
  if (!is.null(element_else))
    return(element_else)
}

inp_map_card <- function(map_id, title, cond_panel, is_lc = T) {
  card(
    card_header(
      div(icon("layer-group"), title),
      class = "bg_theme d-flex justify-content-between",
      if_element(
        is_lc,
        dateInput(
          paste0(map_id, "_date"),
          NULL,
          format = "d-M-yyyy",
          width = "105px"
        )
      ),
      span(
        if_element(
          !is_lc,
          conditionalPanel(
            condition = paste0("!", cond_panel),
            span(icon("trash-can"), "Remove", style = "margin-right:20px;")
          )
        ),
        span(icon("upload"), "Upload") |> pop_inp_map(inp_map_id = paste0(map_id, "_file"))
      )
    ),
    id = paste0(map_id, "_card"),
    full_screen = TRUE,
    card_body(padding = 0, leafletOutput(paste0(map_id, "_plot")))
  )
}

out_map_card <- function(id, title) {
  card(
    card_header(div(
      title, co2_unit("(t"), per_ha_unit("", ")")
    ), class = "bg_theme"),
    id = paste0(id, "_card"),
    full_screen = TRUE,
    card_body(padding = 0, leafletOutput(paste0(id, "_out")))
  )
}

box_emission <- function(id, title, icon_str, theme) {
  value_box(
    title = title,
    value = htmlOutput(paste0("box_", id, "_emission_out")),
    showcase = icon(icon_str),
    htmlOutput(paste0("box_", id, "_emission_info")),
    theme = theme
  )
}

please_input <- p("Please import QUES-C Database first.")

#### PROJECTION VALUE BOX ####
projection_value_box <- layout_column_wrap(
  width = "250px",
  fill = FALSE,
  value_box(
    id = "box_scenario",
    title = p("Number of modified conversion") ,
    value = textOutput("n_scenario"),
    showcase = icon("arrows-split-up-and-left"),
    theme = "success"
  ),
  value_box(
    id = "box_final_area",
    title = p("The largest projected area") ,
    value = textOutput("final_area1"),
    uiOutput("box_final_area_info"),
    showcase = plotlyOutput("final_plot"),
    showcase_layout = showcase_left_center(
      width = 0.4,
      width_full_screen = "1fr",
      max_height = "150px"
    ),
    full_screen = TRUE,
    theme = "info"
  ),
  value_box(
    id = "box_emission",
    title = "Net emission offset",
    value = textOutput("emission_margin"),
    uiOutput("box_emission_info"),
    showcase = plotlyOutput("emission_plot"),
    showcase_layout = showcase_left_center(width = 0.4, max_height = "150px"),
    full_screen = TRUE,
    theme = "dark"
  )
)

projection_scenario_selection <- layout_column_wrap(
  div(tags$b("Select scenario"), style = "text-align:right;"),
  selectInput("select_scenario", NULL, NULL),
  actionButton(
    "add_scenario",
    "Add new scenario",
    style = "height:38px; width:100%; padding:5px;",
    icon = icon("plus")
  ),
  actionButton(
    "remove_scenario",
    "Remove current scenario",
    style = "height:38px; width:100%; padding:5px;",
    icon = icon("trash-can")
  )
)

projection_scenario_modification <- layout_column_wrap(
  height = "100%",
  style = css(grid_template_columns = "3fr 2fr"),
  card(
    card_header("Projected land cover changes", class = "bg_theme"),
    card_body(
      padding = 5,
      accordion(
        open = F,
        accordion_panel(
          "How to modify the projection",
          tags$li(
            "Select the period by clicking the horizontal year axis on the area chart below"
          ),
          tags$li(
            "Click on the the land cover label or bar chart to edit the projection scenario"
          ),
          tags$li(
            "The input is on targeted hectare area by default.
                  Put '%' after the number to modify the percentage of projection rate"
          ),
          tags$li("Put a prefix '+' or '-' to add or reduce from the default value")
        )
      ),
      abacuslibOutput("edit_scenario", width = "100%")
    )
  ),
  card(
    card_header("Scenario details", class = "bg_theme"),
    card_body(# padding = 0,
      navset_underline(
        nav_panel(title = "Projection rate", reactableOutput("tpm_list")),
        nav_panel(
          title = "Land cover",
          reactableOutput("new_lc_list"),
          div(
            actionButton("remove_lc_scenario", "Remove"),
            actionButton("edit_lc_scenario", "Edit"),
            actionButton("add_lc_scenario", "Add"),
            style = "text-align:right;"
          )
        ),
        nav_panel(
          title = "Description",

          p(tags$b("Label"), textOutput("scenario_label")),
          p(tags$b("Description"), textOutput("scenario_desc")),
          div(actionButton("edit_scenario_label_btn", "Edit"), style = "text-align:right;")

        )
      ))
  )
)

download_panel <- layout_column_wrap(
  fill = F,
  card(
    card_header("Parameters data", class = "bg_theme"),
    p(
      "Download all the input parameters in one bundling (.zip) file.",
      tags$br(),
      "This file can be used later for the parameterization and uploaded at once."
    ),
    p(
      downloadButton("download_params", "Download all the data parameters"),
      actionButton("viewReport", "View report", icon = icon("file-code")) 
    ),
    p("This bundling file contains the following data:"),
    generate_download_link(table_file_df, "Tabular data parameters"),
    p(p(tags$b(
      "Scenario parameters"
    )), uiOutput("scenario_output_params"))
  ),
  card(
    card_header("Output data", class = "bg_theme"),
    generate_download_link(output_table_file_df, "Baseline projection"),
    p(p(tags$b(
      "Scenario projection"
    )), uiOutput("scenario_output_download"))
  )
)


#### UI MAIN PAGE ####
ui <-
  page_navbar(
    id = "main_page",
    theme = bs_theme(
      primary = "#540b0e",
      dark = "#404040",
      success = "#47602B",
      info = "#335C67",
      warning = "#E09F3E",
      #"#FFF3B0",
      danger = "#9E2A2B",
      secondary = "#BE191E",
      font_scale = 0.9
    ),
    useShinyjs(),
    bg = "#FCE5E6",
    header =
      tags$head(
        tags$link(rel="shortcut icon", href="favicon.ico"),
        tags$style(
          HTML(
            ".bg_theme{background-color: #FCE5E6;}
            .bg_theme2{background-color: #FEF7F8;}
            .bg_warning{background-color: #F4DDBA;}

            .matrix_diagonal{background-color: #E6E6E6}
            .selectize-input{height:20px; font-size:1em;}
            .section{padding:10px; margin:0px; background-color: #FEF7F8;border: 1px solid #FCE5E6;border-radius: 5px;}
            .header_item{padding:3px 10px; margin-left:5px; background-color: #9E2A2B;
              border-radius: 8px;border: 1px solid #E6E6E6; color:white}
            .accordion {--bs-accordion-border-width: 0px;}
          "
          )
        ),
        tags$script(src = "jexcel.js"),
        tags$link(rel = "stylesheet", href = "jexcel.css", type = "text/css"),
        tags$script(src = "jsuites.js"),
        tags$link(rel = "stylesheet", href = "jsuites.css", type = "text/css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "table.css")
      ),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    window_title = "SCIENDO-Scenario Builder",
    title =
    #   HTML(
    #   "<span style='color:#E42E34; background:#540b0e; padding:2px 20px 4px 20px; border-radius:15px'>
    #   <b>REDD<span style='color: white;'>Abacus</span>2</b></span>"
    # )
    span(tags$img(height = 18, src = "images/abacus2_logo_white.svg", style = "margin-right:5px"),
         "SCIENDO", span("ScenarioBuilder", style = "color: white;", .noWS = c('before', "after") ),
         style = "color:#E42E34; background:#540b0e; padding:2px 20px 4px 20px; border-radius:15px; font-weight:bold"),

    underline = TRUE,

    ### IMPORT QUESC #############################
    nav_panel(
      title = "Import QUES-C Database",
      id = "panel_import",
      icon = icon("database"),
      layout_sidebar(
        sidebar = sidebar(
          width = "20%",
          open = "always",
          fileInput("quescdb", "Load QUES-C Database (.csv)", accept = ".csv"),
          div(style = "display: flex; flex-direction: column; gap: 10px;",
              shinyDirButton("output_dir", "Select output directory", "Please select a directory"),
              # actionButton("processSCIENDO", "Run", 
              #              style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
              hidden(
                actionButton("openReport", "Open Report",
                             style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
              ),
              actionButton("returnButton", "Return to Main Menu", 
                           style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
          )
        ),
        includeMarkdown("../helpfile/help.md")
      ),
    ),
    
    ### PROJECTION #############################
    nav_panel(
      title = "Projection",
      id = "panel_projection",
      icon = icon("arrow-trend-up"),
      conditionalPanel(condition = "!output.is_matrix", please_input),
      conditionalPanel(
        condition = "output.is_matrix",
        projection_value_box,
        projection_scenario_selection,
        projection_scenario_modification,
        actionButton(
          "process_build_scenario",
          "Run Analysis",
          # style = "height:38px; width:100%; padding:5px;",
          style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;",
          icon = icon("gear")
        )
      )
    ),

    ### OUTPUT #############################
    # nav_panel(
    #   title = "Download",
    #   icon = icon("download"),
    #   conditionalPanel(condition = "!output.is_matrix", please_input),
    #   conditionalPanel(condition = "output.is_matrix", download_panel)
    # ),
    
    nav_panel(
      title = "Help",
      icon = icon("circle-question"),
      includeMarkdown("../helpfile/help.md")
    ),

    ### MENU #############################
    nav_spacer(),
    nav_menu(
      title = "Options",
      align = "right",
      nav_item(actionLink(
        "upload_params", div(icon("upload"), "Load saved parameters (.zip)")
      )),
      nav_item(actionLink(
        "import_abacus_1",
        div(icon("file-import"), "Import REDD-Abacus-1 data (.car)")
      ))
    )

  )
