page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Planning Unit Reconciliation (PUR) Build",
    width = 600,
    fileInput("ref_map", 
              "Reference Map", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T,
              placeholder = "input all related shapefiles"),
    fileInput("ref_class", "Reference Class", accept = c(".csv"), placeholder = "input your csv file"),
    fileInput("ref_mapping", "Reference Class of Reference Map", accept = c(".csv"), placeholder = "input your csv file"),
    fileInput("pu_units", "List of Planning Units", accept = c(".csv"), placeholder = "input your csv file"),
    textInput("map_resolution", "Map Resolution"),
    shinyDirButton("wd", "Select Output Directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run PUR Build")
    ),
# To display the report
  card(
    card_header("Guide"),
    card_body(
      if (file.exists("01_pur1/helpfile/help.Rmd")) {
        includeMarkdown("01_pur1/helpfile/help.Rmd")
      } else if (file.exists("../helpfile/help.Rmd")) {
        includeMarkdown("../helpfile/help.Rmd")
      } else {
        HTML("<p>User guide file not found.</p>")
      }
    )
  ),
  actionButton("viewReport", "View report", icon = icon("file-code")) 
)




