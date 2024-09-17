page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebar = sidebar(
    title = "Planning Unit Reconciliation (PUR) Build",
    width = 600,
    textInput("area_name", "Area Name"),
    fileInput("ref_map", 
              "Reference Map", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T,
              placeholder = "input all related shapefiles"),
    fileInput("ref_class", "Reference Class", accept = c(".csv"), placeholder = "input your csv file"),
    fileInput("ref_mapping", "Reference Class of Reference Map", accept = c(".csv"), placeholder = "input your csv file"),
    fileInput("pu_units", "List of Planning Units", accept = c(".csv"), placeholder = "input your csv file"),
    textInput("map_resolution", "Map Resolution"),
    shinyDirButton("wd", "Select Working Directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run PUR Build")
    ),
# To display the report
  card(
    card_header("Guide"),
    card_body(
      includeMarkdown("../helpfile/help.Rmd")
    )
  ),
  actionButton("viewReport", "View report", icon = icon("file-code")),
  actionButton("returnButton", "Return to Main Menu", 
               style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
)
