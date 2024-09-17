page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebar = sidebar(
    title = "Planning Unit Reconciliation (PUR) Reconcile",
    width = 600,
    textInput("area_name", "Area Name"),
    fileInput("recon_file", 
              "Unresolved Planning Unit Map", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T,
              placeholder = "input all related shapefiles"),
    fileInput("unresolved_table", "Unresolved Attribute Table", accept = c(".xlsx"), placeholder = "input your xlsx file"),
    textInput("map_resolution", "Map Resolution"),
    shinyDirButton("wd", "Select Working Directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run PUR Reconcile")
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
