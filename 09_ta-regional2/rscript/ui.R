page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Trade-off Analysis (Regional 2)",
    width = 600,
    fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
    fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
    fileInput("mapz_file", 
              "Planning Unit", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T, 
              placeholder = "All related shapefiles"),
    fileInput("carbon_file", "Carbon stock lookup table", accept = c(".csv")),
    textInput("map1_year", "Year of T1"),
    textInput("map2_year", "Year of T2"),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("processQUESC", "Run")
  ),
  
  # To display the report
  card(
    card_header("Guide"),
    card_body(
      includeMarkdown("../helpfile/help.md")
    )
  ),
  actionButton("viewReport", "View report", icon = icon("file-code")) 
)