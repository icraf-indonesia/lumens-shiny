page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "QuES-C input",
    width = 400,
    fileInput("map1_file", "Initial land cover", accept = c("image/tiff")),
    fileInput("map2_file", "Final land cover", accept = c("image/tiff")),
    fileInput("mapz_file", 
              "Zone", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T, 
              placeholder = "All related shapefiles"),
    fileInput("carbon_file", "Carbon lookup table", accept = c(".csv")),
    textInput("map1_year", "Initial time"),
    textInput("map2_year", "Final time"),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("processQUESC", "Submit")
  ),
  
  # To display the report
  card(
    card_header("QuES-C Report"),
    htmlOutput("reportOutput")
  ),
  downloadButton("downloadReport", "Download Report") 
)