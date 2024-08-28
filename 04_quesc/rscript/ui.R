page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "QuES-C",
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
    actionButton("processQUESC", "Run"),
    tags$button(
      id = 'close',
      type = "button",
      class = "btn action-button",
      onclick = "setTimeout(function(){window.close();},500);",  # close browser
      "Close window"
    )
  ),
  
  # To display the report
  card(
    card_header("Guide"),
    card_body(
      includeMarkdown("../helpfile/help.md")
    )
  ),
  # actionButton("viewReport", "View report", icon = icon("file-code"))
)
