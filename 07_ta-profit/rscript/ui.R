page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Trade-off Analysis (Profit)",
    width = 600,
    fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
    fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
    fileInput("npv_file", "NPV lookup table", accept = c(".csv")),
    fileInput("carbon_file", "QUES-C table", accept = c(".csv")),
    textInput("year1", "Year of T1"),
    textInput("year2", "Year of T2"),
    textInput("raster_nodata", "No Data"),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run")
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
