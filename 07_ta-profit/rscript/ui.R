page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Trade-off Analysis (Profit)",
    width = 600,
    fileInput("landuse1_file", "Land cover map at T1", accept = c("image/tiff")),
    fileInput("landuse2_file", "Land cover map at T2", accept = c("image/tiff")),
    fileInput("npv_file", "NPV lookup table", accept = c(".csv")),
    fileInput("quesc_db_file", "QUES-C table", accept = c(".csv")),
    textInput("landuse1_year", "Year of T1"),
    textInput("landuse2_year", "Year of T2"),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("processProfit", "Run")
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