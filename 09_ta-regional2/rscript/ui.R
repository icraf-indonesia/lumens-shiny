page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Trade-off Analysis (Regional 2)",
    width = 600,
    fileInput("land_req_file", "Land Requirement Database", accept = c(".Rdata")),
    fileInput("projected_land_use_file", "Projected Landuse", accept = c("image/tiff")),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("processTAReg2", "Run")
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