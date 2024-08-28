page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Trade-off Analysis (Regional 1)",
    width = 600,
    fileInput("sector_file", "Sector table", accept = c(".csv")),
    fileInput("int_con_file", "Intermediate Demand table", accept = c(".csv")),
    fileInput("fin_dem_struc_file", "Final Demand Component table", accept = c(".csv")),
    fileInput("fin_dem_file", "Final Demand table", accept = c(".csv")),
    fileInput("add_val_struc_file", "Added Value Component table", accept = c(".csv")),
    fileInput("add_val_file", "Added Value table", accept = c(".csv")),
    fileInput("labour_file", "Labour table", accept = c(".csv")),
    fileInput("land_use_file", "Land Use map", accept = c("image/tiff")),
    fileInput("land_distribution_file", "Land Distribution table", accept = c(".csv")),
    textInput("unit", "Unit"),
    textInput("location", "Location"),
    textInput("I_O_period", "IO Period"),
    shinyDirButton("wd", "Select working directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("processTAReg1", "Run")
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