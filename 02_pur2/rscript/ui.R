page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Planning Unit Reconciliation (PUR) Reconcile",
    width = 600,
    #stextInput("area_name", "Area Name"),
    fileInput("recon_file", 
              "Unresolved Planning Unit Map", 
              accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
              multiple = T,
              placeholder = "input all related shapefiles"),
    fileInput("unresolved_table", "Unresolved Attribute Table", accept = c(".xlsx"), placeholder = "input your xlsx file"),
    #textInput("map_resolution", "Map Resolution"),
    shinyDirButton("wd", "Select Working Directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run PUR Reconcile"),
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
      if (file.exists("02_pur2/helpfile/help.Rmd")) {
        includeMarkdown("02_pur2/helpfile/help.Rmd")
      } else if (file.exists("../helpfile/help.Rmd")) {
        includeMarkdown("../helpfile/help.Rmd")
      } else {
        HTML("<p>User guide file not found.</p>")
      }
    )
  ),
  actionButton("viewReport", "View report", icon = icon("file-code")) 
)
