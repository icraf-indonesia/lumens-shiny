ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")  
  ),
  titlePanel("PUR Build Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("ref_map", 
                "Reference Map", 
                accept = c(".shp", ".dbf", ".shx", ".prj"), 
                multiple = T,
                placeholder = "input shapefiles (.shp, .dbf, .shx, .prj)"),
      fileInput("ref_class", "Reference Class", accept = c(".csv"), placeholder = "input table (.csv)"),
      fileInput("ref_mapping", "Reference Class of Reference Map", accept = c(".csv"), placeholder = "input table (.csv)"),
      fileInput("pu_units", "List of Planning Units", accept = c(".csv"), placeholder = "input table (.csv)"),
      textInput("map_resolution", "Map Resolution (m)", placeholder = "e.g., 100, 30, etc."),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("selected_directory", placeholder = TRUE),
          actionButton("run_analysis", "Run PUR Build",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
            actionButton("open_output_folder", "Open Output Folder",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          actionButton("returnButton", "Return to Main Menu", 
                       style = "font-size: 18px; padding: 10px 15px; background-color: #FA8072; color: white;")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide",
                 div(
                   style = "height: 800px; overflow-y: scroll; padding: 15px; border: 1px solid #ddd; border-radius: 5px;",
                   uiOutput("user_guide")
                 )
        ),
        tabPanel("Log",
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 verbatimTextOutput("success_message")
        )
      )
    )
  )
)