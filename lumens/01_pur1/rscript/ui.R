ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("PUR Build Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("ref_map", 
                "Reference Map", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                multiple = T,
                placeholder = "input all related shapefiles"),
      fileInput("ref_class", "Reference Class", accept = c(".csv"), placeholder = "input your csv file"),
      fileInput("ref_mapping", "Reference Class of Reference Map", accept = c(".csv"), placeholder = "input your csv file"),
      fileInput("pu_units", "List of Planning Units", accept = c(".csv"), placeholder = "input your csv file"),
      textInput("map_resolution", "Map Resolution", value = 100),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("selected_directory", placeholder = TRUE),
          actionButton("process", "Run PUR Build",
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
                 uiOutput("user_guide"),
                 div(style = "height: calc(100vh - 100px); overflow-y: auto;",
                     card_body(
                       if (file.exists("01_pur1/helpfile/help.Rmd")) {
                         includeMarkdown("01_pur1/helpfile/help.Rmd")
                       } else if (file.exists("../helpfile/help.Rmd")) {
                         includeMarkdown("../helpfile/help.Rmd")
                       } else {
                         HTML("<p>User guide file not found.</p>")
                       }
                     )
                 )
        ),
        tabPanel("Analysis",
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 plotOutput("result_plot")
        )
      )
    )
  )
)