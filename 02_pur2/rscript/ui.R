ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  titlePanel("PUR Reconcile Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("recon_file", 
                "Unresolved Planning Unit Map", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                multiple = T,
                placeholder = "input all related shapefiles"),
      fileInput("unresolved_table", "Unresolved Attribute Table", accept = c(".xlsx"), placeholder = "input your xlsx file"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("selected_directory", placeholder = TRUE),
          actionButton("process", "Run PUR Reconcile",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          ),
          hidden(
            actionButton("open_output_folder", "Open Output Folder",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide",
                 uiOutput("user_guide"),
                 div(style = "height: calc(100vh - 100px); overflow-y: auto;",
                     card_body(
                       if (file.exists("02_pur2/helpfile/help.Rmd")) {
                         includeMarkdown("02_pur2/helpfile/help.Rmd")
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