fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("Regional Economic Descriptive Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("sector_file", "Sector table", accept = c(".csv")),
      fileInput("int_con_file", "Intermediate Demand table", accept = c(".csv")),
      fileInput("fin_dem_struc_file", "Final Demand Component table", accept = c(".csv")),
      fileInput("fin_dem_file", "Final Demand table", accept = c(".csv")),
      fileInput("add_val_struc_file", "Added Value Component table", accept = c(".csv")),
      fileInput("add_val_file", "Added Value table", accept = c(".csv")),
      fileInput("labour_file", "Labour table", accept = c(".csv")),
      fileInput("land_distribution_file", "Land Distribution table", accept = c(".csv")),
      fileInput("land_use_file", "Land Use map", placeholder = "Please input your raster file", accept = c("image/tiff")),
      fileInput("landuse_table_file", "Land Use Lookup Table", accept = c(".csv")),
      textInput("unit", "Unit", placeholder = "Input currency (e.g. Million IDR)"),
      textInput("location", "Location", placeholder = "Input location name"),
      textInput("I_O_period", "IO Period", placeholder = "Input year"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          textOutput("selected_directory"),
          actionButton("processTAReg1", "Run Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "View Report",
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
        tabPanel("User Guide", uiOutput("user_guide")),
        tabPanel("Log",
                 br(),
                 textOutput("selected_dir"),
                 verbatimTextOutput("status_messages"),
                 verbatimTextOutput("error_messages"),
                 plotOutput("result_plot")
        )
      )
    )
  )
)