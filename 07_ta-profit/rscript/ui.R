fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  titlePanel("Trade-off Analysis (Profit)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("map1_file", "Land cover map at T1", accept = c("image/tiff")),
      fileInput("map2_file", "Land cover map at T2", accept = c("image/tiff")),
      fileInput("npv_file", "NPV lookup table", accept = c(".csv")),
      fileInput("carbon_file", "QUES-C table", accept = c(".csv")),
      textInput("year1", "Year of T1"),
      textInput("year2", "Year of T2"),
      textInput("raster_nodata", "No Data"),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          textOutput("selected_directory"),
          actionButton("process", "Run Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("viewReport", "View Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          )
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