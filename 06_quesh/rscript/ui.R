ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  titlePanel("QuES-H Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rainfall_file", "Total Annual Precipitation Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("dem_file", "Digital Elevation Model (DEM)", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("sand_file", "Sand Content Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("silt_file", "Silt Content Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("clay_file", "Clay Content Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("orgc_file", "Organic Content Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("lc_dir", "Land Cover Map", accept = c(".tif"), placeholder = "input your raster file"),
      fileInput("pu_file", 
                "Planning Unit Map", 
                accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"), 
                multiple = T,
                placeholder = "input all related shapefiles"),
      fileInput("c_ref_file", "C Factor Attribute", accept = c(".csv"), placeholder = "input your csv file"),
      numericInput("map_resolution", "Map Resolution", value = 100),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("wd", "Select Output Directory", "Please select a directory"),
          actionButton("process", "Run QuES-H",
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
                 uiOutput("user_guide")),
                  div(style = "height: calc(100vh - 100px); overflow-y: auto;",
                      card_body(
                        if (file.exists("06_quesh/helpfile/help.Rmd")) {
                          includeMarkdown("06_quesh/helpfile/help.Rmd")
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