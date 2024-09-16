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
      fileInput("pu_file", "Planning Unit Map", accept = c(".shp"), placeholder = "input your shapefile"),
      fileInput("c_ref_file", "C Factor Attribute", accept = c(".csv"), placeholder = "input your csv file"),
      numericInput("map_resolution", "Map Resolution", value = 100),
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          actionButton("run_analysis", "Run QuES-H Analysis",
                       style = "font-size: 18px; padding: 10px 15px; background-color: #4CAF50; color: white;"),
          hidden(
            actionButton("open_report", "Open Report",
                         style = "font-size: 18px; padding: 10px 15px; background-color: #008CBA; color: white;")
          )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("User Guide", uiOutput("user_guide")),
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

# ui <- fluidPage(
#   page_sidebar(
#   title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
#   theme = bs_theme(version = 5),
#   sidebar = sidebar(
#     title = "QUES-H RUSLE Analysis",
#     width = 600,
#     fileInput("rainfall_file", "Total Annual Precipitation Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("dem_file", "Digital Elevation Model (DEM)", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("sand_file", "Sand Content Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("silt_file", "Silt Content Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("clay_file", "Clay Content Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("orgc_file", "Organic Content Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("lc_dir", "Land Cover Map", accept = c(".tif"), placeholder = "input your raster file"),
#     fileInput("pu_file", "Planning Unit Map", accept = c(".shp"), placeholder = "input your shapefile"),
#     fileInput("c_ref_file", "C Factor Attribute", accept = c(".csv"), placeholder = "input your csv file"),
#     shinyDirButton("wd", "Select Working Directory", "Select a folder"),
#     textOutput("selected_directory"),
#     actionButton("process", "Run PUR Build")
#   ),
#   # To display the report
#   card(
#     card_header("Guide"),
#     card_body(
#       includeMarkdown("06_quesh/helpfile/quesh_quick_user_guide.Rmd")
#     )
#   ),
#   actionButton("viewReport", "View report", icon = icon("file-code")) 
#   )
# )
