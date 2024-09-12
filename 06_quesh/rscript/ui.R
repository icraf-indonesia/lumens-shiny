page_sidebar(
  title = "Land Use Planning for Multiple Environmental Services (LUMENS)",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "QUES-H RUSLE Analysis",
    width = 600,
    fileInput("rainfall_file", "Total Annual Precipitation Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("dem_file", "Digital Elevation Model (DEM)", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("sand_file", "Sand Content Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("silt_file", "Silt Content Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("clay_file", "clay Content Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("orgc_file", "Organic Content Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("lc_dir", "Land Cover Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("pu_file", "Planning Unit Map", accept = c(".tif"), placeholder = "input your raster file"),
    fileInput("c_ref_file", "C Factor Attribute", accept = c(".csv"), placeholder = "input your csv file"),
    shinyDirButton("wd", "Select Working Directory", "Select a folder"),
    textOutput("selected_directory"),
    actionButton("process", "Run PUR Build")
  ),
  # To display the report
  card(
    card_header("Guide"),
    card_body(
      includeMarkdown("06_quesh/report_template/quesh_report.Rmd")
    )
  ),
  actionButton("viewReport", "View report", icon = icon("file-code")) 
)
