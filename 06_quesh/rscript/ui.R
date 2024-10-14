ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(version = 5),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  titlePanel("QuES-H Module"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rainfall_file", "Total Annual Precipitation Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("dem_file", "Digital Elevation Model (DEM)", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("sand_file", "Sand Content Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("silt_file", "Silt Content Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("clay_file", "Clay Content Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("orgc_file", "Organic Content Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
      fileInput("pu_file", "Planning Unit Map", 
                accept = c(".shp", ".dbf", ".prj", ".shx"), 
                multiple = TRUE,
                placeholder = "input shapefiles (.shp, .dbf, .prj, .shx)"),
      fileInput("c_ref_file", "C Factor Attribute", accept = c(".csv"), placeholder = "input table (.csv)"),
      numericInput("map_resolution", "Map Resolution", value = NULL),
      
      # Select P Factor Option
      radioButtons("practice", "P Factor Map Available?", 
                   choices = c("Yes" =  "yes", "No" = "no"),
                   selected = "no"),
      conditionalPanel(
        condition = "input.practice == 'yes'",
        fileInput("practice_file", "P Factor Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)")
      ),
      conditionalPanel(
        condition = "input.practice == 'no'",
        HTML("<p><i>The P factor is assumed to have a value of 1</i></p>")
      ),
      
      # Select Multiple Time Series Option
      radioButtons("multiseries", "Multiple Time Series Analysis",
                   choices = c("Single Step" = "single_step", "Two Step" = "two_step")),
      conditionalPanel(
        condition = "input.multiseries == 'two_step'",
        fileInput("lc_t1_file", "Initial Land Cover/Use Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
        numericInput("t1", "Initial Year", value = NULL, min = 1000, max = 9999, step = 1),
        fileInput("lc_t2_file", "Final Land Cover/Use Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
        numericInput("t2", "Final Year", value = NULL, min = 1000, max = 9999, step = 1),
      ),
      conditionalPanel(
        condition = "input.multiseries == 'single_step'",
        fileInput("lc_t1_file", "Land Cover/Use Map", accept = c(".tif", ".tiff"), placeholder = "input raster (.tif)"),
        numericInput("t1", "Year", value = NULL, min = 1000, max = 9999, step = 1)
      ),
      
      div(style = "display: flex; flex-direction: column; gap: 10px;",
          shinyDirButton("output_dir", "Select Output Directory", "Please select a directory"),
          verbatimTextOutput("print_output_dir", placeholder = TRUE),
          actionButton("run_analysis", "Run QuES-H Analysis",
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
                   uiOutput("dynamic_guide", inline = TRUE)
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