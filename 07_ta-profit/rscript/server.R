server <- function(input, output, session) {
  
  #### Initialize reactive values ####
  rv <- reactiveValues(
    wd = "",
    report_file = NULL,
    map1_file = NULL,
    map2_file = NULL,
    year1 = NULL,
    year2 = NULL,
    carbon_file = NULL,
    npv_file = NULL,
    map1_rast = NULL,
    map2_rast = NULL,
    tbl_npv = NULL,
    quesc_tbl = NULL,
    all_tbl_carbon = NULL,
    tbl_carbon = NULL,
    period = NULL,
    tot_area = NULL,
    opcost_table = NULL,
    npv_chg_map = NULL,
    opcost_map = NULL,
    emission_map = NULL,
    raster_nodata = NULL,
    cost_threshold = NULL,
    map_npv1 = NULL,
    map_npv2 = NULL,
    opcost_curve = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  # Define helper functions
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  #### File Inputs ####
  observeEvent(input$map1_file, {
    rv$map1_file <- input$map1_file$datapath
    rv$map1_rast <- raster(rv$map1_file)
  })
  
  observeEvent(input$map2_file, {
    rv$map2_file <- input$map2_file$datapath
    rv$map2_rast <- raster(rv$map2_file)
  })
  
  observeEvent(input$carbon_file, {
    rv$carbon_file <- input$carbon_file$datapath
    df_c <- read.csv(rv$carbon_file)
    rv$quesc_tbl <- df_c
    rv$tbl_carbon <- df_c %>% dplyr::select(ID = 1, Carbon = 3)
  })
  
  observeEvent(input$npv_file, {
    rv$npv_file <- input$npv_file$datapath
    df_n <- read.csv(rv$npv_file)
    rv$tbl_npv <- df_n %>% dplyr::select(ID_LC = 1, NPV = 3)
  })
  
  #### Read and Process Years ####
  observeEvent(c(input$year1, input$year2), {
    rv$period <- as.numeric(input$year2) - as.numeric(input$year1)
  })
  
  observeEvent(input$cost_threshold, {
    rv$cost_threshold <- as.numeric(input$cost_threshold)
  })
  
  #### Process Data ####
  observeEvent(input$process, {
    
    # Validate inputs
    if (is.null(rv$map1_rast) || is.null(rv$map2_rast) || is.null(rv$tbl_carbon) || is.null(rv$tbl_npv)) {
      showNotification("Please upload all required files", type = "error")
      return()
    }
    withProgress(message = "Processing Data", value = 0, {
    
      # Prepare NPV Lookup Table
      npv_result <- prepare_npv_lookup(rv$tbl_npv, rv$quesc_tbl)
      rv$quesc_tbl <- npv_result$quesc_tbl
      tot_area <- npv_result$tot_area
      
      # Build Opportunity Cost Table
      opcost_result <- build_opcost_table(rv$quesc_tbl, rv$period, tot_area)
      rv$opcost_table <- opcost_result$opcost_all
      rv$opcost_table$order<-c(1:nrow(rv$opcost_table))
      find_x_val<-subset(rv$opcost_table, opcost_log>=log10(rv$cost_threshold))
      x_val<-find_x_val$order[1]
      
      # Carbon Accounting
      carbon_result <- carbon_accounting(rv$map1_rast, rv$map2_rast, rv$tbl_npv, rv$tbl_carbon, input$raster_nodata)
      rv$map_carbon1 <- carbon_result$map_carbon1
      rv$map_carbon2 <- carbon_result$map_carbon2
      rv$emission_map <- carbon_result$emission_map
      
      # NPV Accounting
      npv_result <- npv_accounting(rv$map1_rast, rv$map2_rast, rv$tbl_npv)
      rv$map_npv1 <- npv_result$map_npv1
      rv$map_npv2 <- npv_result$map_npv2
      rv$npv_chg_map <- npv_result$npv_chg_map
      
      # Calculate Opportunity Cost Map
      rv$opcost_map <- calculate_opcost_map(rv$npv_chg_map, rv$emission_map)
      
      # Generate Output Maps
      generate_output_maps(rv$map_carbon1, rv$map_carbon2, rv$emission_map, rv$opcost_map, rv$wd)
      
      # Generate the Opportunity Cost Curve (source: https://www.r-bloggers.com/2015/07/waterfall-plots-what-and-how/)
      rv$opcost_curve <- generate_opportunity_cost_curve(rv$opcost_table)
      
      setProgress(1, message = "Processing Complete")
      showNotification("All outputs have been generated", type = "message")
    })
  })
  
  # Set working directory
  shinyDirChoose(
    input, 
    'wd',
    roots = volumes,
    session = session
  )
  
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  # # Generate Report
  report_content <- reactive({
    params <- list(
      map_carbon1 = rv$map_carbon1,
      map_carbon2 = rv$map_carbon2,
      emission_map = rv$emission_map,
      opcost_map = rv$opcost_map,
      opcost_table = rv$opcost_table,
      opcost_curve = rv$opcost_curve,
      npv1_map = rv$map_npv1,
      npv2_map = rv$map_npv1,
      delta_npv = rv$npv_chg_map
    )
    
    output_file <- paste0("ta-profit_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    output_dir <- rv$wd
    rv$report_file <- paste(output_dir, output_file, sep = "/")
    
    render(
      "../report_template/report_template.Rmd",
      output_file = output_file,
      output_dir = output_dir,
      params = params,
      envir = new.env(parent = globalenv())
    )
  })
  
  observeEvent(input$viewReport, {
    file.show(report_content())
  })
}
