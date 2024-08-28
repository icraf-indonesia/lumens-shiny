server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = "",
    int_con_data = NULL,
    add_val_data = NULL,
    fin_dem_data = NULL,
    fin_dem_struc_data = NULL,
    add_val_struc_data = NULL,
    sector_data = NULL,
    labour_data = NULL,
    land_distribution_data = NULL,
    land_use_data = NULL,
    results = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  #### File Input Observers ####
  file_inputs <- c("int_con", "add_val", "fin_dem", "fin_dem_struc", "add_val_struc", "sector", "labour", "land_distribution", "land_use")
  lapply(file_inputs, function(id) {
    observeEvent(input[[paste0(id, "_file")]], {
      file <- input[[paste0(id, "_file")]]
      if (!is.null(file)) {
        rv[[paste0(id, "_data")]] <- if(id == "land_use") {
          raster(file$datapath)
        } else {
          read.csv(file$datapath, header = FALSE)
        }
      }
    })
  })
  
  #### Set Working Directory ####
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  observeEvent(input$wd, {
    rv$wd <- parseDirPath(volumes, input$wd)
  })
  
  output$selected_directory <- renderText({
    if (length(rv$wd) == 0) return()
    paste0("Selected output directory: ", rv$wd)
  })
  
  #### Processing the Data ####
  observeEvent(input$processTAReg1, {
    rv$results <- isolate({
      # Convert reactive data to matrices
      int_con.m <- as.matrix(rv$int_con_data)
      add_val.m <- as.matrix(rv$add_val_data)
      sector <- rv$sector_data
      labour <- rv$labour_data
      land_distribution <- rv$land_distribution_data
      land_use <- rv$land_use_data
      fin_dem <- rv$fin_dem_data
      
      dim <- ncol(int_con.m)
      
      # Calculate Inverse Leontief
      int_con.ctot <- colSums(int_con.m)
      add_val.ctot <- colSums(add_val.m)
      fin_con <- 1 / (int_con.ctot + add_val.ctot)
      fin_con[is.infinite(fin_con)] <- 0
      t.input.invers <- diag(fin_con)
      A <- int_con.m %*% t.input.invers
      I <- as.matrix(diag(dim))
      I_A <- I - A
      Leontief <- solve(I_A)
      
      # browser()
      
      # Calculate Direct Backward and Forward Linkages
      DBL <- colSums(Leontief) / mean(colSums(Leontief))
      DFL <- rowSums(Leontief) / mean(rowSums(Leontief))
      
      # Create Linkages Table
      colnames(sector) <- c("SECTOR", "CATEGORY")
      Linkages_table <- create_linkages_table(sector, DBL, DFL)
      
      # Calculate Land Requirements
      land.requirement_table <- calculate_land_requirements(land_distribution, land_use, fin_dem, int_con.m, sector)
      
      # Generate Graphs
      BPD_graph <- create_graph(sector, DBL, "DBL", "Direct Backward Linkages")
      FPD_graph <- create_graph(sector, DFL, "DFL", "Direct Forward Linkages")
      LRC_graph <- create_graph(sector, land.requirement_table$LRC, "LRC", "Land Requirement Coefficient")
      
      # Return Results
      list(
        Linkages_table = Linkages_table,
        land.requirement_table = land.requirement_table,
        BPD_graph = BPD_graph,
        FPD_graph = FPD_graph,
        LRC_graph = LRC_graph
      )
    })
  })
  
  #### Render Outputs ####
  output$Linkages_table <- renderTable({
    rv$results$Linkages_table
  })
  
  output$land.requirement_table <- renderTable({
    rv$results$land.requirement_table
  })
  
  output$BPD_graph <- renderPlot({
    rv$results$BPD_graph
  })
  
  output$FPD_graph <- renderPlot({
    rv$results$FPD_graph
  })
  
  output$LRC_graph <- renderPlot({
    rv$results$LRC_graph
  })
  
  #### Report Generation ####
  report_content <- reactive({
    params <- list(
      linkages_table = rv$results$Linkages_table,
      landreq_table = rv$results$land.requirement_table,
      BPD_graph = rv$results$BPD_graph,
      FPD_graph = rv$results$FPD_graph,
      LRC_graph = rv$results$LRC_graph
    )
    output_file <- paste0("ta_regional1_report_", Sys.Date(), ".html")
    output_dir <- rv$wd
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