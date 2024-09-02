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
    landuse_table_data = NULL,
    results = NULL,
    unit = NULL,
    location = NULL,
    I_O_period = NULL,
    landuse_area0 = NULL,
    BPD_graph = NULL,
    FPD_graph = NULL,
    LRC_graph = NULL,
    Linkages_table = NULL,
    land.requirement_table = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  #### File Input Observers ####
  file_inputs <- c("int_con", "add_val", "fin_dem", "fin_dem_struc", "add_val_struc", "sector", "labour", "land_distribution", "land_use", "landuse_table")
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
      int_con <- rv$int_con_data
      add_val <- rv$add_val_data
      int_con.m <- as.matrix(rv$int_con_data)
      add_val.m <- as.matrix(rv$add_val_data)
      sector <- rv$sector_data
      labour <- rv$labour_data
      land_distribution <- rv$land_distribution_data
      land_use <- rv$land_use_data
      fin_dem <- rv$fin_dem_data
      fin_dem_struc <- rv$fin_dem_struc_data
      add_val_struc <- rv$add_val_struc_data
      landuse_lut <- rv$landuse_table_data
      land_use <- rv$land_use_data

      # Generate the file path for saving the raster
      save_location <- file.path(rv$wd, "landuse_area0.tif")
      # Attempt to save the raster and then load it
      tryCatch({
        writeRaster(rv$land_use_data, save_location, overwrite = TRUE)
        # Load the raster from the saved location
        landuse_area0 <- raster(save_location)
        rv$landuse_area0 <- landuse_area0
      })
      
      # Calculate Inverse Leontief
      dim <- ncol(int_con.m)
      int_con.ctot <- colSums(int_con.m)
      add_val.ctot <- colSums(add_val.m)
      fin_con <- 1 / (int_con.ctot + add_val.ctot)
      fin_con[is.infinite(fin_con)] <- 0
      t.input.invers <- diag(fin_con)
      A <- int_con.m %*% t.input.invers
      I <- as.matrix(diag(dim))
      I_A <- I - A
      Leontief <- solve(I_A)

      # Calculate Direct Backward and Forward Linkages
      DBL <- colSums(Leontief) / mean(colSums(Leontief))
      DFL <- rowSums(Leontief) / mean(rowSums(Leontief))
      
      # Filter out non-finite values from sector data or any data used in plotting
      colnames(sector) <- c("SECTOR", "CATEGORY")
      DBL <- DBL[is.finite(DBL)]
      DFL <- DFL[is.finite(DFL)]
      
      # Create Linkages Table
      
      Linkages_table <- create_linkages_table(sector, DBL, DFL)
      rv$Linkages_table <- Linkages_table
      
      # Calculate Land Requirements
      land.requirement_table <- calculate_land_requirements(land_distribution, land_use, fin_dem, int_con.m, sector)
      land.requirement_table <- land.requirement_table[is.finite(land.requirement_table$LRC), ]
      rv$land.requirement_table <- land.requirement_table
      
      # Generate Graphs
      BPD_graph <- create_graph(sector, DBL, "DBL", "Direct Backward Linkages")
      FPD_graph <- create_graph(sector, DFL, "DFL", "Direct Forward Linkages")
      LRC_graph <- create_graph(sector, land.requirement_table$LRC, "LRC", "Land Requirement Coefficient")
      
      rv$BPD_graph <- BPD_graph
      rv$FPD_graph <- FPD_graph
      rv$LRC_graph <- LRC_graph
      
      # Generate Land Distribution Prop
      lc_freq <- freq(land_use)
      lc_freq <- as.data.frame(na.omit(lc_freq))
      landuse_area <- as.matrix((lc_freq$count))
      land_distribution_t <- as.matrix(land_distribution)
      landuse_area_diag <- diag(as.numeric(landuse_area))
      land_distribution_val <- land_distribution_t %*% landuse_area_diag
      
      land_requirement <- rowSums(land_distribution_val)
      land_distribution_ctot<-colSums(land_distribution_val)
      land.distribution.prop<-land_distribution_val %*% diag(1/land_distribution_ctot)
      land.distribution.prop[is.na(land.distribution.prop)]<-0

      # Generate GDP
      GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
      GDP.val.m<-as.matrix(GDP.val)
      GDP.val.m<-as.numeric(GDP.val.m)
      OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
      OUTPUT.val.m<-as.matrix(OUTPUT.val)
      OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
      GDP<-cbind(sector,GDP.val,OUTPUT.val)
      colnames(GDP)[1] <- "SECTOR"
      colnames(GDP)[2] <- "CATEGORY"
      colnames(GDP)[3] <- "GDP"
      colnames(GDP)[4] <- "OUTPUT"
      GDP$GDP_PROP<-GDP$GDP/GDP$OUTPUT
      GDP[is.na(GDP)]<-0
      colnames(GDP)[5] <- "P_OUTPUT"
      GDP_tot<-as.matrix(GDP$GDP)
      GDP_tot<-colSums(GDP_tot)
      
      #OUTPUT MULTIPLIER 
      Out.multiplier<-colSums(Leontief)
      Out.multiplier<-cbind(sector,Out.multiplier)
      
      #INCOME MULTIPLIER
      V.income<-as.matrix(GDP.val*fin_con)
      Inc.multiplier<-Leontief%*%V.income
      multiplier<-cbind(Out.multiplier,Inc.multiplier)
      
      #LABOUR MULTIPLIER
      labour.m<-as.matrix(labour*fin_con)
      labour.m<-labour.m/1000000
      Lab.multiplier<-Leontief%*%labour.m
      multiplier<-cbind(multiplier,Lab.multiplier)
      colnames(multiplier)[1] <- "SECTOR"
      colnames(multiplier)[2] <- "CATEGORY"
      colnames(multiplier)[5] <- "Lab.multiplier"
      multiplier$Out.multiplier<-round(multiplier$Out.multiplier, digits=3)
      Lab.multiplier<-cbind(sector,Lab.multiplier)
      colnames(Lab.multiplier)[3]<-"Lab.multiplier"
      
      # Return Results
        list(
        Linkages_table = Linkages_table,
        land.requirement_table = land.requirement_table,
        BPD_graph = BPD_graph,
        FPD_graph = FPD_graph,
        LRC_graph = LRC_graph,
        sector = sector,
        int_con = int_con,
        add_val = add_val,
        add_val_struc = add_val_struc,
        fin_dem = fin_dem,
        fin_dem_struc = fin_dem_struc,
        labour = labour,
        land_distribution = land_distribution,
        landuse_lut = landuse_lut,
        land.distribution.prop = land.distribution.prop,
        GDP = GDP,
        GDP.val = GDP.val,
        GDP_tot = GDP_tot,
        Lab.multiplier = Lab.multiplier,
        landuse_area0 = landuse_area0
      )
      
      #=Save all params into .Rdata objects
      save(int_con,
           add_val,
           fin_dem,
           fin_dem_struc,
           add_val_struc,
           sector,
           labour,
           Linkages_table,
           Leontief,
           GDP,
           GDP.val,
           GDP_tot,
           Lab.multiplier,
           land_distribution_ctot,
           land.requirement_table,
           land.distribution.prop,
           landuse_area0,
           landuse_lut,
           file=paste0(rv$wd, '/LandRequirement_db.Rdata'))
    })
  })
  
  #### Render Outputs ####
  output$Linkages_table <- renderTable({
    rv$Linkages_table
  })
  
  output$land.requirement_table <- renderTable({
    rv$land.requirement_table
  })
  
  output$BPD_graph <- renderPlot({
    rv$BPD_graph
  })
  
  output$FPD_graph <- renderPlot({
    rv$FPD_graph
  })
  
  output$LRC_graph <- renderPlot({
    rv$LRC_graph
  })
  
  #### Report Generation ####
  report_content <- reactive({
    params <- list(
      Linkages_table = rv$Linkages_table,
      land.requirement_table = rv$land.requirement_table,
      BPD_graph = rv$BPD_graph,
      FPD_graph = rv$FPD_graph,
      LRC_graph = rv$LRC_graph
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