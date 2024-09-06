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
    land.requirement_table = NULL,
    P.sector = NULL,
    GDP_graph = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  #### Set Working Directory ####
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  observeEvent(input$wd, {
    rv$wd <- parseDirPath(volumes, input$wd)
  })
  
  output$selected_directory <- renderText({
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  output$user_guide <- renderUI({
    guide_path <- "../helpfile/help.md"
    if (file.exists(guide_path)) {
      html_content <- rmarkdown::render(guide_path, output_format = "html_fragment", quiet = TRUE,
                                        output_options = list(metadata = list(title = "Trade-Off Analysis (Regional 1)")))
      HTML(readLines(html_content))
    } else {
      HTML("<p>User guide file not found.</p>")
    }
  })
  
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
    
    withProgress(message = 'Running TA Regional 1 Analysis', value = 0, {
      tryCatch({
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
        PRS_graph<-ggplot(Linkages_table, aes(x=BPD, y=FPD, color=CATEGORY)) + geom_point(shape=19, size=5) + geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
        
        # Calculate Land Requirements
        land.requirement_table <- calculate_land_requirements(land_distribution, land_use, fin_dem, int_con.m, sector)
        land.requirement_table <- land.requirement_table[is.finite(land.requirement_table$LRC), ]
        rv$land.requirement_table <- land.requirement_table
        
        # Generate Graphs
        BPD_graph <- create_graph(sector, DBL, "DBL", "Direct Backward Linkages")
        FPD_graph <- create_graph(sector, DFL, "DFL", "Direct Forward Linkages")
        LRC_graph <- create_graph(sector, land.requirement_table$LRC, "LRC", "Land Requirement Coefficient")
        GDP_graph <- create_graph(sector, GDP, "GDP", "Gross Domestic Product")
        # GDP_graph<-ggplot(data=order_GDP10, aes(x=SECTOR, y=GDP, fill=SECTOR)) + 
        #   geom_bar(colour="black", stat="identity")+ coord_flip() +  
        #   guides(fill=FALSE) + xlab("Sectors") + ylab("GDP") 
        
        rv$BPD_graph <- BPD_graph
        rv$FPD_graph <- FPD_graph
        rv$LRC_graph <- LRC_graph
        rv$PRS_graph <- PRS_graph
        rv$GDP_graph <- GDP_graph
        
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
        
        #SELECTION OF PRIMARY SECTOR
        P.sector<-cbind(DBL,DFL)
        colnames (P.sector) [1]<-"Sectors"
        P.sector[4]<-NULL
        P.sector[4]<-NULL
        P.sector.selected <- P.sector[ which(P.sector$DBL >= 1),]
        P.sector.selected <- P.sector.selected[ which(P.sector.selected$DFL >= 1),]
        colnames(P.sector.selected)[1] <- "SECTOR"
        colnames(P.sector.selected)[2] <- "CATEGORY"
        rv$P.sector <- P.sector
        
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
        order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier),])
        order_Out.multiplier <-head(order_Out.multiplier,n=20)
        OMPL_graph<-ggplot(data=order_Out.multiplier, aes(x=V1, y=Out.multiplier, fill=V2)) + 
          geom_bar(colour="black", stat="identity")+ coord_flip() +  
          guides(fill=FALSE) + xlab("Sectors") + ylab("Output multiplier")
        
        #INCOME MULTIPLIER
        V.income<-as.matrix(GDP.val*fin_con)
        Inc.multiplier<-Leontief%*%V.income
        multiplier<-cbind(Out.multiplier,Inc.multiplier)
        Inc.multiplier<-cbind(sector,Inc.multiplier)
        colnames(Inc.multiplier)[3]<-"Inc.multiplier"
        order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
        order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
        IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=V1, y=Inc.multiplier, fill=V2)) + 
          geom_bar(colour="black", stat="identity")+ coord_flip() +  
          guides(fill=FALSE) + xlab("Sectors") + ylab("Income multiplier") 
        
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
        Inc.multiplier<-cbind(sector,Inc.multiplier)
        colnames(Inc.multiplier)[3]<-"Inc.multiplier"
        order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
        order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
        IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=V1, y=Inc.multiplier, fill=V2)) + 
          geom_bar(colour="black", stat="identity")+ coord_flip() +  
          guides(fill=FALSE) + xlab("Sectors") + ylab("Income multiplier") 
        
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
        
        setProgress(1, message = "Processing Complete")
        showNotification("All outputs have been generated", type = "message")
        
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("viewReport")
        
      }, error = function(e) {
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
    })
  })
  
  addTable(rtffile,land.requirement_table,font.size=7.5)
  
  addParagraph(rtffile, "\\b\\fs20 Table 1. Sectoral linkages\\b0\\fs20.")
  addTable(rtffile,Linkages_table,font.size=8)
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,BPD_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 1. Ten sectors with highest Backward power of dispersion\\b0\\fs20.")
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,FPD_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 2. Ten sectors with highest Forward power of dispersion\\b0\\fs20.")
  
  addPlot(rtffile,plot.fun=print, width=6,height=4,res=300,PRS_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 3. Sectoral typology based on linkages analysis\\b0\\fs20.")
  
  addParagraph(rtffile, "\\b\\fs20 Table 2. Primary sectors based on potential linkage\\b0\\fs20.")
  addTable(rtffile,P.sector.selected,font.size=8)
  
  addParagraph(rtffile, "\\b\\fs20 Table 3. Sectoral GDP\\b0\\fs20.")
  addTable(rtffile,GDP,font.size=8)
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,GDP_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 4. Twenty sectors with highest GDP\\b0\\fs20.")
  
  addParagraph(rtffile, "\\b\\fs20 Table 4. Sectoral multiplier\\b0\\fs20.")
  addTable(rtffile,multiplier,font.size=8)
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,OMPL_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 5. Twenty sectors with highest Output multiplier\\b0\\fs20.")
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,IMPL_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 6. Twenty sectors with highest Income multiplier\\b0\\fs20.")
  
  addPlot(rtffile,plot.fun=print, width=5,height=3,res=300,LMPL_graph)
  addParagraph(rtffile, "\\b\\fs20 Figure 5. Twenty sectors with highest Labour multiplier\\b0\\fs20.")
  
  #### Report Generation ####
  report_content <- reactive({
    params <- list(
      Linkages_table = rv$Linkages_table,
      land.requirement_table = rv$land.requirement_table,
      BPD_graph = rv$BPD_graph,
      FPD_graph = rv$FPD_graph,
      LRC_graph = rv$LRC_graph,
      GDP = rv$GDP,
      
    )
    output_file <- paste0("ta_regional1_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
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
    showNotification("Opening report...", type = "message")
    file.show(report_content())
  })
}