server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  #' Initialize reactive values to store various data inputs and outputs.
  #' These values are updated dynamically as the app runs.
  rv <- reactiveValues(
    wd = "",  # Working directory path
    int_con_data = NULL,  # Intermediate consumption data
    add_val_data = NULL,  # Added value data
    fin_dem_data = NULL,  # Final demand data
    fin_dem_struc_data = NULL,  # Final demand structure data
    add_val_struc_data = NULL,  # Added value structure data
    sector_data = NULL,  # Sector data
    labour_data = NULL,  # Labour data
    land_distribution_data = NULL,  # Land distribution data
    land_use_data = NULL,  # Land use raster data
    landuse_table_data = NULL,  # Land use lookup table data
    results = NULL,  # Store results from analysis
    unit = NULL,  # Unit for the data (e.g., hectares, tons)
    location = NULL,  # Location information (e.g., region)
    I_O_period = NULL,  # Input-output analysis period
    landuse_area0_table = NULL,  # Initial land use area
    BPD_graph = NULL,  # Backward linkages graph
    FPD_graph = NULL,  # Forward linkages graph
    LRC_graph = NULL,  # Land requirement coefficient graph
    PRS_graph = NULL,  # Primary sector graph
    GDP_graph = NULL,  # GDP graph
    OMPL_graph = NULL,  # Output multiplier graph
    IMPL_graph = NULL,  # Income multiplier graph
    LMPL_graph = NULL,  # Labour multiplier graph
    Linkages_table = NULL,  # Table for sectoral linkages
    land.requirement_table = NULL,  # Land requirement table
    P.sector = NULL,  # Primary sector data
    P.sector.selected = NULL,  # Selected primary sectors
    GDP = NULL,  # GDP data,
    int_con_path = NULL,  # Intermediate consumption path
    add_val_path = NULL,  # Added value path
    fin_dem_path = NULL,  # Final demand path
    fin_dem_struc_path = NULL,  # Final demand structure path
    add_val_struc_path = NULL,  # Added value structure path
    sector_path = NULL,  # Sector path
    labour_path = NULL,  # Labour path
    land_distribution_path = NULL,  # Land distribution path
    land_use_path = NULL,  # Land use raster path
    landuse_table_path = NULL  # Land use lookup table path
  )
  
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  
  #### Set Working Directory ####
  #' Choose and set the working directory where output files will be saved.
  shinyDirChoose(input, 'wd', roots = volumes, session = session)
  observeEvent(input$wd, {
    rv$wd <- parseDirPath(volumes, input$wd)
  })
  
  output$selected_directory <- renderText({
    #' Display the selected output directory to the user.
    rv$wd <- parseDirPath(volumes, input$wd)
    if(length(rv$wd) == 0) {
      return()
    } else {
      paste0("Selected output directory: ",  rv$wd)
    }
  })
  
  output$user_guide <- renderUI({
    #' Render the user guide if available, or display a message if not found.
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
  #' Watch for file inputs and update reactive values based on the selected file.
  #' The raster file is handled specifically for land use data.
  file_inputs <- c("int_con", "add_val", "fin_dem", "fin_dem_struc", "add_val_struc", "sector", "labour", "land_distribution", "land_use", "landuse_table")
  lapply(file_inputs, function(id) {
    observeEvent(input[[paste0(id, "_file")]], {
      file <- input[[paste0(id, "_file")]]
      if (!is.null(file)) {
        rv[[paste0(id, "_data")]] <- if(id == "land_use") {
          rast(file$datapath)
        } else {
          read.csv(file$datapath, header = FALSE)
        }
      }
    })
  })
  
  #### Processing the Data ####
  #' When the user triggers the analysis, process all the input data and compute the required results.
  observeEvent(input$processTAReg1, {
    if (is.null(rv$sector_data)) {
      showNotification("Sector data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$int_con_data)) {
      showNotification("Intermediate Consumption data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$fin_dem_struc_data)) {
      showNotification("Final Demand structure data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$fin_dem_data)) {
      showNotification("Final Demand data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$add_val_struc_data)) {
      showNotification("Added Value structure data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$add_val_data)) {
      showNotification("Added Value data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$labour_data)) {
      showNotification("Labour data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$land_distribution_data)) {
      showNotification("Land Distribution data file is missing.", type = "error")
      return()
    }
    if (is.null(rv$land_use_data)) {
      showNotification("Land use map file is missing.", type = "error")
      return()
    }
    if (is.null(rv$landuse_table_data)) {
      showNotification("Land use table file is missing.", type = "error")
      return()
    }
    
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
      
      #### Save raster to file ####
      landuse_area0 <- rv$land_use_data
      landuse_area0_freq <- freq(landuse_area0)
      landuse_area0_freq$layer<-NULL
      landuse_area0_table <- as.data.frame(na.omit(landuse_area0_freq))
      colnames(landuse_area0_table) <- c("ID", "COUNT")
      rv$landuse_area0_table <- landuse_area0_table
      
      #### Leontief Inverse Calculation ####
      #' Compute the Leontief inverse from the input-output matrix, which is essential for economic impact analysis.
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
          
          #### Direct Linkages Calculation ####
          #' Calculate Direct Backward and Forward Linkages (DBL, DFL).
          DBL <- colSums(Leontief) / mean(colSums(Leontief))
          DFL <- rowSums(Leontief) / mean(rowSums(Leontief))
          
          #' Filter finite values in the DBL and DFL calculations to avoid errors in plotting.
          colnames(sector) <- c("SECTOR", "CATEGORY")
          DBL <- DBL[is.finite(DBL)]
          DFL <- DFL[is.finite(DFL)]
          
          #### Linkages Table ####
          #' Create a table showing sectoral linkages based on DBL and DFL values.
          Linkages_table <- create_linkages_table(sector, DBL, DFL)
          rv$Linkages_table <- Linkages_table
          #' Generate a plot for the primary sectors based on linkages.
          PRS_graph<-ggplot(Linkages_table, aes(x=DBL, y=DFL, color=CATEGORY)) + 
            geom_point(shape=19, size=5) + 
            geom_hline(aes(yintercept=1), colour="#BB0000", linetype="dashed") + 
            geom_vline(aes(xintercept=1), colour="#BB0000", linetype="dashed")
          
          #### Sector Selection ####
          #' Identify the primary sectors based on DBL and DFL values greater than or equal to 1.
          P.sector<-cbind(sector,DBL,DFL)
          colnames (P.sector) [1]<-"Sectors"
          P.sector.selected <- P.sector[ which(P.sector$DBL >= 1),]
          P.sector.selected <- P.sector.selected[ which(P.sector.selected$DFL >= 1),]
          colnames(P.sector.selected)[1] <- "SECTOR"
          colnames(P.sector.selected)[2] <- "CATEGORY"
          rownames(P.sector.selected)<-NULL
          rv$P.sector <- P.sector
          
          #### Land Requirements Calculation ####
          #' Calculate land requirements based on the input data and save the results in the reactive values.
          land.requirement_table <- calculate_land_requirements(land_distribution, land_use, fin_dem, int_con.m, sector)
          land.requirement_table <- land.requirement_table[is.finite(land.requirement_table$LRC), ]
          rv$land.requirement_table <- land.requirement_table
          
          # Generate Graphs
          #' Create and plot graphs for Direct Backward Linkages (BPD), Direct Forward Linkages (FPD), and Land Requirement Coefficient (LRC)
          #' These graphs are used to visualize the relationships between sectors and various economic or environmental linkages
          BPD_graph <- create_graph(sector, DBL, "DBL", "Direct Backward Linkages")
          FPD_graph <- create_graph(sector, DFL, "DFL", "Direct Forward Linkages")
          LRC_graph <- create_graph(sector, land.requirement_table$LRC, "LRC", "Land Requirement Coefficient")
          
          # Generate Land Distribution Prop
          #' Calculate land distribution properties by computing the frequency of land use, adjusting the land distribution matrix, and generating the proportion of land use
          lc_freq <- freq(land_use)
          lc_freq <- as.data.frame(na.omit(lc_freq))
          landuse_area <- as.matrix((lc_freq$count))
          land_distribution_t <- as.matrix(land_distribution)
          landuse_area_diag <- diag(as.numeric(landuse_area))
          land_distribution_val <- land_distribution_t %*% landuse_area_diag
          
          #' Calculate the total land requirement and land distribution proportions
          land_requirement <- rowSums(land_distribution_val)
          land_distribution_ctot<-colSums(land_distribution_val)
          land.distribution.prop<-land_distribution_val %*% diag(1/land_distribution_ctot)
          land.distribution.prop[is.na(land.distribution.prop)]<-0
          
          # Generate GDP
          #' Calculate GDP based on added value matrices, and prepare data for plotting GDP and sector output
          GDP.val<-as.data.frame(add_val.m[2,]+add_val.m[3,])
          GDP.val.m<-as.matrix(GDP.val)
          GDP.val.m<-as.numeric(GDP.val.m)
          OUTPUT.val<-as.data.frame(add_val.m[2,]+add_val.m[3,]+add_val.m[1,]+int_con.ctot)
          OUTPUT.val.m<-as.matrix(OUTPUT.val)
          OUTPUT.val.m<-as.numeric(OUTPUT.val.m)
          
          #' Combine sector information with calculated GDP and output values, and compute the GDP proportion for each sector
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
          GDP$P_GDP<-round((GDP$GDP/GDP_tot), digits=2)
          rownames(GDP)<-NULL
          GDP_tot <- sum(GDP$GDP)
          GDP$P_GDP <- round((GDP$GDP / GDP_tot), digits = 2)
          
          #' Add a total row to the GDP data frame and reorder by GDP value for graphing
          total_row <- data.frame(SECTOR = "Total", CATEGORY = "", GDP = GDP_tot, OUTPUT = "", P_OUTPUT = "", P_GDP = "")
          GDP <- rbind(GDP, total_row)
          rownames(GDP) <- NULL
          order_GDP <- as.data.frame(GDP[order(-GDP$GDP),])
          order_GDP10<-head(order_GDP,n=20)
          
          #' Plot the GDP by sector
          GDP_graph<-ggplot(data=order_GDP10, aes(x=SECTOR, y=GDP, fill=CATEGORY)) +
            geom_bar(colour="black", stat="identity")+ coord_flip() +
            xlab("Sectors") + ylab("GDP")
          
          # OUTPUT MULTIPLIER 
          #' Calculate the output multiplier for each sector using the Leontief matrix 
          Out.multiplier<-colSums(Leontief)
          Out.multiplier<-cbind(sector,Out.multiplier)
          order_Out.multiplier <- as.data.frame(Out.multiplier[order(-Out.multiplier$Out.multiplier),])
          order_Out.multiplier <-head(order_Out.multiplier,n=20)
          
          #' Plot the output multiplier by sector
          OMPL_graph<-ggplot(data=order_Out.multiplier, aes(x=SECTOR, y=Out.multiplier, fill=CATEGORY)) + 
            geom_bar(colour="black", stat="identity")+ coord_flip() +  
            xlab("Sectors") + ylab("Output multiplier")
          
          # INCOME MULTIPLIER
          #' Calculate income multiplier for each sector using the GDP and financial consumption values
          V.income<-as.matrix(GDP.val*fin_con)
          Inc.multiplier<-Leontief%*%V.income
          multiplier<-cbind(Out.multiplier,Inc.multiplier)
          Inc.multiplier<-cbind(sector,Inc.multiplier)
          colnames(Inc.multiplier)[3]<-"Inc.multiplier"
          order_Inc.multiplier <- as.data.frame(Inc.multiplier[order(-Inc.multiplier$Inc.multiplier),])
          order_Inc.multiplier <-head(order_Inc.multiplier,n=20)
          
          #' Plot the income multiplier by sector
          IMPL_graph<-ggplot(data=order_Inc.multiplier, aes(x=SECTOR, y=Inc.multiplier, fill=CATEGORY)) + 
            geom_bar(colour="black", stat="identity")+ coord_flip() +  
            xlab("Sectors") + ylab("Income multiplier") 
          
          # LABOUR MULTIPLIER
          #' Calculate labour multiplier for each sector using labour and financial consumption values
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
          order_Lab.multiplier <- as.data.frame(Lab.multiplier[order(-Lab.multiplier$Lab.multiplier),])
          order_Lab.multiplier <-head(order_Lab.multiplier,n=20)
          
          #' Plot the labour multiplier by sector
          LMPL_graph<-ggplot(data=order_Lab.multiplier, aes(x=SECTOR, y=Lab.multiplier, fill=CATEGORY)) + 
            geom_bar(colour="black", stat="identity")+ coord_flip() +  
            xlab("Sectors") + ylab("Labour multiplier")
          colnames(multiplier)[4]<-"Inc.multiplier"
          
          rv$GDP <- GDP
          rv$P.sector.selected <- P.sector.selected
          rv$BPD_graph <- BPD_graph
          rv$FPD_graph <- FPD_graph
          rv$LRC_graph <- LRC_graph
          rv$PRS_graph <- PRS_graph
          rv$GDP_graph <- GDP_graph
          rv$multiplier <- multiplier
          rv$OMPL_graph <- OMPL_graph
          rv$IMPL_graph <- IMPL_graph
          rv$LMPL_graph <- LMPL_graph
          rv$sector_path <- rename_uploaded_file(input$sector_file)
          rv$int_con_path <- rename_uploaded_file(input$int_con_file)
          rv$fin_dem_struc_path <- rename_uploaded_file(input$fin_dem_struc_file)
          rv$fin_dem_path <- rename_uploaded_file(input$fin_dem_file)
          rv$add_val_struc_path <- rename_uploaded_file(input$add_val_struc_file)
          rv$add_val_path <- rename_uploaded_file(input$add_val_file)
          rv$labour_path <- rename_uploaded_file(input$labour_file)
          rv$land_distribution_path <- rename_uploaded_file(input$land_distribution_file)
          rv$land_use_path <- rename_uploaded_file(input$land_use_file)
          rv$landuse_table_path <- rename_uploaded_file(input$landuse_table_file)
          rv$unit <- input$unit
          rv$location <- input$location
          rv$I_O_period <- input$I_O_period
          
          # Return Results
          list(
            Linkages_table = Linkages_table,
            land.requirement_table = land.requirement_table,
            BPD_graph = BPD_graph,
            FPD_graph = FPD_graph,
            LRC_graph = LRC_graph,
            PRS_graph = PRS_graph,
            GDP_graph = GDP_graph,
            OMPL_graph = OMPL_graph,
            IMPL_graph = IMPL_graph,
            LMPL_graph = LMPL_graph,
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
            Prop_GDP = GDP$P_OUTPUT,
            Lab.multiplier = Lab.multiplier,
            Out.multiplier = Out.multiplier,
            Inc.multiplier = Inc.multiplier,
            landuse_area0_table = landuse_area0_table
          )
          
          # Save results and create return list
          #' Save the calculated tables and graphs, then display a completion message
          save(int_con,add_val,
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
               Out.multiplier,
               Inc.multiplier,
               land_distribution_ctot,
               land.requirement_table,
               land.distribution.prop,
               landuse_area0_table,
               landuse_lut,
               file=paste0(rv$wd, '/LandRequirement_db.Rdata'))
          
          # Notify user of successful completion
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
  
  #### Report Generation ####
  #' This section generates a final report in HTML format, summarizing the results of the analysis.
  report_content <- reactive({
    params <- list(
      BPD_graph = rv$BPD_graph,
      FPD_graph = rv$FPD_graph,
      Linkages_table = rv$Linkages_table,
      PRS_graph = rv$PRS_graph,
      P.sector.selected = rv$P.sector.selected,
      GDP = rv$GDP,
      GDP_graph = rv$GDP_graph,
      multiplier = rv$multiplier,
      OMPL_graph = rv$OMPL_graph,
      IMPL_graph = rv$IMPL_graph,
      LMPL_graph = rv$LMPL_graph,
      land.requirement_table = rv$land.requirement_table,
      LRC_graph = rv$LRC_graph,
      session_log = format_session_info_table(),
      sector_path = rv$sector_path,
      int_con_path = rv$int_con_path,
      fin_dem_struc_path = rv$fin_dem_struc_path,
      fin_dem_path = rv$fin_dem_path,
      add_val_struc_path = rv$add_val_struc_path,
      add_val_path = rv$add_val_path,
      labour_path = rv$labour_path,
      land_distribution_path = rv$land_distribution_path,
      land_use_path = rv$land_use_path,
      landuse_table_path = rv$landuse_table_path,
      unit = rv$unit,
      location = rv$location,
      I_O_period = rv$I_O_period,
      output_dir = rv$wd
    )
    output_file <- paste0("ta_regional1_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
    output_dir <- rv$wd
    render(
      "../report_template/ta-regional1_report.Rmd",
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
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  observeEvent(input$returnButton, {
    js$closeWindow()
    message("Return to main menu!")
    # shinyjs::delay(1000, stopApp())
  })
}