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
    landuse_table_path = NULL,  # Land use lookup table path
    report_file = NULL
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
  
  observeEvent(c(input$unit, input$location,input$I_O_period), {
    rv$unit <- input$unit
    rv$location <- input$location
    if (is_numeric_str(input$I_O_period)) {
      rv$I_O_period <- as.numeric(input$I_O_period)
    }
  })
  
  #### Processing the Data ####
  #' When the user triggers the analysis, process all the input data and compute the required results.
  observeEvent(input$processTAReg1, {
    
    # Initialize an empty list to track missing inputs
    missing_inputs <- c()
    
    # Validate input files to ensure all required data is uploaded before proceeding
    if (is.null(rv$sector_data)) {
      missing_inputs <- c(missing_inputs, "Sector Data File")
    }
    if (is.null(rv$int_con_data)) {
      missing_inputs <- c(missing_inputs, "Intermediate Consumption Data File")
    }
    if (is.null(rv$fin_dem_struc_data)) {
      missing_inputs <- c(missing_inputs, "Final Demand Component Data File")
    }
    if (is.null(rv$fin_dem_data)) {
      missing_inputs <- c(missing_inputs, "Final Demand Data File")
    }
    if (is.null(rv$add_val_struc_data)) {
      missing_inputs <- c(missing_inputs, "Added Value Component Data File")
    }
    if (is.null(rv$add_val_data)) {
      missing_inputs <- c(missing_inputs, "Added Value Data File")
    }
    if (is.null(rv$labour_data)) {
      missing_inputs <- c(missing_inputs, "Labour Data File")
    }
    if (is.null(rv$land_distribution_data)) {
      missing_inputs <- c(missing_inputs, "Land Distribution Data File")
    }
    if (is.null(rv$land_use_data)) {
      missing_inputs <- c(missing_inputs, "Land Use Map File")
    }
    if (is.null(rv$landuse_table_data)) {
      missing_inputs <- c(missing_inputs, "Land Use Table File")
    }
    if (is.null(rv$unit) || length(rv$unit) == 0 || is.na(rv$unit) || rv$unit == "") {
      missing_inputs <- c(missing_inputs, "Unit Value")
    }
    if (is.null(rv$location) || length(rv$location) == 0 || is.na(rv$location) || rv$location == "") {
      missing_inputs <- c(missing_inputs, "Location Value")
    }
    if (is.null(rv$I_O_period)) {
      missing_inputs <- c(missing_inputs, "IO Period Value")
    }
    if (is.null(rv$wd) || length(rv$wd) == 0 || is.na(rv$wd) || rv$wd == "") {
      missing_inputs <- c(missing_inputs, "Output Directory")
    }
    
    # If there are missing inputs, show a notification and stop
    if (length(missing_inputs) > 0) {
      showNotification(
        paste("Please upload the following inputs:", paste(missing_inputs, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
      #### Leontief Inverse Calculation ####
      #' Compute the Leontief inverse from the input-output matrix, which is essential for economic impact analysis.
    withProgress(message = 'Running TA Regional 1 Analysis', value = 0, {
      tryCatch({
        
        sector_path <- rename_uploaded_file(input$sector_file)
        int_con_path <- rename_uploaded_file(input$int_con_file)
        fin_dem_struc_path <- rename_uploaded_file(input$fin_dem_struc_file)
        fin_dem_path <- rename_uploaded_file(input$fin_dem_file)
        add_val_struc_path <- rename_uploaded_file(input$add_val_struc_file)
        add_val_path <- rename_uploaded_file(input$add_val_file)
        labour_path <- rename_uploaded_file(input$labour_file)
        land_distribution_path <- rename_uploaded_file(input$land_distribution_file)
        land_use_path <- rename_uploaded_file(input$land_use_file)
        landuse_table_path <- rename_uploaded_file(input$landuse_table_file)
        unit <- input$unit
        location <- input$location
        I_O_period <- input$I_O_period
        output_dir <- rv$wd
        
        # Define local variables
        sector <- read.csv(sector_path, header = FALSE)
        int_con <- read.csv(int_con_path, header = FALSE)
        fin_dem_struc <- read.csv(fin_dem_struc_path, header = FALSE)
        fin_dem <- read.csv(fin_dem_path, header = FALSE)
        add_val_struc <- read.csv(add_val_struc_path, header = FALSE)
        add_val <- read.csv(add_val_path, header = FALSE)
        labour <- read.csv(labour_path, header = FALSE)
        int_con.m <- as.matrix(read.csv(int_con_path, header = FALSE))
        add_val.m <- as.matrix(read.csv(add_val_path, header = FALSE))
        land_distribution <- read.csv(land_distribution_path, header = FALSE)
        landuse_lut <- read.csv(landuse_table_path, header = TRUE)
        
        # Capture the start time at the beginning of the process
        start_time <- Sys.time()
        
        # Define initial Landuse Area
        landuse_area0 <- rast(land_use_path)
        landuse_area0_freq <- freq(landuse_area0)
        landuse_area0_freq$layer<-NULL
        landuse_area0_table <- as.data.frame(na.omit(landuse_area0_freq))
        colnames(landuse_area0_table) <- c("ID", "COUNT")
        rv$landuse_area0_table <- landuse_area0_table
        
        # Calculate Inverse Leontief
        incProgress(0.1, detail = "Calculating Leontief matrix")
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
        incProgress(0.2, detail = "Calculating Linkages table")
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
        incProgress(0.3, detail = "Calculating Primary Sector")
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
        incProgress(0.4, detail = "Calculating Land Requirements Calculation")
        land_use <- rast(land_use_path)
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
        incProgress(0.5, detail = "Calculating GDP")
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
        incProgress(0.6, detail = "Calculating Multiplier")
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
        incProgress(0.8, detail = "Saving data to Rdata")
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
        
        # Capture the end time at the end of the process
        end_time <- Sys.time()
        
        #### Report Generation ####
        #' This section generates a final report in HTML format, summarizing the results of the analysis.
        params <- list(
          start_time = as.character(format(start_time, "%Y-%m-%d %H:%M:%S")),
          end_time = as.character(format(end_time, "%Y-%m-%d %H:%M:%S")),
          BPD_graph = BPD_graph,
          FPD_graph = FPD_graph,
          Linkages_table = Linkages_table,
          PRS_graph = PRS_graph,
          P.sector.selected = P.sector.selected,
          GDP = GDP,
          GDP_graph = GDP_graph,
          multiplier = multiplier,
          OMPL_graph = OMPL_graph,
          IMPL_graph = IMPL_graph,
          LMPL_graph = LMPL_graph,
          land.requirement_table = land.requirement_table,
          LRC_graph = LRC_graph,
          session_log = format_session_info_table(),
          sector_path = sector_path,
          int_con_path = int_con_path,
          fin_dem_struc_path = fin_dem_struc_path,
          fin_dem_path = fin_dem_path,
          add_val_struc_path = add_val_struc_path,
          add_val_path = add_val_path,
          labour_path = labour_path,
          land_distribution_path = land_distribution_path,
          land_use_path = land_use_path,
          landuse_table_path = landuse_table_path,
          unit = unit,
          location = location,
          I_O_period = I_O_period,
          output_dir = output_dir
        )
        
        output_file <- paste0("ta_regional1_report_", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".html")
        rv$report_file <- paste(output_dir, output_file, sep = "/")
        
        render(
          "../report_template/ta-regional1_report.Rmd",
          output_file = output_file,
          output_dir = output_dir,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        # Notify user of successful completion
        setProgress(1, message = "Processing Complete")
        showNotification("All outputs have been generated", type = "message")
        
        output$status_messages <- renderText("Analysis completed successfully!")
        showNotification("Analysis completed successfully!", type = "message")
        shinyjs::show("open_report")
        shinyjs::show("open_output_folder")
        
      }, error = function(e) {
        output$error_messages <- renderText(paste("Error in analysis:", e$message))
        showNotification(paste("Error in analysis:", e$message), type = "error")
      })
    })
  })
  
  # Open Output Folder button observer
  observeEvent(input$open_output_folder, {
    if (!is.null(rv$wd) && dir.exists(rv$wd)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$wd)
      } else {
        system2("open", args = rv$wd)
      }
    } else {
      showNotification("Output directory not found", type = "error")
    }
  })
  
  # Open Report button observer
  observeEvent(input$open_report, {
    if (!is.null(rv$report_file) && file.exists(rv$report_file)) {
      if (.Platform$OS.type == "windows") {
        shell.exec(rv$report_file)
      } else {
        system2("open", args = rv$report_file)
      }
    } else {
      showNotification("Report file not found", type = "error")
    }
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