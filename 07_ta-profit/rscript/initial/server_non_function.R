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
    rv$tbl_carbon <- df_c %>% select(ID = 1, Carbon = 3)
  })
  
  observeEvent(input$npv_file, {
    rv$npv_file <- input$npv_file$datapath
    df_n <- read.csv(rv$npv_file)
    rv$tbl_npv <- df_n %>% select(ID_LC = 1, NPV = 3)
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
      
      # #Prepare NPV Lookup Table
      lookup_n<-rv$tbl_npv
      colnames(lookup_n)[1] ="ID_LC1"
      colnames(lookup_n)[2] ="NPV1"
      rv$quesc_tbl<-merge(rv$quesc_tbl,lookup_n,by="ID_LC1")
      colnames(lookup_n)[1] ="ID_LC2"
      colnames(lookup_n)[2] ="NPV2"
      rv$quesc_tbl<-merge(rv$quesc_tbl,lookup_n,by="ID_LC2")
      tot_area<-sum(rv$quesc_tbl$Ha)

      #Build Opcost Table
      data_em_sel <- rv$quesc_tbl
      period<-rv$period
      data_em_sel <- data_em_sel[ which(data_em_sel$EM > 0),]
      data_em_sel<-within(data_em_sel, {
        em_rate<-((C_T1-C_T2)*(Ha*3.67))/(tot_area*period)
        em_tot<- (C_T1-C_T2)*3.67
        sq_rate<-((C_T1-C_T2)*(Ha*3.67))/(tot_area*period)
        sq_tot<- (C_T1-C_T2)*3.67
        opcost<-(NPV1-NPV2)/em_tot
        opcost_sq<-(NPV1-NPV2)/sq_tot
        cumsum_em<-cumsum(em_rate)
        cumsum_sq<-cumsum(sq_rate)
      })

      lcc_col<-as.data.frame(data_em_sel$LU_CHG)
      zone_col<-as.data.frame(data_em_sel$PU)
      opcost_col<-as.data.frame(data_em_sel$opcost)
      em_col<-as.data.frame(data_em_sel$em_rate)
      opcost_tab<-cbind(lcc_col,zone_col)
      opcost_tab<-cbind(opcost_tab,opcost_col)
      opcost_tab<-cbind(opcost_tab,em_col)
      names(opcost_tab)[1] <- "luchg"
      names(opcost_tab)[2] <- "zone"
      names(opcost_tab)[3] <- "opcost"
      names(opcost_tab)[4] <- "emrate"

      #Build Positive Opcost Table
      opcost_tab_p<- opcost_tab[ which(opcost_tab$opcost >= 0),]
      opcost_tab_p<- opcost_tab_p[order(opcost_tab_p$opcost),]
      opcost_tab_p$cum_emrate<-cumsum(opcost_tab_p$emrate)
      TA_opcost_database<-opcost_tab_p
      opcost_tab_p$opcost_log<-log10(opcost_tab_p$opcost)
      is.na(opcost_tab_p) <- sapply(opcost_tab_p, is.infinite)
      opcost_tab_p[is.na(opcost_tab_p)] <- 0

      #Build Negative Opcost Table
      opcost_tab_n<- opcost_tab[ which(opcost_tab$opcost < 0),]
      opcost_tab_n<- opcost_tab_n[order(opcost_tab_n$opcost),]
      opcost_tab_n$cum_emrate<-cumsum(opcost_tab_n$emrate)
      opcost_tab_n$opcost_log<-opcost_tab_n$opcost*-1
      opcost_tab_n$opcost_log<-log10(opcost_tab_n$opcost_log)*-1

      #Combine Positive && Negative Opcost
      opcost_all<-rbind(opcost_tab_n, opcost_tab_p)
      opcost_all$cum_emrate2<-as.factor(opcost_all$cum_emrate)

      #Find Cost Threshold
      rv$opcost_table <- opcost_all
      rv$opcost_table$order<-c(1:nrow(rv$opcost_table))
      find_x_val<-subset(rv$opcost_table, opcost_log>=log10(rv$cost_threshold))
      x_val<-find_x_val$order[1]

      # Load land use maps and set NoData value
      NAvalue(rv$map1_rast) <- as.numeric(input$raster_nodata)
      NAvalue(rv$map2_rast) <- as.numeric(input$raster_nodata)

      # Merge NPV and Carbon data
      names(rv$tbl_carbon)[names(rv$tbl_carbon) == "ID"] <- "ID_LC"
      merged_data <- merge(rv$tbl_npv, rv$tbl_carbon, by = "ID_LC")
      reclassify_matrix <- as.matrix(merged_data[, c("ID_LC", "Carbon")])

      # Carbon Accounting
      map_carbon1 <- reclassify(rv$map1_rast, reclassify_matrix)
      map_carbon2 <- reclassify(rv$map2_rast, reclassify_matrix)

      # Calculate Emissions
      chk_em <- map_carbon1 > map_carbon2
      rv$emission_map <- ((map_carbon1 - map_carbon2) * 3.67) * chk_em

      # NPV Accounting
      npv_matrix <- as.matrix(merged_data[, c("ID_LC", "NPV")])
      rv$map_npv1 <- reclassify(rv$map1_rast, npv_matrix)
      rv$map_npv2 <- reclassify(rv$map2_rast, npv_matrix)

      # Calculate NPV Change and Opportunity Cost
      rv$npv_chg_map <- rv$map_npv2 - rv$map_npv1
      rv$opcost_map <- rv$npv_chg_map / rv$emission_map

      # Generate Output Maps
      writeRaster(map_carbon1, file.path(rv$wd, "carbon_map_t1.tif"), overwrite = TRUE)
      writeRaster(map_carbon2, file.path(rv$wd, "carbon_map_t2.tif"), overwrite = TRUE)
      writeRaster(rv$emission_map, file.path(rv$wd, "emission_map.tif"), overwrite = TRUE)
      writeRaster(rv$opcost_map, file.path(rv$wd, "opcost_map.tif"), overwrite = TRUE)

      # Generate the Opportunity Cost Curve (source: https://www.r-bloggers.com/2015/07/waterfall-plots-what-and-how/)
      df_curve <- data.frame(
        emission = rv$opcost_table$emrate,
        opportunity_cost = rv$opcost_table$opcost,
        land_use_change = rv$opcost_table$luchg
      )
      
      df_grouped <- df_curve %>%
        group_by(land_use_change) %>%
        summarise(emission = sum(emission),
                  opportunity_cost = sum(opportunity_cost))
      
      df_all <- df_grouped %>% filter(opportunity_cost != 0)
      df_order <- df_all[order(df_all$opportunity_cost),]
      df_order$order<-c(1:nrow(df_order))
      
      opcost_curve <- ggplot(df_order, aes(x=order, y=opportunity_cost)) +
        labs(x = NULL,
             y = "Opportunity Cost ($/ton CO2-eq)",
             title = "Waterfall Plot for Opportunity Cost") +
        theme_classic() %+replace%
        theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
              axis.title.y = element_text(face="bold",angle=90)) +
        coord_cartesian(ylim = c(-5000,5000))
      
      rv$opcost_curve <- opcost_curve + geom_bar(stat="identity", width=0.7, position = position_dodge(width=0.4))
      
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
