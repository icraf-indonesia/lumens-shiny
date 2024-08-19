server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    wd = "",
    map1_year = NULL,
    map2_year = NULL,
    map1_file = NULL,
    map2_file = NULL,
    mapz_file = NULL,
    map1_rast = NULL,
    map2_rast = NULL,
    mapz_rast = NULL,
    mapc_spat = NULL,
    maps_spat = NULL,
    mapz_df = NULL,
    tbl_c = NULL,
    tbl_quesc = NULL,
    period_year = NULL,
    preques = list()
  )
  
  lc_list <- c("map1", "map2")
  map_list <- c("mapz", "map1", "map2")

  volumes <- c(
      # Home = fs::path_home(),
      getVolumes()()
  )
  
  is_numeric_str <- function(s) {
    return(!is.na(as.integer(as.character(s))))
  }
  
  #### Read file inputs ####
  lapply(lc_list, function(id) {
    inputs <- paste0(id, "_file")
    files <- paste0(id, "_file")
    rasters <- paste0(id, "_rast")
    df <- paste0(id, "_df")
    
    observeEvent(input[[inputs]], {
      rv[[files]] <- input[[inputs]]
      rv[[rasters]] <- rv[[files]]$datapath %>% raster() 
    })
  })
  
  observeEvent(input$mapz_file, {
    shp <- input$mapz_file
    if(is.null(shp))
      return()
    
    prev_wd <- getwd()
    uploaded_dir <- dirname(shp$datapath[1])
    setwd(uploaded_dir)
    for(i in 1:nrow(shp)){
      file.rename(shp$datapath[i], shp$name[i])
    }
    setwd(prev_wd)
    
    rv$mapz_file <- paste(uploaded_dir, shp$name[grep(pattern="*.shp$", shp$name)], sep = "/")
    
    zone_sf <- rv$mapz_file %>% st_read()
    zone <- zone_sf %>% 
      rasterise_multipolygon(
        raster_res = c(100, 100), 
        field = "IDS" 
      )
    rv$mapz_df <- data.frame(ID_PU = zone_sf[[1]], PU = zone_sf[[2]])
    rv$mapz_rast <- zone %>% raster()
  })
  
  #### Read year input ####
  lapply(lc_list, function(x) {
    id <- paste0(x, "_year")
    observeEvent(input[[id]], {
      rv[[id]] <- input[[id]]
    })
  })
  
  #### Read carbon lookup table ####
  observeEvent(input$carbon_file, {
    f <- input$carbon_file
    df_c <- read.csv(f$datapath)
    
    if(nrow(df_c) == 0)
      return()
    if(nrow(df_c) < 2)
      return()
    if(!is_numeric_str(df_c[1, 1]))
      return()
    
    df <- data.frame("ID_LC" = as.integer((as.character(df_c[, 1]))))
    df$LC <- df_c[, 2]
    df$CARBON <- df_c[, 3]
    rv$tbl_c <- df
  })
  
  #### Set working directory ####
  shinyDirChoose(
    input, 
    'wd',
    roots = volumes,
    session = session
  )
  
  #### Do the calculation and store it to the markdown content ####
  report_content <- eventReactive(input$processQUESC, {
    rv$map1_rast <- rv$map1_rast %>% spatial_sync_raster(rv$mapz_rast)
    rv$map2_rast <- rv$map2_rast %>% spatial_sync_raster(rv$mapz_rast)
    
    print("load maps")
    lc_t1 <- rv$map1_rast %>% rast() %>%
      add_legend_to_categorical_raster(., lookup_table = rv$tbl_c, year = as.numeric(rv$map1_year))
    lc_t2 <- rv$map2_rast %>% rast() %>%
      add_legend_to_categorical_raster(., lookup_table = rv$tbl_c, year = as.numeric(rv$map2_year))  
    zone <- rv$mapz_rast %>% rast() %>%
      add_legend_to_categorical_raster(., lookup_table = rv$mapz_df)  
    
    rv$preques <- ques_pre(lc_t1, lc_t2, zone)
    rv$period_year <- as.numeric(rv$map1_year) - as.numeric(rv$map1_year)
    lucDummy <- generate_dummy_crosstab(rv$tbl_c, rv$mapz_df)
    
    # join table
    print("create QUESC-DB")
    df_lucdb <- rv$tbl_c %>% dplyr::rename(ID_LC1 = 1, C_T1 = 3) %>% 
      rename_with(.cols = 2, ~rv$map1_year) %>% right_join(lucDummy, by="ID_LC1")
    df_lucdb <- rv$tbl_c %>% dplyr::rename(ID_LC2 = 1, C_T2 = 3) %>% 
      rename_with(.cols = 2, ~rv$map2_year) %>% right_join(df_lucdb, by="ID_LC2")
    df_lucdb <- rv$mapz_df %>% dplyr::rename(ID_PU = 1) %>% 
      rename_with(.cols = 2, ~names(zone)) %>% right_join(df_lucdb, by="ID_PU") 
    df_lucdb <- df_lucdb %>% 
      left_join(
        rv$preques[["landscape_level"]][["crosstab_long"]], 
        by = c(names(zone), rv$map1_year, rv$map2_year)
      ) 
    # the full version of preques database from preques analysis combined with all possible landcover listed in the lookup table
    df_lucdb <- df_lucdb %>% replace(is.na(df_lucdb), 0)
    
    # create new matrix reclassification 
    reclassify_matrix <- as.matrix(rv$tbl_c[,1]) %>% 
      cbind(., as.matrix(rv$tbl_c[,3])) %>%
      rbind(., c(0, NA))
    
    # create all maps
    print("generate cabon, emission, and sequestration maps")
    map_carbon1 <- lc_t1 %>% classify(reclassify_matrix)
    map_carbon2 <- lc_t2 %>% classify(reclassify_matrix)
    map_emission <- ((map_carbon1 - map_carbon2) * 3.67) * (map_carbon1 > map_carbon2)
    map_sequestration <- ((map_carbon2 - map_carbon1) * 3.67) * (map_carbon1 < map_carbon2)
    
    # quescdatabase
    df_lucdb <- df_lucdb %>% mutate(
      EM = (C_T1 - C_T2) * (C_T1 > C_T2) * Ha * 3.67,
      SQ = (C_T2 - C_T1) * (C_T1 < C_T2) * Ha * 3.67,
      LU_CHG = do.call(paste, c(df_lucdb[c(rv$map1_year, rv$map2_year)], sep = " to "))
    )
    rv$tbl_quesc <- df_lucdb
    
    # save maps and db
    print(paste0("The output is successfully stored in ", input$wd))
    rv$wd <- parseDirPath(volumes, input$wd)
    write.table(df_lucdb,
                paste0(rv$wd, "/quesc_database.csv"), 
                quote=FALSE, 
                row.names=FALSE, 
                sep=",")
    writeRaster(map_carbon1,
                paste0(rv$wd, "/carbon_map_t1.tif"), overwrite = T)
    writeRaster(map_carbon2,
                paste0(rv$wd, "/carbon_map_t2.tif"), overwrite = T)
    writeRaster(map_emission,
                paste0(rv$wd, "/emission_map.tif"), overwrite = T)
    writeRaster(map_sequestration,
                paste0(rv$wd, "/sequestration_map.tif"), overwrite = T)
    
    # generate report
    params <- list(
      map_c1 = map_carbon1,
      map_c2 = map_carbon2,
      map_em = map_emission,
      map_sq = map_sequestration,
      ques_db = df_lucdb,
      p1 = rv$map1_year,
      p2 = rv$map2_year
    )
    temp_report <- tempfile(fileext = ".html")
    render("report_template.Rmd",
           output_file = temp_report,
           params = params,
           envir = new.env(parent = globalenv()))
    readLines(temp_report)
  })
    
  output$reportOutput <- renderUI({
    HTML(paste(report_content(), collapse = "\n"))
    # tryCatch({
    #   HTML(paste(report_content(), collapse = "\n"))
    #   
    # }, error = function(e){
    #   return(HTML(paste("Error generating the report:", e$message)))
    # })
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("quesc_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      writeLines(report_content(), file)
    }
  )
}