server <- function(input, output, session) {
  #### Initialize all required reactive values ####
  rv <- reactiveValues(
    landuse_table = NULL,
    land_req = NULL,
    projected_land_use = NULL,
    GDP_summary = NULL,
    Labour_table = NULL,
    LC_graph = NULL,
    GDP_graph = NULL,
    LAB_graph = NULL
  )
  
  volumes <- c(
    getVolumes()()
  )
  
  #### Load and process data ####
  observeEvent(input$land_req_file, {
    land_req <- input$land_req_file$datapath
    rv$land_req <- land_req
  })
  
  observeEvent(input$projected_land_use_file, {
    projected_land_use_path <- input$projected_land_use_file$datapath
    rv$projected_land_use <- raster(projected_land_use_path)
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
  
  #### Perform GDP and Labour Analysis ####
  observeEvent(input$processTAReg2, {
    # Projected Land Use
    load(rv$land_req)
    next_luc_freq <- freq(rv$projected_land_use)
    landuse_area_table <- as.data.frame(na.omit(next_luc_freq))
    colnames(landuse_area_table) <- c("ID", "COUNT")
    landuse_area <- as.matrix(landuse_area_table$COUNT)

    landuse_area0_freq<-freq(landuse_area0)
    landuse_area0_table<-as.data.frame(na.omit(landuse_area0_freq))
    colnames(landuse_area0_table)<-c("ID", "COUNT")
    landuse_area0<-as.matrix(landuse_area0_table$COUNT)
    
    names(landuse_lut) <- as.character(landuse_lut[1,])
    landuse_lut <- landuse_lut[-1,]
    lc.list<-subset(landuse_lut, select=c(ID, LC))
    
    landuse_table <- merge(lc.list, landuse_area0_table, by = "ID")
    landuse_table <- cbind(landuse_table, landuse_area)
    landuse_table$LC<-NULL
    landuse_area_diag<-diag(as.numeric(as.matrix(landuse_area)))
    colnames(landuse_table)[1] <- "LAND_USE"
    colnames(landuse_table)[2] <- "T1_HA"
    colnames(landuse_table)[3] <- "T2_HA"
    landuse_table$CHANGE <- landuse_table$T2_HA - landuse_table$T1_HA
    rv$landuse_table <- landuse_table
    
    # Generate the Land Use Change graph
    LC_graph<-ggplot(data=landuse_table, aes(x=LAND_USE, y=CHANGE)) +
      geom_bar(colour="black", stat="identity", position="dodge")+
      guides(fill=FALSE) + xlab("Land use") + ylab("Change") +ggtitle("Land Use Change")+ theme(axis.text.x  = element_text(angle=90, size=6))
    rv$LC_graph<-LC_graph
    
    #MODEL FINAL DEMAND
    land.distribution.scen<-land.distribution.prop %*% landuse_area_diag
    land.requirement.scen<-rowSums(land.distribution.scen)
    fin_dem.rtot<-rowSums(fin_dem)
    int_con.rtot<-rowSums(int_con)
    demand<-fin_dem.rtot+int_con.rtot
    land.requirement.db<-land.requirement_table
    land.requirement.coeff<-land.requirement.db$LRC
    land.productivity.coeff<-land.requirement.db$LPC
    fin_dem.scen<-land.requirement.scen/land.productivity.coeff
    fin_dem.scen[is.infinite(fin_dem.scen)]<-0
    fin_dem.scen[is.na(fin_dem.scen)]<-0
    
    #CALCULATE FINAL DEMAND AND GDP FROM SCENARIO OF LAND USE CHANGE
    fin.output.scen<-Leontief %*% fin_dem.scen
    fin.output.scen<-round(fin.output.scen, digits=1)
    colnames(fin.output.scen)[1]<-"OUTPUT_Scen"
    GDP.prop.from.output<-GDP.val/demand
    GDP.prop.from.output[is.na(GDP.prop.from.output)]<-0
    GDP.scen<-GDP.prop.from.output*fin.output.scen
    GDP.scen<-round(GDP.scen, digits=1)
    GDP.scen[is.na(GDP.scen)]<-0
    colnames(GDP.scen)[1] <- "GDP_scen"
    GDP.diff<-GDP.scen-GDP$GDP
    GDP.diff<-round(GDP.diff, digits=1)
    colnames(GDP.diff)[1] <- "GDP_diff"
    GDP.rate<-GDP.diff/GDP.val
    GDP.rate[is.na(GDP.rate)]<-0
    GDP.rate<-round(GDP.rate, digits=2)
    colnames(GDP.rate)[1] <- "GDP_rate"
    GDP_summary<-cbind(GDP,GDP.scen,fin.output.scen,GDP.diff, GDP.rate)
    GDP_summary$P_OUTPUT<-NULL
    GDP_summary$P_GDP<-NULL
    
    #CALCULATE TOTAL GDP
    GDP_tot_scen<-as.matrix(GDP_summary$GDP_scen)
    GDP_tot_scen<-colSums(GDP_tot_scen)
    GDP_tot_diff<-GDP_tot_scen-GDP_tot
    GDP_tot_rate<-GDP_tot_diff/GDP_tot
    text1<-"Total GDP"
    text2<-"Scenario GDP"
    text3<-"GDP difference"
    text4<-"Rate of difference"
    GDP_overall1<-rbind(text1,text2,text3,text4)
    GDP_overall2<-rbind(GDP_tot, GDP_tot_scen,GDP_tot_diff,GDP_tot_rate)
    GDP_overall<-cbind(GDP_overall1,GDP_overall2)
    
    order_GDP_scen <- as.data.frame(GDP_summary[order(-GDP_summary$GDP_scen),])
    order_GDP_scen10<-head(order_GDP_scen,n=20)
    GDP_summary.melt <- melt(data = order_GDP_scen10, id.vars=c('SECTOR'), measure.vars=c('GDP','GDP_scen'))
    rv$GDP_summary<-GDP_summary
    
    # Generate the GDP graph
    GDP_graph <- ggplot(data = GDP_summary.melt, aes(x = SECTOR, y = value, fill = variable)) +
      geom_bar(colour = "black", stat = "identity", position = "dodge") +
      guides(fill = FALSE) + xlab("Sectors") + ylab("GDP") +
      ggtitle("Comparison of GDP Baseline and Scenario") +
      theme(axis.text.x  = element_text(angle = 90, size = 6))
    rv$GDP_graph<-GDP_graph
    
    #CALCULATE TOTAL LABOUR
    Labour_table<-Lab.multiplier
    Labour_table$Lab.multiplier<-as.numeric(format(Labour_table$Lab.multiplier, digits=3, width=5))
    Labour_table<-cbind(Labour_table,fin.output.scen)
    Labour_table<-cbind(Labour_table,labour)
    colnames(Labour_table)[1] <- "SECTOR"
    colnames(Labour_table)[2] <- "CATEGORY"
    colnames(Labour_table)[4] <- "OUT_scen"
    colnames(Labour_table)[5] <- "Lab_base"
    test<-Labour_table$Lab.multiplier*Labour_table$OUT_scen*1000000
    test<-round(test, digits=0)
    Labour_table$Lab_scen<-test
    Labour_table$Lab_req<-Labour_table$Lab_scen-Labour_table$Lab_base
    test2<-Labour_table$Lab_req
    test2<-cbind(sector, test2)
    rv$Labour_table<-Labour_table
    
    # Generate the Labour graph
    LAB_graph<-ggplot(data=test2, aes(x=SECTOR, y=test2, fill=CATEGORY)) +
      geom_bar(colour="black", stat="identity", position="dodge")+
      guides(fill=FALSE) + xlab("Sector") + ylab("Labour requirement") +ggtitle("Impact of LU Change to Labour")+ theme(axis.text.x  = element_text(angle=90, size=6))
    rv$LAB_graph<-LAB_graph

    # Return Results
    list(
      landuse_table = landuse_table,
      GDP_summary = GDP_summary,
      Labour_table = Labour_table,
      LC_graph = LC_graph,
      GDP_graph = GDP_graph,
      LAB_graph = LAB_graph
    )
  })
  
  #### Render Outputs ####
  output$landuse_table <- renderTable({
    rv$landuse_table
  })
  
  output$GDP_summary <- renderTable({
    rv$GDP_summary
  })
  
  output$Labour_table <- renderTable({
    rv$Labour_table
  })
  
  output$LC_graph <- renderPlot({
    rv$LC_graph
  })
  
  output$GDP_graph <- renderPlot({
    rv$GDP_graph
  })
  
  output$LAB_graph <- renderPlot({
    rv$LAB_graph
  })
  
  #### Report Generation ####
  report_content <- reactive({
    params <- list(
      landuse_table = rv$landuse_table,
      GDP_summary = rv$GDP_summary,
      Labour_table = rv$Labour_table,
      LC_graph = rv$LC_graph,
      GDP_graph = rv$GDP_graph,
      LAB_graph = rv$LAB_graph
    )
    output_file <- paste0("ta_regional2_report_", Sys.Date(), ".html")
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
