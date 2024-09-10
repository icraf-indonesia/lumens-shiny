library(shiny)
# library(callr)

ui <- htmlTemplate("index.html",
  button03 = actionLink("prequesButton", "Pre-QUES"),
  button04 = actionLink("quescButton", "QUES-C"),
  button05 = actionLink("quesbButton", "QUES-B"),
  button10 = actionLink("scenarioBuilderButton", "SCIENDO-scenario builder"),
)

server <- function(input, output) {
  observeEvent(input$prequesButton, {
    showNotification("Running Pre-QUES", type = "message")
    system("rscript.exe call03.R")
  })
  
  observeEvent(input$quescButton, {
    showNotification("Running QUES-C", type = "message")
    # #1
    # rstudioapi::jobRunScript(path = "call04.R")
    #
    # #2
    # a <- r_session$new()
    # a$call(
    #   function(){
    #     shiny::runApp('04_quesc/rscript/',  port = 875)
    #   }
    # )
    # 
    # utils::browseURL("http://localhost:875")
    
    system("rscript.exe call04.R")
  })
  
  observeEvent(input$quesbButton, {
    showNotification("Running QUES-B", type = "message")
    system("rscript.exe call05.R")
  })
  
  observeEvent(input$scenarioBuilderButton, {
    showNotification("Running SCIENDO Scenario Builder", type = "message")
    # rstudioapi::jobRunScript(path = "call10.R")
    system("rscript.exe call10.R")
  })
}

shinyApp(ui=ui, server=server)