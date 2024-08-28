library(shiny)

ui <- htmlTemplate("index.html",
  button03 = actionLink("prequesButton", "Pre-QUES"),
  button04 = actionLink("quescButton", "QUES-C"),
  button10 = actionLink("scenarioBuilderButton", "SCIENDO-scenario builder"),
)

server <- function(input, output) {
  observeEvent(input$prequesButton, {
    showNotification("Running Pre-QUES", type = "message")
    system("rscript.exe call03.R")
  })
  
  observeEvent(input$quescButton, {
    showNotification("Running QUES-C", type = "message")
    system("rscript.exe call04.R")
  })
  
  observeEvent(input$scenarioBuilderButton, {
    showNotification("Running SCIENDO Scenario Builder", type = "message")
    system("rscript.exe call10.R")
  })
}

shinyApp(ui=ui, server=server)