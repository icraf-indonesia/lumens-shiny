library(shiny)

ui <- htmlTemplate("index.html",
                   button01 = actionButton("buildButton", "Build"),
                   button02 = actionButton("reconcileButton", "Reconcile"),
                   button03 = actionButton("prequesButton", "Pre-QUES"),
                   button04 = actionButton("quescButton", "QUES-C"),
                   button05 = actionButton("quesbButton", "QUES-B"),
                   button07 = actionButton("taProfitButton", "Profitability"),
                   button08 = actionButton("re1Button", "RE-1"),
                   button09 = actionButton("re2Button", "RE-2"),
                   button10 = actionButton("scenarioBuilderButton", "RE-Desc")
)

server <- function(input, output) {
  observeEvent(input$buildButton, {
    showNotification("Running PUR Build", type = "message")
    system("rscript.exe call01.R")
  })
  
  observeEvent(input$reconcileButton, {
    showNotification("Running PUR Reconcile", type = "message")
    system("rscript.exe call02.R")
  })
  
  observeEvent(input$prequesButton, {
    showNotification("Running Pre-QUES", type = "message")
    system("rscript.exe call03.R")
  })
  
  observeEvent(input$quescButton, {
    showNotification("Running QUES-C", type = "message")
    system("rscript.exe call04.R")
  })
  
  observeEvent(input$quesbButton, {
    showNotification("Running QUES-B", type = "message")
    system("rscript.exe call05.R")
  })
  
  observeEvent(input$taProfitButton, {
    showNotification("Running TA Profitability", type = "message")
    system("rscript.exe call07.R")
  })
  
  observeEvent(input$re1Button, {
    showNotification("Running TA RE 1", type = "message")
    system("rscript.exe call08.R")
  })
  
  observeEvent(input$re2Button, {
    showNotification("Running TA RE 2", type = "message")
    system("rscript.exe call09.R")
  })  
  
  observeEvent(input$scenarioBuilderButton, {
    showNotification("Running SCIENDO Scenario Builder", type = "message")
    system("rscript.exe call10.R")
  })
}

shinyApp(ui=ui, server=server)