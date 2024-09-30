library(shiny)

ui <- htmlTemplate("index.html",
                   button01 = actionButton("buildButton", "Build"),
                   button02 = actionButton("reconcileButton", "Reconcile"),
                   button03 = actionButton("prequesButton", "Pre-QUES"),
                   button04 = actionButton("quescButton", "QUES-C"),
                   button05 = actionButton("quesbButton", "QUES-B"),
                   button06 = actionButton("queshButton", "QUES-H"),
                   button07 = actionButton("taProfitButton", "LU Profitability"),
                   button08 = actionButton("reDescButton", "RE Descriptive"),
                   button09 = actionButton("reProjButton", "RE Projection"),
                   button10 = actionButton("scenarioBuilderButton", "Scenario Builder"),
                   button11 = actionButton("trainModelButton", "Train Model"),
                   button12 = actionButton("simulateBuilderButton", "Simulate"),
                   button13 = actionButton("lasemButton", "LASEM")
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
  
  observeEvent(input$queshButton, {
    showNotification("Running QUES-H", type = "message")
    system("rscript.exe call06.R")
  })
  
  observeEvent(input$taProfitButton, {
    showNotification("Running LU Profitability", type = "message")
    system("rscript.exe call07.R")
  })
  
  observeEvent(input$reDescButton, {
    showNotification("Running RE Descriptive", type = "message")
    system("rscript.exe call08.R")
  })
  
  observeEvent(input$reProjButton, {
    showNotification("Running RE Projection", type = "message")
    system("rscript.exe call09.R")
  })  
  
  observeEvent(input$scenarioBuilderButton, {
    showNotification("Running SCIENDO Scenario Builder", type = "message")
    system("rscript.exe call10.R")
  })
  
  observeEvent(input$trainModelButton, {
    showNotification("Running SCIENDO Train Model", type = "message")
    system("rscript.exe call11.R")
  })
  
  observeEvent(input$simulateBuilderButton, {
    showNotification("Running SCIENDO Simulate", type = "message")
    system("rscript.exe call12.R")
  })
  
  observeEvent(input$lasemButton, {
    showNotification("Running LASEM", type = "message")
    system("rscript.exe call13.R")
  })
}

shinyApp(ui=ui, server=server)