library(shiny)

call_lumens_shiny_module <- function(r_script, module_name) {
  msg <- paste0("Running ", module_name)
  showNotification(msg, type = "message", duration = NULL, id = "running_message")
  
  command <- paste0("rscript.exe ", r_script)
  system(command)
  
  removeNotification("running_message")
  msg <- paste0(module_name, " has closed")
  showNotification(msg, type = "message")
}

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
    call_lumens_shiny_module("call01.R", "PUR Build")
  })
  
  observeEvent(input$reconcileButton, {
    call_lumens_shiny_module("call02.R", "PUR Reconcile")
  })
  
  observeEvent(input$prequesButton, {
    call_lumens_shiny_module("call03.R", "Pre-QUES")
  })
  
  observeEvent(input$quescButton, {
    call_lumens_shiny_module("call04.R", "QUES-C")
  })
  
  observeEvent(input$quesbButton, {
    call_lumens_shiny_module("call05.R", "QUES-B")
  })
  
  observeEvent(input$queshButton, {
    call_lumens_shiny_module("call06.R", "QUES-H")
  })
  
  observeEvent(input$taProfitButton, {
    call_lumens_shiny_module("call07.R", "LU Profitability")
  })
  
  observeEvent(input$reDescButton, {
    call_lumens_shiny_module("call08.R", "RE Descriptive")
  })
  
  observeEvent(input$reProjButton, {
    call_lumens_shiny_module("call09.R", "RE Projection")
  })  
  
  observeEvent(input$scenarioBuilderButton, {
    call_lumens_shiny_module("call10.R", "Scenario Builder")
  })
  
  observeEvent(input$trainModelButton, {
    call_lumens_shiny_module("call11.R", "SCIENDO Train Model")
  })
  
  observeEvent(input$simulateBuilderButton, {
    call_lumens_shiny_module("call12.R", "SCIENDO Simulate")
  })
  
  observeEvent(input$lasemButton, {
    call_lumens_shiny_module("call13.R", "LASEM")
  })
}

shinyApp(ui=ui, server=server)