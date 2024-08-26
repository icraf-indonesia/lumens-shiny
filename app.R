library(shiny)

ui <- htmlTemplate("index.html",
  button04 = actionLink("quescButton", "QUES-C"),
  button10 = actionLink("scenarioBuilderButton", "SCIENDO-scenario builder"),
)

server <- function(input, output) {
  observeEvent(input$quescButton, {
    rstudioapi::jobRunScript(path = "call04.R")
  })
  
  observeEvent(input$scenarioBuilderButton, {
    rstudioapi::jobRunScript(path = "call10.R")
  })
}

shinyApp(ui=ui, server=server)