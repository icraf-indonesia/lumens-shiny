# load library ----------------------------------------------------------------------------
library(shiny)
library(DT)
library(readr)

# ui ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Planning Unit Reconcilitation (PUR)"),

  fluidRow(
    column(6,  # Left side: Inputs and buttons
           fluidRow(
             column(12, fileInput("reference_map", "Reference Map", accept = c(".tif", ".tiff"), placeholder = "input your raster file")),
             column(12, fileInput("attribute_table", "Reference Map Attribute Table", accept = c(".csv"), placeholder = "input your csv file")),
             column(12, fileInput("reference_class", "Reference Class Table", accept = c(".csv"), placeholder = "input your csv file"))
           ),
           
           hr(),
           
           fluidRow(
             column(12, 
                    actionButton("add_btn", "Add Planning Unit"),
                    actionButton("rm_btn", "Remove Planning Unit"),
                    actionButton("load_data", "Load Data"),
                    textOutput("counter")
             )
           ),
           
           hr(),
           
           fluidRow(
             column(12, 
                    div(
                      style = "border: 1px solid #ddd; padding: 10px; margin-top: 10px;",
                      h4("Planning Unit Inputs"),
                      uiOutput("form_table")
                    )
             )
           ),
           
           fluidRow(
             column(12, 
                    div(
                      style = "margin-top: 20px;",
                      h4("Collected Planning Unit Data"),
                      DTOutput("data_table")
                    )
             )
           ),
           
           hr(),
    ),
    
    column(6,  # Right side: HTML previewer
           div(
             style = "border: 1px solid #ddd; padding: 10px; margin-top: 20px;",
             h4("HTML Previewer"),
             htmlOutput("html_previewer")  # Placeholder for HTML previewer
           )
    )
  )
)

# server ------------------------------------------------------------------------------
server <- function(input, output, session) {
  counter <- reactiveValues(n = 0)
  
  observeEvent(input$add_btn, {counter$n <- counter$n + 1})
  observeEvent(input$rm_btn, {
    if (counter$n > 0) counter$n <- counter$n - 1
  })
  
  output$counter <- renderText({
    paste("Number of Planning Units:", counter$n)
  })
  
  form_inputs <- reactive({
    n <- counter$n
    
    if (n > 0) {
      tagList(
        div(style = "display: flex; flex-direction: column; border-bottom: 1px solid #ccc; border-radius: 5px; padding: 10px; background-color: #f9f9f9;",
            div(style = "display: flex; font-weight: bold; border-bottom: 2px solid #ccc; padding-bottom: 10px; margin-bottom: 10px;",
                div(style = "flex: 0.5; min-width: 50px;text-align: center;", "PU"),
                div(style = "flex: 2; min-width: 150px;", "Name"),
                div(style = "flex: 2; min-width: 150px;", "File"),
                div(style = "flex: 1; min-width: 100px;", "Action")
            ),
            lapply(seq_len(n), function(i) {
              div(style = "display: flex; align-items: center; margin-bottom: 10px;",
                  div(style = "flex: 0.5; min-width: 50px; text-align: center;", paste("PU", i)),
                  div(style = "flex: 2; min-width: 150px; padding-right: 10px;", 
                      textInput(inputId = paste0("textin", i),
                                label = NULL,
                                placeholder = paste("Planning Unit", i))),
                  div(style = "flex: 2; min-width: 150px; padding-right: 10px;", 
                      fileInput(inputId = paste0("filein", i),
                                label = NULL,
                                accept = c(".tif", ".tiff"))),
                  div(style = "flex: 1; min-width: 100px;", 
                      selectInput(inputId = paste0("action", i),
                                  label = NULL,
                                  choices = c("Reconciliation", "Additional")))
              )
            })
        )
      )
    }
  })
  
  output$form_table <- renderUI({ form_inputs() })
  
  collected_data <- eventReactive(input$load_data, {
    n <- counter$n
    if (n > 0) {
      data <- data.frame(
        Name = sapply(seq_len(n), function(i) input[[paste0("textin", i)]]),
        Path = sapply(seq_len(n), function(i) {
          file <- input[[paste0("filein", i)]]
          if (!is.null(file)) file$name else "No file uploaded"
        }),
        Action = sapply(seq_len(n), function(i) input[[paste0("action", i)]]),
        stringsAsFactors = FALSE
      )
      return(data)
    }
    return(NULL)
  })
  
  output$data_table <- renderDT({
    data <- collected_data()
    if (!is.null(data)) {
      datatable(data, options = list(pageLength = 10))
    }
  })
  
  attribute_table <- reactive({
    req(input$attribute_table)
    df <- read_csv(input$attribute_table$datapath)
    colnames(df)[1] <- "Attribute value"
    df$`Reference class` <- ""  # Add a new empty column
    df
  })
  
  reference_class <- reactive({
    req(input$reference_class)
    read_csv(input$reference_class$datapath)
  })
  
  observeEvent(input$load_reference_class, {
    req(attribute_table(), reference_class())
    attr_table <- attribute_table()
    ref_class <- reference_class()
    
    # Extract unique classes from the reference_class CSV
    class_options <- unique(ref_class$class)
    
    output$attribute_table_display <- renderDT({
      datatable(
        attr_table,
        editable = list(target = "cell", disable = list(columns = 1)),
        options = list(
          columnDefs = list(
            list(
              targets = which(colnames(attr_table) == "Reference class") - 1,
              render = JS(sprintf(
                "function(data, type, row, meta) {
                  var options = %s;
                  var selectHtml = '<select>';
                  selectHtml += '<option value=\"\">Select</option>';
                  for (var i = 0; i < options.length; i++) {
                    selectHtml += '<option value=\"' + options[i] + '\"' + (data == options[i] ? ' selected' : '') + '>' + options[i] + '</option>';
                  }
                  selectHtml += '</select>';
                  return selectHtml;
                }",
                jsonlite::toJSON(class_options)
              ))
            )
          )
        )
      )
    })
  })
  
  observeEvent(input$save_attr_actions, {
    attr_table <- hot_to_r(input$attribute_table_display)
    output$attribute_table_display <- renderDT({
      datatable(attr_table, options = list(pageLength = 10))
    })
  })
  
  output$html_previewer <- renderUI({
    # Placeholder for HTML content
    HTML("<p>HTML preview will be displayed here.</p>")
  })
}

# shinyapp ------------------------------------------------------------------------------
shinyApp(ui, server)
