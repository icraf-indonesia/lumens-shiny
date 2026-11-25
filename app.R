library(shiny)

MODULE_PORT <- 875L

is_port_open <- function(port = MODULE_PORT, host = "127.0.0.1") {
  con <- tryCatch(
    suppressWarnings(socketConnection(host = host, port = port, open = "r+", blocking = TRUE, timeout = 1)),
    error = function(e) NULL
  )
  if (is.null(con)) return(FALSE)
  close(con)
  TRUE
}

get_landing_url <- function(session) {
  cd <- session$clientData
  protocol <- cd$url_protocol
  hostname <- cd$url_hostname
  port <- cd$url_port
  pathname <- cd$url_pathname
  if (!nzchar(protocol) || !nzchar(hostname)) return("")
  port_part <- if (nzchar(port)) paste0(":", port) else ""
  paste0(protocol, "//", hostname, port_part, pathname)
}

get_module_url <- function(session, port = MODULE_PORT) {
  cd <- session$clientData
  protocol <- if (nzchar(cd$url_protocol)) cd$url_protocol else "http:"
  hostname <- if (nzchar(cd$url_hostname)) cd$url_hostname else "127.0.0.1"
  paste0(protocol, "//", hostname, ":", port, "/")
}

ui <- tagList(
  tags$head(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('lumens_navigate', function(url) { window.location.replace(url); });"
    ))
  ),
  htmlTemplate("index.html",
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
)

server <- function(input, output, session) {
  pending_launch <- reactiveVal(NULL)

  call_lumens_shiny_module <- function(r_script, module_name) {
    if (!is.null(pending_launch()) || is_port_open(MODULE_PORT)) {
      showNotification("Another module is already starting or running.", type = "warning", duration = 4)
      return(invisible(NULL))
    }

    showNotification(paste0("Running ", module_name, "..."), type = "message", duration = NULL, id = "running_message")

    landing_url <- get_landing_url(session)
    if (nzchar(landing_url)) {
      Sys.setenv(LUMENS_LANDING_URL = landing_url)
    }
    Sys.setenv(LUMENS_LAUNCH_BROWSER = "FALSE")
    on.exit(Sys.unsetenv(c("LUMENS_LANDING_URL", "LUMENS_LAUNCH_BROWSER")), add = TRUE)

    rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    null_dev <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
    system2(
      rscript_bin,
      args = shQuote(r_script),
      stdout = null_dev,
      stderr = null_dev,
      stdin = null_dev,
      wait = FALSE
    )

    pending_launch(list(module_name = module_name, started_at = Sys.time()))
  }

  observe({
    launch <- pending_launch()
    req(launch)

    if (is_port_open(MODULE_PORT)) {
      pending_launch(NULL)
      removeNotification("running_message")
      session$sendCustomMessage("lumens_navigate", get_module_url(session, MODULE_PORT))
    } else if (as.numeric(difftime(Sys.time(), launch$started_at, units = "secs")) > 60) {
      pending_launch(NULL)
      removeNotification("running_message")
      showNotification(paste0("Timed out starting ", launch$module_name, "."), type = "error", duration = 6)
    } else {
      invalidateLater(300, session)
    }
  })

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