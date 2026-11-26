library(shiny)

MODULE_PORT <- 875L

launch_state <- new.env(parent = emptyenv())
launch_state$pending <- NULL
launch_state$lines_read <- 0L

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

lumens_button <- function(id, label, abbr) {
  actionButton(
    id,
    tagList(
      tags$span(class = "btn-title", label),
      tags$span(class = "btn-abbr", abbr)
    )
  )
}

ui <- htmlTemplate("index.html",
                   button01 = lumens_button("buildButton", "Build", "Planning Unit Reconciliation - Build"),
                   button02 = lumens_button("reconcileButton", "Reconcile", "Planning Unit Reconciliation - Reconcile"),
                   button03 = lumens_button("prequesButton", "Pre-QUES", "Preliminary Quantification of Ecosystem Services"),
                   button04 = lumens_button("quescButton", "QUES-C", "Quantification of Environmental Services on Carbon"),
                   button05 = lumens_button("quesbButton", "QUES-B", "Quantification of Environmental Services on Biodiversity"),
                   button06 = lumens_button("queshButton", "QUES-H", "Quantification of Environmental Services on Hydrology"),
                   button07 = lumens_button("taProfitButton", "LU Profitability", "Land Use Profitability Analysis"),
                   button08 = lumens_button("reDescButton", "RE Descriptive", "Regional Economic Descriptive Analysis"),
                   button09 = lumens_button("reProjButton", "RE Projection", "Regional Economic Projection Analysis"),
                   button10 = lumens_button("scenarioBuilderButton", "Scenario Builder", "SCIENDO Scenario Builder"),
                   button11 = lumens_button("trainModelButton", "Train Model", "SCIENDO Train Model"),
                   button12 = lumens_button("simulateBuilderButton", "Simulate", "SCIENDO Simulate"),
                   button13 = lumens_button("lasemButton", "LaSEM", "Land Suitability Evaluation Module")
)

LUMENS_MODULES <- list(
  buildButton           = list(script = "call01.R", name = "PUR Build"),
  reconcileButton       = list(script = "call02.R", name = "PUR Reconcile"),
  prequesButton         = list(script = "call03.R", name = "Pre-QUES"),
  quescButton           = list(script = "call04.R", name = "QUES-C"),
  quesbButton           = list(script = "call05.R", name = "QUES-B"),
  queshButton           = list(script = "call06.R", name = "QUES-H"),
  taProfitButton        = list(script = "call07.R", name = "LU Profitability"),
  reDescButton          = list(script = "call08.R", name = "RE Descriptive"),
  reProjButton          = list(script = "call09.R", name = "RE Projection"),
  scenarioBuilderButton = list(script = "call10.R", name = "Scenario Builder"),
  trainModelButton      = list(script = "call11.R", name = "SCIENDO Train Model"),
  simulateBuilderButton = list(script = "call12.R", name = "SCIENDO Simulate"),
  lasemButton           = list(script = "call13.R", name = "LaSEM")
)

server <- function(input, output, session) {
  session$allowReconnect("force")
  is_polling <- reactiveVal(!is.null(launch_state$pending))

  if (!is.null(launch_state$pending)) {
    showNotification(
      paste0("Running ", launch_state$pending$module_name, "..."),
      type = "message",
      duration = NULL,
      id = "running_message"
    )
  }

  call_lumens_shiny_module <- function(r_script, module_name) {
    if (!is.null(launch_state$pending) || is_port_open(MODULE_PORT)) {
      showNotification("Another module is already starting or running.", type = "warning", duration = 4)
      return(invisible(NULL))
    }

    showNotification(paste0("Running ", module_name, "..."), type = "message", duration = NULL, id = "running_message")

    landing_url <- get_landing_url(session)
    if (nzchar(landing_url)) {
      Sys.setenv(LUMENS_LANDING_URL = landing_url)
    }
    Sys.setenv(
      LUMENS_LAUNCH_BROWSER = "FALSE",
      LUMENS_MODULE_SCRIPT = r_script,
      R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
    )
    on.exit(Sys.unsetenv(c("LUMENS_LANDING_URL", "LUMENS_LAUNCH_BROWSER", "LUMENS_MODULE_SCRIPT", "R_LIBS")), add = TRUE)

    rscript_bin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    null_dev <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
    log_file <- tempfile(fileext = ".log")
    file.create(log_file)
    launch_state$lines_read <- 0L

    message(sprintf("[LUMENS] Launching %s (%s)...", module_name, r_script))
    system2(
      rscript_bin,
      args = shQuote(r_script),
      stdout = log_file,
      stderr = log_file,
      stdin = null_dev,
      wait = FALSE
    )

    launch_state$pending <- list(
      module_name = module_name,
      started_at = Sys.time(),
      log_file = log_file
    )
    is_polling(TRUE)
  }

  flush_launch_log <- function(log_file) {
    if (is.null(log_file) || !file.exists(log_file)) return(invisible(NULL))
    lines <- tryCatch(suppressWarnings(readLines(log_file, warn = FALSE)), error = function(e) character(0))
    n <- length(lines)
    if (n > launch_state$lines_read) {
      cat(paste(lines[(launch_state$lines_read + 1L):n], collapse = "\n"), "\n", sep = "")
      launch_state$lines_read <- n
    }
  }

  observe({
    req(is_polling())
    launch <- launch_state$pending
    if (is.null(launch)) {
      removeNotification("running_message")
      is_polling(FALSE)
      return(invisible(NULL))
    }

    flush_launch_log(launch$log_file)

    if (is_port_open(MODULE_PORT)) {
      launch_state$pending <- NULL
      is_polling(FALSE)
      removeNotification("running_message")
      message(sprintf("[LUMENS] %s ready on port %d.", launch$module_name, MODULE_PORT))
      session$sendCustomMessage("lumens_navigate", get_module_url(session, MODULE_PORT))
    } else if (as.numeric(difftime(Sys.time(), launch$started_at, units = "secs")) > 90) {
      launch_state$pending <- NULL
      is_polling(FALSE)
      removeNotification("running_message")
      showNotification(paste0("Timed out starting ", launch$module_name, "."), type = "error", duration = 6)
    } else {
      invalidateLater(300, session)
    }
  })

  observeEvent(session$clientData$url_search, {
    search <- session$clientData$url_search
    if (!nzchar(search)) return(invisible(NULL))
    relaunch_script <- parseQueryString(search)[["relaunch"]]
    if (is.null(relaunch_script) || !nzchar(relaunch_script)) return(invisible(NULL))

    session$sendCustomMessage("lumens_clean_url", session$clientData$url_pathname)

    matched <- Filter(function(m) identical(m$script, relaunch_script), LUMENS_MODULES)
    if (length(matched) == 1L) {
      mod <- matched[[1L]]
      call_lumens_shiny_module(mod$script, mod$name)
    }
  }, once = TRUE)

  lapply(names(LUMENS_MODULES), function(btn_id) {
    mod <- LUMENS_MODULES[[btn_id]]
    observeEvent(input[[btn_id]], {
      call_lumens_shiny_module(mod$script, mod$name)
    })
  })
}

shinyApp(ui=ui, server=server)