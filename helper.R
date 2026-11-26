if(rmarkdown::pandoc_available() == FALSE) {
  APP_HOME <- unlist(strsplit(normalizePath(Sys.getenv("R_HOME")), "R-Portable"))[1]
  Sys.setenv(
    RSTUDIO_PANDOC = paste0(
      APP_HOME, 
      "RStudioPortable\\App\\RStudio\\bin\\pandoc"
    )
  )
}

if (!exists(".lumens_state", envir = globalenv(), inherits = FALSE)) {
  .lumens_state <- new.env(parent = emptyenv())
  .lumens_state$active_sessions <- 0L
  .lumens_state$stop_token <- 0L
  .lumens_state$explicit_close <- FALSE
  assign(".lumens_state", .lumens_state, envir = globalenv())
} else {
  .lumens_state <- get(".lumens_state", envir = globalenv(), inherits = FALSE)
}

jscode <- local({
  landing_url <- Sys.getenv("LUMENS_LANDING_URL", unset = "")
  leave_action <- if (nzchar(landing_url)) {
    sprintf("window.location.replace(%s);", jsonlite::toJSON(landing_url, auto_unbox = TRUE))
  } else {
    "window.close();"
  }
  paste0(
    "(function() {\n",
    "  if (!window.__lumensBeforeUnloadAdded) {\n",
    "    window.__lumensBeforeUnloadAdded = true;\n",
    "    window.__lumensAllowLeave = false;\n",
    "    window.addEventListener('beforeunload', function(e) {\n",
    "      if (window.__lumensAllowLeave) return;\n",
    "      e.preventDefault();\n",
    "      e.returnValue = '';\n",
    "    });\n",
    "  }\n",
    "})();\n",
    "shinyjs.closeWindow = function() {\n",
    "  window.__lumensAllowLeave = true;\n",
    "  if (window.Shiny) {\n",
    "    try { Shiny.setInputValue('__lumens_explicit_close', Date.now()); } catch (e) {}\n",
    "  }\n",
    "  setTimeout(function() { ", leave_action, " }, 60);\n",
    "}"
  )
})

# Intercept unqualified stopApp() calls in modules so a browser refresh has a
# short grace period to reconnect before shutting down the Shiny server.
stopApp <- function(returnValue = invisible()) {
  if (.lumens_state$active_sessions > 0L) {
    return(invisible(returnValue))
  }
  token <- .lumens_state$stop_token + 1L
  .lumens_state$stop_token <- token
  delay_secs <- if (isTRUE(.lumens_state$explicit_close)) 0.25 else 2.5
  later::later(function() {
    if (identical(.lumens_state$stop_token, token) && .lumens_state$active_sessions <= 0L) {
      shiny::stopApp(returnValue = returnValue)
    }
  }, delay = delay_secs)
  invisible(returnValue)
}
assign("stopApp", stopApp, envir = globalenv())

run_lumens_module <- function(app_dir, port = 875L) {
  launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
  app_obj <- shiny::shinyAppDir(app_dir)
  orig_server_source <- app_obj$serverFuncSource
  app_obj$serverFuncSource <- function() {
    orig_server <- orig_server_source()
    function(input, output, session) {
      .lumens_state$active_sessions <- .lumens_state$active_sessions + 1L
      .lumens_state$stop_token <- .lumens_state$stop_token + 1L
      .lumens_state$explicit_close <- FALSE

      shiny::observeEvent(input[["__lumens_explicit_close"]], {
        .lumens_state$explicit_close <- TRUE
      }, ignoreInit = TRUE)

      session$onSessionEnded(function() {
        .lumens_state$active_sessions <- max(0L, .lumens_state$active_sessions - 1L)
      })

      if (length(formals(orig_server)) >= 3L) {
        orig_server(input, output, session)
      } else {
        orig_server(input, output)
      }
    }
  }
  shiny::runApp(app_obj, launch.browser = launch_browser, port = port)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
