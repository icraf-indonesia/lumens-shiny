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
  module_script <- Sys.getenv("LUMENS_MODULE_SCRIPT", unset = "")
  relaunch_url <- if (nzchar(landing_url) && nzchar(module_script)) {
    sep <- if (grepl("\\?", landing_url)) "&" else "?"
    paste0(landing_url, sep, "relaunch=", utils::URLencode(module_script, reserved = TRUE))
  } else {
    ""
  }
  landing_json <- jsonlite::toJSON(landing_url, auto_unbox = TRUE)
  relaunch_json <- jsonlite::toJSON(relaunch_url, auto_unbox = TRUE)
  leave_action <- if (nzchar(landing_url)) {
    sprintf("window.location.replace(%s);", landing_json)
  } else {
    "window.close();"
  }
  paste0(
    "(function() {\n",
    "  var landingUrl = ", landing_json, ";\n",
    "  var relaunchUrl = ", relaunch_json, ";\n",
    "  if (!window.__lumensLifecycleAdded) {\n",
    "    window.__lumensLifecycleAdded = true;\n",
    "    window.__lumensAllowLeave = false;\n",
    "    window.__lumensUnloading = false;\n",
    "    window.addEventListener('beforeunload', function(e) {\n",
    "      if (window.__lumensAllowLeave) return;\n",
    "      e.preventDefault();\n",
    "      e.returnValue = '';\n",
    "    });\n",
    "    window.addEventListener('pagehide', function() {\n",
    "      window.__lumensUnloading = true;\n",
    "    });\n",
    "    window.setInterval(function() {\n",
    "      if (window.__lumensAllowLeave || window.__lumensUnloading) return;\n",
    "      if (window.Shiny && Shiny.shinyapp && typeof Shiny.shinyapp.isConnected === 'function' && Shiny.shinyapp.isConnected()) {\n",
    "        try { Shiny.setInputValue('__lumens_keepalive', Date.now()); } catch (e) {}\n",
    "      }\n",
    "    }, 25000);\n",
    "    function showDisconnectCard() {\n",
    "      if (window.__lumensAllowLeave || window.__lumensUnloading) return;\n",
    "      if (document.getElementById('lumens-disconnect-card')) return;\n",
    "      var wrap = document.createElement('div');\n",
    "      wrap.id = 'lumens-disconnect-card';\n",
    "      wrap.setAttribute('style', 'position:fixed;inset:0;z-index:999999;display:flex;align-items:center;justify-content:center;pointer-events:auto;font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Arial,sans-serif;');\n",
    "      var card = document.createElement('div');\n",
    "      card.setAttribute('style', 'background:#ffffff;color:#1e293b;border:1px solid #e2e8f0;border-radius:12px;padding:24px 28px;max-width:400px;width:90%;box-shadow:0 12px 32px rgba(0,0,0,0.25);text-align:center;');\n",
    "      var title = document.createElement('h4');\n",
    "      title.textContent = 'Session Disconnected';\n",
    "      title.setAttribute('style', 'margin:0 0 8px 0;font-size:18px;font-weight:600;color:#0f172a;');\n",
    "      var msg = document.createElement('p');\n",
    "      msg.textContent = 'Connection to the module was interrupted. Return to the main menu or reload the module to continue.';\n",
    "      msg.setAttribute('style', 'margin:0 0 18px 0;font-size:13px;line-height:1.5;color:#475569;');\n",
    "      var btns = document.createElement('div');\n",
    "      btns.setAttribute('style', 'display:flex;gap:10px;justify-content:center;flex-wrap:wrap;');\n",
    "      if (landingUrl) {\n",
    "        var backBtn = document.createElement('button');\n",
    "        backBtn.type = 'button';\n",
    "        backBtn.textContent = 'Return to Main Menu';\n",
    "        backBtn.setAttribute('style', 'background:#096456;color:#ffffff;border:none;border-radius:6px;padding:8px 16px;font-size:13px;font-weight:500;cursor:pointer;');\n",
    "        backBtn.onclick = function() { window.__lumensAllowLeave = true; window.location.replace(landingUrl); };\n",
    "        btns.appendChild(backBtn);\n",
    "      }\n",
    "      var reloadBtn = document.createElement('button');\n",
    "      reloadBtn.type = 'button';\n",
    "      reloadBtn.textContent = 'Reload Module';\n",
    "      reloadBtn.setAttribute('style', 'background:#f1f5f9;color:#1e293b;border:1px solid #cbd5e1;border-radius:6px;padding:8px 16px;font-size:13px;font-weight:500;cursor:pointer;');\n",
    "      reloadBtn.onclick = function() {\n",
    "        fetch(window.location.href, { method: 'HEAD', cache: 'no-store' }).then(function() {\n",
    "          window.__lumensAllowLeave = true;\n",
    "          window.location.reload();\n",
    "        }).catch(function() {\n",
    "          if (relaunchUrl) {\n",
    "            window.__lumensAllowLeave = true;\n",
    "            window.location.replace(relaunchUrl);\n",
    "          } else {\n",
    "            msg.textContent = 'The module server stopped while inactive. Please reopen it from LUMENS.';\n",
    "            reloadBtn.style.display = 'none';\n",
    "          }\n",
    "        });\n",
    "      };\n",
    "      btns.appendChild(reloadBtn);\n",
    "      card.appendChild(title);\n",
    "      card.appendChild(msg);\n",
    "      card.appendChild(btns);\n",
    "      wrap.appendChild(card);\n",
    "      document.body.appendChild(wrap);\n",
    "    }\n",
    "    if (window.jQuery) {\n",
    "      jQuery(document).on('shiny:disconnected', function() {\n",
    "        window.setTimeout(showDisconnectCard, 300);\n",
    "      });\n",
    "      jQuery(document).on('shiny:connected', function() {\n",
    "        var el = document.getElementById('lumens-disconnect-card');\n",
    "        if (el) el.remove();\n",
    "      });\n",
    "    }\n",
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
