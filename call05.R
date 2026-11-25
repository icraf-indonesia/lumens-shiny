launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
shiny::runApp('05_quesb/rscript/', launch.browser = launch_browser, port = 875)