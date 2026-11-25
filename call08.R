launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
shiny::runApp('08_ta-regional1/rscript/', launch.browser = launch_browser, port = 875)