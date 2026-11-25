launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
shiny::runApp('07_ta-profit/rscript/', launch.browser = launch_browser, port = 875)