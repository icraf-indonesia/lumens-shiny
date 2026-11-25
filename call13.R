launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
shiny::runApp('12_lasem/rscript/', launch.browser = launch_browser, port = 875)