# Load Pre-QuES analysis functions 
launch_browser <- tolower(Sys.getenv("LUMENS_LAUNCH_BROWSER", unset = "true")) != "false"
shiny::runApp('03_preques/rscript/', launch.browser = launch_browser, port = 875)