if(rmarkdown::pandoc_available() == FALSE) {
  APP_HOME <- unlist(strsplit(Sys.getenv("R_HOME"), "apps"))[1]
  Sys.setenv(
    RSTUDIO_PANDOC = paste0(
      APP_HOME, 
      "/apps/RStudioPortable/App/RStudio/bin/pandoc"
    )
  )
}

jscode <- "shinyjs.closeWindow = function() { window.close(); }"