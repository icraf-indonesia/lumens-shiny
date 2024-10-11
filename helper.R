if(rmarkdown::pandoc_available() == FALSE) {
  APP_HOME <- unlist(strsplit(normalizePath(Sys.getenv("R_HOME")), "R-Portable"))[1]
  Sys.setenv(
    RSTUDIO_PANDOC = paste0(
      APP_HOME, 
      "RStudioPortable\\App\\RStudio\\bin\\pandoc"
    )
  )
}

jscode <- "shinyjs.closeWindow = function() { window.close(); }"