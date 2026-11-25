if(rmarkdown::pandoc_available() == FALSE) {
  APP_HOME <- unlist(strsplit(normalizePath(Sys.getenv("R_HOME")), "R-Portable"))[1]
  Sys.setenv(
    RSTUDIO_PANDOC = paste0(
      APP_HOME, 
      "RStudioPortable\\App\\RStudio\\bin\\pandoc"
    )
  )
}

jscode <- local({
  landing_url <- Sys.getenv("LUMENS_LANDING_URL", unset = "")
  if (nzchar(landing_url)) {
    sprintf(
      "shinyjs.closeWindow = function() { window.location.replace(%s); }",
      jsonlite::toJSON(landing_url, auto_unbox = TRUE)
    )
  } else {
    "shinyjs.closeWindow = function() { window.close(); }"
  }
})

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}