extremesReport <- function(data) {
  
  ts <- data
  tbl <- sitevisitpeakTable(ts)
  formTable <- padTable(tbl)
  cat(formTable)
  
}