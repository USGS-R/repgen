sitevisitpeakReport <- function(data) {
  
  ts <- data
  tbl <- sitevisitpeakTable(ts)
  formTable <- padTableSVP(tbl)
  cat(formTable)
  
}

