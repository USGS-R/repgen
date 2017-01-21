sitevisitpeakReport <- function(reportObject) {
  
  tbl <- sitevisitpeakTable(reportObject)
  formTable <- padTable(tbl)
  cat(formTable)
  
}

