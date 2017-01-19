sensorreadingReport <- function(reportObject) {
  
  tbl <- sensorreadingTable(reportObject)
  formTable <- padTable(tbl)
  cat(formTable)
}

