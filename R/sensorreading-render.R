sensorreadingReport <- function(reportObject) {
  
  tbl <- sensorreadingTable(reportObject)
  formTable <- padTableSRS(tbl)
  cat(formTable)
}

