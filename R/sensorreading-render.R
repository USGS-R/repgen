sensorreadingReport <- function(reportObject) {
  
  ts <- reportObject
  tbl <- sensorreadingTable(ts)
  formTable <- padTableSRS(tbl)
  cat(formTable)
}

