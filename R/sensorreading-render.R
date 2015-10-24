sensorreadingReport <- function(data) {
  
  ts <- data
  tbl <- sensorreadingTable(ts)
  formTable <- padTableSRS(tbl)
  cat(formTable)
}

