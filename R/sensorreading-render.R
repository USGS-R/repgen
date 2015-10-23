sensorreadingReport <- function(data) {
  
  ts <- data
  tbl <- sensorreadingTable(ts)
  formTable <- padTable(tbl)
  cat(formTable)
}

