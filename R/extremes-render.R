extremesReport <- function(data) {
  
  ts <- data
  tbl <- extremesTable(ts)
  formTable <- padTable(tbl)
  cat(formTable)

}