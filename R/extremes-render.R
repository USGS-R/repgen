extremesReport <- function(reportObject) {
  
  tbl <- extremesTable(reportObject)
  formTable <- padTable(tbl)
  cat(formTable)

}