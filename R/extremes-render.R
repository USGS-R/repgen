#' Render Extremes report
#' @description given a report object representing an extremes report, will create an HTML report
#' @param reportObject extremes report json object
extremesReport <- function(reportObject) {
  tbl <- extremesTable(reportObject)
  formTable <- padTable(tbl)
  cat(formTable)
}