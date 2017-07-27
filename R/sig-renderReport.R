#' Render Report
#' 
#' @param data raw json data
#' @param reportType type of report, will error if not supported
#' @param author label for person generating report
#' @param ... remaining parameters
#' @rdname renderReport
#' @importFrom whisker whisker.render
#' @examples
#' library(jsonlite)
#' renderReport(
#'   fromJSON(
#'     system.file(
#'       'extdata', 'example-report.json', package = 'repgen'
#'     )
#'   ), 
#' 'examplereport', 
#' 'Author Name')
#' @export
setGeneric(name="renderReport",def=function(data, reportType, author, ...){standardGeneric("renderReport")})

#'@aliases renderReport
#'@rdname renderReport
setMethod("renderReport", signature = c("list", "character", "character"), 
          definition = function(data, reportType, author, ...) {
            class(data) <- reportType #sets the type of report this data represents
            return(startTemplatedRender(data, author))
          }
)
