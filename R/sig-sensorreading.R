#' Sensor Reading Summary Report
#' 
#' @param reportObject Local data (as list), or URL.
#' @param ... everything else
#' @rdname sensorreadingsummary
#' @importFrom rmarkdown render
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @examples
#' library(jsonlite)
#' 
#' reportObject <-
#'  fromJSON(
#'    system.file(
#'      'extdata', 'sensorreadingsummary', "sensorReadingSummary-example.json", package = 'repgen'
#'    )
#'  )
#' sensorreadingsummary(reportObject, 'Author Name')
#' @rdname sensorreadingsummary
#' @export
setGeneric(name="sensorreadingsummary",def=function(reportObject, ...){standardGeneric("sensorreadingsummary")})

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary

setMethod("sensorreadingsummary", signature = c("list"), 
          definition = function(reportObject, ...) {
            author <- list(...)
            return(startRender(reportObject, author, 'sensorreading'))
          }
)