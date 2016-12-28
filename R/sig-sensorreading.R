#' @title sensorreadingsummary report
#' @param data local data (as list) or URL
#' @param ... everything else
#' @rdname sensorreadingsummary
#' @importFrom rmarkdown render
#' @importFrom jsonlite fromJSON
#' @examples
#' library(jsonlite)
#' 
#' data <-
#'  fromJSON(
#'    system.file(
#'      'extdata', 'sensorreadingsummary', "sensorReadingSummary-example.json", package = 'repgen'
#'    )
#'  )
#' sensorreadingsummary(data, 'Author Name')
#' @rdname sensorreadingsummary
#' @export
setGeneric(name="sensorreadingsummary",def=function(data, ...){standardGeneric("sensorreadingsummary")})

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary

setMethod("sensorreadingsummary", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'sensorreading'))
          }
)

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary
setMethod("sensorreadingsummary", signature = c("character"), 
          definition = function(data) {
            
            ts_list <- fromJSON(data)
            sensorreadingsummary(ts_list)
          }
)