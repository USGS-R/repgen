#'@title sensorreadingsummary report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... everything else
#'@rdname sensorreadingsummary
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"sensorReadingSummary-example.json",package = 'repgen'))
#'sensorreadingsummary(data, 'html', 'Author Name')
#'@rdname sensorreadingsummary
#'@export
setGeneric(name="sensorreadingsummary",def=function(data, output, ...){standardGeneric("sensorreadingsummary")})

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary

setMethod("sensorreadingsummary", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startRender(data, output, author, 'sensorreading'))
          }
)

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary
setMethod("sensorreadingsummary", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            sensorreadingsummary(ts_list, output)
          }
)

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary
setMethod("sensorreadingsummary", signature = c("list", "missing"), 
          definition = function(data, output) {
            sensorreadingReport(data)
          }
)