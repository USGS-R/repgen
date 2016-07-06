#'@title Five-year Groundwater Summary report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname fiveyeargwsum
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'data <- fromJSON(system.file('extdata','fiveyeargwsum','fiveyeargwsum-example.json', package = 'repgen'))
#'fiveyeargwsum(data, 'html', 'Author Name')
#'
#'@rdname fiveyeargwsum
#'@export
setGeneric(name="fiveyeargwsum",def=function(data, output, ...){standardGeneric("fiveyeargwsum")})

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startRender(data, output, author, 'fiveyeargwsum'))
          }
)

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("character", "character"), 
          definition = function(data, output, ...) {
            data <- getJSON(url = data, ...)
            fiveyeargwsum(data,output)
          }
)

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("list", "missing"), 
          definition = function(data, output, ...) {
            
            fiveyeargwsumPlot(data)
          }
)