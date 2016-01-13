#'@title Corrections-At-A-Glance report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname correctionsataglance
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'data <- fromJSON(system.file('extdata','correctionsataglance-example2.json', package = 'repgen'))
#'correctionsataglance(data, 'html', 'Author Name')
#'
#'@rdname correctionsataglance
#'@export
setGeneric(name="correctionsataglance",def=function(data, output, ...){standardGeneric("correctionsataglance")})

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startRender(data, output, author, 'correctionsataglance'))
          }
)

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("character", "character"), 
          definition = function(data, output, ...) {
            data <- getJSON(url = data, ...)
            correctionsataglance(data,output)
          }
)

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("list", "missing"), 
          definition = function(data, output, ...) {
            
            correctionsataglanceReport(data)
          }
)