
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... everything else
#'@rdname extremes
#'@importFrom rmarkdown render
#'@examples
#'library(jsonlite)
#'library(dplyr)
#'data <- fromJSON(system.file('extdata','extremes','extremes-example-site-train.json',package = 'repgen'))
#'extremes(data, 'html', 'Author Name')
#'@rdname extremes
#'@export
setGeneric(name="extremes",def=function(data, output, ...){standardGeneric("extremes")})

#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startRender(data, output, author, 'extremes'))
          }
)


#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            extremes(ts_list,output)
          }
)

#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("list", "missing"), 
          definition = function(data, output) {
            extremesReport(data)
          }
)