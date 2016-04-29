#'@title sitevisitpeak report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... everythingn else
#'@rdname sitevisitpeak
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata','sitevisitpeak',"sitevisitpeak-example.json",package = 'repgen'))
#'sitevisitpeak(data, 'html', 'Author Name')
#'@rdname sitevisitpeak
#'@export
setGeneric(name="sitevisitpeak",def=function(data, output, ...){standardGeneric("sitevisitpeak")})

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak

setMethod("sitevisitpeak", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startRender(data, output, author, 'sitevisitpeak'))
          }
)

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak
setMethod("sitevisitpeak", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            sitevisitpeak(ts_list,output)
          }
)

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak
setMethod("sitevisitpeak", signature = c("list", "missing"), 
          definition = function(data, output) {
            sitevisitpeakReport(data)
          }
)