#'@title Corrections-At-A-Glance report
#'@param data local data (as list) or URL
#'@param ... additional params passed to GET
#'@rdname correctionsataglance
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'library(dplyr)
#'Sys.setenv(TZ = "UTC")
#'
#'data <-
#'  fromJSON(
#'    system.file(
#'      'extdata', 'correctionsataglance', 'correctionsataglance-example2.json',
#'      package = 'repgen'
#'    )
#'  )
#'correctionsataglance(data, 'Author Name')
#'@rdname correctionsataglance
#'@export

setGeneric(name="correctionsataglance",def=function(data, ...){standardGeneric("correctionsataglance")})

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'correctionsataglance'))
          }
)

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("character"), 
          definition = function(data, ...) {
            data <- getJSON(url = data, ...)
            correctionsataglance(data)
          }
)