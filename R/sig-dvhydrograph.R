#'@title DV Hydrograph report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname dvhydrograph
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'library(dplyr)
#'Sys.setenv(TZ = "UTC")
#'data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-example.json', package = 'repgen'))
#'dvhydrograph(data, 'Author Name')
#'@rdname dvhydrograph
#'@export
setGeneric(name="dvhydrograph",def=function(data, ...){standardGeneric("dvhydrograph")})

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'dvhydrograph'))
          }
)

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("character"), 
          definition = function(data, ...) {
            data <- getJSON(url = data, ...)
            dvhydrograph(data)
          }
)

