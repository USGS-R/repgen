#'@title Five-year Groundwater Summary report
#'@param data local data (as list) or URL
#'@param ... additional params passed to GET or authenticateUser
#'@rdname fiveyeargwsum
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'data <- fromJSON(system.file('extdata','fiveyeargwsum','fiveyeargwsum-example.json', package = 'repgen'))
#'fiveyeargwsum(data, 'Author Name')
#'
#'@rdname fiveyeargwsum
#'@export
setGeneric(name="fiveyeargwsum",def=function(data, ...){standardGeneric("fiveyeargwsum")})

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'fiveyeargwsum'))
          }
)

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("character"), 
          definition = function(data, ...) {
            data <- getJSON(url = data, ...)
            fiveyeargwsum(dat)
          }
)