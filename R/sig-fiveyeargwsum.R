#' Five Year Groundwater Summary Report
#' 
#' @param data Local data (as list), or URL.
#' @param ... Additional params passed to GET
#' @rdname fiveyeargwsum
#' @importFrom rmarkdown render
#' @examples
#' library(gsplot)
#' library(jsonlite)
#' library(lubridate)
#' 
#' data <-
#'  fromJSON(
#'    system.file(
#'      'extdata', 'fiveyeargwsum', 'fiveyeargwsum-example.json', package = 'repgen'
#'    )
#'  )
#' fiveyeargwsum(data, 'Author Name')
#' @rdname fiveyeargwsum
#' @export
setGeneric(name="fiveyeargwsum",def=function(data, ...){standardGeneric("fiveyeargwsum")})

#'@aliases fiveyeargwsum
#'@rdname fiveyeargwsum
setMethod("fiveyeargwsum", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'fiveyeargwsum'))
          }
)