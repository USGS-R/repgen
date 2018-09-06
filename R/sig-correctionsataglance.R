#' Corrections-at-a-Glance report
#' 
#' @param data Local data (as list), or URL.
#' @param ... Additional parameters passed to GET.
#' @rdname correctionsataglance
#' @importFrom rmarkdown render
#' @examples
#' library(gsplot)
#' library(jsonlite)
#' library(lubridate)
#' library(dplyr)
#' Sys.setenv(TZ = "UTC")
#' 
#' data <-
#'   fromJSON(
#'     system.file(
#'       'extdata', 'correctionsataglance', 'refactored_corr.json',
#'       package = 'repgen'
#'     )
#'   )
#' correctionsataglance(data, 'Author Name')
#' @rdname correctionsataglance
#' @export

setGeneric(name="correctionsataglance",def=function(data, ...){standardGeneric("correctionsataglance")})

#'@aliases correctionsataglance
#'@rdname correctionsataglance
setMethod("correctionsataglance", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'correctionsataglance'))
          }
)