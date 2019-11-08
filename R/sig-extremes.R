#' Extremes report.
#' 
#' @param data Local data (as list), or URL.
#' @param ... Everything else.
#' @rdname extremes
#' @importFrom rmarkdown render
#' @examples
#' library(jsonlite)
#' library(dplyr)
#' library(stringr)
#' data <-
#'   fromJSON(
#'     system.file(
#'       'extdata', 'extremes', 'extremes-example-site-train.json', package = 'repgen'
#'     )
#'   )
#' extremes(data, 'Author Name')
#' @rdname extremes
#' @export
setGeneric(name="extremes",def=function(data, ...){standardGeneric("extremes")})

#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'extremes'))
          }
)
