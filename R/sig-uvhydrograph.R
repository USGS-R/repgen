#' UV Hydrograph Report
#' 
#' @param data Local data (as list), or URL.
#' @param ... Additional parameters passed to GET.
#' @rdname uvhydrograph
#' @importFrom rmarkdown render
#' @examples
#' library(gsplot)
#' library(jsonlite)
#' library(lubridate)
#' library(dplyr)
#'
#' gwData <-
#'   fromJSON(
#'     system.file(
#'       'extdata', 'uvhydrograph', 'uvhydro-groundwater.json', package = 'repgen'
#'     )
#'   )
#'  uvhydrograph(gwData, 'Author Name')
#' @rdname uvhydrograph
#' @export
setGeneric(name="uvhydrograph",def=function(data, ...){standardGeneric("uvhydrograph")})

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'uvhydrograph'))
          }
)