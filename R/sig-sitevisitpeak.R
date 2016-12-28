#' @title sitevisitpeak report
#' @param data local data (as list) or URL
#' @param ... everythingn else
#' @rdname sitevisitpeak
#' @importFrom rmarkdown render
#' @importFrom jsonlite fromJSON
#' @examples
#' library(jsonlite)
#' 
#'  data <-
#'   fromJSON(
#'     system.file(
#'       'extdata', 'sitevisitpeak', "sitevisitpeak-example.json", package = 'repgen'
#'     )
#'   )
#' sitevisitpeak(data, 'Author Name')
#' @rdname sitevisitpeak
#' @export
setGeneric(name="sitevisitpeak",def=function(data, ...){standardGeneric("sitevisitpeak")})

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak

setMethod("sitevisitpeak", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'sitevisitpeak'))
          }
)

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak
setMethod("sitevisitpeak", signature = c("character"), 
          definition = function(data) {
            
            ts_list <- fromJSON(data)
            sitevisitpeak(ts_list)
          }
)