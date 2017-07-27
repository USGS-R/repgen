#' V Diagram Report
#' 
#' @param data Local data (as list), or URL.
#' @param ... addtional params passed to GET
#' @rdname vdiagram
#' @import gsplot
#' @importFrom rmarkdown render
#' @importFrom jsonlite fromJSON
#' @examples
#' library(jsonlite)
#' library(gsplot)
#' json_file <- system.file('extdata','vdiagram','vdiagram-example.json', package = 'repgen')
#' data <-fromJSON(json_file)
#' vdiagram(data, 'Author Name')
#' @rdname vdiagram
#' @export
setGeneric(name="vdiagram",def=function(data, ...){standardGeneric("vdiagram")})

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'vdiagram'))
          }
)