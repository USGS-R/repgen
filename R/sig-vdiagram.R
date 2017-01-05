#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param ... addtional params passed to GET or authenticateUser
#'@rdname vdiagram
#'@import gsplot
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'library(gsplot)
#'Sys.setenv(TZ = "UTC")
#'json_file <- system.file('extdata','vdiagram','vdiagram-example.json', package = 'repgen')
#'data <-fromJSON(json_file)
#'vdiagram(data, 'Author Name')
#'@rdname vdiagram
#'@export
setGeneric(name="vdiagram",def=function(data, ...){standardGeneric("vdiagram")})

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'vdiagram'))
          }
)

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("character"), 
          definition = function(data,...) {
            data <- getJSON(data,...)
            vdiagram(data)
          }
)

