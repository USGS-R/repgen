#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param token an auth token (see \code{\link{authenticate_user}})
#'@rdname vdiagram
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'json_file <- system.file('extdata','vdiagram_example.json', package = 'repgen')
#'data <-fromJSON(json_file)
#'vdiagram(data, 'html')
#'vdiagram(data, 'pdf')
#'@export
setGeneric(name="vdiagram",def=function(data, output){standardGeneric("vdiagram")})

setMethod("vdiagram", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('extdata','vdiagram.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir)
            return(out_file)
          }
)


setMethod("vdiagram", signature = c("character", "character"), 
          definition = function(data, output) {
            
            data <- fromJSON(data)
            vdiagram(data,output)
          }
)