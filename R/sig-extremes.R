
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... everything else
#'@rdname extremes
#'@importFrom rmarkdown render
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
#'extremes(data, 'html', 'Author Name')
#'@rdname extremes
#'@export
setGeneric(name="extremes",def=function(data, output, ...){standardGeneric("extremes")})

#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            output_dir <- getwd()
            ts <- data
            author <- list(...)
            rmd_file <- system.file('extremes','extremes.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author),
                               output_dir = output_dir)
            return(out_file)
          }
)


#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            extremes(ts_list,output)
          }
)

#'@aliases extremes
#'@rdname extremes
setMethod("extremes", signature = c("list", "missing"), 
          definition = function(data, output) {
            extremesReport(data)
          }
)