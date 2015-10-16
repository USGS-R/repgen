#'@title sensorreading report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@rdname sensorreading
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"sensorreading-example.json",package = 'repgen'))
#'sensorreading(data, 'pdf', 'Author Name')
#'sensorreading(data, 'html', 'Author Name')
#'@rdname sensorreading
#'@export
setGeneric(name="sensorreading",def=function(data, output, ...){standardGeneric("sensorreading")})

#'@aliases sensorreading
#'@rdname sensorreading

setMethod("sensorreading", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            output_dir <- getwd()
            ts <- data
            author <- list(...)
            rmd_file <- system.file('sensorreading','sensorreading.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author),
                               output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases sensorreading
#'@rdname sensorreading
setMethod("sensorreading", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            sensorreading(ts_list,output)
          }
)

#'@aliases sensorreading
#'@rdname sensorreading
setMethod("sensorreading", signature = c("list", "missing"), 
          definition = function(data, output) {
            sensorreadingReport(data)
          }
)