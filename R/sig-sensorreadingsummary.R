#'@title sensorreadingsummary report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@rdname sensorreadingsummary
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"sensorreadingsummary-example.json",package = 'repgen'))
#'sensorreadingsummary(data, 'pdf', 'Author Name')
#'sensorreadingsummary(data, 'html', 'Author Name')
#'@rdname sensorreadingsummary
#'@export
setGeneric(name="sensorreadingsummary",def=function(data, output, ...){standardGeneric("sensorreadingsummary")})

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary

setMethod("sensorreadingsummary", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            output_dir <- getwd()
            ts <- data
            author <- list(...)
            rmd_file <- system.file('sensorreadingsummary','sensorreadingsummary.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author),
                               output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary
setMethod("sensorreadingsummary", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            sensorreadingsummary(ts_list,output)
          }
)

#'@aliases sensorreadingsummary
#'@rdname sensorreadingsummary
setMethod("sensorreadingsummary", signature = c("list", "missing"), 
          definition = function(data, output) {
            sensorreadingsummary(data)
          }
)