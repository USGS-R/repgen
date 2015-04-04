
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@rdname extremes
#'@importFrom rmarkdown render
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
#'extremes(data, 'pdf')
#'extremes(data, 'html')
#'@export
setGeneric(name="extremes",def=function(data, output){standardGeneric("extremes")})

setMethod("extremes", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            ts <- data
            rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir)
            return(out_file)
          }
)


setMethod("extremes", signature = c("character", "character"), 
          definition = function(data, output) {
           
            ts_list <- fromJSON(data)
            extremes(ts_list,output)
          }
)

setMethod("extremes", signature = c("list", "missing"), 
          definition = function(data, output) {
            
            ts <- data
            tbl <- extremesTable(ts)
            formTable <- padTable(tbl)
            cat(formTable)
          }
)