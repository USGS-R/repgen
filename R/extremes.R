
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param token an auth token (see \code{\link{authenticate_user}})
#'@rdname extremes
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'data <- fromJSON(system.file('extdata',"06899500_2012_TS.json",package = 'repgen'))
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



