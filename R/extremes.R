
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param token an auth token (see \code{\link{authenticate_user}})
#'@rdname extremes
#'@importFrom knitr knit pandoc
#'@export
setGeneric(name="extremes",def=function(data, output, token){standardGeneric("extremes")})

setMethod("extremes", signature = c("list", "character", "missing"), 
          definition = function(data, output, token) {
            output_dir <- getwd()
            ts <- data
            rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir)
            return(out_file)
          }
)


setMethod("extremes", signature = c("character", "character", "character"), 
          definition = function(data, output, token) {
           
            authHeader <- paste0("Bearer " , token)
            ts_list <- getJSON(url = data, auth = authHeader)
            extremes(ts_list,output)
          }
)



