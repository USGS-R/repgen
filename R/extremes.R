
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional arguments (e.g., username & password for authentication)
#'@rdname extremes
#'@importFrom knitr knit
#'@export
setGeneric(name="extremes",def=function(data, output, token){standardGeneric("extremes")})

setMethod("extremes", signature = c("list", "character", "missing"), 
          definition = function(data, output, token) {
            cat('this function does all the work')
            md_file <- 'out.md'
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
            ts <- data
            knit(rmd_file, output = md_file)          
            out_file <- pandoc(md_file, format = output)
            file.remove(md_file)
            return(out_file)
          }
)


#'@importFrom httr GET
setMethod("extremes", signature = c("character", "character", "character"), 
          definition = function(data, output, token) {
            cat('this function uses the token to get data, and then calls the other one\n')
           
            authHeader <- paste0("Bearer " , token)
            ts_list <- getJSON(url = data, auth = authHeader)
            extremes(ts_list,output)
          }
)






