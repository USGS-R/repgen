
#'@title extremes report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional arguments (e.g., username & password for authentication)
#'@rdname extremes
#'@importFrom knitr knit
#'@export
setGeneric(name="extremes",def=function(data, output, ...){standardGeneric("extremes")})

setMethod("extremes", signature = c("list", "character"), 
          definition = function(data, output) {
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
setMethod("extremes", signature = c("character", "character"), 
          definition = function(data, output,...) {
            cat('this function authenticates, goes out and gets data, and then calls the other one\n')
            
            token <- paste0('Bearer ', authenticateUser(...))
            ts_list <- getJSON(url = data, auth = token)
            extremes(ts_list,output)
          }
)