#'@title sitevisitpeak report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... everythingn else
#'@rdname sitevisitpeak
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata',"sitevisitpeak-example.json",package = 'repgen'))
#'sitevisitpeak(data, 'pdf', 'Author Name')
#'sitevisitpeak(data, 'html', 'Author Name')
#'@rdname sitevisitpeak
#'@export
setGeneric(name="sitevisitpeak",def=function(data, output, ...){standardGeneric("sitevisitpeak")})

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak

setMethod("sitevisitpeak", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            output_dir <- getwd()
            ts <- data
            author <- list(...)
            rmd_file <- system.file('sitevisitpeak','sitevisitpeak.Rmd',package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author),
                               output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak
setMethod("sitevisitpeak", signature = c("character", "character"), 
          definition = function(data, output) {
            
            ts_list <- fromJSON(data)
            sitevisitpeak(ts_list,output)
          }
)

#'@aliases sitevisitpeak
#'@rdname sitevisitpeak
setMethod("sitevisitpeak", signature = c("list", "missing"), 
          definition = function(data, output) {
            sitevisitpeakReport(data)
          }
)