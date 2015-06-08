#'@title rating-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname rating
#'@importFrom rmarkdown render
#'
#'@export
setGeneric(name="rating",def=function(data, output, ...){standardGeneric("rating")})

#'@aliases rating
#'@rdname rating
setMethod("rating", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('rating','rating.Rmd', package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases rating
#'@rdname rating
setMethod("rating", signature = c("character", "character"), 
          definition = function(data, output, ...) {
            
            data <- getJSON(url = data, ...)
            rating(data,output)
          }
)

#'@aliases rating
#'@rdname rating
setMethod("rating", signature = c("list", "missing"), 
          definition = function(data, output, ...) {
            
            ratingPlot(data)
          }
)
