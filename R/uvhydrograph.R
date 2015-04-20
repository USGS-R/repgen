#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@rdname uvhydrograph
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'#json_file <- system.file('extdata','uvhydrograph-example.json', package = 'repgen')
#'#data <-fromJSON(json_file)
#'uvhydrograph(output = 'html')
#'uvhydrograph(output = 'pdf')
#'@rdname uvhydrograph
#'@export
setGeneric(name="uvhydrograph",def=function(data, output){standardGeneric("uvhydrograph")})

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('extdata', 'uvhydrograph.Rmd', package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir)
            return(out_file)
          }
)

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("character", "character"), 
          definition = function(data, output) {
            
            data <- fromJSON(data)
            uvhydrograph(data,output)
          }
)

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("missing", "character"), 
          definition = function(data, output) {
            siteNumber <- '04085427'
            endDate <- '2012-03-31'
            startDate <- '2012-03-01'
            discharge <- dataRetrieval::readNWISdv(siteNumber, parameterCd = "00060", startDate, endDate,
                               statCd = "00003")
            
            discharge <- discharge[, 5]
          
            gage <- log(discharge) # fake gage
            data <- list(discharge = discharge, gage = gage)
            uvhydrograph(data,output)
          }
)
