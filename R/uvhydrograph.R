#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname uvhydrograph
#'@importFrom rmarkdown render
#'@examples
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata','uvhydro-example.json', package = 'repgen'))
#'uvhydrograph(data)
#'uvhydrograph(data, 'html')
#'\dontrun{
#' url <- paste0('https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swuvhydrograph/',
#' '?station=05421682&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#' 'Gage+height.ft.Work&dailyDischargeIdentifier=Discharge.ft%5E3%2Fs.Mean',
#' '&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2011')
#'
#'# pass in additional params to authenticateUser
#'uvhydrograph(url, 'html', verbose = TRUE, username = 'bbadger', password = '12345')
#'uvhydrograph(url, 'html')
#'}
#'@rdname uvhydrograph
#'@export
setGeneric(name="uvhydrograph",def=function(data, output, ...){standardGeneric("uvhydrograph")})

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('uvhydrograph','uvhydrograph.Rmd', package = 'repgen')
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("character", "character"), 
          definition = function(data, output, ...) {
            
            data <- getJSON(url = data, ...)
            uvhydrograph(data,output)
          }
)

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list", "missing"), 
          definition = function(data, output, ...) {
            
            uvhydrographPlot(data)
          }
)

