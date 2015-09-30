#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname uvhydrograph
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'data <- fromJSON(system.file('extdata','uvhydro-example.json', package = 'repgen'))
#'uvhydrograph(data, 'html', 'Author Name')
#'\dontrun{
#' url <- paste0('https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swuvhydrograph/',
#' '?station=05421682&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#' 'Gage+height.ft.Work&dailyDischargeIdentifier=Discharge.ft%5E3%2Fs.Mean',
#' '&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2011')
#'
#'# pass in additional params to authenticateUser
#'uvhydrograph(url, 'html', verbose = TRUE, username = 'bbadger', password = '12345')
#'uvhydrograph(url, 'html', 'Author Name')
#'}
#'@rdname uvhydrograph
#'@export
setGeneric(name="uvhydrograph",def=function(data, output, ...){standardGeneric("uvhydrograph")})

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startUvhydrographRender(data, output, author))
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


