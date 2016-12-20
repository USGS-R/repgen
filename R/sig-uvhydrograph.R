#'@title UV Hydrograph report
#'@param data local data (as list) or URL
#'@param ... additional params passed to GET or authenticateUser
#'@rdname uvhydrograph
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'library(lubridate)
#'library(dplyr)
#'gwData <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-groundwater.json', package = 'repgen'))
#'uvhydrograph(gwData, 'Author Name')
#'\dontrun{
#' url <- paste0('https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swuvhydrograph/',
#' '?station=05421682&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#' 'Gage+height.ft.Work&dailyDischargeIdentifier=Discharge.ft%5E3%2Fs.Mean',
#' '&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2011')
#'
#'# pass in additional params to authenticateUser
#'uvhydrograph(url, verbose = TRUE, username = 'bbadger', password = '12345')
#'uvhydrograph(url, 'Author Name')
#'}
#'@rdname uvhydrograph
#'@export
setGeneric(name="uvhydrograph",def=function(data, ...){standardGeneric("uvhydrograph")})

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'uvhydrograph'))
          }
)

#'@aliases uvhydrograph
#'@rdname uvhydrograph
setMethod("uvhydrograph", signature = c("character"), 
          definition = function(data, ...) {
            data <- getJSON(url = data, ...)
            uvhydrograph(data)
          }
)


