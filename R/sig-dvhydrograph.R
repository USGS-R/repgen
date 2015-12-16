#'@title DV Hydrograph report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... additional params passed to GET or authenticateUser
#'@rdname dvhydrograph
#'@importFrom rmarkdown render
#'@examples
#'library(gsplot)
#'library(jsonlite)
#'data <- fromJSON(system.file('extdata','dvhydro-example.json', package = 'repgen'))
#'dvhydrograph(data, 'html', 'Author Name')
#'\dontrun{
#' url <- paste0('https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swdvhydrograph/',
#' '?station=05421682&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#' 'Gage+height.ft.Work&dailyDischargeIdentifier=Discharge.ft%5E3%2Fs.Mean',
#' '&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2011')
#'
#'# pass in additional params to authenticateUser
#'dvhydrograph(url, 'html', verbose = TRUE, username = 'bbadger', password = '12345')
#'dvhydrograph(url, 'html', 'Author Name')
#'}
#'@rdname dvhydrograph
#'@export
setGeneric(name="dvhydrograph",def=function(data, output, ...){standardGeneric("dvhydrograph")})

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            author <- list(...)
            return(startdvhydrographRender(data, output, author))
          }
)

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("character", "character"), 
          definition = function(data, output, ...) {
            data <- getJSON(url = data, ...)
            dvhydrograph(data,output)
          }
)

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("list", "missing"), 
          definition = function(data, output, ...) {
            
            createDvhydrographPlot(data)
          }
)

