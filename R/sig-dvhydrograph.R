#' DV hydrograph report.
#' 
#' @param data Local data (as list), or URL.
#' @param ... Additional parameters passed to GET or \code{authenticateUser}.
#' @rdname dvhydrograph
#' @importFrom rmarkdown render
#' @examples
#' library(gsplot)
#' library(jsonlite)
#' library(lubridate)
#' library(dplyr)
#' data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-example.json', package = 'repgen'))
#' dvhydrograph(data, 'Author Name')
#' \dontrun{
#'  url <- paste0('https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swdvhydrograph/',
#'  '?station=05421682&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#'  'Gage+height.ft.Work&dailyDischargeIdentifier=Discharge.ft%5E3%2Fs.Mean',
#'  '&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2011')
#' 
#' # pass in additional params to authenticateUser
#' dvhydrograph(url, verbose = TRUE, username = 'bbadger', password = '12345')
#' dvhydrograph(url, 'Author Name')
#' }
#' @rdname dvhydrograph
#' @export
setGeneric(name="dvhydrograph",def=function(data, ...){standardGeneric("dvhydrograph")})

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("list"), 
          definition = function(data, ...) {
            author <- list(...)
            return(startRender(data, author, 'dvhydrograph'))
          }
)

#'@aliases dvhydrograph
#'@rdname dvhydrograph
setMethod("dvhydrograph", signature = c("character"), 
          definition = function(data, ...) {
            data <- getJSON(url = data, ...)
            dvhydrograph(data)
          }
)

