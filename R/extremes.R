# #'@export
# extremes <- function(data, output, auth = NULL){
#   rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
#   
#   url = 'https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/extremes/?station=06899500&dischargeIdentifier=Discharge.TS0058&stageIdentifier=Gage+height.TS0005&dailyDischargeIdentifier=Discharge.TS0058&waterYear=2012'
#   auth = 'Bearer aasdfa-asdfasdf-wdfe'
#   knit('vignettes/extremes.Rmd')
#   # use knit pandoc ...
#   system("pandoc -s extremes.md -o extremes.pdf")
#   system("pandoc -s extremes.md -o extremes.html")
#   
#   return(filename)
# }
#"username" #"password"

# application/x..codeencoded-url

#post to https://internal.cida.usgs.gov/auth-webservice/token
#'@title extremes report
#'@param data
#'@param output
#'@param ... additional arguments (e.g., username & password for authentication)
#'@rdname extremes
#'@export
setGeneric(name="extremes",def=function(data, output, ...){standardGeneric("extremes")})

setMethod("extremes", signature = c("list", "character"), 
          definition = function(data, output) {
            cat('this function does all the work')
            
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
            ts <- data
            knit('vignettes/extremes.Rmd', output = output)
            
            return(output)
          }
)

#'@importFrom httr GET
setMethod("extremes", signature = c("character", "character"), 
          definition = function(data, output,...) {
            cat('this function authenticates, goes out and gets data, and then calls the other one\n')
            
            token <- paste0('Bearer ', authenticateUser(...))
            browser()
            ts_list <- getJSON(url = data, auth = token)
            extremes(ts_list,'output.html')
          }
)