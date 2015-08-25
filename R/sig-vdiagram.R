#'@title v-diagram report
#'@param data local data (as list) or URL
#'@param output a supported pandoc output format (see \code{system("pandoc -h")} for options)
#'@param ... addtional params passed to GET or authenticateUser
#'@rdname vdiagram
#'@import gsplot
#'@importFrom rmarkdown render
#'@importFrom jsonlite fromJSON
#'@examples
#'library(jsonlite)
#'json_file <- system.file('extdata','vdiagram-v6.json', package = 'repgen')
#'data <-fromJSON(json_file)
#'vdiagram(data, 'html', 'Author Name')
#'vdiagram(data, 'pdf', 'Author Name')
#'\dontrun{
#'url <- paste0('http://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swreviewvdiagram/?',
#' 'station=01350000&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#'  'Gage+height.ft.Work.DD002&dailyDischargeIdentifier=', 
#'  'Discharge.ft%5E3%2Fs.Mean&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2014')
#'vdiagram(data = url, verbose = TRUE) # plot to screen with auth
#'}
#'@rdname vdiagram
#'@export
setGeneric(name="vdiagram",def=function(data, output, ...){standardGeneric("vdiagram")})

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("list", "character"), 
          definition = function(data, output, ...) {
            output_dir <- getwd()
            author <- list(...)
            rmd_file <- pagingVdiagram(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
            out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                               output_dir = output_dir, intermediates_dir=output_dir)
            return(out_file)
          }
)

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("character", "character"), 
          definition = function(data, output,...) {
            data <- getJSON(data,...)
            vdiagram(data,output)
          }
)

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("character", "missing"), 
          definition = function(data, output,...) {
            data <- getJSON(data,...)
            vdiagram(data)
          }
)


#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("list", "missing"), 
          definition = function(data) {
            if (!is.null(data$pages)){
              for (i in 1:length(names(data$pages))){
                pageName <- names(data$pages)[i]
                plotVdiagram(data$pages[[pageName]])
              }
            } else {
              plotVdiagram(data)
            }
            
          })

