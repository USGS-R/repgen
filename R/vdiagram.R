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
#'vdiagram(data, 'html')
#'vdiagram(data, 'pdf')
#'\dontrun{
#'url <- paste0('http://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/swreviewvdiagram/?',
#' 'station=01350000&dischargeIdentifier=Discharge.ft%5E3%2Fs&stageIdentifier=',
#'  'Gage+height.ft.Work.DD002&dailyDischargeIdentifier=', 
#'  'Discharge.ft%5E3%2Fs.Mean&ratingModelIdentifier=Gage+height-Discharge.STGQ&waterYear=2014')
#'vdiagram(data = url, verbose = TRUE) # plot to screen with auth
#'}
#'@rdname vdiagram
#'@export
setGeneric(name="vdiagram",def=function(data, output,...){standardGeneric("vdiagram")})

#'@aliases vdiagram
#'@rdname vdiagram
setMethod("vdiagram", signature = c("list", "character"), 
          definition = function(data, output) {
            output_dir <- getwd()
            # elements of data are now in memory, will be used to knit w/ report
            rmd_file <- pagingVdiagram(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
            out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir, 
                               intermediates_dir=output_dir)
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

plotVdiagram <- function(data){
  extendStageBy = 0.5
  shiftPoints <- getRatingShifts(data, 'shiftPoints', required = TRUE)
  stagePoints <- getRatingShifts(data, 'stagePoints', required = TRUE)
  shiftId <- getRatingShifts(data, 'shiftNumber', required = TRUE)
  maxShift <- getErrorBars(data, 'errorMaxShiftInFeet', as.numeric = TRUE)
  minShift <- getErrorBars(data, 'errorMinShiftInFeet', as.numeric = TRUE)
  obsShift <- getErrorBars(data, 'shiftInFeet', as.numeric = TRUE)
  obsIDs <- getErrorBars(data, 'shiftNumber', as.numeric = TRUE)
  obsGage <- getErrorBars(data, 'meanGageHeight', as.numeric = TRUE)
  obsCallOut <- getErrorBars(data, 'measurementNumber')
  histFlag <- getErrorBars(data, 'historic')
  
  vplot <- gsplot() %>%
    points(NA,NA, ylab='Stage, in feet', xlab='Shift, in feet') %>%
    callouts(x=c(0,0),y=c(getMinStage(data, required = TRUE), getMaxStage(data, required = TRUE)), col = 'red', lwd = 3, angle=0) %>%
    grid(lty = "dotted") %>%
    addVdiagErrorBars(x = obsShift, y = obsGage, xError0 = minShift, xError1 = maxShift, histFlag, IDs = obsIDs)

  
  for (i in 1:numShifts(data)) {
    vplot <- addRatingShifts(vplot, shiftPoints[[i]],stagePoints[[i]], ID = shiftId[i], extendStageBy = extendStageBy) #skip black as a color
  }

  if (any(!is.na(obsShift)) && any(!histFlag)){
    vplot <- callouts(vplot,x = obsShift[!histFlag], y = obsGage[!histFlag], labels=obsCallOut[!histFlag], cex=0.6)
  }
  
  print(vplot) 
}