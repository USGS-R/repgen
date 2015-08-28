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
    callouts(x=c(0,0),y=c(getMinStage(data, required = TRUE), getMaxStage(data, required = TRUE)), labels="", col = 'red', lwd = 3, angle=0, legend.name="Max and min gage height for the period shown") %>%
    grid(lty = "dotted") %>%
    axis(side=c(2,4)) %>%
    addVdiagErrorBars(x = obsShift, y = obsGage, xError0 = minShift, xError1 = maxShift, histFlag, IDs = obsIDs)
  
  for (i in 1:numShifts(data)) {
    vplot <- addRatingShifts(vplot, shiftPoints[[i]],stagePoints[[i]], ID = shiftId[i], extendStageBy = extendStageBy) #skip black as a color
  }
  
  if (any(!is.na(obsShift)) && any(!histFlag)){
    vplot <- callouts(vplot,x = obsShift[!histFlag], y = obsGage[!histFlag], labels=obsCallOut[!histFlag], cex=0.6)
  }
  par(mar=c(7, 3, 4, 2))
  print(vplot) 
}