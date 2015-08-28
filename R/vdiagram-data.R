parseVDiagramData <- function(data){
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
  maxStage <- getMaxStage(data, required = TRUE)
  
  return(data.frame(
    extendStageBy=extendStageBy, 
    shiftPoints=shiftPoints, 
    shiftId=shiftId, 
    maxShift=maxShift, 
    minShift=minShift, 
    obsShift=obsShift, 
    obsIDs=obsIDs, 
    obsGage=obsGage, 
    obsCallOut=obsCallOut, 
    histFlag=histFlag, 
    maxStage=maxStage, 
    stringsAsFactors = FALSE))
}