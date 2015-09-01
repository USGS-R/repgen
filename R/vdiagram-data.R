parseVDiagramData <- function(data){
  shiftPoints <- getRatingShifts(data, 'shiftPoints', required = TRUE)
  stagePoints <- getRatingShifts(data, 'stagePoints', required = TRUE)
  shiftId <- getRatingShifts(data, 'shiftNumber', required = TRUE)
  maxShift <- getErrorBars(data, 'errorMaxShiftInFeet', as.numeric = TRUE)
  minShift <- getErrorBars(data, 'errorMinShiftInFeet', as.numeric = TRUE)
  obsShift <- getErrorBars(data, 'shiftInFeet', as.numeric = TRUE)
  obsIDs <- getErrorBars(data, 'shiftNumber', as.numeric = TRUE)
  obsGage <- getErrorBars(data, 'meanGageHeight', as.numeric = TRUE)
  obsCallOut <- getErrorBars(data, 'measurementNumber')
  histFlag <- defaultHistFlags(getErrorBars(data, 'historic'))
  maxStage <- getMaxStage(data, required = TRUE)
  minStage <- getMinStage(data, required = TRUE)
  numOfShifts <- numShifts(data)
  
  return(list(
    shiftPoints=shiftPoints, 
    stagePoints=stagePoints, 
    shiftId=shiftId, 
    maxShift=maxShift, 
    minShift=minShift, 
    obsShift=obsShift, 
    obsIDs=obsIDs, 
    obsGage=obsGage, 
    obsCallOut=obsCallOut, 
    histFlag=histFlag, 
    maxStage=maxStage, 
    numOfShifts=numOfShifts,
    minStage=minStage))
}

defaultHistFlags <- function(histFlag){
  if (length(histFlag)==1 && histFlag == " "){
    histFlag <- rep(TRUE, length(x))
  }
  
  return(histFlag);
}