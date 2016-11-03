parseVDiagramData <- function(data){
  shiftPoints <- getRatingShifts(data, 'shiftPoints', required = TRUE)
  stagePoints <- getRatingShifts(data, 'stagePoints', required = TRUE)
  shiftId <- getRatingShifts(data, 'shiftNumber', required = TRUE)
  maxShift <- getMeasurements(data, 'errorMaxShiftInFeet', as.numeric = TRUE)
  minShift <- getMeasurements(data, 'errorMinShiftInFeet', as.numeric = TRUE)
  obsShift <- getMeasurements(data, 'shiftInFeet', as.numeric = TRUE)
  obsIDs <- getMeasurements(data, 'shiftNumber', as.numeric = TRUE)
  obsGage <- getMeasurements(data, 'meanGageHeight', as.numeric = TRUE)
  obsCallOut <- getMeasurements(data, 'measurementNumber')
  histFlag <- defaultHistFlags(getMeasurements(data, 'historic'))
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

historyMeasurementsLabel <- function(data) {
  label <- ""
  if(!is.null(data[['reportMetadata']][['priorYearsHistoric']]) && data[['reportMetadata']][['priorYearsHistoric']] != "0") {
    label <- paste("Unlabeled blue points are historical measurements from the last ",
      data[['reportMetadata']][['priorYearsHistoric']], " year(s).\n")
  }
}

defaultHistFlags <- function(histFlag){
  x <- NULL
  if (length(histFlag)==1 && histFlag == " "){
    histFlag <- rep(TRUE, length(x))
  }
  if (histFlag==""){
    histFlag <- FALSE
  }
  return(histFlag);
}

percentError <- function(MeasurementGrade) {
  percents = rep(0, length(MeasurementGrade))
  percents[grep("fair", MeasurementGrade)] = 0.08
  # other options to be added
  return(percents)
}