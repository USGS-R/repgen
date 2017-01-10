parseVDiagramData <- function(data){
  ratingShifts <- fetchRatingShifts(data)
  
  shiftPoints <- ratingShifts$shiftPoints
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- ratingShifts$stagePoints
  validParam(stagePoints, "stagePoints")
  
  shiftId <- ratingShifts$shiftNumber
  validParam(stagePoints, "shiftNumber")
  
  measurements <- fetchMeasurements(data)
  
  maxShift <- measurements$errorMaxShiftInFeet
  validParam(stagePoints, "errorMaxShiftInFeet")
  
  minShift <- measurements$errorMinShiftInFeet
  validParam(stagePoints, "errorMinShiftInFeet")
  
  obsShift <- measurements$shiftInFeet
  validParam(stagePoints, "shiftInFeet")
  
  obsIDs <- measurements$shiftNumber
  validParam(stagePoints, "shiftNumber")
  
  obsGage <- measurements$meanGageHeight
  validParam(stagePoints, "meanGageHeight")

  obsCallOut <- measurements$measurementNumber
  
  
  histFlag <- defaultHistFlags(measurements$historic)
  maxStage <- getMaxStage(data, required = TRUE)
  minStage <- getMinStage(data, required = TRUE)
  numOfShifts <- sizeOf(ratingShifts)
  
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

  if (isEmptyOrBlank(histFlag)){
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