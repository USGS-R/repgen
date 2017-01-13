parseVDiagramData <- function(reportObject){
  ratingShifts <- fetchRatingShifts(reportObject)
  
  shiftPoints <- ratingShifts$shiftPoints
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- ratingShifts$stagePoints
  validParam(stagePoints, "stagePoints")
  
  shiftId <- ratingShifts$shiftNumber
  validParam(shiftId, "shiftNumber")
  
  measurements <- fetchMeasurements(reportObject)
  
  maxShift <- measurements$errorMaxShiftInFeet
  validParam(maxShift, "errorMaxShiftInFeet")
  
  minShift <- measurements$errorMinShiftInFeet
  validParam(minShift, "errorMinShiftInFeet")
  
  obsShift <- measurements$shiftInFeet
  validParam(obsShift, "shiftInFeet")
  
  obsIDs <- measurements$shiftNumber
  validParam(obsIDs, "shiftNumber")
  
  obsGage <- measurements$meanGageHeight
  validParam(obsGage, "meanGageHeight")

  obsCallOut <- measurements$measurementNumber
  validParam(obsCallOut, "measurementNumber")
  
  histFlag <- defaultHistFlags(measurements$historic)
  
  maxStage <- fetchMaxStage(reportObject)
  validParam(maxStage, "maxStage")
  
  minStage <- fetchMinStage(reportObject)
  validParam(minStage, "minStage")
  
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

historyMeasurementsLabel <- function(reportObject) {
  label <- ""
  if(!is.null(reportObject[['reportMetadata']][['priorYearsHistoric']]) && reportObject[['reportMetadata']][['priorYearsHistoric']] != "0") {
    label <- paste("Unlabeled blue points are historical measurements from the last ",
      reportObject[['reportMetadata']][['priorYearsHistoric']], " year(s).\n")
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