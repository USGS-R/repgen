#' Parse Vdiagram Data
#' @description Takes in a V diagram report and returns the necessary data for a V Diagram
#' @param reportObject An R object with the raw data required for a V Diagram
#' @return A list containing the relevant data for a V Diagram
#'
parseVDiagramData <- function(reportObject){
  ratingShifts <- fetchRatingShifts(reportObject)
  
  ### Which of these are required? None of them stop if they're not valid currently.
  ### Alternatively, do we want to assign validParam(blah, blah) to each (i.e. shiftPoints <- validParam(etc,etc))
  ### I assume we do as otherwise the "validParam" statements do nothing.

  shiftPoints <- ratingShifts[["shiftPoints"]]
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- ratingShifts[["stagePoints"]]
  validParam(stagePoints, "stagePoints")
  
  shiftId <- ratingShifts[["shiftNumber"]]
  validParam(shiftId, "shiftNumber")
  
  startTime <- ratingShifts[["applicableStartDateTime"]]
  validParam(startTime, "applicableStartDateTime")
  
  curveNumber <- ratingShifts[["curveNumber"]]
  validParam(curveNumber, "curveNumber")
  
  
  measurements <- fetchMeasurements(reportObject)
  
  maxShift <- measurements[["errorMaxShiftInFeet"]]
  validParam(maxShift, "errorMaxShiftInFeet")
  
  minShift <- measurements[["errorMinShiftInFeet"]]
  validParam(minShift, "errorMinShiftInFeet")
  
  obsShift <- measurements[["shiftInFeet"]]
  validParam(obsShift, "shiftInFeet")
  
  obsIDs <- measurements[["shiftNumber"]]
  validParam(obsIDs, "shiftNumber")
  
  obsGage <- measurements[["meanGageHeight"]]
  validParam(obsGage, "meanGageHeight")

  ### Is this correct to validate this param?
  obsCallOut <- measurements[["measurementNumber"]]
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
    startTime=startTime,
    curveNumber=curveNumber,
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

#' History Measurements Label
#' @description A function that checks to see if there are any historic measurements and, if so, creates a label
#' that describes how long the historic data goes back.
#' @param reportObject The V Diagram report
historyMeasurementsLabel <- function(reportObject) {
  label <- ""
  if(!is.null(reportObject[['reportMetadata']][['priorYearsHistoric']]) && reportObject[['reportMetadata']][['priorYearsHistoric']] > "0") {
    label <- paste("Unlabeled blue points are historical measurements from the last",
      reportObject[['reportMetadata']][['priorYearsHistoric']], "year(s).\n")
  }
}

#' Set default historic flags
#' @description Sets the historic flag to false if " " is passed in.
#' @param histFlag The historic flag field from a V Diagram.
#' @return FALSE if " " is passed in or if the field is empty, otherwise it returns what was passed in. 
defaultHistFlags <- function(histFlag){
  
  if (isEmptyOrBlank(histFlag) || (length(histFlag)==1 && histFlag == " ")){
    histFlag <- FALSE
  }
  
  return(histFlag);
}
