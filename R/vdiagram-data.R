#' Parse Vdiagram Data
#' @description Takes in a V diagram report and returns the necessary data for a V Diagram
#' @param reportObject An R object with the raw data required for a V Diagram
#' @return A list containing the relevant data for a V Diagram
#'
parseVDiagramData <- function(reportObject){
  
  ### Which of these are required? None of them stop if they're not valid currently.
  ### Alternatively, do we want to assign validParam(blah, blah) to each (i.e. shiftPoints <- validParam(etc,etc))
  ### I assume we do as otherwise the "validParam" statements do nothing.

  shiftPoints <- fetchRatingShiftsField(reportObject, "shiftPoints")
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- fetchRatingShiftsField(reportObject, "stagePoints")
  validParam(stagePoints, "stagePoints")
  
  shiftId <- fetchRatingShiftsField(reportObject, "shiftNumber")
  validParam(shiftId, "shiftNumber")
  
  startTime <- fetchRatingShiftsField(reportObject, "applicableStartDateTime")
  validParam(startTime, "applicableStartDateTime")
  
  rating <- fetchRatingShiftsField(reportObject, "curveNumber")
  validParam(rating, "curveNumber")
  
  
  maxShift <- fetchMeasurementsField(reportObject, "errorMaxShiftInFeet")
  validParam(maxShift, "errorMaxShiftInFeet")
  
  minShift <- fetchMeasurementsField(reportObject, "errorMinShiftInFeet")
  validParam(minShift, "errorMinShiftInFeet")
  
  obsShift <- fetchMeasurementsField(reportObject, "shiftInFeet")
  validParam(obsShift, "shiftInFeet")
  
  obsIDs <- fetchMeasurementsField(reportObject, "shiftNumber")
  validParam(obsIDs, "shiftNumber")
  
  obsGage <- fetchMeasurementsField(reportObject, "meanGageHeight")
  validParam(obsGage, "meanGageHeight")

  ### Is this correct to validate this param?
  obsCallOut <- fetchMeasurementsField(reportObject, "measurementNumber")
  validParam(obsCallOut, "measurementNumber")
  
  histFlag <- defaultHistFlags(fetchMeasurementsField(reportObject,"historic"))
  
  maxStage <- fetchMaxStage(reportObject)
  validParam(maxStage, "maxStage")
  
  minStage <- fetchMinStage(reportObject)
  validParam(minStage, "minStage")
  
  ratingShifts <- fetchRatingShifts(reportObject)
  
  numOfShifts <- ifelse(!isEmptyOrBlank(ratingShifts), sizeOf(ratingShifts), 0)
  
  return(list(
    shiftPoints=shiftPoints, 
    stagePoints=stagePoints, 
    shiftId=shiftId, 
    startTime=startTime,
    rating=rating,
    maxShift=maxShift, 
    minShift=minShift, 
    obsShift=obsShift, 
    obsIDs=obsIDs, 
    obsGage=obsGage, 
    obsCallOut=obsCallOut, 
    histFlag=histFlag, 
    numOfShifts=numOfShifts,
    maxStage=maxStage,
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
