#' Create a Flat Text, "sensorreading table" Type Output Table
#' 
#' @description Takes a JSON string and extracts and formats readings for the report
#' 
#' @param reportObject A sensorreading report JSON string.
#' 
#' @return a table of data suitable for including in the html report
#' 
sensorreadingTable <- function(reportObject) {
  if (length(reportObject)==0) return ("The dataset requested is empty.")
	
	timezone <- fetchReportMetadataField(reportObject, 'timezone')
  
  includeComments <- isNullOrFalse(fetchRequestParametersField(reportObject, 'excludeComments'))

  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Recorder w/in Uncertainty?", 
                   "Indicated Correction",
                   "Applied Correction",
                   "Corrected w/in Reference?",
                   "Value",
                   "Time",
                   "Qualifier"
  )
  
  #Sends in list of readings, and gets back the formatted data.frame
  results <- formatSensorData(reportObject[["readings"]], columnNames, includeComments, timezone, fetchQualifierMetadata(reportObject))
  
  return(results)
}


#' Creates the formatted data.frame for the report
#' 
#' @description Takes a JSON data string, a list of column names and a flag 
#' for comments and returns formatted data.frame for the report
#' 
#' @param readings Sensor reading report readings JSON string.
#' 
#' @param columnNames list of column names for the report
#' 
#' @param includeComments flag for TRUE or FALSE depending on user selection on 
#' whether they want comments included in the report output
#' 
#' @return data.frame table
#' 
formatSensorData <- function(readings, columnNames, includeComments, timezone, qualifierMetadata){
  if (length(readings)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)

  lastRefComm <- ''
  lastRecComm <- ''
  lastDate <- ''
  
  for(listRows in row.names(readings)){
    listElements <- readings[listRows,]

    displayTime <- flexibleTimeParse(listElements[["displayTime"]], timezone, FALSE, TRUE)
    nearestCorrectedTime <- flexibleTimeParse(listElements[["nearestCorrectedTime"]], timezone, FALSE, TRUE)
    timeFormatted <- timeFormatting(displayTime, "%m/%d/%Y", " ")
    timeFormattedCorrected <- timeFormatting(nearestCorrectedTime, "%m/%d/%Y", " ")

    rec <- getRecorderWithinUncertainty(listElements[["uncertainty"]], listElements[["value"]], listElements[["recorderValue"]])
    ind <- getIndicatedCorrection(listElements[["recorderValue"]], listElements[["value"]])
    app <- getAppliedCorrection(listElements[["nearestRawValue"]], listElements[["nearestCorrectedValue"]])
    corr <- getCorrectedRef(listElements[["value"]], listElements[["nearestCorrectedValue"]], listElements[["uncertainty"]])
    
    qualifiers <- tryCatch({
    	readSRSQualifiers(listElements, timezone, qualifierMetadata)
    }, error=function(e){
    	warning(paste("Returning list() for SRS Qualifiers. Error:", e))
    	return(list())
    })
    
    qual <- formatQualifiersStringList(as.data.frame(qualifiers))

    toAdd = c(timeFormatted[[1]],
              timeFormatted[[2]],
              nullMask(listElements[["party"]]), 
              nullMask(listElements[["sublocation"]]),
              ##
              nullMask(listElements[["monitoringMethod"]]),
              nullMask(listElements[["type"]]),
              nullMask(listElements[["value"]]),
              nullMask(listElements[["uncertainty"]]),
              ##
              nullMask(listElements[["recorderMethod"]]),
              nullMask(listElements[["recorderType"]]),
              nullMask(listElements[["recorderValue"]]),
              nullMask(listElements[["recorderUncertainty"]]),
              ##
              rec, 
              ind, 
              app, 
              corr,
              ##
              nullMask(listElements[["nearestCorrectedValue"]]),
              timeFormattedCorrected[[2]],
              qual
    )
    
    
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
    
    if(includeComments) {
      refComm <- formatComments(nullMask(listElements[["referenceComments"]]))
      recComm <- formatComments(nullMask(listElements[["recorderComments"]]))
      selectedRefComm <- getUniqueComments(refComm, timeFormatted[[1]], lastDate, lastRefComm)
      selectedRecComm <- getUniqueComments(recComm, timeFormatted[[1]], lastDate, lastRecComm)
      
      lastDate = timeFormatted[[1]]
      lastRefComm <- selectedRefComm
      lastRecComm <- selectedRecComm
      
      columnRow = c(
        '', '', '', '',
        ##
        paste(selectedRefComm), '', '', '',
        ##
        paste(selectedRecComm), '', '', '',
        ##
        '', '', '', '',
        ##
        '', '', ''
      )
      toRet <- rbind(toRet, data.frame(t(columnRow),stringsAsFactors = FALSE))
    }
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  return(list(toRet=toRet))
}

#' calculate the recorder w/in uncertainty
#' 
#' @description This takes the uncertainty values, the reading values and the recorderValues
#' and returns a Yes or No repsonse if the reading value is within the uncertainty level. 
#' If the recorderValue is not available, it compares the reading value with the recorderValue
#' and if the values match, returns Yes, if not, No. The uncertainty range is inclusive.
#' 
#' @param uncertainty The value for uncertainty for the reading
#' 
#' @param value The value for the reading
#'
#' @param recorderValue The recorderValue for the reading
#' 
#' @return recorderWithin Yes or No response on whether the recorderValue is within the
#' uncertainty value known
#' 
getRecorderWithinUncertainty <- function(uncertainty, value, recorderValue) {  
  if (!isEmpty(recorderValue) &&
      !isEmpty(uncertainty) && 
      !isEmpty(value)) {
    ref <- as.numeric(value)
    unc <- as.numeric(uncertainty)
    rec <- as.numeric(recorderValue)
    val1 <- round(ref+unc, getSrsPrecision())
    val2 <- round(ref-unc, getSrsPrecision())
    if ((rec <= val1) && (rec >= val2)) {
      recorderWithin <- "Yes"
    } else {
      recorderWithin <- "No"
    }
  } else if (!isEmpty(recorderValue) &&
      !isEmpty(value) &&
      (isEmpty(uncertainty))
      ) { #in this case, check if recorderValue is the same as value
    ref <- round(as.numeric(value), getSrsPrecision())
    rec <- round(as.numeric(recorderValue), getSrsPrecision())
    
    if (rec == ref) {
      recorderWithin <- "Yes"
    } else {
      recorderWithin <- "No"
    }
  } else {
    recorderWithin <- "-"
  }
  return(recorderWithin)
}

#' calculate indicated correction
#' 
#' @description Takes a recorderValue and a reading value and returns the 
#' difference between the recorder value and the reference value.
#' 
#' @param recorderValue The recorderValue from the data
#'
#' @param value The reading value from the data
#' 
#' @return indicatedCorrection A rounded difference value for correction or 
#' an empty character if the recorderValue and reading value are empty/missing
#' 
getIndicatedCorrection <- function(recorderValue, value) {
  if ((!isEmpty(recorderValue)) && (!isEmpty(value))) {
    rec <- as.numeric(recorderValue)
    ref <- as.numeric(value)
    indicatedCorrection <- round(ref - rec, getSrsPrecision())
  } else {
    indicatedCorrection <- ""
  }
  return(indicatedCorrection)
}

#' get applied correction
#' 
#' @description Takes a raw and corrected value and calculates the applied correction
#' if they are not null or empty
#' 
#' @param raw The raw reading value
#' 
#' @param corrected The corrected reading value
#' 
#' @return The rounded difference between the raw and corrected values or 
#' empty character if the raw and corrected value passed in are null or empty
#' 
getAppliedCorrection <- function(raw, corrected) {
  if ((!isEmpty(raw)) && (!isEmpty(corrected))) {
    raw <- as.numeric(raw)
    corrected <- as.numeric(corrected)
    appliedCorrection <- round(corrected-raw, getSrsPrecision())
  } else {
    appliedCorrection <- ""
  }
  return(appliedCorrection)
} 

#' Are the corrected values within the reference values accounting for 
#' their specified uncertainty?
#' 
#' @description This function returns a yes or no when comparing whether the 
#' corrections applied to the recorder reading are within the reference values
#' accounting for their specified uncertainty?
#' 
#' @param value The reading value
#' 
#' @param nearestCorrectedValue The nearest corrected value to the reading value
#'
#' @param uncertainty The uncertainty specified for the reading value
#' 
#' @return Yes or No if the corrections applied are within the reference
#' values for their specified uncertainty
#' 
getCorrectedRef <- function (value, nearestCorrectedValue, uncertainty) {
  if ((!isEmpty(value)) && (!isEmpty(uncertainty)) && (!isEmpty(nearestCorrectedValue))) {
    value <- as.numeric(value) 
    nearest <- as.numeric(nearestCorrectedValue) 
    unc <- as.numeric(uncertainty)
    lower <- round(value-unc, getSrsPrecision()) 
    upper <- round(value+unc, getSrsPrecision()) 
    if ((lower <= nearest) && (upper >= nearest)) { 
      correctedRef <- "Yes"
      }
    else {
      correctedRef <- "No"
    }
  } else {
    correctedRef <- ""
  }
  return(correctedRef)
}

#' Sets a precision value for some known numbers rather
#' than having a hardcode precision number sprinkled out.
#' 
#' @description Provides a precision value which can be used any time we want to 
#' control precision of values
#' 
#' @return The precision identified in the function
#' 
getSrsPrecision <- function() {
  return(2);
}

#' Checks comment to see if it already printed the comment for the same date 
#' otherwise if the same prints empty char
#' 
#' @description Takes the comments and compares the last comment and date to 
#' decide whether or not to print the comments
#' 
#' @param comments a single string of comments
#' 
#' @param date the date for the current comments param formatted mm/dd/yyyy
#' 
#' @param lastDate the last date for which it printed comments to compare
#'
#' @param lastComm the last comment it printed to compare
#' 
#' @return selectedComm which is the new comment to print or empty character
#' if the comments are the same as what it just printed

getUniqueComments <- function(comments, date, lastDate, lastComm) {
  selectedComm <- ''

  #only display comments that haven't already been displayed and are in this same date
  if((date == lastDate && lastComm != comments) || (lastDate != date)) {
    selectedComm <- comments
  }    
  return(selectedComm)
}