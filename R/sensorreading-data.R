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
  
  includeComments <- isNullOrFalse(reportObject[['reportMetadata']][['excludeComments']])
  
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
  results <- formatSensorData(reportObject[["readings"]], columnNames, includeComments)
  
  return(results)
}


#' Creates the formatted data.frame for the report
#' 
#' @description Takes a JSON data string, a list of column names and a flag 
#' for comments and returns formatted data.frame for the report
#' 
#' @param reportObject Sensor reading report readings JSON string.
#' 
#' @param columnNames list of column names for the report
#' 
#' @param includeComments flag for TRUE or FALSE depending on user selection on 
#' whether they want comments included in the report output
#' 
#' @return data.frame table
#' 
formatSensorData <- function(reportObject, columnNames, includeComments){
  if (length(reportObject)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  
  lastRefComm <- ''
  lastRecComm <- ''
  lastDate <- ''
  
  for(listRows in row.names(reportObject)){
    listElements <- reportObject[listRows,]
    
    if ("displayTime" %in% names(reportObject)) {
      if(!is.na(listElements[["displayTime"]]) || is.null(listElements[["time"]])) {
        tf <- timeFormatting(listElements[["displayTime"]],"%m/%d/%Y")
        # get just the time part of the list
        timeFormatted <- tf[[2]]
        # get just the date part of the list
        date <- tf[[1]]
        } else {
          timeFormatted <- ""
        }
    }
    #Get the time out of the nearest corrected iv time, don't need the date
    if ("nearestcorrectedTime" %in% names(reportObject)) {
      if (!isEmpty(listElements[["nearestcorrectedTime"]])) {
        tfc <- timeFormatting(listElements[["nearestcorrectedTime"]],"%m/%d/%Y")
        # get just the time part of the list
        timeFormattedCorrected <- tfc[[2]]
      }
    } else {
      timeFormattedCorrected <- ""
    }
    
    rec <- getRecorderWithinUncertainty(listElements[["uncertainty"]], listElements[["value"]], listElements[["recorderValue"]])
    ind <- getIndicatedCorrection(listElements[["recorderValue"]], listElements[["value"]])
    app <- getAppliedCorrection(listElements[["nearestrawValue"]], listElements[["nearestcorrectedValue"]])
    corr <- getCorrectedRef(listElements[["value"]], listElements[["nearestcorrectedValue"]], listElements[["uncertainty"]])
    qual <- getSRSQualifiers(listElements[["qualifiers"]])

    toAdd = c(date,
              timeFormatted,
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
              nullMask(listElements[["nearestcorrectedValue"]]),
              timeFormattedCorrected,
              qual
    )
    
    
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
    
    if(includeComments) {
      #insert column row
      #THIS IS HTML ONLY, YUGE HACK
      refComm <- formatComments(getComments(listElements[["referenceComments"]]))
      recComm <- formatComments(getComments(listElements[["recorderComments"]]))
      selectedRefComm <- ''
      selectedRecComm <- ''
      
      #only display comments that haven't already been displayed and are in this same date
      if((date == lastDate && lastRefComm != refComm) || (lastDate != date)) {
        selectedRefComm <- refComm
        lastRefComm <- selectedRefComm
      }    
      
      if((date == lastDate && lastRecComm != recComm) || (lastDate != date)) {
        selectedRecComm <- recComm
        lastRecComm <- selectedRecComm
      }
      
      lastDate = date
      
      columnRow = c(
        '', '', '', '',
        ##
        paste("<div class='floating-comment'>", selectedRefComm, "</div>"), '', '', '',
        ##
        paste("<div class='floating-comment'>", selectedRecComm, "</div>"), '', '', '',
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
#' @return The rounded difference between the raw and corrected values
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
getCorrectedRef <- function (value, nearestcorrectedValue, uncertainty) {
  if ((!isEmpty(value)) && (!isEmpty(uncertainty)) && (!isEmpty(nearestcorrectedValue))) {
    value <- as.numeric(value) 
    nearest <- as.numeric(nearestcorrectedValue) 
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


#' Get the qualifiers for use in the main table
#' 
#' @description takes the qualifiers and parses them to format for use in the table
#' 
#' @param inQualifiers The qualifiers coming in from the JSON data string
#' 
#' @return Formatted string of qualifiers 
#' 
getSRSQualifiers <- function(inQualifiers) {
  if(length(inQualifiers) < 1) return("");
  q <- inQualifiers[[1]]$code
  
  if(is.null(q) || length(q) < 1) return("");
  
  qualifiers <- q
  
  builtQualifiers <- ""
  if(length(qualifiers) > 0) {
    for(i in seq_along(qualifiers)) {
      #Due to HTML hack being used for comments can't use kable to render table and thus need to use a hack to show greaterthan and other special HTML codes
      builtQualifiers <- paste0(builtQualifiers, convertStringToTableDisplay(qualifiers[i]), ",")
    }
    strLength <- nchar(builtQualifiers)
    if(strLength > 0) {
      builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
    }
  }
  
  return(builtQualifiers)
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

#' Create a table of qualifiers that are used in the report and prepare
#' them in a table for use in the bottom of the report
#' 
#' @param reportObject The JSON data for the report requested
#' 
#' @param table A vector from which to derive qualifiers from that
#' were displayed in the report itself.
#'  
#' @return A table to print at the bottom of the report or an empty
#' data.frame if there are no qualifiers
#' 
srsQualifiersTable <- function(reportObject, table) {
  #Construct List of all qualifiers
  if(!isEmptyOrBlank(reportObject[["readings"]]$qualifiers)){
    qualifiersList <- data.frame(unlist(reportObject[["readings"]][["qualifiers"]], recursive=FALSE))
  } else {
    qualifiersList <- data.frame()
  }
  
  if (isEmptyOrBlank(qualifiersList) || nrow(qualifiersList)==0) return ()
  
  columnNames <- c("Code",
                   "Identifier",
                   "Description")
  
  #Construct a list of qualifiers used in the report
  usedQualifiers <- getSrsTableQualifiers(table)
  qualifiersList <- qualifiersList[which(qualifiersList[["code"]] %in% usedQualifiers),]
  
  toRet <- data.frame(stringsAsFactors = FALSE, qualifiersList[["code"]], qualifiersList[["identifier"]], qualifiersList[["displayName"]])
  toRet <- toRet[!duplicated(toRet), ]
  colnames(toRet) <- columnNames

  return(toRet)
}

#' Takes a list of data and extracts the qualifiers from it
#' 
#' @description when passed a list of data, extracts the qualifiers from it
#' 
#' @param table The table of data created for the SRS report
#' 
#' @return Extracts the unique qualifiers and returns a list
#'
getSrsTableQualifiers <- function(table){
  toRet <- list()

  #Extract Necessary Data Columns
  relevantData <- strsplit(unlist(table[["toRet"]]$Qualifier[nchar(table[["toRet"]]$Qualifier) > 0]), ",")

  #Convert HTML codes back to equivalent characters
  relevantData <- lapply(relevantData, function(x){return(convertTableDisplayToString(x))})
    
  toRet <- unlist(relevantData)

  return(toRet[!duplicated(toRet)])
}

