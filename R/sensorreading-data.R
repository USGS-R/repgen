#'@title create a flat text 'sensorreading table' type output table
#'@param data sensorreading report json string
#'@importFrom dplyr mutate
#'@importFrom htmlTable htmlTable
#'@importFrom pander pander
#'@return data.frame table
#'@export
#'
sensorreadingTable <- function(data){
  if (length(data)==0) return ("The dataset requested is empty.")
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
  
  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSensorData(data$readings,columnNames)
  
  return(results)
}

formatSensorData <- function(data, columnNames){
  if (length(data)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  
  lastRefComm <- ''
  lastRecComm <- ''
  lastDate <- ''
  
  for(listRows in row.names(data)){
    listElements <- data[listRows,]
    
    if ("displayTime" %in% names(data)) {
      if(!is.na(listElements$displayTime) || is.null(listElements$time)) {
        dateTime <- (strsplit(listElements$displayTime, split="[T]"))
        date <- strftime(dateTime[[1]][1], "%m/%d/%Y")
        
        #Break apart, format dates/times, put back together.
        timeFormatting <- sapply(dateTime[[1]][2], function(s)  {
          m <- regexec("([^-+]+)([+-].*)", s)
          splitTime <- unlist(regmatches(s, m))[2:3]
          return(splitTime)
        }) 
        timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
        timeFormatting[[2]] <- paste(" (UTC",timeFormatting[[2]], ")")
        timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
        
          estDateTime <- (strsplit(listElements$displayTime, split="[T]"))
          estDate <- strftime(estDateTime[[1]][1], "%m/%d/%Y")
        } else {
          estDate <- ""
          timeFormatting <- ""
        }
    }
    #Get the time out of the nearest corrected iv time, don't need the date
    if ("nearestcorrectedTime" %in% names(data)) {
      if (!isEmpty(listElements$nearestcorrectedTime)) {
        dateTimeCorrected <- (strsplit(listElements$nearestcorrectedTime, split="[T]"))
        dateCorrected <- strftime(dateTimeCorrected[[1]][1], "%m/%d/%Y")
        
        #Break apart, format dates/times, put back together.
        timeFormattingCorrected <- sapply(dateTimeCorrected[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
        timeFormattingCorrected[[1]] <- sapply(timeFormattingCorrected[[1]], function(s) sub(".000","",s))
        timeFormattingCorrected[[2]] <- paste0(" (UTC ",timeFormattingCorrected[[2]], ")")
        timeFormattingCorrected <-  paste(timeFormattingCorrected[[1]],timeFormattingCorrected[[2]]) 
      }
    } else {
      timeFormattingCorrected <- ""
    }
    
    rec <- getRecorderWithinUncertainty(listElements$uncertainty, listElements$value, listElements$recorderValue)
    ind <- getIndicatedCorrection(listElements$recorderValue, listElements$value)
    app <- getAppliedCorrection(listElements$nearestrawValue, listElements$nearestcorrectedValue)
    corr <- getCorrectedRef(listElements$value, listElements$nearestcorrectedValue, listElements$uncertainty)
    qual <- getSRSQualifiers(listElements$qualifiers)

    toAdd = c(date,
              timeFormatting,
              nullMask(listElements$party), 
              nullMask(listElements$sublocation),
              ##
              nullMask(listElements$monitoringMethod),
              nullMask(listElements$type),
              nullMask(listElements$value),
              nullMask(listElements$uncertainty),
              ##
              nullMask(listElements$recorderMethod),
              nullMask(listElements$recorderType),
              nullMask(listElements$recorderValue),
              nullMask(listElements$recorderUncertainty),
              ##
              rec, 
              ind, 
              app, 
              corr,
              ##
              nullMask(listElements$nearestcorrectedValue),
              timeFormattingCorrected,
              qual
    )
    
    
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
    
    #insert column row
    #THIS IS HTML ONLY, YUGE HACK
    refComm <- formatComments(getComments(listElements$referenceComments))
    recComm <- formatComments(getComments(listElements$recorderComments))
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
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  return(list(toRet=toRet))
}

nullMask <- function(val) {
  if(!is.null(val)) {
    result <- val
  } else {
    result <- ""
  }
  return(result)
}

#calculate the recorder w/in uncertainty
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

#calculate indicated correction
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

# get applied correction
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

getSRSQualifiers <- function(inQualifiers) {
  if(length(inQualifiers) < 1) return("");
  q <- inQualifiers[[1]]$code
  
  if(is.null(q) || length(q) < 1) return("");
  
  qualifiers <- q
  
  builtQualifiers <- ""
  if(length(qualifiers) > 0) {
    for(i in seq_along(qualifiers)) {
      builtQualifiers <- paste0(builtQualifiers, qualifiers[i], ",")
    }
    strLength <- nchar(builtQualifiers)
    if(strLength > 0) {
      builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
    }
  }
  
  return(builtQualifiers)
}

getComments <- function(comments) {
  comm <- unlist(comments)
  if (!isEmptyOrBlank(comm)) {
    value <- comm
    
  } else {
    value <- ""
  }
  return(value)
}
#used simply to set precision for some known numbers rather
#than having a hardcode precision number sprinkled out.
getSrsPrecision <- function() {
  return(2);
}
