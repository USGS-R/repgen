#'@title create a flat text 'sensorreading table' type output table
#'@param rawData sensorreading report json string
#'@importFrom dplyr mutate
#'@return data.frame table

#'@export
# Starting point, creates RMD and runs rendering
#


#'@title create a flat text 'sensorreading table' type output table
#'@param rawData sensorreading report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export
#'


sensorreadingTable <- function(data){
  if (length(data)==0) return ("The dataset requested is empty.")
  columnNames <- c("Date",
                   "Party",
                   "Method",
                   "Sublocation",
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
                   "Time"
                   #"Qualifier"
  )

  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSensorData(data$readings,columnNames)
  
  return(results)
}

formatSensorData <- function(data, columnNames){
  if (length(data)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  for(listRows in row.names(data)){
    listElements <- data[listRows,]
    
    if(!is.na(listElements$time) || is.null(listElements$time)) {
    dateTime <- (strsplit(listElements$time, split="[T]"))
    date <- strftime(dateTime[[1]][1], "%m/%d/%Y")
    
    #Break apart, format dates/times, put back together.
      timeFormatting <- sapply(dateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
      timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
      timeFormatting[[2]] <- paste(" (UTC",timeFormatting[[2]], ")")
      timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
      
      if(!is.null(listElements$estimatedTime) && !is.na(listElements$estimatedTime)) {
        estDateTime <- (strsplit(listElements$estimatedTime, split="[T]"))
        estDate <- strftime(estDateTime[[1]][1], "%m/%d/%Y")
      } else {
        estDate <- ""
      }
    }
    
    rec <- getRecorderWithinUncertainty(listElements$recorderUncertainty, listElements$value, listElements$recorderValue)
    ind <- getIndicatedCorrection(listElements$recorderValue, listElements$value)
    app <- getAppliedCorrection(listElements$nearestrawValue, listElements$nearestcorrectedValue)
    corr <- getCorrectedRef(listElements$value, listElements$nearestcorrectedValue, listElements$uncertainty)
    
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
              nullMask(listElements$nearestcorrectedTime)
              #qualifiers?
              )
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  
  return(toRet)
}

nullMask <- function(val) {
  if(!is.null(val)) {
    result <- val
  } else {
    result <- ""
  }
  return(result)
}

getQualifiers <- function(time, inQualifiers) {
  if(length(inQualifiers) < 1) return("");
  q <- inQualifiers[[1]]
  
  if(is.null(q) || length(q) < 1) return("");
  
  qualifiers <- q[time>q$startDate & q$endDate>time,]
  
  builtQualifiers <- ""
  if(nrow(qualifiers) > 0) {
    for(i in 1:nrow(qualifiers)) {
      builtQualifiers <- paste0(builtQualifiers, qualifiers[i,]$code, ",")
    }
    strLength <- nchar(builtQualifiers)
    if(strLength > 0) {
      builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
    }
  }
  
  return(builtQualifiers)
}

#calculate the recorder w/in uncertainty
getRecorderWithinUncertainty <- function(uncertainty, value, recorderValue) { 
  ref <- as.numeric(value)
  unc <- as.numeric(uncertainty)
  rec <- as.numeric(recorderValue)
  if (!is.null(unc) && !is.na(unc) && !is.null(ref) && !is.na(ref)) {
    val1 <- ref+unc
    val2 <- ref-unc
      if ((rec<val1) && (rec>val2)) {
        recorderWithin <- "Yes"
      } else {
        recorderWithin <- "No"
      }
  } 
  if (is.na(unc) || is.na(ref)) {
    recorderWithin <- "NA"
  }
  return(recorderWithin)
}

#calculate indicated correction
getIndicatedCorrection <- function(recorderValue, value) {
  if ((!is.null(recorderValue)) && (!is.null(value)))
  {
    rec <- as.numeric(recorderValue)
    ref <- as.numeric(value)
    indicatedCorrection <- rec-ref
  }
  return(indicatedCorrection)
}

# get applied correction
getAppliedCorrection <- function(raw, corrected) {
  if ((!is.null(raw)) && (!is.null(corrected))) 
  {
    raw <- as.numeric(raw)
    corrected <- as.numeric(corrected)
    appliedCorrection <- corrected-raw
  }
  return(appliedCorrection)
} 

getCorrectedRef <- function (value, nearestcorrectedValue, uncertainty) {
  value <- as.numeric(value) 
  nearest <- as.numeric(nearestcorrectedValue) 
  unc <- as.numeric(uncertainty)
  if ((!is.null(value)) && (!is.na(value)) && (!is.null(uncertainty)) && (!is.na(uncertainty))) {
    lower <- value-unc 
    upper <- value+unc 
    if ((lower<=nearest) && (upper>=nearest)) { 
      correctedRef <- "Yes"
    }
    else {
      correctedRef <- "No"
    }
  } 
  if (is.na(value) || is.na(unc)) {
    correctedRef <- "NA"
  }
  return(correctedRef)
}


getIvDifference <- function(readingVal, ivVal) {
  result <- "NA"
  v1 <- as.numeric(readingVal)
  v2 <- as.numeric(ivVal)
  if(is.numeric(v1) & is.numeric(v2)) {
    val <- v2-v1
    if(!is.na(val)) {
      result <- as.character(round(val, digits = nchar(ivVal)))
      
      if(abs(val) > 0.05) {
        result <- paste(result, "*")
      }
    }
  }
  return(result)
}
