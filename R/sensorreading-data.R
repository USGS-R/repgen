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
                   "Qualifier",
                   "Comments"
  )

  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSensorData(data$readings,columnNames)
  
  return(results)
}

formatSensorData <- function(data, columnNames){
  if (length(data)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  comments_table <- data.frame(Number=character(), Comments=character(),stringsAsFactors = FALSE)
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
      
      #Get the time out of the nearest corrected iv time, don't need the date
      if (!is.null(listElements$nearestrawTime) || is.na(listElements$nearestrawTime)) {
        dateTimeRaw <- (strsplit(listElements$nearestrawTime, split="[T]"))
        dateRaw <- strftime(dateTimeRaw[[1]][1], "%m/%d/%Y")
        
        #Break apart, format dates/times, put back together.
        timeFormattingRaw <- sapply(dateTimeRaw[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
        timeFormattingRaw[[1]] <- sapply(timeFormattingRaw[[1]], function(s) sub(".000","",s))
        timeFormattingRaw[[2]] <- paste(" (UTC",timeFormattingRaw[[2]], ")")
        timeFormattingRaw <-  paste(timeFormattingRaw[[1]],timeFormattingRaw[[2]]) 
      }
    }
    
    rec <- getRecorderWithinUncertainty(listElements$recorderUncertainty, listElements$value, listElements$recorderValue)
    ind <- getIndicatedCorrection(listElements$recorderValue, listElements$value)
    app <- getAppliedCorrection(listElements$nearestrawValue, listElements$nearestcorrectedValue)
    corr <- getCorrectedRef(listElements$value, listElements$nearestcorrectedValue, listElements$uncertainty)
    qual <- cleanQualifiers(listElements$qualifiers)
    comm <- getComments(listElements$comments, listRows)
    
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
              nullMask(listElements$nearestrawValue),
              timeFormattingRaw,
              qual,
              comm
              )
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
    if (!is.na(listElements$comments)) {
      comments_table <- commentTable(listElements$comments, listRows, comments_table)
    }
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  colnames(comments_table) <- c("Row","Comments")
  return(list(toRet=toRet,comments_table=comments_table))
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
    recorderWithin <- ""
  }
  return(recorderWithin)
}

#calculate indicated correction
getIndicatedCorrection <- function(recorderValue, value) {
  if ((!is.null(recorderValue)) && (!is.null(value)))
  {
    rec <- as.numeric(recorderValue)
    ref <- as.numeric(value)
    indicatedCorrection <- round(rec-ref, 2)
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
    correctedRef <- ""
  }
  return(correctedRef)
}


cleanQualifiers <- function(qualifiers) {
  if (!is.null(qualifiers)) {
    qualifiers <- gsub("\\[|\\]","",qualifiers)
  } 
  return(qualifiers)
}

getComments <- function(comments, listRows) {
  if (!is.null(comments) && !is.na(comments)) {
    value <- listRows
  } else {
    value <- ""
  }
  return(value)
}

commentTable <- function(comments, listRows, comments_table) {
  add <- data.frame(Number=listRows, Comments=comments, stringsAsFactors=FALSE)
  comments_table <- rbind(comments_table, add)
  return(comments_table)
}
  