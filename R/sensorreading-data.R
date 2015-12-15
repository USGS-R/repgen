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
      if (!is.null(listElements$nearestcorrectedTime) || !is.na(listElements$nearestcorrectedTime)) {
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
    
    rec <- getRecorderWithinUncertainty(listElements$recorderUncertainty, listElements$value, listElements$recorderValue)
    ind <- getIndicatedCorrection(listElements$recorderValue, listElements$value)
    app <- getAppliedCorrection(listElements$nearestrawValue, listElements$nearestcorrectedValue)
    corr <- getCorrectedRef(listElements$value, listElements$nearestcorrectedValue, listElements$uncertainty)
    qual <- getSRSQualifiers(listElements$qualifiers)
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
              nullMask(listElements$nearestcorrectedValue),
              timeFormattingCorrected,
              qual,
              comm
    )
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
    if (("comments" %in% names(data)) && (!is.na(listElements$comments))) {
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
  if ("uncertainty" %in% names(data) && "value" %in% names(data) && "recorderValue" %in% names(data)) {  
    if (!is.null(uncertainty) && !is.na(uncertainty) && !is.null(value) && !is.na(value)) {
      ref <- as.numeric(value)
      unc <- as.numeric(uncertainty)
      rec <- as.numeric(recorderValue)
      val1 <- ref+unc
      val2 <- ref-unc
      if ((rec<val1) && (rec>val2)) {
        recorderWithin <- "Yes"
      } else {
        recorderWithin <- "No"
      }
    }
  } else {
    recorderWithin <- ""
  }
  return(recorderWithin)
}

#calculate indicated correction
getIndicatedCorrection <- function(recorderValue, value) {
  if ("recorderValue" %in% names(data) && "value" %in% names(data)) {  
    if ((!is.null(recorderValue)) && (!is.null(value))) {
      rec <- as.numeric(recorderValue)
      ref <- as.numeric(value)
      indicatedCorrection <- round(rec-ref, 2)
    }
  } else {
    indicatedCorrection <- ""
  }
  return(indicatedCorrection)
}

# get applied correction
getAppliedCorrection <- function(raw, corrected) {
  if ("raw" %in% names(data) && "corrected" %in% names(data)) {  
    if ((!is.null(raw)) && (!is.null(corrected))) {
      raw <- as.numeric(raw)
      corrected <- as.numeric(corrected)
      appliedCorrection <- round(corrected-raw, 2)
    }
  } else {
    appliedCorrection <- ""
  }
  return(appliedCorrection)
} 

getCorrectedRef <- function (value, nearestcorrectedValue, uncertainty) {
  if ("value" %in% names(data) && "nearestcorrectedValue" %in% names(data) && "uncertainty" %in% names(data)) {
    if ((!is.null(value)) && (!is.na(value)) && (!is.null(uncertainty)) && (!is.na(uncertainty))) {
      value <- as.numeric(value) 
      nearest <- as.numeric(nearestcorrectedValue) 
      unc <- as.numeric(uncertainty)
      lower <- value-unc 
      upper <- value+unc 
      if ((lower<=nearest) && (upper>=nearest)) { 
        correctedRef <- "Yes"
      }
      else {
        correctedRef <- "No"
      }
    }
  } else {
    correctedRef <- ""
  }
  return(correctedRef)
}

getSRSQualifiers <- function(inQualifiers) {
  if(length(inQualifiers) < 1) return("");
  q <- inQualifiers[[1]]
  
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
