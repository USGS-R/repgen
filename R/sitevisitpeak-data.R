#'@title create a flat text 'sitevisitpeak table' type output table
#'@param data sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@importFrom htmlTable htmlTable
#'@importFrom pander pander
#'@return data.frame table

#'@export
# Starting point, creates RMD and runs rendering
#

sitevisitpeakTable <- function(data){
  if (length(data)==0) return ("The dataset requested is empty.")
  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "Verification Method",
                   "Reading",
                   "Uncertainty",
                   "Estimated Date",
                   "Estimated Time",
                   "Verification Comments",
                   "Corrected Value",
                   "Qualifier",
                   "Date",
                   "Time",
                   "Difference from Peak Verification Reading")
  
  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSVPData(data$readings,columnNames)
  
  return(results)
}

formatSVPData <- function(data, columnNames){
  if (length(data)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  for(listRows in row.names(data)){
    listElements <- data[listRows,]
    
    fvTimeFormatting <- timeFormatting(listElements$time)
    estTimeFormatting <- timeFormatting(listElements$estimatedTime)
    ivTimeFormatting <- timeFormatting(listElements$associatedIvTime)
    
    quals <- getQualifiers(listElements$associatedIvTime, listElements$associatedIvQualifiers)
    
    diff <- getIvDifference(listElements$value, listElements$associatedIvValue)
    
    toAdd = c(fvTimeFormatting$date,
              fvTimeFormatting$time,
              nullMask(listElements$party), 
              nullMask(listElements$sublocation), 
              nullMask(listElements$monitoringMethod), 
              nullMask(listElements$value),
              nullMask(listElements$uncertainty),
              estTimeFormatting$date,
              estTimeFormatting$time,
              nullMask(listElements$comments),
              nullMask(listElements$associatedIvValue),
              quals,
              ivTimeFormatting$date,
              ivTimeFormatting$time,
              diff)
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  
  return(toRet)
}

timeFormatting <- function(timeVals){
  if(!is.null(timeVals) && !is.na(timeVals)) {
    dateTime <- (strsplit(timeVals, split="[T]"))
    dateFormat <- strftime(dateTime[[1]][1], "%m/%d/%Y")
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste(" (UTC",timeFormatting[[2]], ")")
    timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
  } else {
    dateFormat <- ""
    timeFormatting <- ""
  }
  return(list(date = dateFormat, time = timeFormatting))
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

getIvDifference <- function(readingVal, ivVal) {
  result <- "NA"
  v1 <- as.numeric(readingVal)
  v2 <- as.numeric(ivVal)
  if(is.numeric(v1) & is.numeric(v2)) {
    val <- v1-v2
    if(!is.na(val) && all(c(length(v1),length(v2)) != 0)) {
      result <- as.character(round(val, digits = nchar(ivVal)))
      
      if(abs(val) > 0.05) {
        result <- paste(result, "*")
      }
    }
  }
  return(result)
}
