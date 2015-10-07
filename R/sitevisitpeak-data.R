#'@title create a flat text 'sitevisitpeak table' type output table
#'@param rawData sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@return data.frame table

#'@export
# Starting point, creates RMD and runs rendering
#


#'@title create a flat text 'sitevisitpeak table' type output table
#'@param rawData sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export
#'


sitevisitpeakTable <- function(data){
  if (length(data)==0) return ("The dataset requested is empty.")
  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "||",
                   "Verification Method",
                   "Reading",
                   "Uncertainty",
                   "Estimated Date",
                   "Verification Comments",
                   "||",
                   "Corrected Value",
                   "Qualifier",
                   "Date ",
                   "Time ",
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
    
    
    if(!is.null(listElements$associatedIvTime) && !is.na(listElements$associatedIvTime)) {
      ivDateTime <- (strsplit(listElements$associatedIvTime, split="[T]"))
      ivDate <- strftime(ivDateTime[[1]][1], "%m/%d/%Y")
      
      ivTimeFormatting <- sapply(ivDateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
      ivTimeFormatting[[1]] <- sapply(ivTimeFormatting[[1]], function(s) sub(".000","",s))
      ivTimeFormatting[[2]] <- paste(" (UTC",ivTimeFormatting[[2]], ")")
      ivTimeFormatting <-  paste(ivTimeFormatting[[1]],ivTimeFormatting[[2]])
    } else {
      ivDate <- ""
      ivTimeFormatting <- ""
    }
    
    quals <- getQualifiers(listElements$associatedIvTime, listElements$associatedIvQualifiers)
    
    diff <- getIvDifference(listElements$value, listElements$associatedIvValue)
    
    toAdd = c(date,
              timeFormatting,
              nullMask(listElements$party), 
              nullMask(listElements$sublocation), 
              "||",
              nullMask(listElements$monitoringMethod), 
              nullMask(listElements$value),
              nullMask(listElements$uncertainty),
              estDate, 
              nullMask(listElements$comments),
              "||",
              nullMask(listElements$associatedIvValue),
              quals,
              ivDate,
              ivTimeFormatting,
              diff)
    
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
