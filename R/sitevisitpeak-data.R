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
  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Verification Method",
                   "Reading",
                   "Uncertainty",
                   "Estimated Date",
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
  #TODO extract qualifiers
  toRet = data.frame(stringsAsFactors = FALSE)
  for(listRows in row.names(data)){
    listElements <- data[listRows,]
    
    dateTime <- (strsplit(listElements$time, split="[T]"))
    date <- strftime(dateTime[[1]][1], "%m/%d/%Y")
    
    estDateTime <- (strsplit(listElements$estimatedTime, split="[T]"))
    estDate <- strftime(estDateTime[[1]][1], "%m/%d/%Y")
    
    ivDateTime <- (strsplit(listElements$associatedIvTime, split="[T]"))
    ivDate <- strftime(ivDateTime[[1]][1], "%m/%d/%Y")
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste(" (UTC",timeFormatting[[2]], ")")
    timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
    
    ivTimeFormatting <- sapply(ivDateTime[[1]][2], function(s) strsplit(s,split="[-]")[[1]])
    ivTimeFormatting[[1]] <- sapply(ivTimeFormatting[[1]], function(s) sub(".000","",s))
    ivTimeFormatting[[2]] <- paste(" (UTC",ivTimeFormatting[[2]], ")")
    ivTimeFormatting <-  paste(ivTimeFormatting[[1]],ivTimeFormatting[[2]])
    
    quals <- getQualifiers(listElements$time, listElements$qualifiers)
    
    diff <- getIvDifference(listElements$value, listElements$associatedIvValue)
    
    toAdd = c(date,
              timeFormatting,
              listElements$party, 
              listElements$monitoringMethod, 
              listElements$value,
              listElements$uncertainty, 
              estDate, 
              listElements$comments,
              listElements$associatedIvValue,
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

getQualifiers <- function(time, inQualifiers) {
  if(length(inQualifiers) < 1) return("");
  q <- inQualifiers[[1]]
  
  qualifiers <- q[time>q$startDate & q$endDate>time,]
  
  builtQualifiers <- ""
  if(length(qualifiers) > 0) {
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
    result <- as.character(round(val, digits = nchar(ivVal)))
    
    if(abs(val) > 0.05) {
      result <- paste(result, "*")
    }
  }
  return(result)
}
