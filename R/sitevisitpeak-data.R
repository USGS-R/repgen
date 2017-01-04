#'@title create a flat text 'sitevisitpeak table' type output table
#'@param data sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@importFrom htmlTable htmlTable
#'@return data.frame table

#'@export
# Starting point, creates RMD and runs rendering
#

sitevisitpeakTable <- function(data){
  if (length(data)==0) return ("The dataset requested is empty.")
  
  includeComments <- isNullOrFalse(data[['reportMetadata']][['excludeComments']])
                   
  if(includeComments){
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
  } else {
    columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "Verification Method",
                   "Reading",
                   "Uncertainty",
                   "Estimated Date",
                   "Estimated Time",
                   "Corrected Value",
                   "Qualifier",
                   "Date",
                   "Time",
                   "Difference from Peak Verification Reading")
  }
  
  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSVPData(data$readings,columnNames, includeComments)
  
  return(results)
}

formatSVPData <- function(data, columnNames, includeComments){
  if (length(data)==0) return ("The dataset requested is empty.")
  toRet = data.frame(stringsAsFactors = FALSE)
  for(listRows in row.names(data)){
    listElements <- data[listRows,]
    
    fvTimeFormatting <- timeFormatting(listElements$time)
    estTimeFormatting <- timeFormatting(listElements$estimatedTime)
    ivTimeFormatting <- timeFormatting(listElements$associatedIvTime)
    
    quals <- getQualifiers(listElements$associatedIvTime, listElements$associatedIvQualifiers)
    
    diff <- getIvDifference(listElements$value, listElements$associatedIvValue)
    
    if(includeComments){
      toAdd = c(fvTimeFormatting$date,
              fvTimeFormatting$time,
              nullMask(listElements$party), 
              nullMask(listElements$sublocation), 
              nullMask(listElements$monitoringMethod), 
              nullMask(listElements$value),
              nullMask(listElements$uncertainty),
              estTimeFormatting$date,
              estTimeFormatting$time,
              nullMask(formatComments(getComments((listElements$comments)))),
              nullMask(listElements$associatedIvValue),
              quals,
              ivTimeFormatting$date,
              ivTimeFormatting$time,
              diff)
    } else {
      toAdd = c(fvTimeFormatting$date,
              fvTimeFormatting$time,
              nullMask(listElements$party), 
              nullMask(listElements$sublocation), 
              nullMask(listElements$monitoringMethod), 
              nullMask(listElements$value),
              nullMask(listElements$uncertainty),
              estTimeFormatting$date,
              estTimeFormatting$time,
              nullMask(listElements$associatedIvValue),
              quals,
              ivTimeFormatting$date,
              ivTimeFormatting$time,
              diff)
    
    }
    
    toRet <- rbind(toRet, data.frame(t(toAdd),stringsAsFactors = FALSE))
  }
  colnames(toRet) <- columnNames
  rownames(toRet) <- NULL
  
  return(toRet)
}

timeFormatting <- function(timeVals){
  if(!isEmpty(timeVals)) {
    dateTime <- (strsplit(timeVals, split="[T]"))
    dateFormat <- strftime(dateTime[[1]][1], "%m/%d/%Y")
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) {
      m <- regexec("([^-+]+)([+-].*)", s)
      splitTime <- unlist(regmatches(s, m))[2:3]
      return(splitTime)
    })
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste0(" (UTC ",timeFormatting[[2]], ")")
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
      #Due to HTML hack being used for comments on SRS reports can't use kable to render table and thus need to use a hack to show greaterthan and other special HTML codes
      #Same method is used here for consistency since both reports use HTML tables formatted in the same way
      builtQualifiers <- paste0(builtQualifiers, convertStringToTableDisplay(qualifiers[i,]$code), ",")
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
    if(!is.na(val) && all(c(length(v1),length(v2)) != 0)) {
      result <- as.character(round(val, digits = nchar(ivVal)))
      
      if(abs(val) > 0.05) {
        result <- paste(result, "**")
      }
    }
  }
  return(result)
}

containsOutsideUncertainty <- function(data) {
  readings_diff <- list()
  readings_data <- data$readings
  for(listRows in row.names(readings_data)){
    listElements <- readings_data[listRows,]
    readings_diff <- append(readings_diff, getIvDifference(listElements$value, listElements$associatedIvValue))
  }

  return(length(readings_diff[grepl("\\*\\*", readings_diff)]) > 0)
}

#'@title create flat text 'qualifiers table' type output table
#'@param data report data
#'@importFrom dplyr mutate
#'@return string table
#'@export
svpQualifiersTable <- function(data, table){
  #Construct List of all qualifiers
  if(!isEmptyOrBlank(data$readings$associatedIvQualifiers)){
      qualifiersList <- do.call("rbind", data$readings$associatedIvQualifiers)
  } else {
      qualifiersList <- data.frame()
  }
  
  if (isEmptyOrBlank(qualifiersList) || nrow(qualifiersList) == 0) return ()
  
  columnNames <- c("Code",
                  "Identifier",
                  "Description"
  )
  
  #Construct a list of qualifiers used in the report
  usedQualifiers <- getSvpTableQualifiers(table)
  qualifiersList <- qualifiersList[which(qualifiersList$code %in% usedQualifiers),]
  
  toRet <- data.frame(stringsAsFactors = FALSE, qualifiersList$code, qualifiersList$identifier, qualifiersList$displayName)
  toRet <- toRet[!duplicated(toRet), ]
  colnames(toRet) <- columnNames

  return(toRet)
}

getSvpTableQualifiers <- function(table){
  toRet <- list()

  #Extract Necessary Data Columns
  relevantData <- strsplit(unlist(table$Qualifier[nchar(table$Qualifier) > 0]), ",")

  #Convert HTML codes back to equivalent characters
  relevantData <- lapply(relevantData, function(x){return(convertTableDisplayToString(x))})
    
  toRet <- unlist(relevantData)

  return(toRet[!duplicated(toRet)])
}
