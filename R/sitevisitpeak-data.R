#' Create a Flat Text, "sitevisitpeak table" Type Output Table
#' 
#' @param readingsData data frame of parsed field visit readings data.
#' @param excludeComments boolean from report metadata of whether comments are included.
#' @return data frame of site visit peak data
sitevisitpeakTable <- function(readingsData, excludeComments){
  dateFormat <- "%m/%d/%Y"
  toRet = data.frame(stringsAsFactors = FALSE)
  includeComments <- isNullOrFalse(excludeComments)
  columnNames <- getSVPColumns(includeComments)
  
  for(listRows in row.names(readingsData)){
    listElements <- readingsData[listRows,]
    
    fvTimeFormatting <- timeFormatting(listElements$time, dateFormat)
    estTimeFormatting <- timeFormatting(listElements$estimatedTime, dateFormat)
    ivTimeFormatting <- timeFormatting(listElements$associatedIvTime, dateFormat)
    
    quals <- formatQualifiersStringList(listElements$qualifiers[[1]])
    
    diff <- getIvDifference(listElements$value, listElements$associatedIvValue)
    
    if(includeComments){
      toAdd = c(fvTimeFormatting$date,
                fvTimeFormatting$time,
                listElements$party, 
                listElements$sublocation, 
                listElements$monitoringMethod, 
                listElements$value,
                listElements$uncertainty,
                estTimeFormatting$date,
                estTimeFormatting$time,
                formatComments(getComments((listElements$comments))),
                listElements$associatedIvValue,
                quals,
                ivTimeFormatting$date,
                ivTimeFormatting$time,
                listElements$diffPeak)
    } else {
      toAdd = c(fvTimeFormatting$date,
                fvTimeFormatting$time,
                listElements$party, 
                listElements$sublocation, 
                listElements$monitoringMethod, 
                listElements$value,
                listElements$uncertainty,
                estTimeFormatting$date,
                estTimeFormatting$time,
                listElements$associatedIvValue,
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

containsOutsideUncertainty2 <- function(data) {
  readings_diff <- list()
  readings_data <- data$readings
  for(listRows in row.names(readings_data)){
    listElements <- readings_data[listRows,]
    readings_diff <- append(readings_diff, getIvDifference(listElements$value, listElements$associatedIvValue))
  }

  return(length(readings_diff[grepl("\\*\\*", readings_diff)]) > 0)
}

#' Create Flat Text, "qualifiers table" Type Output Table
#' 
#' @param data Report data.
#' @param table A vector from which to derive qualifiers from.
#' @importFrom dplyr mutate
#' @return A vector of strings.
#' @export
svpQualifiersTable <- function(data, table) {
  #Construct List of all qualifiers
  if(!isEmptyOrBlank(data$readings$associatedIvQualifiers)){
      qualifiersList <- do.call("rbind", data$readings$associatedIvQualifiers)
  } else {
      qualifiersList <- data.frame()
  }
  
  if (isEmptyOrBlank(qualifiersList) || nrow(qualifiersList) == 0) return ()
  
  columnNames <- c("Code",
                   "Identifier",
                   "Description")
  
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
