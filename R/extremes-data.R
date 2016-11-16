#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export

extremesTable <- function(rawData){
  
  data <- applyQualifiers(rawData)

  no_primary <- isEmptyOrBlank(data$primary$min) && isEmptyOrBlank(data$primary$max)
  no_upchain <- isEmptyOrBlank(data$upchain$min) && isEmptyOrBlank(data$upchain$max)
  no_dv <- isEmptyOrBlank(data$dv$min) && isEmptyOrBlank(data$dv$max)

  #No valid data recieved
  if(no_primary && no_upchain && no_dv){
    return("The dataset requested is empty.")
  }
    
  primaryLabel <- getReportMetadata(rawData,'primaryLabel')
  primaryParameter <- getReportMetadata(rawData,'primaryParameter')
  primaryUnit <- getReportMetadata(rawData,'primaryUnit')
  
  columnNames <- c("", "Date", "Time", paste("Primary series </br>", primaryParameter, "</br> (", primaryUnit, ")"))
  maxRows <- list()
  minRows <- list()

  if(!no_upchain){
    upchainLabel <- getReportMetadata(rawData,'upchainLabel')
    upchainParameter <- getReportMetadata(rawData,'upchainParameter')
    upchainUnit <- getReportMetadata(rawData,'upchainUnit')

    columnNames <- append(columnNames, paste("Upchain series </br>", upchainParameter, "</br> (", upchainUnit, ")"))

    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "max", paste("Max Inst ", upchainParameter, " and corresponding ", primaryParameter), TRUE))
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter, " and corresponding ", upchainParameter), FALSE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "min", paste("Min Inst ", upchainParameter, " and corresponding ", primaryParameter), TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter, " and corresponding ", upchainParameter), FALSE))
  } else {
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter), FALSE, FALSE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter), FALSE, FALSE))
  }
  
  if(!no_dv){
    dvLabel <- getReportMetadata(rawData,'dvLabel')
    dvParameter <- getReportMetadata(rawData,'dvParameter')
    dvComputation <- getReportMetadata(rawData,'dvComputation')
    dvUnit <- getReportMetadata(rawData,'dvUnit')
    if(!no_upchain){
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste("Max Daily ", dvComputation, " ", dvParameter), FALSE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste("Min Daily ", dvComputation, " ", dvParameter), FALSE))
    } else {
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste("Max Daily ", dvComputation, " ", dvParameter), FALSE, FALSE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste("Min Daily ", dvComputation, " ", dvParameter), FALSE, FALSE))
    }
  }

  dataRows <- c(maxRows, minRows)

  #Change column and row names to their correct forms and add them into the dataframe.
  toRet <- data.frame()
  
  for(i in 1:length(dataRows)){
    toAdd <- dataRows[[i]]
    colnames(toAdd) <- columnNames
    toRet <- rbind(toRet,toAdd)
  }
  
  return(toRet)
}

#'@title create flat text 'qualifiers table' type output table
#'@param data report data
#'@importFrom dplyr mutate
#'@return string table
#'@export
extremesQualifiersTable <- function(data, table){
  #Construct List of all qualifiers
  qualifiersList <- list(data.frame(data$dv$qualifiers), data.frame(data$upchain$qualifiers), data.frame(data$primary$qualifiers))
  qualifiersList <- Reduce(function(...) merge(..., all=T), qualifiersList)
  columnNames <- c("Code",
                  "Identifier",
                  "Description"
  )
  
  #Construct a list of qualifiers used in the report
  usedQualifiers <- getExtremesTableQualifiers(table)
  qualifiersList <- qualifiersList[which(qualifiersList$code %in% usedQualifiers),]
  
  #Return with no table if no qualifiers used
  if (length(qualifiersList)==0) return ()
  
  toRet <- data.frame(stringsAsFactors = FALSE, qualifiersList$code, qualifiersList$identifier, qualifiersList$displayName)
  toRet <- toRet[!duplicated(toRet), ]
  colnames(toRet) <- columnNames

  return(toRet)
}

getExtremesTableQualifiers <- function(table){
  toRet <- list()

  #Extract Necessary Data Columns
  relevantData <- strsplit(unlist(table[grepl("Primary|Upchain", names(table))]), " ")
  
  for(i in 1:length(relevantData)){
    if(length(relevantData[[i]]) > 1){
      if(nchar(relevantData[[i]][[1]]) > 0){
        toRet <- append(toRet, strsplit(relevantData[[i]][[1]], ","))
      }
    }
  }
  
  toRet <- unlist(toRet)

  return(toRet[!duplicated(toRet)])
}

#'@title create a set of rows for one data parameter
#'@param data a set of extremes report data for either upchain, primary, or dv
#'@param param either "min" or "max" to specify if we are generating minimum or maximum rows
#'@param rowName the name to use for the specified row
#'@param isUpchain whether or not this is the upchain dataset and it needs to compare to primary
#'@param includeRelated whether or not there is a second column of corresponding data
#'@param doMerge whether or not we should merge duplicate rows
#'@return list dataRows
#'@export
createDataRows <- function(data, param, rowName, isUpchain, includeRelated=TRUE, doMerge=TRUE){  
    subsetData <- data[which(names(data)%in%c(param))]

    #Generate Data Frame of Rows from data using given params
    dataRows <- lapply(subsetData, function(x) {
      #Formatting for times/dates
      dateTime <- t(data.frame(strsplit(x$points$time, split="[T]")))
      dateTime[,1] <- strftime(dateTime[,1], "%m-%d-%Y")
      
      #Break apart, format dates/times, put back together.
      if(ncol(dateTime) > 1) {
        timeFormatting <- sapply(dateTime[,2], function(s) {
          m <- regexec("([^-+]+)([+-].*)", s)
          splitTime <- unlist(regmatches(s, m))[2:3]
          return(splitTime)
        })
        timeFormatting[1,] <- sapply(timeFormatting[1,], function(s) sub(".000","",s))
        timeFormatting[2,] <- paste0(" (UTC ",timeFormatting[2,], ")")
        timeFormatting <-  paste(timeFormatting[1,],timeFormatting[2,])
      } else {
        timeFormatting <- sapply(dateTime[,1], function(s) {
          return("")
        })
      }
      
      primaryValue <- x$points$value
      dataRows <- data.frame()
      
      #Add related points to the series if we are including them
      if(includeRelated){
        relatedValue <- "N/A"

        if(isUpchain){
          relatedSet <- x$relatedPrimary
        } else {
          relatedSet <- x$relatedUpchain
        }

        if(!is.null(relatedSet))
        {
          relatedValue <- relatedSet$value
        }

        if(!isUpchain){
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, related=relatedValue, stringsAsFactors = FALSE)
        } else {
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=relatedValue, related=primaryValue, stringsAsFactors = FALSE)
        }
      } else {
        dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, stringsAsFactors = FALSE)
      }

      return(dataRows)
    })

    #Clean Data Rows
    if(!is.null(dataRows[[1]])){
      dataRows <- dataRows[[1]]

      #Merge Data Rows Based on Criteria
      if(doMerge){
        if(includeRelated){
          #For rows that have the same date but multiple corresponding values, keep only min/max corresponding value row
          if(!isUpchain){
            #Keep Maximum or Minimum based on current param
            if(param == "max"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$related, decreasing = TRUE),]
            } else  if (param == "min"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$related, decreasing = FALSE),]
            }
          } else {
            #Keep Maximum or Minimum based on current param
            if(param == "max"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$primary, decreasing = TRUE),]
            } else  if (param == "min"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$primary, decreasing = FALSE),]
            }
          }
          
          #Keep only the non-duplicated rows which results in first row of each date section being selected
          dataRows <- duplicateRows[!duplicated(duplicateRows[c("date")]),]
          
          #Re-sort by date ascending
          dataRows <- dataRows[with(dataRows, order(dataRows$date, decreasing = FALSE)),]
          
          #Keep only first instance of rows with same primary <-> related combination
          dataRows <- dataRows[!duplicated(dataRows[c("primary", "related")]),]
        } else {
          #Only have primary data so just keep the first row, all others are duplicates at different date/times
          dataRows <- dataRows[!duplicated(dataRows[c("primary", "date")]),]
        }
      }

      #Replace Duplicate Names with blank names
      if(NROW(dataRows[duplicated(dataRows["name"]),]) > 0){
        dataRows[duplicated(dataRows["name"]),]["name"] <- ""
      }      
    }
    return(list(dataRows))
}

applyQualifiers <- function(data) {
  consolidatedQualifiers <- list(
    primary=data$primary$qualifiers, 
    upchain=data$upchain$qualifiers,
    dv=data$dv$qualifiers)
  
  return(sapply(data, function(x) {
    if(! is.null(x$qualifiers)) {
      x$max$points <- applyQualifiersToValues(x$max$points, x$qualifiers)
      x$min$points <- applyQualifiersToValues(x$min$points, x$qualifiers)
      x$max$relatedUpchain <- applyQualifiersToValues(x$max$relatedUpchain, consolidatedQualifiers$upchain)
      x$min$relatedUpchain <- applyQualifiersToValues(x$min$relatedUpchain, consolidatedQualifiers$upchain)
      x$max$relatedPrimary <- applyQualifiersToValues(x$max$relatedPrimary, consolidatedQualifiers$primary)
      x$min$relatedPrimary <- applyQualifiersToValues(x$min$relatedPrimary, consolidatedQualifiers$primary)
    }
    return(x)
  }))
}

applyQualifiersToValues <- function(points, qualifiers) {
  if(identical("",points)){
    points <- NULL
  }
  if(is.null(points)) {
    return(points)
  }
  
  getQualifierString <- function(p) {
    builtQualifiers <- ""
    if(length(qualifiers) > 0) {
      for(i in 1:nrow(qualifiers)) {
        q <- qualifiers[i,]
        startDate <- q$startDate
        endDate <- q$endDate

        if(nchar(p$time) > 10){
          if(p$time > startDate & p$time < endDate) {
            builtQualifiers <- paste0(builtQualifiers, q$code, ",")
          }
        } else {
          if(p$time >= as.Date(startDate) & p$time <= as.Date(endDate)) {
            builtQualifiers <- paste0(builtQualifiers, q$code, ",")
          }
        }
      }
      strLength <- nchar(builtQualifiers)
      if(strLength > 0) {
        builtQualifiers <- substr(builtQualifiers, 1, strLength-1)
      }
    }
    return(builtQualifiers)
  }
  
  points <- mutate(points, 
         value = paste(getQualifierString(points), points$value))
  return(points)
}

