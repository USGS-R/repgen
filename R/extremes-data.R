#' Create a flat text "extremes table" type output table.
#' 
#' @param rawData Extremes report JSON string.
#' @importFrom dplyr mutate
#' @return A vector of strings.
#' @export
extremesTable <- function(rawData) {
  
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

    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "max", paste("Max Inst ", upchainParameter, " and corresponding ", primaryParameter), isUpchain=TRUE))
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter, " and corresponding ", upchainParameter)))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "min", paste("Min Inst ", upchainParameter, " and corresponding ", primaryParameter), isUpchain=TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter, " and corresponding ", upchainParameter)))
  } else {
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter), includeRelated=FALSE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter), includeRelated=FALSE))
  }
  
  if(!no_dv){
    dvLabel <- getReportMetadata(rawData,'dvLabel')
    dvParameter <- getReportMetadata(rawData,'dvParameter')
    dvComputation <- getReportMetadata(rawData,'dvComputation')
    dvUnit <- getReportMetadata(rawData,'dvUnit')
    if(!no_upchain){
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste("Max Daily ", dvComputation, " ", dvParameter), isDv=TRUE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste("Min Daily ", dvComputation, " ", dvParameter), isDv=TRUE))
    } else {
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste("Max Daily ", dvComputation, " ", dvParameter), isDv=TRUE, includeRelated=FALSE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste("Min Daily ", dvComputation, " ", dvParameter), isDv=TRUE, includeRelated=FALSE))
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

#' Create flat text "qualifiers table" type output table.
#' 
#' @param data Report data.
#' @param table A vector to derive qualifiers from.
#' @return A vector of qualifiers.
#' @importFrom dplyr mutate
#' @export
extremesQualifiersTable <- function(data, table) {
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

#' Create a set of rows for one data parameter.
#' 
#' @param data A set of extremes report data for either upchain, primary, or DV.
#' @param param Either "min" or "max" to specify if we are generating minimum or
#'        maximum rows.
#' @param rowName The name to use for the specified row.
#' @param isUpchain Whether or not this is the upchain dataset and it needs to
#'        compare to primary.
#' @param isDv Context is daily values when TRUE; not-daily-values otherwise.
#' @param includeRelated Whether or not there is a second column of
#'        corresponding data.
#' @param doMerge Whether or not we should merge duplicate rows.
#' @return list dataRows
#' @export
createDataRows <-
  function(data, param, rowName, isUpchain = FALSE, isDv = FALSE, includeRelated = TRUE, doMerge = TRUE) {
    subsetData <- data[which(names(data)%in%c(param))]

    #Generate Data Frame of Rows from data using given params
    dataRows <- lapply(subsetData, function(x) {
      #Formatting for times/dates
      if(!isDv){
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
      } else {
        timeFormatting <- ""
        dateTime <- format(as.Date(x$points$time), "%m-%d-%Y")
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

        if(isDv){
          dataRows <- data.frame(name=rowName, date=dateTime, time=timeFormatting, primary=primaryValue, related=relatedValue, stringsAsFactors = FALSE)
        } else if(!isUpchain){
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, related=relatedValue, stringsAsFactors = FALSE)
        } else {
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=relatedValue, related=primaryValue, stringsAsFactors = FALSE)
        }
      } else {
        if(isDv){
          dataRows <- data.frame(name=rowName, date=dateTime, time=timeFormatting, primary=primaryValue, stringsAsFactors = FALSE)
        } else {
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, stringsAsFactors = FALSE)
        }
      }

      return(dataRows)
    })

    #Clean Data Rows
    if(!is.null(dataRows[[1]])){
      dataRows <- dataRows[[1]]

      #Merge Data Rows Based on Criteria
      if(doMerge){
        if(includeRelated && !isDv){
          #For rows that have the same date but multiple corresponding values, keep only min/max corresponding value row
          if(!isUpchain){
            #Keep Maximum or Minimum based on current param
            if(param == "max"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$related, dataRows$time, decreasing = TRUE),]
            } else  if (param == "min"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$related, dataRows$time, decreasing = FALSE),]
            }
          } else {
            #Keep Maximum or Minimum based on current param
            if(param == "max"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$primary, dataRows$time, decreasing = TRUE),]
            } else  if (param == "min"){
              duplicateRows <- dataRows[order(dataRows$date, dataRows$primary, dataRows$time, decreasing = FALSE),]
            }
          }

          dataRows <- applyNoteToDuplicates(duplicateRows, "*", includeRelated, "date")

          #Re-sort by date ascending
          dataRows <- dataRows[with(dataRows, order(dataRows$date, dataRows$time, decreasing = FALSE)),]
          
          #Keep only first instance of rows with same primary <-> related combination
          dataRows <- dataRows[!duplicated(dataRows[c("primary", "related")]),]
        } else if(isDv) {
          dataRows <- dataRows[order(dataRows$date, decreasing = FALSE),]
          if(includeRelated){
            dataRows <- applyNoteToDuplicates(dataRows, "**", includeRelated, "primary")
          } else {
            dataRows <- applyNoteToDuplicates(dataRows, "*", includeRelated, "primary")
          }
          dataRows <- dataRows[!duplicated(dataRows[c("primary")]),]
        } else {
          dataRows <- dataRows[order(dataRows$date, dataRows$time, decreasing = FALSE),]
          dataRows <- applyNoteToDuplicates(dataRows, "*", includeRelated, "primary")
          dataRows <- dataRows[!duplicated(dataRows[c("primary")]),]
        }
      }

      #Replace Duplicate Names with blank names
      if(NROW(dataRows[duplicated(dataRows["name"]),]) > 0){
        dataRows[duplicated(dataRows["name"]),]["name"] <- ""
      }      
    }
    return(list(dataRows))
}

#' @importFrom dplyr rowwise
#' @importFrom dplyr filter
applyNoteToDuplicates <- function(rows, note, includeRelated, duplicateField){
  #Make sure that rows are properly sorted before being fed to this function.
  
  # work around irrelevant warnings from devtools::check()
  isDuplicateStart <- NULL
  isDuplicateEnd <- NULL
  
  #Keep only the non-duplicated rows which results in first row of each date section being selected
  filteredRows <- rows %>% 
  mutate(isDuplicateStart = duplicated(rows[duplicateField]),
         isDuplicateEnd = duplicated(rows[duplicateField], fromLast=TRUE)) %>% 
  rowwise() %>% # dplyr converts to tibble df
  mutate(date = ifelse(isDuplicateStart || isDuplicateEnd, paste(date, note), date)) %>% 
  filter(!isDuplicateStart) %>% 
  data.frame() # unconvert from tibble
  
  if(includeRelated){
    dataRows <- filteredRows[1:5]
  } else {
    dataRows <- filteredRows[1:4]
  }
  
  return(dataRows)
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

