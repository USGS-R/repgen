#' Get Extremes Report constants
#' @description list of constants used by extremes report
getExtremesConstants <- function() {
  return(list(
          PRIMARY_HEADER_PREFIX="Primary series",
          UPCHAIN_HEADER_PREFIX="Upchain series"
          ))
}

#' Create a Flat Text "extremes table" Type Output Table
#' 
#' @param reportObject An extremes report JSON string.
#' @importFrom dplyr mutate
#' @return string table
extremesTable <- function(reportObject) {
  data <- applyQualifiers(reportObject)
  
  #constants
  EXT <- getExtremesConstants()
  MAX_INST <- "Max Inst"
  MIN_INST <- "Min Inst"
  MAX_DAILY <- "Max Daily"
  MIN_DAILY <- "Min Daily"
  CORRESPONDING <- "and corresponding"
  
  no_primary <- isEmptyOrBlank(data$primary$min) && isEmptyOrBlank(data$primary$max)
  no_upchain <- isEmptyOrBlank(data$upchain$min) && isEmptyOrBlank(data$upchain$max)
  no_dv <- isEmptyOrBlank(data$dv$min) && isEmptyOrBlank(data$dv$max)

  #No valid data recieved
  if(no_primary && no_upchain && no_dv){
    return("The dataset requested is empty.")
  }
    
  primaryLabel <- fetchReportMetadataField(reportObject,'primaryLabel')
  primaryParameter <- fetchReportMetadataField(reportObject,'primaryParameter')
  primaryUnit <- fetchReportMetadataField(reportObject,'primaryUnit')
  invertedFlag <- parseReportMetadataField(reportObject, 'isInverted', FALSE)

  #Invert Extremes Labels
  if(invertedFlag){
    MAX_INST <- "Min Inst"
    MIN_INST <- "Max Inst"
    MAX_DAILY <- "Min Daily"
    MIN_DAILY <- "Max Daily"
  }
  
  columnNames <- c("", "Date", "Time", paste(EXT$PRIMARY_HEADER_PREFIX, "</br>", primaryParameter, "</br> (", primaryUnit, ")"))
  maxRows <- list()
  minRows <- list()

  if(!no_upchain){
    upchainLabel <- fetchReportMetadataField(reportObject,'upchainLabel')
    upchainParameter <- fetchReportMetadataField(reportObject,'upchainParameter')
    upchainUnit <- fetchReportMetadataField(reportObject,'upchainUnit')

    columnNames <- append(columnNames, paste(EXT$UPCHAIN_HEADER_PREFIX, "</br>", upchainParameter, "</br> (", upchainUnit, ")"))

    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "max", paste(MAX_INST, upchainParameter, CORRESPONDING, primaryParameter), isUpchain=TRUE))
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste(MAX_INST, primaryParameter, CORRESPONDING, upchainParameter)))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "min", paste(MIN_INST, upchainParameter, CORRESPONDING, primaryParameter), isUpchain=TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste(MIN_INST, primaryParameter, CORRESPONDING, upchainParameter)))
  } else {
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste(MAX_INST, primaryParameter), includeRelated=FALSE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste(MIN_INST, primaryParameter), includeRelated=FALSE))
  }
  
  if(!no_dv){
    dvLabel <- fetchReportMetadataField(reportObject,'dvLabel')
    dvParameter <- fetchReportMetadataField(reportObject,'dvParameter')
    dvComputation <- fetchReportMetadataField(reportObject,'dvComputation')
    dvUnit <- fetchReportMetadataField(reportObject,'dvUnit')
    if(!no_upchain){
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste(MAX_DAILY, dvComputation, " ", dvParameter), isDv=TRUE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste(MIN_DAILY, dvComputation, " ", dvParameter), isDv=TRUE))
    } else {
      maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste(MAX_DAILY, dvComputation, " ", dvParameter), isDv=TRUE, includeRelated=FALSE))
      minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste(MIN_DAILY, dvComputation, " ", dvParameter), isDv=TRUE, includeRelated=FALSE))
    }
  }

  dataRows <- c(maxRows, minRows)

  #Change column and row names to their correct forms and add them into the dataframe.
  toRet <- data.frame()
  
  footnote <- any(lapply(dataRows, function(x) { return(any(x[["footnote"]]))}))
  
  for(i in 1:length(dataRows)){
    dataRows[[i]][["footnote"]] <- NULL
    toAdd <- dataRows[[i]]
    colnames(toAdd) <- columnNames
    toRet <- rbind(toRet,toAdd)
  }
  
  return(list(toRet=toRet, footnote=footnote))
}

#' Create a Flat Text "qualifiers table" Type Output Table
#' 
#' @param reportObject Report data.
#' @param table A vector to derive qualifiers from.
#' @param primaryHeaderTerm a unique term to help determine which column title is for primary data
#' @param upchainHeaderTerm a unique term to help determine which column title is for upchain data
#' @return A vector of qualifiers.
#' @importFrom dplyr mutate
extremesQualifiersTable <- function(reportObject, table, primaryHeaderTerm, upchainHeaderTerm) {
  #Construct List of all qualifiers
  qualifiersList <- list(data.frame(reportObject$dv$qualifiers), data.frame(reportObject$upchain$qualifiers), data.frame(reportObject$primary$qualifiers))
  qualifiersList <- Reduce(function(...) merge(..., all=T), qualifiersList)
  columnNames <- c("Code",
                  "Identifier",
                  "Description"
  )
  
  #Construct a list of qualifiers used in the report
  usedQualifiers <- getExtremesTableQualifiers(table, primaryHeaderTerm, upchainHeaderTerm)
  qualifiersList <- qualifiersList[which(qualifiersList$code %in% usedQualifiers),]
  
  #Return with no table if no qualifiers used
  if (length(qualifiersList)==0) return ()
  
  toRet <- data.frame(stringsAsFactors = FALSE, qualifiersList$code, qualifiersList$identifier, qualifiersList$displayName)
  toRet <- toRet[!duplicated(toRet), ]
  colnames(toRet) <- columnNames

  return(toRet)
}

#' Extract Qualifiers Used
#' @description Given an extremes table, look through the primary/upchain columns for qualifiers
#' @param table table/list representation (already processed data) of an extremes report.
#' @param primaryHeaderTerm a unique term to help determine which column title is for primary data
#' @param upchainHeaderTerm a unique term to help determine which column title is for upchain data
#' @return unique list of qualifier strings found in Primary/Unpchain columns
getExtremesTableQualifiers <- function(table, primaryHeaderTerm, upchainHeaderTerm){
  toRet <- list()

  #Extract Necessary Data Columns
  relevantData <- strsplit(unlist(table[grepl(paste0(primaryHeaderTerm, "|", upchainHeaderTerm), names(table))]), " ")
  
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

#' Create a Set of Rows for One Data Parameter
#' 
#' @param reportObject A set of extremes report data for either upchain, primary, or DV.
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
createDataRows <-
  function(reportObject, param, rowName, isUpchain = FALSE, isDv = FALSE, includeRelated = TRUE, doMerge = TRUE) {
    subsetData <- reportObject[which(names(reportObject)%in%c(param))]

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
      
      footnote <- FALSE
      
      #Add related points to the series if we are including them
      if(includeRelated){
        
        relatedValue <- "N/A"

        if(isUpchain){
          relatedSet <- x$relatedPrimary
        } else {
          relatedSet <- x$relatedUpchain
        }

        if(!is.null(relatedSet) && !isDv)
        {
          relatedValue <- relatedSet$value
          
          if(nrow(relatedSet) != nrow(x$points)) {
            relatedValue <- mergeAndStretch(x$points, relatedSet)
            footnote <- TRUE
          }
        } else if(!isDv) {
          footnote <- TRUE
        }
        
        if(isDv){
          dataRows <- data.frame(name=rowName, date=dateTime, time=timeFormatting, primary=primaryValue, related=relatedValue, footnote=footnote,  stringsAsFactors = FALSE)
        } else if(!isUpchain){
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, related=relatedValue, footnote=footnote, stringsAsFactors = FALSE)
        } else {
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=relatedValue, related=primaryValue, footnote=footnote, stringsAsFactors = FALSE)
        }
      } else {
        if(isDv){
          dataRows <- data.frame(name=rowName, date=dateTime, time=timeFormatting, primary=primaryValue, footnote=footnote, stringsAsFactors = FALSE)
        } else {
          dataRows <- data.frame(name=rowName, date=dateTime[,1], time=timeFormatting, primary=primaryValue, footnote=footnote, stringsAsFactors = FALSE)
        }
      }

      return(dataRows)
    })

    # declare objects to get rid of dplyr warning in Check
    # these are column names and will be used appropriately when it gets to that line
    related <- time <- primary <- '.dplyr.var'
    
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
              duplicateRows <- dataRows %>% arrange(desc(related), date, time) %>% as.data.frame()
            } else  if (param == "min"){
              duplicateRows <- dataRows %>% arrange(related, date, time) %>% as.data.frame()
            }
          } else {
            #Keep Maximum or Minimum based on current param
            if(param == "max"){
              duplicateRows <- dataRows %>% arrange(desc(primary), date, time) %>% as.data.frame()
            } else  if (param == "min"){
              duplicateRows <- dataRows %>% arrange(primary, date, time) %>% as.data.frame()
            }
          }

          dataRows <- filterAndMarkDuplicates(duplicateRows, "*", includeRelated, "date")

          #Re-sort by date ascending
          dataRows <- dataRows[with(dataRows, order(as.numeric(dataRows$date), as.numeric(dataRows$time))),]
          
          #Keep only first instance of rows with same primary <-> related combination
          dataRows <- dataRows[!duplicated(dataRows[c("primary", "related")]),]
        } else if(isDv) {
          dataRows <- dataRows[order(as.numeric(dataRows$date)),]
          if(includeRelated){
            dataRows <- filterAndMarkDuplicates(dataRows, "**", includeRelated, "primary")
          } else {
            dataRows <- filterAndMarkDuplicates(dataRows, "*", includeRelated, "primary")
          }
          dataRows <- dataRows[!duplicated(dataRows[c("primary")]),]
        } else {
          dataRows <- dataRows[order(as.numeric(dataRows$date), as.numeric(dataRows$time)),]
          dataRows <- filterAndMarkDuplicates(dataRows, "*", includeRelated, "primary")
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

#' Filter and Mark Duplicates
#' @description Given a list of rows, will remove duplicates. The first row of a set of duplicates will be kept, and it's date value marked with the note provided
#' @param extremesRows the data frame of extremes data as characters (fields: name, date, time, primary, related) 
#' @param note a note to append to the date value of the remaining duplicate row
#' @param includeRelated true to include the related field in the returned data frame
#' @param fieldToCheck the field compare for duplicate values
#' @importFrom dplyr rowwise
#' @importFrom dplyr filter
filterAndMarkDuplicates <- function(extremesRows, note, includeRelated, fieldToCheck){
  #Make sure that rows are properly sorted before being fed to this function.
  
  # work around irrelevant warnings from devtools::check()
  isDuplicateStart <- NULL
  isDuplicateEnd <- NULL
  
  #Keep only the non-duplicated rows which results in first row of each date section being selected
  filteredRows <- extremesRows %>% 
  mutate(isDuplicateStart = duplicated(extremesRows[fieldToCheck]),
         isDuplicateEnd = duplicated(extremesRows[fieldToCheck], fromLast=TRUE)) %>% 
  rowwise() %>% # dplyr converts to tibble df
  mutate(date = ifelse(isDuplicateStart || isDuplicateEnd, paste(date, note), date)) %>% 
  filter(!isDuplicateStart) %>% 
  data.frame() # unconvert from tibble
  
  if(includeRelated){
    keep <- c("name","date","time","primary","related","footnote")
    dataRows <- filteredRows[keep]
  } else {
    keep <- c("name","date","time","primary","footnote")
    dataRows <- filteredRows[keep]
  }
  
  return(dataRows)
}

#' Apply qualifiers
#' @description Will apply all qualifiers to all values in the report object
#' @param reportObject the extremes report object
#' @return the same reportObject, but with all values updated with qualifiers prefixed as CSV
applyQualifiers <- function(reportObject) {
  consolidatedQualifiers <- list(
    primary=reportObject$primary$qualifiers, 
    upchain=reportObject$upchain$qualifiers,
    dv=reportObject$dv$qualifiers)
  
  return(sapply(reportObject, function(x) {
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


#' Apply Qualifiers to Points
#' @description given data frames of points and qualifiers, will prepended all applicable qualifiers (comma separated) to each point.
#' @param points data frame of time/value points
#' @param qualifiers of qualifiers (qualifier comment per time range)
#' @return updated point list with qualifiers prepended
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
        
        if (10 < nchar(p$time)) {
          # if date(time) point intersects (the open-open) interval
          if (startDate < p$time & p$time < endDate) {
            builtQualifiers <- paste0(builtQualifiers, q$code, ",")
          }
        } else {
          # if date point intersects (the closed-open) interval
          if (as.Date(startDate) <= p$time & p$time < as.Date(endDate)) {
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

#' Merge and even out two timeseries
#' @description merges two timeseries; giving smaller dataset NA values
#' where a time series gap occurs between them
#' @param points the primary data series containing time and value
#' @param related the related data series containing time and value
#' @return updated related list extended to contain NA where gaps exist
mergeAndStretch <- function(points, related) {
  related <- as.data.frame(related)
  points <- as.data.frame(points)
  merged <- merge(related, points, by.x="time", by.y="time", all=T)
  relatedValue <- merged$value.x
  return(relatedValue)
}