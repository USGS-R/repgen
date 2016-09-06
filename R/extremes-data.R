#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export

extremesTable <- function(rawData){
  
  data <- applyQualifiers(rawData)

  no_upchain <- isEmptyOrBlank(data$upchain$min) && isEmptyOrBlank(data$upchain$max)
  no_dv <- isEmptyOrBlank(data$dv$min) && isEmptyOrBlank(data$dv$max)
    
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

    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "max", paste("Max Inst ", upchainParameter, " and corresponding ", primaryParameter), TRUE, TRUE, TRUE))
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter, " and corresponding ", upchainParameter), TRUE, FALSE, TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("upchain"))]], "min", paste("Min Inst ", upchainParameter, " and corresponding ", primaryParameter), TRUE, TRUE, TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter, " and corresponding ", upchainParameter), TRUE, FALSE, TRUE))

  } else {
    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "max", paste("Max Inst ", primaryParameter), TRUE, FALSE, TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("primary"))]], "min", paste("Min Inst ", primaryParameter), TRUE, FALSE, TRUE))
  }
  
  if(!no_dv){
    dvLabel <- getReportMetadata(rawData,'dvLabel')
    dvParameter <- getReportMetadata(rawData,'dvParameter')
    dvComputation <- getReportMetadata(rawData,'dvComputation')
    dvUnit <- getReportMetadata(rawData,'dvUnit')

    maxRows <- append(maxRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "max", paste("Max Daily ", dvComputation, " ", dvParameter), TRUE, FALSE, TRUE))
    minRows <- append(minRows, createDataRows(data[[which(names(data) %in% c("dv"))]], "min", paste("Min Daily ", dvComputation, " ", dvParameter), TRUE, FALSE, TRUE))
  }

  dataRows <- c(maxRows, minRows)

  #Change column and row names to their correct forms and add them into the dataframe.
  toRet <- data.frame()
  toRet <- rbind(dataRows$name, dataRows)
  colnames(toRet) <- columnNames
  
  return(toRet)
}

createDataRows <- function(data, param, rowName, doMerge, isUpchain, includeRelated){  
    subset <- data[which(names(data)%in%c(param))]

    dataRows <- lapply(subset, function(x) {
      #Formatting for times/dates
      dates <- as.POSIXct(strptime(x$points$time, "%F"))
      dateTime <- t(data.frame(strsplit(x$points$time, split="[T]")))
      
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
          data.frame(name=rowName, date=dates, time=timeFormatting, primary=primaryValue, related=relatedValue, stringsAsFactors = FALSE)
        } else {
          data.frame(name=rowName, date=dates, time=timeFormatting, primary=relatedValue, related=primaryValue, stringsAsFactors = FALSE)
        }
      } else {
        data.frame(name=rowName, date=dates, time=timeFormatting, primary=primaryValue, stringsAsFactors = FALSE)
      }
    })

    return(list(dataRows[[1]]))
}

createDataRows2 <- function(data, param, rowName, doMerge, isUpchain, includeRelated){
  rowList <- data.frame()

  #Fetch relevant data
  paramData <- data[which(names(data)%in%c(param))][[param]]
  paramPoints <- paramData$points
  paramRelated <- paramData[which(names(paramData)%in%c("relatedPrimary", "relatedUpchain"))]
  
  if(!class(paramPoints) == "list"){
    paramPoints <- list(paramPoints)
  }

  if(!class(paramRelated) == "list"){
    paramRelated <- list(paramRelated)
  }

  #Create a row for each point
  for(i in seq_along(paramPoints)){
    point <- paramPoints[[i]]
    relatedValue <- "N/A"
    timeFormatting <- "N/A"

    if(includeRelated){
      if(length(paramRelated) > 0 && !is.null(paramRelated[[i]])){
        relatedValue <- paramRelated[[i]]$value
      }
    }

    if(!is.null(point$time) && length(point$time) > 0){
      date <- as.POSIXct(strptime(point$time, "%F"))
      dateTime <- t(data.frame(strsplit(point$time, split="[T]")))

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
    }

    if(!isUpchain){
      rowList <- append(rowList, data.frame(name=rowName, date=date, time=timeFormatting, value=point$value, related=relatedValue, stringsAsFactors = FALSE));
    } else {
      rowList <- append(rowList, data.frame(name=rowName, date=date, time=timeFormatting, value=relatedValue, related=point$value, stringsAsFactors = FALSE));
    }
  }

  print("BEFORE")
  print(NROW(rowList))

  #Remove duplicates
  if(doMerge && !isUpchain){
    rowList <- rowList[!duplicated(rowList[c("date", "related")])]
    print("DEDUP")
  } else if(doMerge) {
    rowList <- rowList[!duplicated(rowList[c("date", "value")])]
    print("DEDUP")
  }

  print("AFTER")
  print(NROW(rowList))

  return(rowList)
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
        if(p$time > startDate & p$time < endDate) {
          builtQualifiers <- paste0(builtQualifiers, q$code, ",")
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

flattenParam <- function(param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  return(param)
}

orderMaxMin <- function(results, isInverted){
  
  if(isInverted){
    maximums <- results[grep("min", names(results))]
    minimums <- results[grep("max", names(results))]
  } else {
    maximums <- results[grep("max", names(results))]
    minimums <- results[grep("min", names(results))]
  }
  
  maximums_index <- c(grep("upchain", names(maximums)), 
                      grep("primary", names(maximums)), 
                      grep("dv", names(maximums)))
  
  minimums_index <- c(grep("upchain", names(minimums)), 
                      grep("primary", names(minimums)), 
                      grep("dv", names(minimums)))
   
  maximums <- maximums[maximums_index]
  minimums <- minimums[minimums_index]
  
  results <- list()
  results <- append(results, maximums)
  results <- append(results, minimums)
  
  return(results)
}

