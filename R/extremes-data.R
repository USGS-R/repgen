#'@title create a flat text 'extremes table' type output table
#'@param rawData extremes report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export

extremesTable2 <- function(rawData){
  
  data <- applyQualifiers(rawData)

  no_upchain <- isEmptyOrBlank(data$upchain$min) && isEmptyOrBlank(data$upchain$max)
  no_dv <- isEmptyOrBlank(data$dv$min) && isEmptyOrBlank(data$dv$max)
    
  primaryLabel <- getReportMetadata(rawData,'primaryLabel')
  primaryParameter <- getReportMetadata(rawData,'primaryParameter')
  primaryUnit <- getReportMetadata(rawData,'primaryUnit')
  
  columnNames <- c("", "Date", "Time", paste("Primary series </br>", primaryParameter, "</br> (", primaryUnit, ")"))
  maxRowNames <- list()
  minRowNames <- list()

  if(!no_upchain){
    upchainLabel <- getReportMetadata(rawData,'upchainLabel')
    upchainParameter <- getReportMetadata(rawData,'upchainParameter')
    upchainUnit <- getReportMetadata(rawData,'upchainUnit')

    columnNames <- append(columnNames, paste("Upchain series </br>", upchainParameter, "</br> (", upchainUnit, ")"))

    maxRowNames <- append(maxRowNames, c(paste("Max Inst ", upchainParameter, " and corresponding ", primaryParameter),
                                         paste("Max Inst ", primaryParameter, " and corresponding ", upchainParameter)))
    minRowNames <- append(minRowNames, c(paste("Min Inst ", upchainParameter, " and corresponding ", primaryParameter),
                                         paste("Min Inst ", primaryParameter, " and corresponding ", upchainParameter)))
  } else {
    maxRowNames <- append(maxRowNames, c(paste("Max Inst ", primaryParameter)))
    minRowNames <- append(minRowNames, c(paste("Min Inst ", primaryParameter)))
  }
  
  if(!no_dv){
    dvLabel <- getReportMetadata(rawData,'dvLabel')
    dvParameter <- getReportMetadata(rawData,'dvParameter')
    dvComputation <- getReportMetadata(rawData,'dvComputation')
    dvUnit <- getReportMetadata(rawData,'dvUnit')

    maxRowNames <- append(maxRowNames, c(paste("Max Daily ", dvComputation, " ", dvParameter)))
    minRowNames <- append(minRowNames, c(paste("Min Daily ", dvComputation, " ", dvParameter)))
  }

  orderedRowNames <- c(unlist(maxRowNames), unlist(minRowNames))
  
  index <- which(names(data) %in% c("upchain", "primary", "dv")) 
  toRet <- data.frame()
  maxRows <- list()
  minRows <- list()
  
  for(i in index){
    maxRows <- append(maxRows, createDataRows(data[[i]], "max"))
    minRows <- append(minRows, createDataRows(data[[i]], "min"))
  }
  
  orderedRows <- append(maxRows, minRows)

  print(orderedRows)

  toRet <- orderedRows
  #colnames(toRet) <- columnNames
  #rownames(toRet) <- orderedRowNames
  
  return(toRet)
}

extremesTable <- function(rawData){
  
}

createDataRows <- function(data, param, includeRelated=TRUE){
  rowList <- list()

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

    if(includeRelated){
      if(length(paramRelated) > 0 && !is.null(paramRelated[[i]])){
        relatedValue <- paramRelated[[i]]$value
      }
    }

    if(!is.null(point$time) && length(point$time) > 0){
      date <- as.POSIXct(strptime(point$time, "%F"))
      time <- "TIME"
    }

    rowList <- append(rowList, list(list(date=date, time=time, value=point$value, related=relatedValue)));
  }

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

