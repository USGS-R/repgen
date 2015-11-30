

parseDVData <- function(data){
  
  first_dv <- list(time = formatDates(data$firstDownChain$points$time), value = data$firstDownChain$points$value)
  second_dv <- list(time = formatDates(data$secondDownChain$points$time), value = data$secondDownChain$points$value)
  third_dv <- list(time = formatDates(data$thirdDownChain$points$time), value = data$thirdDownChain$points$value)
  
  #to do -- set up a while loop that goes between the start and end dates and creates the needed times and values in a list.
#   est_dv_first <- list(firstEstStartTime = formatDates(data$firstDownChain$estimatedPeriods$startTime), firstEstEndTime = formatDates(data$firstDownChain$estimatedPeriods$endTime), value = data$firstDownChain$points$value)
#   est_dv_second <- list(secondEstStartTime = formatDates(data$secondDownChain$estimatedPeriods$startTime), secondEstEndTime = formatDates(data$secondDownChain$estimatedPeriods$endTime), value = data$secondDownChain$points$value)
#   est_dv_third <- list(thirdEstStartTime = formatDates(data$thirdDownChain$estimatedPeriods$startTime), thirdEstEndTime = formatDates(data$thirdDownChain$estimatedPeriods$endTime), value = data$thirdDownChain$points$value)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[which(!names(allVars) %in% c("data"))]
  
  plotData <- rev(allVars)
  
  return(plotData)
    
}

parseSecRefData <- function(data, parsedData, zero_logic, isVolFlow, seq_horizGrid) {
  
  secondary_ref <- list(time = formatDates(data$secondaryReferenceTimeSeries$points$time), value = data$secondaryReferenceTimeSeries$points$value)
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}

parseTerRefData <- function(data, parsedData, parseSecRefData, zero_logic, isVolFlow, seq_horizGrid) {
  
  tertiary_ref <- list(time = formatDates(data$tertiaryReferenceTimeSeries$points$time), value = data$tertiaryReferenceTimeSeries$points$value)

  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid","parseSecRefData")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}

parseQuaRefData <- function(data, parsedData, parseSecRefData, zero_logic, isVolFlow, seq_horizGrid, parseTerRefData) {
  
  quaternary_ref <- list(time = formatDates(data$quaternaryReferenceTimeSeries$points$time), value = data$quaternaryReferenceTimeSeries$points$value)  
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid","parseSecRefData","parseTerRefData")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}


parseDVSupplemental <- function(data, parsedData, refData, zero_logic){
  
  isVolFlow <- data[['primaryTimeSeries']][['isVolumetricFlow']]
  if(is.null(isVolFlow) || !isVolFlow || zero_logic){
    logAxis <- FALSE
  } else if(isVolFlow && !zero_logic){  
    logAxis <- TRUE
  }

  if(logAxis){
    seq_horizGrid <- unique(floor(log(parsedData$min_iv$value))):unique(ceiling(log(parsedData$max_iv$value)))
  } else {
    seq_horizGrid <- unique(floor(parsedData$min_iv$value)):unique(ceiling(parsedData$max_iv$value))
  }

  horizontalGrid <- signif(seq(from=seq_horizGrid[1], to=seq_horizGrid[2], along.with=seq_horizGrid), 1)

  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid", "refData")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  list(time = formatDates(stat_vals[['time']]),
       value = round(stat_vals[['value']]))
}

formatDates <- function(char_date){
  as.POSIXct(strptime(char_date, "%FT%T"))
}

zeroValues <- function(dataList){    
  logList <- lapply(dataList, function(x) {any(na.omit(x$y) == 0)})
  logVector <- any(unlist(unname(logList)))
}

