parseDVData <- function(data){
  
  stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = FALSE)
  stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = FALSE)
  
  est_stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = TRUE)
  est_stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = TRUE)
  est_stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = TRUE)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {any(unlist(lapply(c(x$time, x$value), function(y) {length(y) != 0}))) } )))]
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


parseDVSupplemental <- function(data, parsedData, zero_logic){
  
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
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  list(time = formatDates(stat_vals[['time']]),
       value = stat_vals[['value']])
}

getStatDerived <- function(data, chain_nm, legend_nm, estimated){
  
  points <- data[[chain_nm]][['points']]
  points$time <- formatDates(points[['time']])
  
  est_dates <- getEstimatedDates(data, chain_nm)
  date_index <- which(points$time >= est_dates[1] & points$time <= est_dates[2])
  
  if(estimated){
    list(time = points[['time']][date_index],
         value = points[['value']][date_index],
         legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]))
  } else if(!estimated && length(date_index) != 0) {
    list(time = points[['time']][-date_index],
         value = points[['value']][-date_index],
         legend.name = data[['reportMetadata']][[legend_nm]])
  } else {
    list(time = points[['time']],
         value = points[['value']],
         legend.name = data[['reportMetadata']][[legend_nm]])
  }
}

getEstimatedDates <- function(data, chain_nm){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- formatDates(data[[chain_nm]]$qualifiers$startDate[i])
  endTime <- formatDates(data[[chain_nm]]$qualifiers$endDate[i])
  return(c(startTime, endTime))
}

formatDates <- function(char_date){
  as.POSIXct(strptime(char_date, "%FT%T"))
}

zeroValues <- function(dataList){    
  logList <- lapply(dataList, function(x) {any(na.omit(x$y) == 0)})
  logVector <- any(unlist(unname(logList)))
}
