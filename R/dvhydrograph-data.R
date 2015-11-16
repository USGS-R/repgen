

parseDVData <- function(data){
  
  est_dv <- list(time = formatDates(data$estimatedTimeSeries$time), value = data$estimatedTimeSeries$value)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[which(!names(allVars) %in% c("data"))]
  
  plotData <- rev(allVars)
  
  return(plotData)
    
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

formatDates <- function(char_date){
  as.POSIXct(strptime(char_date, "%FT%T"))
}

zeroValues <- function(dataList){    
  logList <- lapply(dataList, function(x) {any(na.omit(x$y) == 0)})
  logVector <- any(unlist(unname(logList)))
}

