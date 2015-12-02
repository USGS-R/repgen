

parseDVData <- function(data){
  
  stat <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  
  est_stat <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = TRUE)
  
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
  list(time = formatDates_fiveyr(stat_vals[['time']][1], type=NA),
       value = stat_vals[['value']][1])
}

getStatDerived <- function(data, chain_nm, legend_nm, estimated){
  
  points <- data[[chain_nm]][['points']]
  points$time <- formatDates_fiveyr(points[['time']], type=NA)
  
  est_dates <- getEstimatedDates(data, chain_nm)
  date_index <- which(points$time >= est_dates[1] & points$time <= est_dates[2])
  
  if(estimated){
    list(time = points[['time']][-date_index],
         value = points[['value']][-date_index],
         legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]))
  } else if(!estimated && length(date_index) != 0) {
    list(time = points[['time']][date_index],
         value = points[['value']][date_index],
         legend.name = data[['reportMetadata']][[legend_nm]])
  } else {
    list(time = points[['time']],
         value = points[['value']],
         legend.name = data[['reportMetadata']][[legend_nm]])
  }
}

getEstimatedDates <- function(data, chain_nm){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- formatDates_fiveyr(data[[chain_nm]]$qualifiers$startDate[i], type=NA)
  endTime <- formatDates_fiveyr(data[[chain_nm]]$qualifiers$endDate[i], type=NA)
  return(c(startTime, endTime))
}

formatDates_fiveyr <- function(char_date, type){
  date_formatted <- as.POSIXct(strptime(char_date, "%FT%T"))
  if(!is.na(type) && type=="start"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-01"))
  } else if(!is.na(type) && type=="end"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-30"))
  }
  return(date_formatted)
}

zeroValues <- function(dataList){    
  logList <- lapply(dataList, function(x) {any(na.omit(x$y) == 0)})
  logVector <- any(unlist(unname(logList)))
}

