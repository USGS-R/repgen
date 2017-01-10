#' if there are gaps in the timeseries, don't connect them
#' this creates multiple line/point calls if there are gaps
#' @param data original list format of JSON
#' @param timeSeries current timeseries data
#' @param isDV logic for whether this plot uses daily values or not
#splitDataGaps <- function(timeSeries, timezone, isDV)
splitDataGaps <- function(data, timeSeries, isDV){
  
  # will need to update when we update the function sig
  timezone <- getReportMetadata(data, 'timezone')
  
  data_list <- data[[ts$field[1]]]
  
  #Add zero/negative gaps
  zeroNegativeGaps <- findZeroNegativeGaps(ts$field[1], data, isDV)
  if("gaps"  %in% names(data_list)){
    data_list$gaps <- rbind(data_list$gaps, zeroNegativeGaps)
  } else {
    data_list$gaps <- zeroNegativeGaps
  }
  
  hasGaps <- "gaps"  %in% names(data_list) && !isEmptyOrBlank(data_list$gaps)
  hasEstimatedRangesAsGaps <- (isEmptyOrBlank(ts$estimated) || !ts$estimated) && 
    "estimatedPeriods"  %in% names(data_list) && 
    !isEmptyOrBlank(data_list$estimatedPeriods)
  isEstimated <- !isEmptyOrBlank(ts$estimated) && ts$estimated

  if(hasGaps || hasEstimatedRangesAsGaps || isEstimated){
    tsGaps <- readGaps(timeSeries, timezone)
    
    if(hasEstimatedRangesAsGaps) {
      tsGapsEst <- createGapsFromEstimatedPeriods(timeSeries, timezone, isDV)
      tsGaps$startGaps <- c(tsGaps$startGaps, tsGapsEst$startGaps)
      tsGaps$endGaps <- c(tsGaps$endGaps, tsGapsEst$endGaps)
    }
    
    if(isEstimated){
      tsGapsEst <- createGapsFromEstimatedPeriods(timeSeries, isDV, inverted=TRUE)
      tsGaps$startGaps <- c(tsGaps$startGaps, tsGapsEst$startGaps)
      tsGaps$endGaps <- c(tsGaps$endGaps, tsGapsEst$endGaps)
    }
    
    startGaps <- sort(tsGaps$startGaps)
    endGaps <- sort(tsGaps$endGaps)
  
    
    #This is causing DV steps to be rendered at noon instead of on the day marks. 
    #Re-enable after refactor of time parsing functions?
    #if(isDV){ ts$time <- flexibleTimeParse(ts$time, timezone = timezone) }
    
    # working with list data (fiveyr and dvhydro)
    if(class(ts) == "list"){
      dataWithoutGaps <- data.frame(time = ts$time, value = ts$value,
                                    stringsAsFactors = FALSE)
    } else if(class(ts) == "data.frame"){
      dataWithoutGaps <- ts
    } else {
      dataWithoutGaps <- data.frame()
    }
    
    dataSplit <- list()
    for(g in 1:length(startGaps)){
      
      if(isDV) {
        dataBeforeGap <- dataWithoutGaps[which(flexibleTimeParse(dataWithoutGaps[['time']], timezone, TRUE) <= startGaps[g]),]
        dataWithoutGaps <- dataWithoutGaps[which(flexibleTimeParse(dataWithoutGaps[['time']], timezone, TRUE) >= endGaps[g]),]
      } else {
        dataBeforeGap <- dataWithoutGaps[which(dataWithoutGaps[['time']] <= startGaps[g]),]
        dataWithoutGaps <- dataWithoutGaps[which(dataWithoutGaps[['time']] >= endGaps[g]),]
      }
      
      
      # only add dataBeforeGap if it exists, sometimes gap dates are earlier than any data 
      if(!isEmptyVar(dataBeforeGap)) { 
        dataSplit <- append(dataSplit, list(dataBeforeGap))
      }
      
      #leave the loop when there is no data left to split, sometimes gap dates are later than any
      if(isEmptyVar(dataWithoutGaps)) { 
        break  
      }
      
    }
    
    if(!isEmptyVar(dataWithoutGaps)){
      dataSplit <- append(dataSplit, list(dataWithoutGaps))
    }
    
    if(class(ts) == "list"){
      dataSplit <- lapply(dataSplit, function(d, legend.name){
        d <- as.list(d)
        d$legend.name <- legend.name
        return(d)
      }, legend.name = ts$legend.name)
    }
    
  } else {
    dataSplit <- list(ts)
  }
  
  return(dataSplit)
}

#' use splitDataGaps and format the resulting data correctly
#' @param data original list format of JSON
#' @param relevantData contains all ts/vars that are not empty (equals allVars in the *-data.R script)
#' @param isDV logic for whether this plot uses daily values or not
applyDataGaps <- function(data, relevantData, isDV=FALSE){
  
  #separate data with gaps
  haveField <- unlist(lapply(relevantData, function(v){"field" %in% names(v)}))
  gapData <- unlist(lapply(relevantData[haveField], splitDataGaps, data=data, isDV=isDV), recursive=FALSE)
  
  if(!isEmptyOrBlank(gapData)){
    pattern <- paste0("(", paste(names(relevantData), collapse="|"), ")")
    names(gapData) <- regmatches(names(gapData), m=regexpr(pattern, names(gapData)))
    #add data back together
    relevantDataWithGaps <- append(relevantData[!haveField], gapData)
  } else {
    relevantDataWithGaps <- relevantData
  }
  
  return(relevantDataWithGaps)
}

#' @export
# adds periods of zero or negative data to the gaps field of the specified ts
findZeroNegativeGaps <- function(field, data, isDV){
  #Ensure we are supposed to remove zeros and negatives before doing so
  loggedData <- isLogged(data, data[[field]]$points, field)
  flagZeroNeg <- getReportMetadata(data, 'excludeZeroNegative')
  timezone <- getReportMetadata(data, 'timezone')
  if(!loggedData || isEmptyOrBlank(flagZeroNeg) || !flagZeroNeg){
    return(NULL)
  }
  
  uv_series <- data[[field]]$points
  if(!is.null(uv_series) & nrow(uv_series) != 0){
    uv_series <- uv_series %>% 
      rename(rawTime = time) %>% 
      mutate(time = flexibleTimeParse(rawTime, timezone, isDV)) %>% 
      select(time, rawTime, value)
    
    #Select times from each point that will be excluded
    potentialNewGaps <- uv_series %>% filter(value > 0) %>% select(time, rawTime)
    
    #Determine start / end times for gaps created by these points
    gapTolerance <- ifelse(isDV, 1, 15)
    gapUnits <- ifelse(isDV, "days", "mins")
    potentialNewGaps <- potentialNewGaps %>% mutate(diff = c(difftime(tail(strptime(time, "%Y-%m-%d %H:%M:%S"), -1),
                                                                      head(strptime(time, "%Y-%m-%d %H:%M:%S"), -1), 
                                                                      units=gapUnits),0), 
                                                    prev = lag(diff))
    startGaps <- potentialNewGaps %>% filter(diff > gapTolerance) %>% select(rawTime)
    endGaps <- potentialNewGaps %>% filter(prev > gapTolerance) %>% select(rawTime)
    
    appGaps <- data.frame(startTime = startGaps$rawTime, endTime = endGaps$rawTime)
  }
  
  return(appGaps)
}

#' Find the start and end times for any possible data gaps.
#' 
#' @description Find the start and end times 
#' 
#' @param timeSeries list of points, values, gaps
#' @param timezone string giving the timezone
#' 
readGaps <- function(timeSeries, timezone){
  
  hasGaps <- "gaps"  %in% names(timeSeries) && !isEmptyOrBlank(timeSeries$gaps)
  
  if(hasGaps) {
    startGaps <- flexibleTimeParse(data_list$gaps$startTime, timezone = timezone)
    endGaps <- flexibleTimeParse(data_list$gaps$endTime, timezone = timezone)
  } else {
    startGaps <- c()
    endGaps <- c()
  }
  
  startGaps <- sort(startGaps)
  endGaps <- sort(endGaps)
  
  return(list(startGaps = startGaps, endGaps = endGaps))
}

#' Create gaps in data if there is estimated time series.
#' 
#' @description If there is estimated data, then the estimated 
#' time series should treat non-estimated periods as gaps and 
#' non-estimated time-series should treat estimated periods as gaps.
#' 
#' @param timeSeries list of points, values for estimated data
#' @param timezone string giving the timezone
#' @param isDV logical, does this time series have daily values
#' @param inverted logical, are treating estimated
#' time periods as gaps (FALSE, default) or you are treating
#' non-estimated time periods as gaps (TRUE).
createGapsFromEstimatedPeriods <- function(timeSeries, timezone, isDV, inverted=FALSE){
  
  if(isDV){
    # remove any time value for dv estimated times (should be for a whole day)
    startEstimated <- unlist(as.POSIXct(strptime(data_list$estimatedPeriods$startDate, "%F")))
    endEstimated <-  unlist(as.POSIXct(strptime(data_list$estimatedPeriods$endDate, "%F")))
  } else {
    startEstimated <- data_list$estimatedPeriods$startDate
    endEstimated <- data_list$estimatedPeriods$endDate
  }
  
  startEstimated <- flexibleTimeParse(startEstimated, timezone = timezone)
  endEstimated <- flexibleTimeParse(endEstimated, timezone = timezone)
  
  startGaps <- startEstimated
  endGaps <- endEstimated
  
  if(inverted){
    startGaps <- endEstimated
    endGaps <- startEstimated
  } 
  
  return(list(startGaps = startEstimated, endGaps = endEstimated))
}

