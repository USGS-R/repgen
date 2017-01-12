
#' Split a time series if there are gaps. 
#' 
#' @description If there are gaps in the timeseries, don't connect them.
#' This returns a list of data that can be used for multiple line/point 
#' calls in the plotting functions.
#' 
#' @param timeSeries current timeseries data as a list
#' @param timeSeriesName string giving name that should be given to each list item returned
#' @param timezone string giving the timezone
#' @param flagZeroNeg logical indicating whether or not the zeros & negatives can be 
#' removed for logging the axis
#' @param isDV logical saying whether or not the time series is made of daily values
splitDataGapsTimeSeries <- function(timeSeries, timeSeriesName, timezone, flagZeroNeg, isDV){

  isEstimated <- !isEmptyOrBlank(timeSeries$estimated) && timeSeries$estimated
  
  tsDefinedGaps <- findDefinedGaps(timeSeries, timezone)
  zeroNegativeGaps <- findZeroNegativeGaps(timeValueDF = timeSeries$points, flagZeroNeg, 
                                           timeSeries$isVolumetricFlow, isDV)
  tsEstGaps <- createGapsFromEstimatedPeriods(timeSeries, timezone, isDV, inverted=isEstimated)
  
  startGaps <- c(tsDefinedGaps[['startGaps']], zeroNegativeGaps[['startGaps']], tsEstGaps[['startGaps']])
  startGaps <- sort(startGaps)
  endGaps <- c(tsDefinedGaps[['endGaps']], zeroNegativeGaps[['endGaps']], tsEstGaps[['endGaps']])
  endGaps <- sort(endGaps)
  
  if(!isEmptyOrBlank(startGaps) && !isEmptyOrBlank(endGaps)){

    #This is causing DV steps to be rendered at noon instead of on the day marks. 
    #Re-enable after refactor of time parsing functions?
    #if(isDV){ ts$time <- flexibleTimeParse(ts$time, timezone = timezone) }
    
    ###\\\ this might go away if the fetch has the points field as a data.frame
    # working with list data (fiveyr and dvhydro)
    # if(class(timeSeries[['points']]) == "list"){
    #   timeValueDF <- data.frame(time = timeSeries[['points']][['time']], 
    #                             value = timeSeries[['points']][['value']],
    #                             stringsAsFactors = FALSE)
    # } else if(class(timeSeries[['points']]) == "data.frame"){
    #   timeValueDF <- timeSeries[['points']]
    # } else {
    #   timeValueDF <- data.frame()
    # }
    
    dataSplit <- actuallySplitDataGaps(timeValueDF=timeSeries$points, startGaps, endGaps, timezone, isDV)
    
    # this is adding back the timeSeries metadata fields
    dataSplit <- lapply(dataSplit, function(ds, metadata){
      append(list(points = ds), metadata)
    }, metadata = timeSeries[which(!names(timeSeries) %in% c('points', 'gaps'))])
    names(dataSplit) <- rep(timeSeriesName, length(dataSplit))
    
    # # this is adding back the timeSeries metadata fields
    # if(class(timeSeries[['points']]) == "list"){
    #   dataSplit <- lapply(dataSplit, function(d, legend.name){
    #     d <- as.list(d)
    #     d$legend.name <- legend.name
    #     return(d)
    #   }, legend.name = timeSeries[['points']][['legend.name']])
    # }
    # 
  } else {
    dataSplit <- timeSeries
  }
  
  return(dataSplit)
}

#' Split multiple time series when there are gaps
#' 
#' @description Given a list of time series, this will return a list where gaps in data
#' have been applied to each. The names of the time series will be duplicated if there
#' were gaps that caused the individual time series to break into multiple.
#' 
#' @param allVars contains all ts/vars that are not empty (equals allVars in the *-data.R script)
#' @param timezone string giving the timezone
#' @param flagZeroNeg logical indicating whether or not the zeros & negatives can be 
#' removed for logging the axis
#' @param isDV logical saying whether or not the time series is made of daily values
splitDataGapsList <- function(allVars, timezone, flagZeroNeg, isDV=FALSE){
  
  allVarsToSplit_i <- which(unlist(lapply(allVars, is.list)))
  allVarsNotToSplit <- allVars[which(!unlist(lapply(allVars, is.list)))]
  allVarsToSplit <- unlist(lapply(allVarsToSplit_i, 
                                  function(i, allVars, timezone, flagZeroNeg, isDV){
    splitDataGaps(allVars[[i]], names(allVars)[i], timezone, flagZeroNeg, isDV=FALSE)
  }, allVars=allVars, timezone=timezone, flagZeroNeg=flagZeroNeg, isDV=isDV), 
  recursive = FALSE)
  
  allVarsWithSplits <- append(allVarsToSplit, allVarsNotToSplit)
  
  return(allVarsWithSplits)
}

#' Identify the start and end dates for gaps due to zeroes and negatives
#' 
#' @description Finds periods of zero or negative data to the gaps field of 
#' the specified time series. Only identifies gaps for zeroes and negatives if 
#' they are going to be removed from the time series for logging (flagZeroNeg and
#' isLogged are TRUE). Returns a data frame with two columns, startTime and endTime.
#' 
#' @param timeValueDF data.frame to split with at least two columns, value and time
#' @param flagZeroNeg logical indicating whether or not the zeros & negatives can be 
#' removed for logging the axis
#' @param isVolumetricFlow logical indicating whether the values represent volumetric 
#' flow or not (e.g. FALSE could indicate water level data)
#' @param isDV logical saying whether or not the time series is made of daily values
findZeroNegativeGaps <- function(timeValueDF, flagZeroNeg, isVolumetricFlow, isDV){
  #Ensure we are supposed to remove zeros and negatives before doing so
  loggedData <- isLogged(timeValueDF, isVolumetricFlow, flagZeroNeg)
  
  if(!loggedData || isEmptyOrBlank(flagZeroNeg) || !flagZeroNeg){
    startGaps <- c()
    endGaps <- c()
  } else if(is.null(timeValueDF) & nrow(timeValueDF) == 0){
    startGaps <- c()
    endGaps <- c()
  } else {
    timeValueDF <- timeValueDF %>% 
      rename(rawTime = time) %>% 
      mutate(time = flexibleTimeParse(rawTime, timezone, isDV)) %>% 
      select(time, rawTime, value)
    
    #Select times from each point that will be excluded
    potentialNewGaps <- timeValueDF %>% filter(value > 0) %>% select(time, rawTime)
    
    #Determine start / end times for gaps created by these points
    gapTolerance <- ifelse(isDV, 1, 15)
    gapUnits <- ifelse(isDV, "days", "mins")
    potentialNewGaps <- potentialNewGaps %>% mutate(diff = c(difftime(tail(strptime(time, "%Y-%m-%d %H:%M:%S"), -1),
                                                                      head(strptime(time, "%Y-%m-%d %H:%M:%S"), -1), 
                                                                      units=gapUnits),0), 
                                                    prev = lag(diff))
    startTimes <- potentialNewGaps %>% filter(diff > gapTolerance)
    endTimes <- potentialNewGaps %>% filter(prev > gapTolerance)
    
    startGaps <- startTimes$rawTime
    endGaps <- endTimes$rawTime
  }
  
  return(list(startGaps = startGaps$rawTime, endGaps = endGaps$rawTime))
}

#' Find the start and end times for any possible data gaps.
#' 
#' @description Find the start and end times 
#' 
#' @param timeSeries list of points, values, gaps
#' @param timezone string giving the timezone
#' 
findDefinedGaps <- function(timeSeries, timezone){
  
  hasDefinedGaps <- "gaps"  %in% names(timeSeries) && !isEmptyOrBlank(timeSeries$gaps)
  
  if(hasDefinedGaps) {
    startGaps <- flexibleTimeParse(timeSeries$gaps$startTime, timezone = timezone)
    endGaps <- flexibleTimeParse(timeSeries$gaps$endTime, timezone = timezone)
    startGaps <- sort(startGaps)
    endGaps <- sort(endGaps)
  } else {
    startGaps <- c()
    endGaps <- c()
  }
  
  return(list(startGaps = startGaps, endGaps = endGaps))
}

#' Create gaps in data if there is estimated time series.
#' 
#' @description If there is estimated data, then the estimated time series 
#' should treat non-estimated periods as gaps and non-estimated time-series 
#' should treat estimated periods as gaps. If there is not an estimatedPeriods
#' field, then there will be no gaps due to estimated data for this time series.
#' 
#' @param timeSeries list of points (data frame with at least time and values columns)
#' and estimatedPeriods (data frame with startDate and endDate columns)
#' @param timezone string giving the timezone
#' @param isDV logical saying whether or not the time series is made of daily values
#' @param inverted logical, are treating estimated
#' time periods as gaps (FALSE, default) or you are treating
#' non-estimated time periods as gaps (TRUE).
createGapsFromEstimatedPeriods <- function(timeSeries, timezone, isDV, inverted=FALSE){
  
  if("estimatedPeriods" %in% names(timeSeries) && !isEmptyOrBlank(timeSeries$estimatedPeriods)){
    
    if(isDV){
      # remove any time value for dv estimated times (should be for a whole day)
      startEstimated <- unlist(as.POSIXct(strptime(timeSeries$estimatedPeriods$startDate, "%F")))
      endEstimated <-  unlist(as.POSIXct(strptime(timeSeries$estimatedPeriods$endDate, "%F")))
    } else {
      startEstimated <- timeSeries$estimatedPeriods$startDate
      endEstimated <- timeSeries$estimatedPeriods$endDate
    }
    
    startEstimated <- flexibleTimeParse(startEstimated, timezone = timezone)
    endEstimated <- flexibleTimeParse(endEstimated, timezone = timezone)
    
    startGaps <- startEstimated
    endGaps <- endEstimated
    
    if(inverted){
      startGaps <- endEstimated
      endGaps <- startEstimated
    } 
    
  } else {
    startGaps <- c()
    endGaps <- c()
  }
  
  return(list(startGaps = startGaps, endGaps = endGaps))
}


#' Break apart timeseries data frames for any gaps.
#' 
#' Return a list of new data frames given a single data frame and the 
#' start/end times of gaps.
#' 
#' @param timeValueDF data.frame to split with at least two columns, value and time
#' @param startGaps vector of dates giving the beginning of a gap period
#' @param endGaps vector of dates giving the beginning of a gap period
#' @param timeZone string giving the timezone that the date time values are given
#' @param isDV logical saying whether or not the time series is made of daily values
applyDataGaps <- function(timeValueDF, startGaps, endGaps, timezone, isDV){
  dataWithoutGaps <- timeValueDF
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
  
  return(dataSplit)
}
