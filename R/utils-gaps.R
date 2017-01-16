
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
#' @param isDV logical saying whether or not the time series is made of daily values; default is FALSE
splitDataGapsList <- function(allVars, timezone, flagZeroNeg, isDV=FALSE){
  
  # find out what data is not a list and can't be passed into L18 (unlist messes it up)
  allVarsToSplit_i <- which(unlist(lapply(allVars, is.list)))
  allVarsNotToSplit <- allVars[which(!unlist(lapply(allVars, is.list)))]
  
  # split data that is in list form, splitDataGapsTimeSeries will handle whether
  # there are gaps or not to split it
  allVarsSplit <- unlist(lapply(allVarsToSplit_i, 
                                  function(i, allVars, timezone, flagZeroNeg, isDV){
                                    splitDataGapsTimeSeries(allVars[[i]], names(allVars)[i], 
                                                            timezone, flagZeroNeg, isDV=FALSE)
                                  }, allVars=allVars, timezone=timezone, flagZeroNeg=flagZeroNeg, isDV=isDV), 
                           recursive = FALSE)

  # change list names: [name].[name] >> [name]
  nameslist_allVarsSplit <- strsplit(names(allVarsSplit), '\\.')
  namesToKeep <- !unlist(lapply(nameslist_allVarsSplit, duplicated))
  names_allVarsSplit <- unlist(nameslist_allVarsSplit)[namesToKeep]
  names(allVarsSplit) <- names_allVarsSplit
  
  # combine split data with data that could not be split
  allVars <- append(allVarsSplit, allVarsNotToSplit)
  
  return(allVars)
}

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
#' @param isDV logical saying whether or not the time series is made of daily values; default is FALSE
splitDataGapsTimeSeries <- function(timeSeries, timeSeriesName, timezone, flagZeroNeg, isDV = FALSE){

  isEstimated <- !isEmptyOrBlank(timeSeries$estimated) && timeSeries$estimated
  
  tsDefinedGaps <- findDefinedGaps(timeSeries, timezone)
  zeroNegativeGaps <- findZeroNegativeGaps(timeValueDF = timeSeries$points, timezone, flagZeroNeg, 
                                           timeSeries$isVolumetricFlow, isDV)
  tsEstGaps <- createGapsFromEstimatedPeriods(timeSeries, timezone, isDV, inverted=isEstimated)
  
  startGaps <- c(tsDefinedGaps[['startGaps']], zeroNegativeGaps[['startGaps']], tsEstGaps[['startGaps']])
  startGaps <- sort(startGaps)
  endGaps <- c(tsDefinedGaps[['endGaps']], zeroNegativeGaps[['endGaps']], tsEstGaps[['endGaps']])
  endGaps <- sort(endGaps)
  
  dataSplit <- applyDataGaps(timeValueDF=timeSeries$points, startGaps, endGaps, timezone, isDV)
  
  # this is adding back the timeSeries metadata fields
  dataSplit <- lapply(dataSplit, function(ds, metadata){
    append(list(points = ds), metadata)
  }, metadata = timeSeries[which(!names(timeSeries) %in% c('points', 'gaps'))])
  
  # renames new time series with original name
  names(dataSplit) <- rep(timeSeriesName, length(dataSplit))
  
  return(dataSplit)
}

#' Identify the start and end dates for gaps due to zeroes and negatives
#' 
#' @description Finds periods of zero or negative data to the gaps field of 
#' the specified time series. Only identifies gaps for zeroes and negatives if 
#' they are going to be removed from the time series for logging (flagZeroNeg and
#' isLogged are TRUE). Returns a data frame with two columns, startTime and endTime.
#' 
#' @param timeValueDF data.frame to split with at least two columns, value and time
#' @param timezone string giving the timezone
#' @param flagZeroNeg logical indicating whether or not the zeros & negatives can be 
#' removed for logging the axis
#' @param isVolumetricFlow logical indicating whether the values represent volumetric 
#' flow or not (e.g. FALSE could indicate water level data)
#' @param isDV logical saying whether or not the time series is made of daily values; default is FALSE
findZeroNegativeGaps <- function(timeValueDF, timezone, flagZeroNeg, isVolumetricFlow, isDV = FALSE){
  
  if(missing(timezone) || isEmptyOrBlank(timezone)){stop("timezone is either missing or empty")}
  
  if(isEmptyOrBlank(timeValueDF)){
    startGaps <- as.POSIXct(character(), tz=timezone)
    endGaps <- as.POSIXct(character(), tz=timezone)
  } else {
    #Ensure we are supposed to remove zeros and negatives before doing so
    loggedData <- isLogged(timeValueDF, isVolumetricFlow, flagZeroNeg)
    if(!loggedData || !flagZeroNeg){
      startGaps <- as.POSIXct(character(), tz=timezone)
      endGaps <- as.POSIXct(character(), tz=timezone)
    } else {
      timeValueDF <- timeValueDF %>% 
        mutate(time = flexibleTimeParse(time, timezone, isDV)) %>% 
        select(time, values)
      
      #Select times from each point that will be excluded
      potentialNewGaps <- timeValueDF %>% filter(values > 0) %>% select(time)
      
      #Determine start / end times for gaps created by these points (exclusive date times)
      gapTolerance <- ifelse(isDV, 1, 15)
      gapUnits <- ifelse(isDV, "days", "mins")
      potentialNewGaps <- potentialNewGaps %>% mutate(diff = c(difftime(tail(strptime(time, "%Y-%m-%d %H:%M:%S"), -1),
                                                                        head(strptime(time, "%Y-%m-%d %H:%M:%S"), -1), 
                                                                        units=gapUnits),0), 
                                                      prev = lag(diff))
      startTimes <- potentialNewGaps %>% filter(diff > gapTolerance)
      endTimes <- potentialNewGaps %>% filter(prev > gapTolerance)
      
      # take exclusive gap dates and turn into inclusive by getting the next value for start dates
      # and the previous value for end dates
      exclusiveStartGaps <- startTimes[['time']]
      exclusiveEndGaps <- endTimes[['time']]
      startGaps <- timeValueDF[['time']][which(timeValueDF[['time']] %in% exclusiveStartGaps) + 1]
      endGaps <- timeValueDF[['time']][which(timeValueDF[['time']] %in% exclusiveEndGaps) - 1]
      
      if(isEmptyOrBlank(startGaps) || isEmptyOrBlank(endGaps) ){
        startGaps <- as.POSIXct(character(), tz=timezone)
        endGaps <- as.POSIXct(character(), tz=timezone)
      }
      
    }
  }

  return(list(startGaps = startGaps, endGaps = endGaps))
}

#' Find the start and end times for any possible data gaps.
#' 
#' @description Find the start and end times 
#' 
#' @param timeSeries list of points, values, gaps. The gaps field must be a 
#' data.frame with a minimum of two columns named "startTime" and "endTime".
#' @param timezone string giving the timezone
#' 
findDefinedGaps <- function(timeSeries, timezone){
  
  hasDefinedGaps <- "gaps"  %in% names(timeSeries) && !isEmptyOrBlank(timeSeries$gaps)
  
  if(hasDefinedGaps) {
    
    if(missing(timezone) || isEmptyOrBlank(timezone)){stop("timezone is either missing or empty")}
    if(!all(c('startTime', 'endTime') %in% names(timeSeries$gaps))){stop("unexpected colnames for gaps")}
    
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
#' @param isDV logical saying whether or not the time series is made of daily values; default is FALSE
#' @param inverted logical, you are treating estimated time periods as gaps (FALSE, default)
#' or you are treating non-estimated time periods as gaps (TRUE).
createGapsFromEstimatedPeriods <- function(timeSeries, timezone, isDV = FALSE, inverted=FALSE){
  
  hasEstimatedPeriods <- "estimatedPeriods" %in% names(timeSeries) && !isEmptyOrBlank(timeSeries[['estimatedPeriods']])
  
  if(hasEstimatedPeriods){
    
    if(missing(timezone) || isEmptyOrBlank(timezone)){stop("timezone is either missing or empty")}
    
    if(isDV){
      # remove any time value for dv estimated times (should be for a whole day)
      startEstimated <- unlist(as.POSIXct(strptime(timeSeries[['estimatedPeriods']][['startDate']], "%F")))
      endEstimated <-  unlist(as.POSIXct(strptime(timeSeries[['estimatedPeriods']][['endDate']], "%F")))
    } else {
      startEstimated <- timeSeries[['estimatedPeriods']][['startDate']]
      endEstimated <- timeSeries[['estimatedPeriods']][['endDate']]
    }
    
    startEstimated <- flexibleTimeParse(startEstimated, timezone = timezone, shiftTimeToNoon = isDV)
    endEstimated <- flexibleTimeParse(endEstimated, timezone = timezone, shiftTimeToNoon = isDV)
    
    startGaps <- startEstimated
    endGaps <- endEstimated
    
    if(inverted){
      if(isEmptyOrBlank(timeSeries[['points']])){stop("points data.frame is empty")}
      if(!all(c('time', 'values') %in% names(timeSeries[['points']]))){stop('unexpected colnames for points data.frame')}
      
      # find start and end dates for any non-estimated period that occurs
      # prior to the current estimated period
      startGaps_i <- as.POSIXct(character(), tz=timezone)
      endGaps_i <- as.POSIXct(character(), tz=timezone)
      for(i in seq_along(startGaps)){
        notEst_i <- which(timeSeries[['points']][['time']] < startGaps[i])
        # when there is more than one gap, don't include dates before the last
        # estimatedPeriod end date
        if(i > 1){
          notEst_i <- which(timeSeries[['points']][['time']] < startGaps[i] &
                              timeSeries[['points']][['time']] > endGaps[i-1])
        }
        notEst <- timeSeries[['points']][['time']][notEst_i]
        startGaps_i <- c(startGaps_i, head(notEst, 1))
        endGaps_i <- c(endGaps_i, tail(notEst, 1))
      }
      
      # if there are non estimated periods in the time series after the final
      # estimated period, this adds the start and end dates
      notEstTrailing_i <- which(timeSeries[['points']][['time']] > endGaps[i])
      notEstTrailing <- timeSeries[['points']][['time']][notEstTrailing_i]
      if(!isEmptyOrBlank(notEstTrailing)){
        startGaps <- c(startGaps_i, head(notEstTrailing, 1))
        endGaps <- c(endGaps_i, tail(notEstTrailing, 1))
      } else {
        startGaps <- startGaps_i
        endGaps <- endGaps_i
      }
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
#' @param startGaps vector of dates giving the beginning of a gap period (this time
#' and associated value are included in the returned data). Listed sequentially.
#' @param endGaps vector of dates giving the beginning of a gap period (this time
#' and associated value are included in the returned data). Listed sequentially.
#' @param timeZone string giving the timezone that the date time values are given
#' @param isDV logical saying whether or not the time series is made of daily values; default is FALSE
applyDataGaps <- function(timeValueDF, startGaps, endGaps, timezone, isDV = FALSE){

  if(missing(timeValueDF) || isEmptyOrBlank(timeValueDF)){stop("timeValueDF is either missing or empty")} 
  if(missing(startGaps) || missing(endGaps)){stop("start or end gaps are missing")} 
  if(missing(timezone) || isEmptyOrBlank(timezone)){stop("timezone is either missing or empty")}
  if(length(startGaps) != length(endGaps)){stop("start and end gaps are different lengths")}
  
  dataWithoutGaps <- timeValueDF
  dataSplit <- list()
  
  for(g in seq_along(startGaps)){
    
    formatted_dates <- flexibleTimeParse(dataWithoutGaps[['time']], timezone, shiftTimeToNoon = isDV)
    dataBeforeGap <- dataWithoutGaps[which(formatted_dates <= startGaps[g]),]
    dataWithoutGaps <- dataWithoutGaps[which(formatted_dates >= endGaps[g]),]
    
    # only add dataBeforeGap if it exists, sometimes gap dates are earlier than any data 
    if(!isEmptyVar(dataBeforeGap)) { 
      dataSplit <- append(dataSplit, list(dataBeforeGap))
    }
    
    #leave the loop when there is no data left to split, sometimes gap dates are later than any
    if(isEmptyVar(dataWithoutGaps)) { 
      break  
    }
    
  }
  
  # if loop is skipped, dataWithoutGaps == timeValueDF and is the only thing returned
  # if loop is entered, dataSplit should have other values, and remaining data in dataWithoutGaps is added
  if(!isEmptyVar(dataWithoutGaps)){
    dataSplit <- append(dataSplit, list(dataWithoutGaps))
  } 
  
  return(dataSplit)
}
