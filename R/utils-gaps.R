
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
  if(!isEmptyOrBlank(allVarsSplit) && !isEmptyOrBlank(names(allVarsSplit))){
    nameslist_allVarsSplit <- strsplit(names(allVarsSplit), '\\.')
    namesToKeep <- !unlist(lapply(nameslist_allVarsSplit, duplicated))
    names_allVarsSplit <- unlist(nameslist_allVarsSplit)[namesToKeep]
    names(allVarsSplit) <- names_allVarsSplit
  }
  
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
#' @param timeSeries current timeseries data as a list, required: estimated, isVolumetricFlow,
#' points data frame
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
  
  # WHAT HAPPENS WHEN THERE ARE OVERLAPPING GAPS?
  # OR STARTGAP = 2010-10-01, 2010-10-15 & ENDGAP = 2010-10-31, 2010-10-20
  # SORTING WILL SCREW THIS UP
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
#' @importFrom dplyr select
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
      time <- NULL #make R checks not detect the uses of these vars below as global vars
      value <- NULL
      lag <- NULL
      prev <- NULL
      
      timeValueDF <- timeValueDF %>% 
        mutate(time = flexibleTimeParse(time, timezone, isDV)) %>% 
        dplyr::select(time, value)
      
      #Select times from each point that will be excluded
      potentialNewGaps <- timeValueDF %>% filter(value > 0) %>% dplyr::select(time)
      
      #Determine start / end times for gaps created by these points (exclusive date times)
      gapTolerance <- ifelse(isDV, 1, 15)
      gapUnits <- ifelse(isDV, "days", "mins")
      potentialNewGaps <- potentialNewGaps %>% mutate(diff = c(difftime(tail(strptime(time, "%Y-%m-%d %H:%M:%S"), -1),
                                                                        head(strptime(time, "%Y-%m-%d %H:%M:%S"), -1), 
                                                                        units=gapUnits),0), 
                                                      prev = lag(diff))
      startTimes <- potentialNewGaps %>% filter(diff > gapTolerance)
      endTimes <- potentialNewGaps %>% filter(prev > gapTolerance)
      
      # the start and end times are already exclusive because we've eliminated the
      # values that are zero or negative, so only the values next to those remain
      startGaps <- startTimes[['time']]
      endGaps <- endTimes[['time']]
      
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
    startGaps <- as.POSIXct(character(), tz=timezone)
    endGaps <- as.POSIXct(character(), tz=timezone)
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
#' @importFrom lubridate minutes
createGapsFromEstimatedPeriods <- function(timeSeries, timezone, isDV = FALSE, inverted=FALSE){
  
  hasEstimatedPeriods <- "estimatedPeriods" %in% names(timeSeries) && !isEmptyOrBlank(timeSeries[['estimatedPeriods']])
  
  if(hasEstimatedPeriods){
    if(isEmptyOrBlank(timeSeries[['points']])){stop("points data.frame is empty")}
    if(!all(c('time', 'value') %in% names(timeSeries[['points']]))){stop('unexpected colnames for points data.frame')}
    if(missing(timezone) || isEmptyOrBlank(timezone) || timezone == ""){stop("timezone is either missing or empty")}
    
    #sort the estimated periods dataframe by start date otherwise bad things happen later
    timeSeries[["estimatedPeriods"]] <- timeSeries[["estimatedPeriods"]][order(timeSeries[["estimatedPeriods"]][["startDate"]]),]
    
    if(isDV){
      # remove any time value for dv estimated times (should be for a whole day)
      startEstimated <- unlist(as.POSIXct(strptime(timeSeries[['estimatedPeriods']][['startDate']], "%F")))
      endEstimated <-  unlist(as.POSIXct(strptime(timeSeries[['estimatedPeriods']][['endDate']], "%F")))
      gapIncrement <- days(1)
    } else {
      startEstimated <- timeSeries[['estimatedPeriods']][['startDate']]
      endEstimated <- timeSeries[['estimatedPeriods']][['endDate']]
      gapIncrement <- minutes(15)
    }
    
    # #sort periods in case they're unsorted
    # startEstimated %>% arrange(start)
    
    startEstimated <- flexibleTimeParse(startEstimated, timezone = timezone, shiftTimeToNoon = isDV)
    endEstimated <- flexibleTimeParse(endEstimated, timezone = timezone, shiftTimeToNoon = isDV)
     
    #*Qualifiers*, start dates are inclusive and end dates are exclusive. (AKA estimated range)
    time_data <- timeSeries[['points']][['time']]

    #Inverted means we are looking at a time series that is estimated so the gaps should
    #come from the non-estimated periods.
    if(inverted){
      # gaps are the exclusive ranges for non-estimated data
      startGaps <- as.POSIXct(character(), tz=timezone)
      endGaps <- as.POSIXct(character(), tz=timezone)
      for(i in seq_along(startEstimated)){
        #In an estimated time series the first data point should be equal to the start time of the
        #first estimated period in the time series. This should not be marked as an end gap because
        #it is the start of the time series and there was no start gap before it. So we ignore this
        # data point and only create gaps from data points that are after the first point in the series.
        if(startEstimated[i] > time_data[[1]]){
          # estimated start times are inclusive, so the date is exclusive when
          # using it for non-estimated data
          endGaps <- c(endGaps, startEstimated[i])
          
          if(i == 1){ 
            #if it's the first estimated period, use the head of the data
            #as the start of the non-estimated data (exclusive, so subtract gapIncrement)
            # FYI: needs to be <= because if estimatedPeriod is at the beginning of the
            # data, notEst_i would come back empty & head() will fail.
            notEst_i <- time_data[which(time_data <= endGaps)]
            startGaps <- c(startGaps, head(notEst_i, 1) - gapIncrement)
          } else {
            #if it's past the first estimated period, use the ending of the previous 
            #gap period as the start of the current non-estimated data. Since estimated end
            #times are exclusive, you will need to subtract the gapIncrement (to find the 
            #time for last estimated value)
            startGaps <- c(startGaps, endEstimated[i-1] - gapIncrement)
          }
        }
      }
      
      # if there are is more data in the time series after the final
      # endEstimated, this adds the start and end dates for that non-
      # estimated data
      trailing_i <- time_data[which(time_data > endEstimated[i])]
      if(!isEmptyOrBlank(trailing_i)){
        startGaps <- c(startGaps, endEstimated[i] - gapIncrement)
        endGaps <- c(endGaps, tail(trailing_i, 1) + gapIncrement)
      }
      
    } else {
      # gaps are the exclusive ranges for estimated data
      
      # estimated start times are inclusive, so we need to subtract
      # the gapIncrement to get an exclusive time
      startGaps <- startEstimated - gapIncrement
      # estimated end times are already exclusive
      endGaps <- endEstimated
    }
    
  } else {
    startGaps <- as.POSIXct(character(), tz=timezone)
    endGaps <- as.POSIXct(character(), tz=timezone)
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
#' @param timezone string giving the timezone that the date time values are given
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
