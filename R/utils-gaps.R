#' If There are Gaps in the Time Series, Don't Connect Them
#' 
#' @description This creates multiple line/point calls if there are gaps.
#' @param data Full report data structure.
#' @param ts Current time series data.
#' @param isDV Logic for whether this plot uses daily values or not.
splitDataGaps <- function(data, ts, isDV) {
  
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
    
    if(hasGaps) {
      startGaps <- flexibleTimeParse(data_list$gaps$startTime, timezone = data$reportMetadata$timezone)
      endGaps <- flexibleTimeParse(data_list$gaps$endTime, timezone = data$reportMetadata$timezone)
    } else {
      startGaps <- c()
      endGaps <- c()
    }
    
    if(hasEstimatedRangesAsGaps) {
      
      if(isDV){
        # remove any time value for dv estimated times (should be for a whole day)
        startEstimated <- unlist(as.POSIXct(strptime(data_list$estimatedPeriods$startDate, "%F")))
        endEstimated <-  unlist(as.POSIXct(strptime(data_list$estimatedPeriods$endDate, "%F")))
      } else {
        startEstimated <- data_list$estimatedPeriods$startDate
        endEstimated <- data_list$estimatedPeriods$endDate
      }
      
      startEstimated <- flexibleTimeParse(startEstimated, timezone = data$reportMetadata$timezone)
      endEstimated <- flexibleTimeParse(endEstimated, timezone = data$reportMetadata$timezone)
      
      startGaps <- c(startGaps, startEstimated)
      endGaps <- c(endGaps, endEstimated)
    }
    
    if(isEstimated){
      
      if(isDV){
        # remove any time value for dv estimated times (should be for a whole day)
        endEstimatedGaps <- unlist(as.POSIXct(strptime(data_list$estimatedPeriods$startDate, "%F")))
        startEstimatedGaps <- unlist(as.POSIXct(strptime(data_list$estimatedPeriods$endDate, "%F")))
      } else {
        endEstimatedGaps <- data_list$estimatedPeriods$startDate
        startEstimatedGaps <- data_list$estimatedPeriods$endDate
      }
      
      startEstimatedGaps <- c(as.POSIXct(strptime(data_list$startTime, "%F")), startEstimatedGaps)
      endEstimatedGaps <- c(as.POSIXct(strptime(data_list$endTime, "%F")), endEstimatedGaps)
      
      startEstimatedGaps <- flexibleTimeParse(startEstimatedGaps, timezone = data$reportMetadata$timezone)
      endEstimatedGaps <- flexibleTimeParse(endEstimatedGaps, timezone = data$reportMetadata$timezone)
      
      startGaps <- c(startGaps, startEstimatedGaps)
      endGaps <- c(endGaps, endEstimatedGaps)
    }
    
    #This is causing DV steps to be rendered at noon instead of on the day marks. 
    #Re-enable after refactor of time parsing functions?
    #if(isDV){ ts$time <- flexibleTimeParse(ts$time, timezone = data$reportMetadata$timezone) }
    
    startGaps <- sort(startGaps)
    endGaps <- sort(endGaps)
    
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
        dataBeforeGap <- dataWithoutGaps[which(flexibleTimeParse(dataWithoutGaps[['time']], data$reportMetadata$timezone, TRUE) <= startGaps[g]),]
        dataWithoutGaps <- dataWithoutGaps[which(flexibleTimeParse(dataWithoutGaps[['time']], data$reportMetadata$timezone, TRUE) >= endGaps[g]),]
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

#' Add Periods of Zero or Negative Data to the Gaps Field of the Specified Time
#' Series
#' 
#' @author Zack Moore
#' @author Andrew Halper
#' @param field A field name.
#' @param data Full report data structure.
#' @param isDV Context is daily values when TRUE; not-daily-values otherwise.
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @export
findZeroNegativeGaps <- function(field, data, isDV) {
  #Ensure we are supposed to remove zeros and negatives before doing so
  flagZeroNeg <- getReportMetadata(data, 'excludeZeroNegative')
  loggedData <- isLogged(data[[field]]$points, data[[field]][['isVolumetricFlow']], flagZeroNeg)
  if(!loggedData || isEmptyOrBlank(flagZeroNeg) || !flagZeroNeg){
    return(NULL)
  }
  
  uv_series <- data[[field]]$points
  if(!is.null(uv_series) & nrow(uv_series) != 0){
    # squelch "no visible binding for global variable" warnings from
    # devtools::check
    time <- NULL
    rawTime <- NULL
    value <- NULL
    prev <- NULL
    
    uv_series <- uv_series %>% 
      rename(rawTime = time) %>% 
      mutate(time = flexibleTimeParse(rawTime, data$reportMetadata$timezone, isDV)) %>% 
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
