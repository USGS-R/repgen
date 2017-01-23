

#' Parse DV Time Series
#'
#' @description Given a full report object and series name
#' reads and formats the estimated and non-estaimted time
#' series for plotting.
#' @param reportObject the full report JSON object
#' @param seriesField the JSON field name for the TS data 
#' @param descriptionField the JSON field name for the TS legend name
#' @param timezone The timezone to parse the TS points into
#' @param excludeZeroNegativeFlag whether or not to exclude zero and negative values
parseDVTimeSeries <- function(reportObject, seriesField, descriptionField, timezone, excludeZeroNegativeFlag, estimated=FALSE){
  timeSeries <- tryCatch({
    if(estimated){
      readEstimatedTimeSeries(reportObject, seriesField, descriptionField, timezone, isDV=TRUE)
    } else {
      readNonEstimatedTimeSeries(reportObject, seriesField, descriptionField, timezone, isDV=TRUE)
    }
  }, error=function(e) {
    warning(paste("Returning NULL for DV Hydro Time Series: {", seriesField, "}. Error:", e))
    NULL
  })

  if(isEmptyOrBlank(timeSeries)){
    return(NULL)
  }

  if(anyDataExist(timeSeries[['points']])){
    #apply gaps
    timeSeries <- formatTimeSeriesForPlotting(timeSeries, excludeZeroNegativeFlag)
  }

  return(timeSeries)
}

#' Parse DV Approvals
#'
#' @description Given a time series read and format the approvals
#' data from it into apprpoval bars to put on the plot.
#' @param timeSeries the time series to get approvals for
#' @param timezone the timezone to parse the approval times into
parseDVApprovals <- function(timeSeries, timezone){
  return(readApprovalBar(timeSeries, timezone, legend_nm=timeSeries[['legend.name']], snapToDayBoundaries=TRUE))
}

#' Parse DV Field Visit Measurements
#'
#' @description Given the full report JSON object reads the field
#' visit measurements and handles read errors.
#' @param reportObject the full report JSON object
parseDVFieldVisitMeasurements <- function(reportObject){
  meas_Q <- tryCatch({
    readFieldVisitMeasurementsQPoints(reportObject)
  }, error = function(e) {
    warning(paste("Returning empty data frame as DV Hydro field visit measurements. Error:", e))
    NULL
  })
  return(meas_Q)
}

#' Parse DV Ground Water Levels
#'
#' @description Given the full report JSON object reads the ground
#' water levels and handles read errors.
#' @param reportObject the full report JSON object
parseDVGroundWaterLevels <- function(reportObject){
  gw_level <- tryCatch({
    readGroundWaterLevels(reportObject)
  }, error = function(e) {
    warning(paste("Returning empty data frame as DV Hydro ground water levels. Error:", e))
    NULL
  })
  return(gw_level)
}

parseDVMinMaxIVs <- function(reportObject, timezone, type, invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag){
  #Get max and min IV points
  max_iv <- getMinMaxIV(reportObject, "MAX", timezone, type, invertedFlag)
  min_iv <- getMinMaxIV(reportObject, "MIN", timezone, type, invertedFlag)
  returnList <- list()

  #If we are excluding min/max points or if we are excluding zero / negative
  #points and the max/min vlaues are zero / negative, then replace them
  #with labels that go on the top of the chart.

  #Max Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) || 
      (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(max_iv$value) && max_iv$value <= 0)){
    returnList <- list(max_iv_label=max_iv)
  }  else {
    returnList <- list(max_iv=max_iv)
  }

  #Min Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) 
      || (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(min_iv$value) && min_iv$value <= 0) ){
    returnList <- append(returnList, list(min_iv_label=min_iv))
  } else {
    returnList <- append(returnList, list(min_iv=min_iv))
  }

  return(returnList)
}

#' Create vertical step edges between estimated and non-estimated series
#' @param stat the parsed non-estimated time series
#' @param est the parsed estimated time series
#' @return a list of vertical lines connecting steps between stat and est
#' @importFrom dplyr arrange
getEstimatedEdges <- function(stat, est){
  estEdges <- list()

  if(isEmptyOrBlank(est$value) || isEmptyOrBlank(stat$value)){
    return(estEdges)
  }
  
  est <- est[c('time', 'value')]
  stat <- stat[c('time', 'value')]

  . <- NULL # work around warnings from devtools::check()
  estData <- est %>% as.data.frame %>% mutate(set=rep('est', nrow(.)))
  statData <- stat %>% as.data.frame %>% mutate(set=rep('stat', nrow(.)))

  #Merge data into a single DF
  data <- rbind(estData, statData)
  
  # work around irrelevant warnings from devtools::check()
  time <- NULL
  y0 <- 0
  value <- 0
  set <- NULL
  
  estEdges <- data %>% arrange(time) %>%
          mutate(y0 = ifelse(set != lag(set), lag(value), NA)) %>%
          filter(set != lag(set)) %>% select(time, y0, y1 = value, newSet=set) %>% as.list

  return(estEdges)
}

#' Get the Min/Max IV Data
#'
#' @description Reads the Max/Min IV Data from the reportObject then takes the
#' first entry and formats it properly for use on the DV Hydrograph report.
#' @param reportObject the full report data
#' @param stat the stat to look up (MAX or MIN)
#' @param tsType the type of the TS (to use for the legend name)
#' @param timezone the timezone to parse the times into
#' @param inverted whether or not the TS is inverted
getMinMaxIV <- function(reportObject, stat, timezone, tsType, inverted){
  IVData <- tryCatch({
    readMinMaxIVs(reportObject, stat, timezone, inverted)
  }, error=function(e) {
    warning(paste("Returning NULL for DV hydro ", stat, " IV value. Error:", e))
    NULL
  })

  if(is.null(IVData) | isEmptyOrBlank(IVData)){
    returnList <- list()
  } else {
    legend_nm <- paste(IVData[['label']], tsType, ":", IVData[['value']][1])
    returnList <- list(time=IVData[['time']][1], value=IVData[['value']][1], legend_nm=legend_nm)
  }

  return(returnList)
}

#' Use the last point plus 1 day in seconds to extend step
#' the points do not have times, but the x limit is extended with a time to show the whole day
#' the step needs to be extended to meet this time
#' @param toPlot list of items that will be called in the do.call 
extendStep <- function(toPlot){
  #check first whether it's a feature added to the plot as a step
  isStep <- 'type' %in% names(toPlot) && toPlot[['type']] == "s"
  
  if(isStep){
    daySeconds <- 24 * 60 * 60 #1 day in seconds
    toPlot$x <- c(toPlot$x,  tail(toPlot$x, 1) + daySeconds)
    toPlot$y <- c(toPlot$y,  tail(toPlot$y,1))
  }
  
  return(toPlot)
}

