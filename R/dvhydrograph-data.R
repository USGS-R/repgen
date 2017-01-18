#' Parse DV Data
#'
#' @description Parses the DV Hydrograph Data from the report
#' JSON and then furhter processes and formats that data for
#' plotting.
#' @param reportObject The raw report JSON object
parseDVData <- function(reportObject){
  #Flags
  removeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMaxFlag')
  not_include <- c("not_include", "reportObject", "approvals", 'removeZeroNegativeFlag', 'excludeMinMaxFlag', 'timezone')

  #Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')

  #Time Series Data
  stat1Timeseries <- tryCatch({
    readTimeSeries(reportObject, 'firstDownChain', timezone, descriptionField='downChainDescriptions1', isDV=TRUE)
  }, error=function(e) {
    list()
  })
  stat1TimeseriesEst <- list()

  stat2Timeseries <- tryCatch({
    readTimeSeries(reportObject, 'secondDownChain', timezone, descriptionField='downChainDescriptions2', isDV=TRUE)
  }, error=function(e) {
    list()
  })
  stat2TimeseriesEst <- list()

  stat3Timeseries <- tryCatch({
    readTimeSeries(reportObject, 'thirdDownChain', timezone, descriptionField='downChainDescriptions3', isDV=TRUE)
  }, error=function(e) {
    list()
  })
  stat3TimeseriesEst <- list()

  comparisonTimeseries <- tryCatch({
    readTimeSeries(reportObject, 'comparisonSeries', timezone, descriptionField='comparisonSeriesDescriptions', isDV=TRUE)
  }, error=function(e) {
    list()
  })
  comparisonTimeseriesEst <- list()

  #Estimated Time Series Data
  if(!isEmptyOrBlank(stat1Timeseries)){
    stat1TimeseriesEst <- readEstimatedTimeSeries(reportObject, 'firstDownChain', timezone, descriptionField='downChainDescriptions1', isDV=TRUE)
    estimated1Edges <- getEstimatedEdges(stat1Timeseries[['points']], stat1TimeseriesEst[['points']])
  }
  
  if(!isEmptyOrBlank(stat2Timeseries)){
    stat2TimeseriesEst <- readEstimatedTimeSeries(reportObject, 'secondDownChain', timezone, descriptionField='downChainDescriptions2', isDV=TRUE)
    estimated2Edges <- getEstimatedEdges(stat2Timeseries[['points']], stat2TimeseriesEst[['points']])
  }

  if(!isEmptyOrBlank(stat3Timeseries)){
    stat3TimeseriesEst <- readEstimatedTimeSeries(reportObject, 'thirdDownChain', timezone, descriptionField='downChainDescriptions3', isDV=TRUE)
    estimated3Edges <- getEstimatedEdges(stat3Timeseries[['points']], stat3TimeseriesEst[['points']])
  }

  if(!isEmptyOrBlank(comparisonTimeseries)){
    comparisonTimeseriesEst <- readEstimatedTimeSeries(reportObject, 'comparisonSeries', timezone, descriptionField='comparisonSeriesDescriptions', isDV=TRUE)
    estimatedComparisonEdges <- getEstimatedEdges(comparisonTimeseries[['points']], comparisonTimeseriesEst[['points']])
  }

  #Min/Max IV Data
  max_iv <- getMinMaxIV(reportObject, "MAX", timezone, stat1Timeseries[['type']], stat1Timeseries[['inverted']])
  min_iv <- getMinMaxIV(reportObject, "MAX", timezone, stat1Timeseries[['type']], stat1Timeseries[['inverted']])

  #If we are excluding min/max points or if we are excluding zero / negative
  #points and the max/min vlaues are zero / negative, then replace them
  #with labels that go on the top of the chart.

  #Max Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) || 
      (!isEmptyOrBlank(removeZeroNegativeFlag) && removeZeroNegativeFlag && !isEmptyOrBlank(max_iv$value) && max_iv$value <= 0)){
    max_iv_label <- max_iv
    not_include <- c(not_include, 'max_iv')
  } 

  #Min Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) 
     || (!isEmptyOrBlank(removeZeroNegativeFlag) && removeZeroNegativeFlag && !isEmptyOrBlank(min_iv$value) && min_iv$value <= 0) ){
    min_iv_label <- min_iv
    not_include <- c(not_include, 'min_iv')
  }

  #Approvals
  approvals <- readApprovalBar(stat1Timeseries, timezone, legend_nm=fetchReportMetadataField(data, "downChainDescriptions1"), snapToDayBoundaries=TRUE)

  #Field Visit Measurements
  meas_Q <- tryCatch({
    subsetByMonth(readFieldVisitMeasurementsQPoints(data), month) 
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
  })
  
  #Ground Water Levels
  gw_level <- tryCatch({
    readGroundWaterLevels(data)
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
  })

  #Format Time Series for plotting
  stat1Timeseries <- formatTimeSeriesForPlotting(stat1Timeseries, removeZeroNegativeFlag)
  stat2Timeseries <- formatTimeSeriesForPlotting(stat2Timeseries, removeZeroNegativeFlag)
  stat3Timeseries <- formatTimeSeriesForPlotting(stat3Timeseries, removeZeroNegativeFlag)
  stat1TimeseriesEst <- formatTimeSeriesForPlotting(stat1TimeseriesEst, removeZeroNegativeFlag)
  stat2TimeseriesEst <- formatTimeSeriesForPlotting(stat2TimeseriesEst, removeZeroNegativeFlag)
  stat3TimeseriesEst <- formatTimeSeriesForPlotting(stat3TimeseriesEst, removeZeroNegativeFlag)
  comparisonTimeseries <- formatTimeSeriesForPlotting(comparisonTimeseries, removeZeroNegativeFlag)
  comparisonTimeseriesEst <- formatTimeSeriesForPlotting(comparisonTimeseriesEst, removeZeroNegativeFlag)

  #Validate that we have data to plot
  if(isEmptyOrBlank(stat1Timeseries) & isEmptyOrBlank(stat2Timeseries) & isEmptyOrBlank(stat3Timeseries)){
    return(NULL)
  }
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)

  plotData <- rev(allVars)
  
  return(plotData)
}

#' Parse Reference Data
#'
#' @description Parses the specified refernec series data from 
#' the report JSON and then furhter processes and formats that 
#' data for plotting.
#' @param reportObject The raw report JSON object
#' @param series The series to fetch (secondary, tertiary, or quaternary)
#' @param description The name of the descriptionField to fetch
parseRefData <- function(reportObject, series, description){
  #Flags
  removeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMaxFlag')
  not_include <- c("not_include", "reportObject", "approvals", 'removeZeroNegativeFlag', 'excludeMinMaxFlag', 'timezone')

  #Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')

  #Time Series Data
  refTimeSeries <- tryCatch({
    readTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE)
  }, error=function(e) {
    list()
  })
  refTimeSeriesEst <- list()

  #Estimated Time Series Data
  if(!isEmptyOrBlank(refTimeSeries)){
    refTimeSeriesEst <- readEstimatedTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE)
    estimatedRefEdges <- getEstimatedEdges(refTimeSeries[['points']], refTimeSeriesEst[['points']])
  }

  #Approvals
  approvals <- readApprovalBar(refTimeSeries, timezone, legend_nm=fetchReportMetadataField(data, description), snapToDayBoundaries=TRUE)

  refTimeSeries <- formatTimeSeriesForPlotting(refTimeSeries, removeZeroNegativeFlag)
  refTimeSeriesEst <- formatTimeSeriesForPlotting(refTimeSeriesEst, removeZeroNegativeFlag)

  # need to name data so that "Switch" in dvhydrograph-styles.R will be able to match
  if(series == "secondaryReferenceSeries"){
    secondaryRefTimeSeries <- refTimeSeries
    secondaryRefTimeSeriesEst <- refTimeSeriesEst
  } else if(series == "tertiaryReferenceSeries"){
    tertiaryRefTimeSeries <- refTimeSeries
    tertiaryRefTimeSeriesEst <- refTimeSeriesEst
  } else if(series == "quaternaryReferenceSeries"){
    quaternaryRefTimeSeries <- refTimeSeries
    quaternaryRefTimeSeriesEst <- refTimeSeriesEst
  }

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)

  plotData <- rev(allVars)
  
  return(plotData)
}

#' Format time series for plotting
#'
#' @description Helper function that primes a time series for plotting
#' by extracing the points data frame from the list, adding the legend
#' name to that data frame, and removing zero/negative value rows if necessary
#' @param series The time series data to format for plotting
#' @param removeZeroNegativeFlag Whether or not to remove zero and negative values
formatTimeSeriesForPlotting <- function(series, removeZeroNegativeFlag){
  if(!isEmptyOrBlank(series[['points']])){
    seriesLegend <- rep(series[['legend.name']], nrow(series[['points']]))
    series <- series[['points']]
    series[['legend.name']] <- seriesLegend

    if(!isEmptyOrBlank(removeZeroNegativeFlag) && removeZeroNegativeFlag){
      series <- removeZeroNegative(series)
    }
  }

  return(series)
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

parseDVSupplemental <- function(data, parsedData){
  possibleNames <- c('firstDownChain', 'secondDownChain', 'thirdDownChain')
  chain_nm <- possibleNames[which(possibleNames %in% names(data))][[1]]
  if(!isEmptyOrBlank(chain_nm)){
    logAxis <- isLogged(parsedData, data[[chain_nm]][['isVolumetricFlow']], fetchReportMetadataField(data, 'excludeZeroNegative'))
    type <- data[[chain_nm]][['type']]
  }
  
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("reportObject", "parsedData", "possibleNames", "chain_nm")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
  return(supplemental)
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
  IVData <- readMinMaxIVs(reportObject, stat, timezone, inverted)
  legend_nm <- paste(IVData[['label']], tsType, ":", IVData[['value']][1])

  return(list(time=IVData[['time']][1], value=IVData[['value']][1], legend_nm=legend_nm))
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

