#' Parse DV Data
#'
#' @description Parses the DV Hydrograph Data from the report
#' JSON and then furhter processes and formats that data for
#' plotting. Reutrns a list with 2 parts:
#' 1). dvData - data that is ready for plotting
#' 2). dvInfo - supplemental data that is used in plotting logic
#' but should not be plotted.
#' @param reportObject The raw report JSON object
parseDVData <- function(reportObject){
  #Flags
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMaxFlag')
  invertedFlag <- fetchReportMetadataField(reportObject, 'isInverted')

  if(is.null(invertedFlag)){
    warning("DV Hydrograph Report JSON metadata field had no 'isInverted' field. Defaulting value to FALSE.")
    invertedFlag <- FALSE
  }

  not_include <- c("not_include", "reportObject", "approvals", 'excludeZeroNegativeFlag', 'excludeMinMaxFlag', 'timezone', 'type', 'logAxis', 'approvalSeries', 'timeSeriesCount', 'invertedFlag')

  #Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')

  #Time Series Data
  timeSeriesCount <- 0

  stat1Timeseries <- getDVHydroTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone)
  stat1TimeseriesEst <- list()
  stat2Timeseries <- getDVHydroTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone)
  stat2TimeseriesEst <- list()
  stat3Timeseries <- getDVHydroTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone)
  stat3TimeseriesEst <- list()
  comparisonTimeseries <- getDVHydroTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesDescriptions', timezone)
  comparisonTimeseriesEst <- list()

  #Estimated Time Series Data
  if(!isEmptyOrBlank(stat1Timeseries) && anyDataExist(stat1Timeseries[['points']])){
    stat1TimeseriesEst <- getDVHydroTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone, estimated=TRUE)
    estimated1Edges <- getEstimatedEdges(stat1Timeseries[['points']], stat1TimeseriesEst[['points']])
    timeSeriesCount <- timeSeriesCount + 1
  }
  
  if(!isEmptyOrBlank(stat2Timeseries) && anyDataExist(stat2Timeseries[['points']])){
    stat2TimeseriesEst <- getDVHydroTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone, estimated=TRUE)
    estimated2Edges <- getEstimatedEdges(stat2Timeseries[['points']], stat2TimeseriesEst[['points']])
    timeSeriesCount <- timeSeriesCount + 1
  }

  if(!isEmptyOrBlank(stat3Timeseries) && anyDataExist(stat3Timeseries[['points']])){
    stat3TimeseriesEst <- getDVHydroTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone, estimated=TRUE)
    estimated3Edges <- getEstimatedEdges(stat3Timeseries[['points']], stat3TimeseriesEst[['points']])
    timeSeriesCount <- timeSeriesCount + 1
  }

  if(!isEmptyOrBlank(comparisonTimeseries) && anyDataExist(comparisonTimeseries[['points']])){
    comparisonTimeseriesEst <- getDVHydroTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesDescriptions', timezone, estimated=TRUE)
    estimatedComparisonEdges <- getEstimatedEdges(comparisonTimeseries[['points']], comparisonTimeseriesEst[['points']])
  }

  #Validate that we have data to plot
  if(timeSeriesCount == 0){
    return(NULL)
  }

  #Get the time series type, logAxis, and approval series from the first
  #time series that exists in the data.
  if(!isEmptyOrBlank(stat1Timeseries)){
    type <- stat1Timeseries[['type']]
    logAxis <- isLogged(stat1Timeseries[['points']], stat1Timeseries[['isVolumetricFlow']], excludeZeroNegativeFlag)
    approvalSeries <- stat1Timeseries
  } else if(!isEmptyOrBlank(stat2Timeseries)){
    type <- stat2Timeseries[['type']]
    logAxis <- isLogged(stat2Timeseries[['points']], stat2Timeseries[['isVolumetricFlow']], excludeZeroNegativeFlag)
    approvalSeries <- stat2Timeseries
  } else if(!isEmptyOrBlank(stat3Timeseries)){
    type <- stat3Timeseries[['type']]
    logAxis <- isLogged(stat3Timeseries[['points']], stat3Timeseries[['isVolumetricFlow']], excludeZeroNegativeFlag)
    approvalSeries <- stat3Timeseries
  } else {
    type <- ""
    logAxis <- FALSE
    approvalSeries <- list()
  }

  #Get max and min IV points
  max_iv <- getMinMaxIV(reportObject, "MAX", timezone, type, invertedFlag)
  min_iv <- getMinMaxIV(reportObject, "MIN", timezone, type, invertedFlag)

  #If we are excluding min/max points or if we are excluding zero / negative
  #points and the max/min vlaues are zero / negative, then replace them
  #with labels that go on the top of the chart.

  #Max Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) || 
      (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(max_iv$value) && max_iv$value <= 0)){
    max_iv_label <- max_iv
    not_include <- c(not_include, 'max_iv')
  } 

  #Min Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) 
     || (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(min_iv$value) && min_iv$value <= 0) ){
    min_iv_label <- min_iv
    not_include <- c(not_include, 'min_iv')
  }

  #Approvals
  approvals <- readApprovalBar(approvalSeries, timezone, legend_nm=approvalSeries[['legend.name']], snapToDayBoundaries=TRUE)

  #Field Visit Measurements
  meas_Q <- tryCatch({
    readFieldVisitMeasurementsQPoints(reportObject)
  }, error = function(e) {
    warning(paste("Returning empty data frame as DV Hydro field visit measurements. Error:", e))
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
  })
  
  #Ground Water Levels
  gw_level <- tryCatch({
    readGroundWaterLevels(reportObject)
  }, error = function(e) {
    warning(paste("Returning empty data frame as DV Hydro ground water levels. Error:", e))
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
  })

  #Format Time Series for plotting
  stat1Timeseries <- formatTimeSeriesForPlotting(stat1Timeseries, excludeZeroNegativeFlag)
  stat2Timeseries <- formatTimeSeriesForPlotting(stat2Timeseries, excludeZeroNegativeFlag)
  stat3Timeseries <- formatTimeSeriesForPlotting(stat3Timeseries, excludeZeroNegativeFlag)
  stat1TimeseriesEst <- formatTimeSeriesForPlotting(stat1TimeseriesEst, excludeZeroNegativeFlag)
  stat2TimeseriesEst <- formatTimeSeriesForPlotting(stat2TimeseriesEst, excludeZeroNegativeFlag)
  stat3TimeseriesEst <- formatTimeSeriesForPlotting(stat3TimeseriesEst, excludeZeroNegativeFlag)
  comparisonTimeseries <- formatTimeSeriesForPlotting(comparisonTimeseries, excludeZeroNegativeFlag)
  comparisonTimeseriesEst <- formatTimeSeriesForPlotting(comparisonTimeseriesEst, excludeZeroNegativeFlag)

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)
  allVars <- list(dvInfo = list(type=type, logAxis=logAxis), dvData = allVars)

  plotData <- rev(allVars)
  
  return(plotData)
}

#' Get DV Hydro Time Series
#' 
#' @description DVHydrograph readTimeSeries() tryCatch wrapper to
#' to properly read in time series and handle read errors.
#' @param reportObject the full report JSON
#' @param series the name of the series to load
#' @param description the name of the field to use for the TS label
#' @param timezone the timezone to parse the time series points into
getDVHydroTimeSeries <- function(reportObject, series, description, timezone, estimated=FALSE){
  returnSeries <- tryCatch({
    if(estimated){
      readEstimatedTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE)
    } else {
      readNonEstimatedTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE)
    }
  }, error=function(e) {
    warning(paste("Returning empty list as DV Hydro Time Series. Error:", e))
    list()
  })

  return(returnSeries)
}

#' Parse Reference Data
#'
#' @description Parses the specified refernec series data from 
#' the report JSON and then furhter processes and formats that 
#' data for plotting.
#' @param reportObject The raw report JSON object
#' @param series The series to fetch (secondary, tertiary, or quaternary)
#' @param description The name of the descriptionField to fetch
parseRefDVData <- function(reportObject, series, description){
  #Flags
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMaxFlag')
  not_include <- c("not_include", "reportObject", "approvals", 'excludeZeroNegativeFlag', 'refTimeSeries', 'refTimeSeriesEst', 'excludeMinMaxFlag', 'timezone', 'series', 'type', 'logAxis', 'description', 'invertedFlag')

  #Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')

  #Time Series Data
  refTimeSeries <- getDVHydroTimeSeries(reportObject, series, description, timezone)
  refTimeSeriesEst <- list()

  #Check for Valid Data for plotting
  if(isEmptyOrBlank(refTimeSeries) || !anyDataExist(refTimeSeries[['points']])){
    return(NULL)
  }

  #Estimated Time Series Data
  if(!isEmptyOrBlank(refTimeSeries)){
    refTimeSeriesEst <- readEstimatedTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE)
    estimatedRefEdges <- getEstimatedEdges(refTimeSeries[['points']], refTimeSeriesEst[['points']])
  }

  type <- refTimeSeries[['type']]
  logAxis <- isLogged(refTimeSeries[['points']], refTimeSeries[['isVolumetricFlow']], excludeZeroNegativeFlag)

  #Approvals
  approvals <- readApprovalBar(refTimeSeries, timezone, legend_nm=fetchReportMetadataField(reportObject, description), snapToDayBoundaries=TRUE)

  refTimeSeries <- formatTimeSeriesForPlotting(refTimeSeries, excludeZeroNegativeFlag)
  refTimeSeriesEst <- formatTimeSeriesForPlotting(refTimeSeriesEst, excludeZeroNegativeFlag)

  # need to name data so that "Switch" in dvhydrograph-styles.R will be able to match
  if(series == "secondaryReferenceTimeSeries"){
    secondaryRefTimeSeries <- refTimeSeries
    secondaryRefTimeSeriesEst <- refTimeSeriesEst
  } else if(series == "tertiaryReferenceTimeSeries"){
    tertiaryRefTimeSeries <- refTimeSeries
    tertiaryRefTimeSeriesEst <- refTimeSeriesEst
  } else if(series == "quaternaryReferenceTimeSeries"){
    quaternaryRefTimeSeries <- refTimeSeries
    quaternaryRefTimeSeriesEst <- refTimeSeriesEst
  }

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)
  allVars <- list(refInfo = list(type=type, logAxis=logAxis), refData = allVars)


  plotData <- rev(allVars)
  
  return(plotData)
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
    returnList <- list(time=as.POSIXct(NA), value=as.numeric(NA), legend_nm=as.character(NA))
    returnList <- na.omit(returnList)
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

