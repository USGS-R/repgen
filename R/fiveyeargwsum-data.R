parseFiveYrData <- function(reportObject){
  #Flags
  removeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMaxFlag')
  not_include <- c("not_include", "reportObject", "approvals", 'removeZeroNegativeFlag', 'excludeMinMaxFlag', 'invertedFlag', 'stat_info', 'timezone', 'type', 'approvalSeries')
  invertedFlag <- fetchReportMetadataField(reportObject, 'isInverted')

  if(is.null(invertedFlag)){
    warning("DV Hydrograph Report JSON metadata field had no 'isInverted' field. Defaulting value to FALSE.")
    invertedFlag <- FALSE
  }


  #Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  
  stat_info <- getPriorityStat(reportObject)
  statSeries <- getFiveYrTimeSeries(reportObject, stat_info$data_nm, stat_info$descr_nm, timezone)
  statSeriesEst <- list()

  #Verify we have data to plot and if so get the estimated series
  if(!isEmptyOrBlank(statSeries) && anyDataExist(statSeries[['points']])){
    statSeriesEst <- getFiveYrTimeSeries(reportObject, stat_info$data_nm, stat_info$descr_nm, timezone, estimated=TRUE)
  } else {
    return(NULL)
  }
  
  #Get max and min IV values
  max_iv <- getMinMaxIV(reportObject, "MAX", timezone, statSeries[['type']], invertedFlag)
  min_iv <- getMinMaxIV(reportObject, "MIN", timezone, statSeries[['type']], invertedFlag)

  #If we are excluding min/max points then replace them
  #with labels that go on the top of the chart.

  #Max Checking
  if(!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag){
    max_iv_label <- max_iv
    not_include <- c(not_include, 'max_iv')
  } 

  #Min Checking
  if(!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag){
    min_iv_label <- min_iv
    not_include <- c(not_include, 'min_iv')
  }

  #Approvals
  approvals <- readApprovalBar(statSeries, fetchReportMetadataField(data, "timezone"), 
                                legend_nm=fetchReportMetadataField(data, stat_info$descr_nm), snapToDayBoundaries=TRUE)
  
  #Groun Water Levels
  gw_level <- tryCatch({
    readGroundWaterLevels(data)
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
  })
  
  statSeries <- formatTimeSeriesForPlotting(statSeries)
  statSeriesEst <- formatTimeSeriesForPlotting(statSeriesEst)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)

  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

#' Get Time Series
#' 
#' @description DVHydrograph readTimeSeries() tryCatch wrapper to
#' to properly read in time series and handle read errors.
#' @param reportObject the full report JSON
#' @param series the name of the series to load
#' @param description the name of the field to use for the TS label
#' @param timezone the timezone to parse the time series points into
getFiveYrTimeSeries <- function(reportObject, series, description, timezone, estimated = FALSE){
  requiredTimeSeriesFields <- c(
    "points",
    "approvals",
    "qualifiers",
    "startTime",
    "endTime",
    "isVolumetricFlow",
    "units",
    "grades",
    "type",
    "gaps",
    "gapTolerances",
    "name"
  )
  
  returnSeries <- tryCatch({
    if(estimated){
      readEstimatedTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE, requiredFields=requiredTimeSeriesFields)
    } else {
      readTimeSeries(reportObject, series, timezone, descriptionField=description, isDV=TRUE, requiredFields=requiredTimeSeriesFields)
    }
  }, error=function(e) {
    list()
  })

  return(returnSeries)
}

parseFiveYrSupplemental <- function(data, parsedData){
  
  logAxis <- isLogged(parsedData, data[["firstDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(data, 'excludeZeroNegative'))  
  startDate <- toStartOfMonth(flexibleTimeParse(data$reportMetadata$startDate, data$reportMetadata$timezone))
  endDate <- toEndOfMonth(flexibleTimeParse(data$reportMetadata$endDate, data$reportMetadata$timezone))
  
  date_seq_mo <- seq(from=startDate, to=endDate, by="month")
  first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
  date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
  month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
  month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
  month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  
  type <- data[['firstDownChain']][['type']]
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid",
                   "first_yr", "month_label_split")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getPriorityStat <- function(data){
  descriptions <- c(data$reportMetadata$downChainDescriptions1, 
                    data$reportMetadata$downChainDescriptions2, 
                    data$reportMetadata$downChainDescriptions3)
  
  match_index <- grep(x = descriptions, pattern = "Mean")
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Max")} 
  if(length(match_index) == 0){match_index <- grep(x = descriptions, pattern = "Min")}
  
  match_names <- c("firstDownChain", "secondDownChain", "thirdDownChain")
  match_description_nm <- c("downChainDescriptions1", "downChainDescriptions2", "downChainDescriptions3")
  
  return(list(data_nm = match_names[match_index], descr_nm = match_description_nm[match_index]))
}
