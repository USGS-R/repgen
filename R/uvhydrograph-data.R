#'@importFrom lubridate parse_date_time

getMonths <- function(data){
  corr <- getTimeSeries(data, "downsampledPrimarySeries")
  uncorr <- getTimeSeries(data, "downsampledPrimarySeriesRaw")
  months <- unique(c(corr$month, uncorr$month))
  return(sort(months))
}

parseUVData <- function(data, plotName, month) {
  
  if(plotName == "primary"){
    
    corr_UV <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeries" ), month)
    est_UV <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeries", estimatedOnly=TRUE), month)
    uncorr_UV <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeriesRaw" ), month)
    comp_UV <- subsetByMonth(getTimeSeries(data, "downsampledComparisonSeries" ), month)
    water_qual <- subsetByMonth(getWaterQualityMeasurements(data), month)
    
    series_corr <- subsetByMonth(getCorrections(data, "primarySeriesCorrections"), month)
    meas_Q <- subsetByMonth(getFieldVisitMeasurementsQPoints(data), month)  
    
    approvals_uv <- getApprovals(data, chain_nm="downsampledPrimarySeries", legend_nm=paste("UV", getTimeSeriesLabel(data, "downsampledPrimarySeries")),
                                        appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), 
                                        subsetByMonth=TRUE, month=month, isDV=FALSE)
    approvals_dv_max <- getApprovals(data, chain_nm="derivedSeriesMax", legend_nm=paste("DV Max", getTimeSeriesLabel(data, "derivedSeriesMax")),
                                            appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                            subsetByMonth=TRUE, month=month, point_type=24, isDV=TRUE)
    approvals_dv_mean <- getApprovals(data, chain_nm="derivedSeriesMean", legend_nm=paste("DV Mean", getTimeSeriesLabel(data, "derivedSeriesMean")),
                                            appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                            subsetByMonth=TRUE, month=month, point_type=21, isDV=TRUE)
    approvals_dv_median <- getApprovals(data, chain_nm="derivedSeriesMedian", legend_nm=paste("DV Median", getTimeSeriesLabel(data, "derivedSeriesMedian")),
                                            appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                            subsetByMonth=TRUE, month=month, point_type=26, isDV=TRUE)
    approvals_dv_min <- getApprovals(data, chain_nm="derivedSeriesMin", legend_nm=paste("DV Min", getTimeSeriesLabel(data, "derivedSeriesMin")),
                                            appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                            subsetByMonth=TRUE, month=month, point_type=25, isDV=TRUE)
     
    approvals <- append(approvals_uv, approvals_dv_max)
    approvals <- append(approvals, approvals_dv_mean)
    approvals <- append(approvals, approvals_dv_median)
    approvals <- append(approvals, approvals_dv_min)
  }
  
  if(plotName == "secondary"){
    
    corr_UV2 <- subsetByMonth(getTimeSeries(data, "downsampledSecondarySeries"), month)
    est_UV2 <- subsetByMonth(getTimeSeries(data, "downsampledSecondarySeries", estimatedOnly=TRUE), month)
    uncorr_UV2 <- subsetByMonth(getTimeSeries(data, "downsampledSecondarySeriesRaw"), month)
    
    series_corr2 <- subsetByMonth(getCorrections(data, "secondarySeriesCorrections"), month)
    
    effect_shift <- subsetByMonth(getTimeSeries(data, "effectiveShifts"), month)
    gage_height <- subsetByMonth(getMeanGageHeights(data), month)
    gw_level <- subsetByMonth(getGroundWaterLevels(data), month)
    meas_shift <- subsetByMonth(getFieldVisitMeasurementsShifts(data), month)
    
    ref_readings <- subsetByMonth(getReadings(data, "reference"), month)
    csg_readings <- subsetByMonth(getReadings(data, "crestStage"), month)
    #hwm_readings <- subsetByMonth(getReadings(data, "waterMark"), month)
    
    approvals <- getApprovals(data, chain_nm="downsampledSecondarySeries", legend_nm=getTimeSeriesLabel(data, "downsampledSecondarySeries"),
                              appr_var_all=c("appr_approved", "appr_inreview", "appr_working"),
                              subsetByMonth=TRUE, month=month, isDV=FALSE)
  }
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "plotName", "month", "approvals", "approvals_uv", 
                                                  "approvals_dv_max", "approvals_dv_mean", "approvals_dv_median",
                                                  "approvals_dv_min"))]
  allVars <- allVars[unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]

  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

#'@importFrom lubridate days_in_month
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@importFrom lubridate ymd
parseUVSupplemental <- function(data, plotName, pts) {
  if(plotName == "primary"){
    
    if(!is.null(pts$corr_UV)){
      lims_UV <- getUvhLims(pts$corr_UV)
    } else {
      lims_UV <- getUvhLims(pts$uncorr_UV)
    }
    
    primary_lbl <- getTimeSeriesLabel(data, "downsampledPrimarySeries")
    date_lbl <- paste(lims_UV$xlim[1], "through", lims_UV$xlim[2])
    comp_UV_lbl <- data$reportMetadata$comparisonStationId
    comp_UV_type <- data[['comparisonSeries']]$type
    dates <- seq(lims_UV$xlim[1], lims_UV$xlim[2], by="days")
    
    logAxis <- isLogged(data, pts, "derivedSeriesMean")
    
    days <- seq(days_in_month(dates[1]))
    year <- year(dates[1])
    month <- month(dates[1])
    plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=data$reportMetadata$timezone)), length=tail(days,1), by="days")
    
  }
  
  if(plotName == "secondary"){
 
    lims_UV2 <- getUvhLims(pts$corr_UV2)
    date_lbl2 <- paste(lims_UV2$xlim[1], "through", lims_UV2$xlim[2])
    secondary_lbl <- getTimeSeriesLabel(data, "downsampledSecondarySeries")
    sec_dates <- seq(lims_UV2$xlim[1], lims_UV2$xlim[2], by="days")
    tertiary_lbl <- getTimeSeriesLabel(data, "effectiveShifts")
    sec_units <- data$secondarySeries$units
    
    days <- seq(days_in_month(sec_dates[1]))
    year <- year(sec_dates[1])
    month <- month(sec_dates[1])
    plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=data$reportMetadata$timezone)), length=tail(days,1), by="days")
    
  }
  
  #for any one plot, all data must be either inverted or not
  isInverted <- all(na.omit(unlist(lapply(names(pts), getInverted, plotName=plotName, data = data))))
  
  allVars <- as.list(environment())
  allVars <- allVars[unlist(lapply(allVars, function(x) {!is.null(x)} ),FALSE,FALSE)]
  allVars <- allVars[unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} ),FALSE,FALSE)]
  supplemental <- allVars[which(!names(allVars) %in% c("data", "plotName", "pts"))]
  
  return(supplemental)
}

correctionsTable <- function(data) {
  if (any(names(data) %in% c("series_corr", "series_corr2"))) {
    corrections <- data[[grep("series_corr", names(data))]]
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), corrections$comment))
    colnames(corrections_table) <- c("", "Comments")
    return(corrections_table)
  } else (return(corrections_table <- NULL))
}

parseLabelSpacing <- function(data, info) {
  
  if (names(data) %in% c("series_corr", "series_corr2")){
    limits <- info[[grep("lims_UV", names(info))]]
    y_positions <- rep(limits$ylim[2], length(data[[1]]$time))
    differences <- as.numeric(diff(data[[1]]$time))
    if(length(differences) > 0) {
      for (i in seq_len(length(differences))) {
        if(abs(differences[i]) < 86400) {y_positions[i+1] <- y_positions[i]-(0.04*info$lims_UV$ylim[2])}
        i <- i + 1
      }
      spacingInfo <- list(y=y_positions, label=seq(length(data[[1]]$time)))
    } else {
      spacingInfo <- list(y=y_positions, label=seq(length(data[[1]]$time)))
    }
  } else {
    spacingInfo <- list()
  }
  
  return(spacingInfo)
}


subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}


##### GET functions
#'Put the SIMS url (if it exists) into the base of the report
#'@param data coming in to create a plot which may have sims info
#'@export
#'@rdname getSimsUrl
getSimsUrl<- function(data){
  url <- data$simsUrl
  if(is.null(url) || url == '') {
    url <- "SIMS URL: NA"
  } else {
    url <- paste("SIMS URL:", url) 
  }
  return(url)
}


getMeanGageHeights<- function(ts, ...){
  y <- ts$fieldVisitMeasurements[['meanGageHeight']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  n <- ts$fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, n=n, month=month, stringsAsFactors = FALSE))
}


getUvhLims <- function(pts = NULL, xMinField = 'time', xMaxField = 'time', yMinField = 'value', yMaxField = 'value'){
  x_mx <- max(pts[[xMaxField]], na.rm = TRUE)
  x_mn <- min(pts[[xMinField]], na.rm = TRUE)
  y_mx <- max(pts[[yMaxField]], na.rm = TRUE)
  y_mn <- min(pts[[yMinField]], na.rm = TRUE)
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in points. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}


getReadings <- function(ts, field) {
  time <- as.POSIXct(strptime(ts[['readings']][['time']], "%FT%T"))
  value <- as.numeric(ts[['readings']][['value']])
  type <- ts[['readings']][['type']]
  uncertainty <- as.numeric(ts[['readings']][['uncertainty']])
  month <- format(time, format = "%y%m") #for subsetting later by month
  
  if (field == "reference") {
    index <- which(type == "ReferencePrimary")
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  } else if (field == "crestStage") {
    typeIndex <- which(type == "ExtremeMax")
    monitorIndex <- which(ts[['readings']][['monitoringMethod']]=="Crest stage")
    index <- ifelse(all(is.na(match(typeIndex, monitorIndex))), 0, match(typeIndex, monitorIndex))
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  } else if (field == "waterMark") {
    index <- which(type == "") ### What is the condition for high water mark?
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  }
  
  
  return(data.frame(time=x, value=y, uncertainty=uncertainty, month=month, stringsAsFactors = FALSE))
  
}

getInverted <- function(data, renderName, plotName) {
  if (plotName == "primary") {   
    dataName <- switch(renderName,
                       corr_UV = "downsampledPrimarySeries",
                       est_UV = "downsampledPrimarySeries",
                       uncorr_UV = "downsampledPrimarySeriesRaw",
                       comp_UV = "downsampledComparisonSeries",  
                       water_qual = "downsampledPrimarySeries",  #if primary is flipping, this will flip
                       max_DV = "derivedSeriesMax",
                       mean_DV = "derivedSeriesMean",
                       median_DV = "derivedSeriesMedian",
                       min_DV = "derivedSeriesMin")
    
  } else if (plotName == "secondary") {   
    dataName <- switch(renderName,
                       corr_UV2 = "secondarySeries",
                       est_UV2 = "secondarySeries",
                       uncorr_UV2 = "secondarySeriesRaw")
  }
  
  isInverted <- ifelse(!is.null(dataName), data[[dataName]][['inverted']], NA)
  return(isInverted)
}
