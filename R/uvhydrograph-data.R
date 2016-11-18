#'@importFrom lubridate parse_date_time

getMonths <- function(data, useDownsampled = FALSE) {
  if (useDownsampled) {
    primarySeriesName <- "downsampledPrimarySeries"
    primarySeriesRawName <- "downsampledPrimarySeriesRaw"
  } else {
    primarySeriesName <- "primarySeries"
    primarySeriesRawName <- "primarySeriesRaw"
  }
  
  corr <- getTimeSeries(data, primarySeriesName)
  uncorr <- getTimeSeries(data, primarySeriesRawName)
  months <- unique(c(corr$month, uncorr$month))
  return(sort(months))
}

parseUVData <- function(data, plotName, month, useDownsampled=FALSE) {
  if(useDownsampled) {
    primarySeriesName <- "downsampledPrimarySeries"
    primarySeriesRawName <- "downsampledPrimarySeriesRaw"
    referenceSeriesName <- "downsampledReferenceSeries"
    comparisonSeriesName <- "downsampledComparisonSeries"
    upchainSeriesName <- "downsampledUpchainSeries"
    upchainSeriesRawName <- "downsampledUpchainSeriesRaw"
  } else {
    primarySeriesName <- "primarySeries"
    primarySeriesRawName <- "primarySeriesRaw"
    referenceSeriesName <- "referenceSeries"
    comparisonSeriesName <- "comparisonSeries"
    upchainSeriesName <- "upchainSeries"
    upchainSeriesRawName <- "upchainSeriesRaw"
  }
  
  if(plotName == "primary"){
    
    corr_UV <- subsetByMonth(getTimeSeries(data, primarySeriesName ), month)
    est_UV <- subsetByMonth(getTimeSeries(data, primarySeriesName, estimatedOnly=TRUE), month)
    uncorr_UV <- subsetByMonth(getTimeSeries(data, primarySeriesRawName ), month)
    comp_UV <- subsetByMonth(getTimeSeries(data, comparisonSeriesName ), month)
    water_qual <- subsetByMonth(getWaterQualityMeasurements(data), month)
    
    series_corr <- subsetByMonth(getCorrections(data, "primarySeriesCorrections"), month)
    meas_Q <- subsetByMonth(getFieldVisitMeasurementsQPoints(data), month)  

    #Add reference data to the plot if it is available and this is a Q plot type
    if(any(grepl("Discharge", getReportMetadata(data,'primaryParameter'))))
    {
      #Reference Time Series Data
      corr_UV_Qref <- subsetByMonth(getTimeSeries(data, referenceSeriesName), month)
      est_UV_Qref <- subsetByMonth(getTimeSeries(data, referenceSeriesName, estimatedOnly=TRUE), month)
    }
    
    approvals_uv <- getApprovals(data, chain_nm=primarySeriesName, legend_nm=paste("UV", getTimeSeriesLabel(data, primarySeriesName)),
                                        appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), 
                                        subsetByMonth=TRUE, month=month)
    approvals_first_stat <- getApprovals(data, chain_nm="firstDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions1"]],
                                        appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                        subsetByMonth=TRUE, month=month, point_type=21, approvalsAtBottom=FALSE, shiftTimeToNoon=TRUE)
    approvals_second_stat <- getApprovals(data, chain_nm="secondDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions2"]],
                                        appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                        subsetByMonth=TRUE, month=month, point_type=24, approvalsAtBottom=FALSE, shiftTimeToNoon=TRUE)
    approvals_third_stat <- getApprovals(data, chain_nm="thirdDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions3"]],
                                        appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                        subsetByMonth=TRUE, month=month, point_type=25, approvalsAtBottom=FALSE, shiftTimeToNoon=TRUE)
    approvals_fourth_stat <- getApprovals(data, chain_nm="fourthDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions4"]],
                                        appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), 
                                        subsetByMonth=TRUE, month=month, point_type=22, approvalsAtBottom=FALSE, shiftTimeToNoon=TRUE)
    
     
    approvals <- append(approvals_uv, approvals_first_stat)
    approvals <- append(approvals, approvals_second_stat)
    approvals <- append(approvals, approvals_third_stat)
    approvals <- append(approvals, approvals_fourth_stat)
  }
  
  if(plotName == "secondary"){
    if(any(grepl(referenceSeriesName, names(data))) && !any(grepl("Discharge", getReportMetadata(data,'primaryParameter')))) {
      #Reference Time Series Data
      corr_UV2 <- subsetByMonth(getTimeSeries(data, referenceSeriesName), month)
      est_UV2 <- subsetByMonth(getTimeSeries(data, referenceSeriesName, estimatedOnly=TRUE), month)
      series_corr2 <- subsetByMonth(getCorrections(data, "referenceSeriesCorrections"), month)
      approvals <- getApprovals(data, chain_nm=referenceSeriesName, legend_nm=getTimeSeriesLabel(data, referenceSeriesName),
                                  appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"),
                                  subsetByMonth=TRUE, month=month)
    } else {
      #Upchain Time Series Data
      corr_UV2 <- subsetByMonth(getTimeSeries(data, upchainSeriesName), month)
      est_U2 <- subsetByMonth(getTimeSeries(data, upchainSeriesName, estimatedOnly=TRUE), month)
      uncorr_UV2 <- subsetByMonth(getTimeSeries(data, upchainSeriesRawName), month)
      series_corr2 <- subsetByMonth(getCorrections(data, "upchainSeriesCorrections"), month)
      approvals <- getApprovals(data, chain_nm=upchainSeriesName, legend_nm=getTimeSeriesLabel(data, upchainSeriesName),
                                 appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"),
                                 subsetByMonth=TRUE, month=month)
    }
    
    effect_shift <- subsetByMonth(getTimeSeries(data, "effectiveShifts"), month)
    gage_height <- subsetByMonth(getMeanGageHeights(data), month)
    gw_level <- subsetByMonth(getGroundWaterLevels(data), month)
    meas_shift <- subsetByMonth(getFieldVisitMeasurementsShifts(data), month)
    
    ref_readings <- subsetByMonth(getReadings(data, "reference"), month)
    csg_readings <- subsetByMonth(getReadings(data, "crestStage"), month)
    #hwm_readings <- subsetByMonth(getReadings(data, "waterMark"), month)
  }
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "plotName", "month", "approvals", "approvals_uv", 
                                                  "approvals_first_stat", "approvals_second_stat", "approvals_third_stat",
                                                  "approvals_fourth_stat",
                                                  "useDownsampled", "primarySeriesName", "primarySeriesRawName", "referenceSeriesName", "comparisonSeriesName", "upchainSeriesName", "upchainSeriesRawName"
                                                  ))]
  
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars)
  
  # optionally exclude negative/zero values here
  
  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

#'@importFrom lubridate days_in_month
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@importFrom lubridate ymd
parseUVSupplemental <- function(data, plotName, pts, useDownsampled=FALSE) {
  if(useDownsampled) {
    primarySeriesName <- "downsampledPrimarySeries"
    primarySeriesRawName <- "downsampledPrimarySeriesRaw"
    referenceSeriesName <- "downsampledReferenceSeries"
    upchainSeriesName <- "downsampledUpchainSeries"
    comparisonSeriesName <- "downsampledComparisonSeries"
  } else {
    primarySeriesName <- "primarySeries"
    primarySeriesRawName <- "primarySeriesRaw"
    referenceSeriesName <- "referenceSeries"
    upchainSeriesName <- "upchainSeries"
    comparisonSeriesName <- "comparisonSeries"
  }
  
  if(plotName == "primary"){
    
    if(!is.null(pts$corr_UV)){
      lims_UV <- getUvhLims(pts$corr_UV)
    } else {
      lims_UV <- getUvhLims(pts$uncorr_UV)
    }

    if(any(grepl("Discharge", getReportMetadata(data,'primaryParameter'))))
    {
      if(!is.null(pts$corr_UV_Qref)){
        lims_UV <- append(lims_UV, getUvhLims(pts$corr_UV_Qref))
      }

      reference_lbl <- getTimeSeriesLabel(data, referenceSeriesName)
      ref_units <- data$referenceSeries$units
    }
    
    primary_lbl <- getTimeSeriesLabel(data, primarySeriesName)
    primary_type <- data[[primarySeriesName]]$type
    reference_type <- data[[referenceSeriesName]]$type
    date_lbl <- paste(lims_UV$xlim[1], "through", lims_UV$xlim[2])
    comp_UV_lbl <- data$reportMetadata$comparisonStationId
    comp_UV_type <- data[['comparisonSeries']]$type
    comp_UV_TS_lbl <- getTimeSeriesLabel(data, "comparisonSeries");
    dates <- seq(lims_UV$xlim[1], lims_UV$xlim[2], by="days")
    
    logAxis <- isLogged(data, pts, "firstDownChain")
    
    days <- seq(days_in_month(dates[1]))
    year <- year(dates[1])
    month <- month(dates[1])
    plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=data$reportMetadata$timezone)), length=tail(days,1), by="days")
    
  }
  
  if(plotName == "secondary"){
    lims_UV2 <- getUvhLims(pts$corr_UV2)

    if(any(grepl(referenceSeriesName, names(data))) && !any(grepl("Discharge", getReportMetadata(data,'primaryParameter')))) {
      secondary_lbl <- getTimeSeriesLabel(data, referenceSeriesName)
      sec_units <- data$referenceSeries$units

    }
    else if(any(grepl(upchainSeriesName, names(data)))) {
      secondary_lbl <- getTimeSeriesLabel(data, upchainSeriesName)
      sec_units <- data$upchainSeries$units
    }

    sec_dates <- seq(lims_UV2$xlim[1], lims_UV2$xlim[2], by="days")
    date_lbl2 <- paste(lims_UV2$xlim[1], "through", lims_UV2$xlim[2])
    days <- seq(days_in_month(sec_dates[1]))
    year <- year(sec_dates[1])
    month <- month(sec_dates[1])
    plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=data$reportMetadata$timezone)), length=tail(days,1), by="days")
    tertiary_lbl <- getTimeSeriesLabel(data, "effectiveShifts")
    
    sec_logAxis <- isLogged(data, pts, 'secondDownChain')
    tertiary_logAxis <- isLogged(data, pts, 'thirdDownChain')
  }

  #for any one plot, all data must be either inverted or not
  isInverted <- all(na.omit(unlist(lapply(names(pts), getInverted, plotName=plotName, data = data, useDownsampled=useDownsampled))))
  
  allVars <- as.list(environment())
  allVars <- allVars[unlist(lapply(allVars, function(x) {!is.null(x)} ),FALSE,FALSE)]
  allVars <- allVars[unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} ),FALSE,FALSE)]
  supplemental <- allVars[which(!names(allVars) %in% c("data", "plotName", "pts"))]
  
  return(supplemental)
}

correctionsTable <- function(data) {
  if (any(names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up"))) {
    corrections <- data[[grep("series_corr", names(data))]]
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), corrections$comment))
    colnames(corrections_table) <- c("", "Comments")
    return(corrections_table)
  } else (return(corrections_table <- NULL))
}

parseLabelSpacing <- function(data, info) {
  
  if (names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up")){
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

getMeanGageHeights<- function(ts, ...){
  if(is.null(ts$fieldVisitMeasurements[['meanGageHeight']])) {
    df <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))
    df <- na.omit(df)
    return(df)
  }
  y <- ts$fieldVisitMeasurements[['meanGageHeight']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  n <- ts$fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, n=n, month=month, field=rep("fieldVisitMeasurements", length(time)), stringsAsFactors = FALSE))
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
  
  
  return(data.frame(time=x, value=y, uncertainty=uncertainty, month=month, 
                    field=rep(field, length(x)), stringsAsFactors = FALSE))
  
}

getInverted <- function(data, renderName, plotName, useDownsampled=FALSE) {
  if(useDownsampled) {
    primarySeriesName <- "downsampledPrimarySeries"
    primarySeriesRawName <- "downsampledPrimarySeriesRaw"
    referenceSeriesName <- "downsampledReferenceSeries"
    comparisonSeriesName <- "downsampledComparisonSeries"
    upchainSeriesName <- "downsampledUpchainSeries"
  } else {
    primarySeriesName <- "primarySeries"
    primarySeriesRawName <- "primarySeriesRaw"
    referenceSeriesName <- "referenceSeries"
    comparisonSeriesName <- "comparisonSeries"
    upchainSeriesName <- "upchainSeries"
  }
  
  if (plotName == "primary") {   
    dataName <- switch(renderName,
                       corr_UV = primarySeriesName,
                       corr_UV_Qref = referenceSeriesName,
                       est_UV = primarySeriesName,
                       est_UV_Qref = referenceSeriesName,
                       uncorr_UV = primarySeriesRawName,
                       comp_UV = comparisonSeriesName,  
                       water_qual = primarySeriesName,  #if primary is flipping, this will flip
                       stat_1 = "firstDownChain",
                       stat_2 = "secondDownChain",
                       stat_3 = "thirdDownChain",
                       stat_4 = "fourthDownChain")
    
  } else if (plotName == "secondary") {
    if(any(grepl(referenceSeriesName, names(data))) && !any(grepl("Discharge", getReportMetadata(data,'primaryParameter')))) {
      dataName <- switch(renderName,
                        corr_UV2 = "referenceSeries",
                        est_UV2 = "referenceSeries"
                        )
    } else { 
      dataName <- switch(renderName,
                        corr_UV2 = "upchainSeries",
                        est_UV2 = "upchainSeries",
                        uncorr_UV2 = "upchainSeriesRaw"
                        )
    }
  }
  
  isInverted <- ifelse(!is.null(dataName), data[[dataName]][['inverted']], NA)
  return(isInverted)
}

extendYaxisLimits <- function(object, error_bar_args){
  side <- ifelse(!is.null(error_bar_args$side), error_bar_args$side, 2)
  side_nm <- paste0('side.', side)
  
  lowest_error_bar <- min(error_bar_args$y - error_bar_args$y.low)
  lowest_y <- min(ylim(object, side=side)[1], lowest_error_bar)
  
  highest_error_bar <- max(error_bar_args$y + error_bar_args$y.high)
  highest_y <- max(ylim(object, side=side)[2], highest_error_bar)
  
  object[[side_nm]][['lim']] <- c(lowest_y, highest_y)
  return(object)
}
addHeight <- function(object){
  yheight <- (object$side.2$lim[2]-object$side.2$lim[1])*0.03 
  return(yheight)
}
