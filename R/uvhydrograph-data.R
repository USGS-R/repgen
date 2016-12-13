#'@importFrom lubridate parse_date_time

getMonths <- function(data, useDownsampled=FALSE){
  if(useDownsampled) {
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
    
    ref_readings <- subsetByMonth(getReadings(data, "reference"), month)
    csg_readings <- subsetByMonth(getReadings(data, "crestStage"), month)
    hwm_readings <- subsetByMonth(getReadings(data, "waterMark"), month)

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
  if (any(names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up", "series_corr2"))) {
    corrections <- data[[grep("series_corr", names(data))]]
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), as.character(corrections$time), corrections$comment))
    colnames(corrections_table) <- c("", "Time", "Comments")
    return(corrections_table)
  } else (return(corrections_table <- NULL))
}

parseLabelSpacing <- function(data, info) {
  
  if (names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up", "series_corr2")){
    limits <- info[[grep("lims_UV", names(info))]]
    #Number of seconds to offset labels by for display
    secondOffset <- 4 * 60 * 60
    #Width of one digit in hours
    digitSeconds <- 4 * 60 * 60
    #Total width of both bounding box left and right margins 
    baseBoxSize <- 4 * 60 * 60
    #Minimum space between the right side of a label box and the next correction line to not merge columns
    minSpacerSize <- 2 * 60 * 60
    #The percentage of the y-range to subtract each time we add a new label to a column
    subtractor <- (limits$ylim[[2]] - limits$ylim[[1]]) * 0.065

    #Save original order as label and re-order by time and then by label (descending)
    corrs <- data[[1]] %>% select(time) %>% mutate(label = row_number()) %>% arrange(time, desc(label))
    #Calculate the largest width label for the current time
    corrs <- corrs %>% mutate(colWidth = ifelse(row_number() == 1 | lag(time) != time, nchar(as.character(label)), 0))
    #Propagate the width value to all rows with the current time
    corrs <- corrs %>% group_by(time) %>% mutate(colWidth = cumsum(colWidth)) %>% ungroup()
    #Calculate the size of each label box
    corrs <- corrs %>% mutate(boxWidth = baseBoxSize + digitSeconds * colWidth)
    #Calculate column breaks based on widths and times
    corrs <- corrs %>% mutate(newCol = ifelse(row_number() == 1 | difftime(time, lag(time), units="secs") >=  secondOffset + boxWidth + minSpacerSize, TRUE, FALSE))
    #Calculate the column number of each row by summing up the newCol column
    corrs <- corrs %>% mutate(colNum = cumsum(as.numeric(newCol)))
    #Calculate the x-position of new columns
    corrs <- corrs %>% arrange(desc(time), desc(label)) %>% mutate(xpos = ifelse(row_number() == 1 | lag(colNum) != colNum, time + secondOffset + boxWidth/2, 0))
    #Propagate the x-position value to all labels in a particular column
    corrs <- corrs %>% group_by(colNum) %>% mutate(xpos = cumsum(xpos)) %>% arrange(colNum, label)
    #Add a y-offset multiplier value to each row that increases within columns
    corrs <- corrs %>% mutate(multiplier = 1) %>% mutate(multiplier = cumsum(multiplier))
    #Calculate the y-position based on the multiplier value
    corrs <- corrs %>% mutate(ypos = ifelse(row_number() == 1 | colNum != lag(colNum), limits$ylim[[2]], limits$ylim[[2]] - (subtractor * (multiplier-1))))
    #Move any x-positions that are off the chart to the left of their location by subtracating double what was added
    corrs <- corrs %>% mutate(shift = ifelse(xpos > limits$xlim[[2]], TRUE, FALSE)) %>% ungroup()
    #Shifted columns should use the earliest date to offset from instead of the latest, so re-calculate xpos from this date
    corrs <- corrs %>% arrange(colNum, time, label) %>% mutate(xpos = ifelse(shift, ifelse(row_number() == 1 | lag(colNum) != colNum, time - secondOffset - boxWidth/2, 0), xpos))
    #Propagate the x-position value to all labels in shifted columns
    corrs <- corrs %>% group_by(colNum) %>% mutate(xpos = ifelse(shift, cumsum(xpos), xpos)) %>% ungroup() %>% arrange(colNum, label)
    #If we shifted any columns to the other side check for overlapping columns (this really only matters for the last column)
    corrs <- corrs %>% mutate(overlap = ifelse(colNum != lag(colNum), ifelse(xpos - lag(xpos) < secondOffset + boxWidth, 1, 0), 0)) %>%
                       mutate(overlap = ifelse(is.na(overlap), 0, ifelse(row_number() < n() & colNum != lead(colNum) & lead(overlap) > 0, 1, overlap)))
    #Propagate found overlap to all rows in this column
    corrs <- corrs %>% group_by(colNum) %>% arrange(desc(overlap)) %>% mutate(overlap = cumsum(overlap)) %>% arrange(colNum, desc(time), label) %>% ungroup()
    #Pull overlapping labels back towards their respective lines and stagger them vertically
    corrs <- corrs %>% mutate(xpos = ifelse(overlap > 0, ifelse(shift, xpos + secondOffset - 1, xpos - secondOffset - 1), xpos)) %>%
                       mutate(ypos = ifelse(overlap > 0, ifelse(!shift, ifelse(row_number() > 1, ypos - (subtractor * (multiplier-1)), ypos), ypos - (subtractor * (multiplier))), ypos))

    ##The scaling factor for the bounding shape of this label in inches. Scaling factor is fairly arbitrary but is relative the cex value used for the text for these labels in the styles and the colWidth
    corrs <- corrs %>% mutate(r = 1+0.5*nchar(as.character(label)))
      
    spacingInfo <- list(x=corrs$xpos, xorigin=corrs$time, y=corrs$ypos, r=corrs$r, label=corrs$label)
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
  
  #Covers the case when no uncertainty is provided. This seems to work since the reference
  #is plotted correctly with no error bars
  uncertainty[is.na(uncertainty)] <- 0
  
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
