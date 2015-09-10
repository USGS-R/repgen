
parseUVData <- function(data, plotName, month) {
  if(plotName == "primary"){
    
    corr_UV <- subsetByMonth(getUvHydro(data, "primarySeries" ), month)
    est_UV <- subsetByMonth(getUvHydro(data, "primarySeries", estimatedOnly=TRUE), month)
    uncorr_UV <- subsetByMonth(getUvHydro(data, "primarySeriesRaw" ), month)
    comp_UV <- subsetByMonth(getUvHydro(data, "comparisonSeries" ), month)
    water_qual <- subsetByMonth(getWaterQualityMeasurements(data), month)
    
    max_DV <- subsetByMonth(getUvHydro(data, "derivedSeriesMax"), month)
    mean_DV <- subsetByMonth(getUvHydro(data, "derivedSeriesMean"), month)
    median_DV <- subsetByMonth(getUvHydro(data, "derivedSeriesMin"), month)
    min_DV <- subsetByMonth(getUvHydro(data, "derivedSeriesMedian"), month)
    
    series_corr <- subsetByMonth(getCorrections(data, "primarySeriesCorrections"), month)
    meas_Q <- subsetByMonth(getFieldVisitErrorBarsQPoints(data), month)  
    
    UV_series <- corr_UV  #add data for series approval
 
  }
  
  if(plotName == "secondary"){
    
    corr_UV2 <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
    est_UV2 <- subsetByMonth(getUvHydro(data, "secondarySeries", estimatedOnly=TRUE), month)
    uncorr_UV2 <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
    
    series_corr2 <- subsetByMonth(getCorrections(data, "secondarySeriesCorrections"), month)
    
    effect_shift <- subsetByMonth(getUvHydro(data, "effectiveShifts"), month)
    gage_height <- subsetByMonth(getMeanGageHeights(data), month)
    gw_level <- subsetByMonth(getGroundWaterLevels(data), month)
    meas_shift <- subsetByMonth(getFieldVisitErrorBarsShifts(data), month)
        
  }
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0} )))]
  plotData <- rev(allVars[which(!names(allVars) %in% c("data", "plotName", "month"))])
  
  return(plotData)
}

parseUVSupplemental <- function(data, plotName, pts_UV) {
  if(plotName == "primary"){
    
    lims_UV <- getUvhLims(pts_UV)
    primary_lbl <- getUvLabel(data, "primarySeries")
    date_lbl <- paste(lims_UV$xlim[1], "through", lims_UV$xlim[2])
    comp_UV_lbl <- data[['comparisonSeries']]$name
    dates <- seq(lims_UV$xlim[1], lims_UV$xlim[2], by="days")
    logAxis <- ifelse(is.null(data$derivedSeriesMean$isVolumetricFlow), 
                      FALSE, data$derivedSeriesMean$isVolumetricFlow)
    
    appr_UV_series <- getApprovals(data, "primarySeries" )
    appr_max_DV <- getApprovals(data, "derivedSeriesMax")
    appr_mean_DV <- getApprovals(data, "derivedSeriesMean")
    appr_median_DV <- getApprovals(data, "derivedSeriesMin")
    appr_min_DV <- getApprovals(data, "derivedSeriesMedian")
    
  }
  
  if(plotName == "secondary"){
    
    lims_UV2 <- getUvhLims(pts_UV)
    date_lbl2 <- paste(lims_UV2$xlim[1], "through", lims_UV2$xlim[2])
    secondary_lbl <- getUvLabel(data, "secondarySeries")
    sec_dates <- seq(lims_UV2$xlim[1], lims_UV2$xlim[2], by="days")
    
    tertiary_lbl <- getUvLabel(data, "effectiveShifts")
  }
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  supplemental <- allVars[which(!names(allVars) %in% c("data", "plotName", "pts_UV"))]
  
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

parseApprovalInfo <- function(data, primaryInfo, x, y) {
  
  if (names(data) %in% c("max_DV", "min_DV", "median_DV", "mean_DV", "UV_series")){
    approvals <- primaryInfo[grep("appr", names(primaryInfo))]
    matchApproval <- grep(names(data), names(approvals))
    approvalColors <- c("lightpink", "yellow2", "lightcyan")
    approvalDescriptions <- c("Working", "In-review", "Approved")
    
    if (length(matchApproval) > 0) {
      approvalInfo <- vector("list", nrow(approvals[[matchApproval]]))
      
      for(i in seq(length(approvalInfo))) {    ### find example with multiple approvals
        a <- approvals[[matchApproval]][i,]
        level <- a$level + 1
        subsetX <- x[x >= a$startTime & x <= a$endTime]
        subsetY <- y[x >= a$startTime & x <= a$endTime]
        
        if (length(subsetX) > 0) {
          xVals <- subsetX
        } else {xVals <- NA}
        
        bg <- approvalColors[level]
        legend.name <- approvalDescriptions[level]
        
        if (names(data) %in% c("max_DV", "min_DV", "median_DV", "mean_DV")) {
          if (length(subsetY) > 0) {
            yVals <- subsetY
          } else {yVals <- NA}
          
          col <- 'black'
        } else if (names(data) == "UV_series") {
          #ylim <- gsplot:::calc_views(uvhplot)$window$ylim
          ylim <- c(0,1)
          
          if (length(subsetY) > 0) {
            yVals <- rep(ylim[1],length(subsetX))
          } else {yVals <- NA}
          
          col <- approvalColors[level]
        }
        approvalInfo[[i]] <- list(x=xVals, y=yVals, col=col, bg=bg, legend.name=legend.name)
      }
      
    } else {
      approvalInfo <- vector("list", 1)
      approvalInfo[[1]] <- list(x=x, y=y, col=approvalColors[1], bg=approvalColors[1], legend.name=approvalDescriptions[1])
    }
  } else {
    approvalInfo <- list()
  } 
  
  
  return(approvalInfo)
}

parseLabelSpacing <- function(data, info) {
  
  if (names(data) %in% c("series_corr", "series_corr2")){
    limits <- info[[grep("lims_UV", names(info))]]
    y_positions <- rep(limits$ylim[2], length(data[[1]]$x))
    differences <- as.numeric(diff(data[[1]]$x))
    if(length(differences) > 0) {
      for (i in 1:length(differences)) {
        if(differences[i] < 86400) {y_positions[i+1] <- y_positions[i]-(2*par()$cxy[2])}
        i <- i + 1
      }
      spacingInfo <- list(y=y_positions, label=seq(length(data$x)))
    } else {
      spacingInfo <- list(y=y_positions, label=seq(length(data$x)))
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
getUvHydro <- function(ts, field, estimatedOnly = FALSE){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  time <- as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  uv_series <- data.frame(x=time, y=y, month=month, stringsAsFactors = FALSE)
  
  if(estimatedOnly) {
    s <- ts[[field]]$estimatedPeriods[['startTime']]
    estimatedStartTimes <- as.POSIXct(strptime(s, "%FT%T"))
    e <- ts[[field]]$estimatedPeriods[['endTime']]
    estimatedEndTimes <- as.POSIXct(strptime(e, "%FT%T"))
    estimatedPeriods <- data.frame(start=estimatedStartTimes, end=estimatedEndTimes)
    
    estimatedSubset <- data.frame(x=as.POSIXct(NA), y=as.character(NA), month=as.character(NA))
    estimatedSubset <- na.omit(estimatedSubset)
    for(i in 1:nrow(estimatedPeriods)) {
      p <- estimatedPeriods[i,]
      startTime <- p$start
      endTime <- p$end
      estimatedSubset <- rbind(estimatedSubset, uv_series[uv_series$x > startTime & uv_series$x < endTime,])
    }
    uv_series <- estimatedSubset
  }
  
  return(uv_series)
}

getApprovals <- function(ts, field){
  level <- ts[[field]]$approvals[['level']]
  s <- ts[[field]]$approvals[['startTime']]
  startTime = as.POSIXct(strptime(s, "%FT%T"))
  e <- ts[[field]]$approvals[['endTime']]
  endTime = as.POSIXct(strptime(e, "%FT%T"))
  return(data.frame(level=level, startTime=startTime, endTime=endTime))
}

getUvLabel<- function(ts, field){
  param <- ts[[field]]$type
  units <- ts[[field]]$units
  
  if(!is.null(units)) {
    return(paste(param, " (", units, ")"))
  } else {
    return(param)
  }
}


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
  y <- ts$fieldVisitErrorBars[['meanGageHeight']]
  x <- ts$fieldVisitErrorBars[['visitStartDate']]
  n <- ts$fieldVisitErrorBars[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(x=time, y=y, n=n, month=month, stringsAsFactors = FALSE))
}


getGroundWaterLevels<- function(ts, ...){
  y <- ts$groundWater[['groundWaterLevel']]
  x <- ts$groundWater[['dateString']]
  time = as.POSIXct(strptime(x, "%Y%m%d"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(x=time, y=y, month=month, stringsAsFactors = FALSE))
}


getWaterQualityMeasurements<- function(ts, ...){
  if(is.null(ts$waterQuality)) {
    df <- data.frame(y=as.numeric(NA), x=as.POSIXct(NA), month=as.character(NA))
    df <- na.omit(df)
    return(df)
  }
  y <- ts$waterQuality$value[['value']]
  x <- ts$waterQuality[['sampleStartDateTime']]
  time = as.POSIXct(strptime(x, "%Y%m%d%H%M"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(x=time, y=y, month=month, stringsAsFactors = FALSE))
}


getFieldVisitErrorBarsQPoints <- function(ts){
  y <- ts$fieldVisitErrorBars[['discharge']]
  x <- ts$fieldVisitErrorBars[['visitStartDate']]
  minQ <- ts$fieldVisitErrorBars[['errorMinDischarge']]
  maxQ <- ts$fieldVisitErrorBars[['errorMaxDischarge']]
  n <- ts$fieldVisitErrorBars[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(x=time, y=y, minQ=minQ, maxQ=maxQ, n=n, month=month, stringsAsFactors = FALSE))
}


getFieldVisitErrorBarsShifts <- function(ts){
  y <- ts$fieldVisitErrorBars[['shiftInFeet']]
  x <- ts$fieldVisitErrorBars[['visitStartDate']]
  minShift <- ts$fieldVisitErrorBars[['errorMinShiftInFeet']]
  maxShift <- ts$fieldVisitErrorBars[['errorMaxShiftInFeet']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(x=time, y=y, minShift=minShift, maxShift=maxShift, month=month, stringsAsFactors = FALSE))
}


getCorrections <- function(ts, field){
  x <- ts[[field]][['startTime']]
  comment <- ts[[field]][['comment']]
  if(!is.null(comment)) {
    comment <- paste("Start", comment, sep=" : ")
  }
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month

  x2 <- ts[[field]][['endTime']]
  comment2 <- ts[[field]][['comment']]
  if(!is.null(comment2)) {
    comment2 <- paste("End", comment2, sep=" : ")
  }
  time2 = as.POSIXct(strptime(x2, "%FT%T"))
  month2 <- format(time2, format = "%y%m") #for subsetting later by month
  return(data.frame(x=c(time, time2), month=c(month, month2), comment=c(comment, comment2), stringsAsFactors = FALSE))
}


getUvhLims <- function(pts = NULL, xMinField = 'x', xMaxField = 'x', yMinField = 'y', yMaxField = 'y'){
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
