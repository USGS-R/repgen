getAllUVdata <- function(data){
  allUVdata <- list() %>% 
    hAppend(getUvHydro()) %>% 
    hAppend(getApprovals()) %>% 
    #if rewrite
    hAppend(getReviewDV) #presumes we rewrite the data incoming? 
    hAppend(getReviewUV...)
  
}


parseUVData <- function(data, plotName) {
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
  plotData <- allVars[which(!names(allVars) %in% c("data", "plotName"))]
  
  return(plotData)
}

parseUVSupplemental <- function(data, plotName, pts_UV) {
  if(plotName == "primary"){
    
    lims_UV <- getUvhLims(pts_UV)
    primary_lbl <- getUvLabel(data, "primarySeries")
    date_lbl <- paste(lims_UV$xlim[1], "through", lims_UV$xlim[2])
    comp_UV_lbl <- getUvName(data, "comparisonSeries")
    dates <- seq(lims_UV$xlim[1], lims_UV$xlim[2], by="days")
    
    appr_UV <- getApprovals(data, "primarySeries" )
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

correctionsTable <- function(secondaryData) {
  if (any(names(secondaryData) == "series_corr2")) {
    corrections_table <- as.data.frame(cbind(seq(nrow(series_corr2)), series_corr2$comment))
    colnames(corrections_table) <- c("", "Comments")
    return(corrections_table)
  } else (return(corrections_table <- NULL))
}

##### working on this now ############################################
parseApprovalInfo <- function(data, primaryInfo) {
  
  grep("appr", names(primaryInfo))
}
##### working on this now ############################################



##### functions
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


getUvName<- function(ts, field){
  return(ts[[field]]$name)
}

isSeriesOfType<- function(ts, field, type){
  return(ts[[field]]$type == type)
}

getSiteLabel<- function(data){
  siteNumber <- data[['sitefile']]$siteNumber
  stationName <- data[['sitefile']]$stationName
  return(paste(siteNumber, " - ", stationName))
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

getFieldVisitErrorBars <- function(ts, param, ...){
  val <- ts$fieldVisitErrorBars[[param]]
  return(validParam(val, param, ...))
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

getNewLegendFrame <- function() {
  return(data.frame(text = character(), symbol = numeric(), color = character(), line = numeric(), stringsAsFactors = FALSE))
}

############ functions:

subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}