
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
  time = as.POSIXct(strptime(x, "%Y%m%d%"))
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