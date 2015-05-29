
getUvHydro <- function(ts, field){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
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
  return(data.frame(x=time, y=y, n=n))
}

getFieldVisitErrorBarsQPoints <- function(ts){
  y <- ts$fieldVisitErrorBars[['discharge']]
  x <- ts$fieldVisitErrorBars[['visitStartDate']]
  minQ <- ts$fieldVisitErrorBars[['errorMinDischarge']]
  maxQ <- ts$fieldVisitErrorBars[['errorMaxDischarge']]
  n <- ts$fieldVisitErrorBars[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y, minQ=minQ, maxQ=maxQ, n=n))
}

getFieldVisitErrorBarsShifts <- function(ts){
  y <- ts$fieldVisitErrorBars[['shiftInFeet']]
  x <- ts$fieldVisitErrorBars[['visitStartDate']]
  minShift <- ts$fieldVisitErrorBars[['errorMinShiftInFeet']]
  maxShift <- ts$fieldVisitErrorBars[['errorMaxShiftInFeet']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y, minShift=minShift, maxShift=maxShift))
}

getNewLegendFrame <- function() {
  return(data.frame(text = character(), symbol = numeric(), color = character(), line = numeric(), stringsAsFactors = FALSE))
}