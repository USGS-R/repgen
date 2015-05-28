
getUvHydro <- function(ts, field){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
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