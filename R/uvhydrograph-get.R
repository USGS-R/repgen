
getUvHydro <- function(ts, field){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
}

getUvLabel<- function(ts, field){
  param <- ts[[field]]$type
  units <- ts[[field]]$units
  return(paste(param, " (", units, ")"))
}

getSiteLabel<- function(data){
  siteNumber <- data[['sitefile']]$siteNumber
  stationName <- data[['sitefile']]$stationName
  return(paste(siteNumber, " - ", stationName))
} 