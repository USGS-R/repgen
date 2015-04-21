
getUvHydro <- function(ts, field){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
}

