getRatingsSiteLabel<- function(data){
  siteNumber <- data[['sitefile']]$siteNumber
  stationName <- data[['sitefile']]$stationName
  return(paste(siteNumber, " - ", stationName))
} 

getCurrentRating <- function(data){
  list(x = data$currentRating$aqcuRatingTable$outputValues, 
       y = data$currentRating$aqcuRatingTable$inputValues)
}

getPreviousRating <- function(data){
  list(x = data$previousRating$aqcuRatingTable$outputValues, 
       y = data$previousRating$aqcuRatingTable$inputValues)
}

getHistoricalDischarge <- function(data){
  out <- data.frame()
  field <- data$historicFieldVisits$dischargeActivities
  for (i in 1:length(field)){
    out <- rbind(out, data.frame(x = field[[i]]$discharge, 
                                 y = field[[i]]$meanGageHeight, 
                                 time = field[[i]]$measurementStartTime,
                                 id = field[[i]]$measurementId, 
                                 stringsAsFactors = FALSE))
  }
  
  return(out)
  
}

getTopTenGage <- function(data,page){
  useNames <- c('measurementId','measurementStartTime','discharge','meanGageHeight')
  setNamesTo <- c('id','time','x','y')
  out <- data.frame(data$pages[[page]]$topTenGageHeights[useNames])
  names(out) <- setNamesTo
  return(out)
}