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