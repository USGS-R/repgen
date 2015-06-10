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
  
  field <- data$historicFieldVisits$dischargeActivities
  return(dischargeActivies(field))
  
}


calcRatingShifts <- function(data,page){
  
  currentRating <- getCurrentRating(data)
  shifts <- data[['pages']][[page]]$ratingShifts
  
  # confusing because approx flips our x and y:
  shiftMat <- lapply(shifts$stagePoints, function(out) list('x' = approx(y = currentRating$x, x = currentRating$y, xout = out)$y)) 
  
  for (i in 1:length(shiftMat)){
    shiftMat[[i]][['y']] <- shifts$stagePoints[[i]]+shifts$shiftPoints[[i]]
    shiftMat[[i]][['id']] <- shifts$shiftNumber[i]
  }
  return(shiftMat)
}

getFieldVisits <- function(data,page){
  
  field <- data[['pages']][[page]]$fieldVisits$dischargeActivities
  return(dischargeActivies(field))
}

getMaxMinStage <- function(data,page){
  
  return(data.frame('y' = c(data$pages[[page]]$maximumStageHeight,data$pages[[page]]$minimumStageHeight)))
}

getTopTenGage <- function(data,page){
  useNames <- c('measurementId','measurementStartTime','discharge','meanGageHeight')
  setNamesTo <- c('id','time','x','y')
  out <- data.frame(data$pages[[page]]$topTenGageHeights[useNames])
  names(out) <- setNamesTo
  return(out)
}

dischargeActivies <- function(field){
  out <- data.frame()

  for (i in 1:length(field)){
    out <- rbind(out, data.frame(x = field[[i]]$discharge, 
                                 y = field[[i]]$meanGageHeight, 
                                 time = field[[i]]$measurementStartTime,
                                 id = field[[i]]$measurementId, 
                                 stringsAsFactors = FALSE))
  }
  return(out)
}