#' @title zeroValues
#' @description Function that returns the data equal to zero.
#' @param timeSeriesData The time series that is checked for data containing zero
#' @return A logical value that is true if there is any data that is zero.
zeroValues <- function(timeSeriesData){ 
  if(class(timeSeriesData) == "list"){
    zeroList <- lapply(timeSeriesData, function(x) {any(na.omit(x[["value"]]) == 0)})
    zeroData <- any(unlist(unname(zeroList)))
  } else {
    zeroData <- any(na.omit(timeSeriesData[["value"]]) == 0)
  }
  return(zeroData)
}

#' @title negValues
#' @description Function that returns negative data
#' @param timeSeriesData The time series that is checked for negative data
#' @return A logical value that is true if there are any negative values.
negValues <- function(timeSeriesData){    
  if(class(timeSeriesData) == "list"){
    negList <- lapply(timeSeriesData, function(x) {any(na.omit(x[["value"]]) < 0)})
    negData <- any(unlist(unname(negList)))
  } else {
    negData <- any(na.omit(timeSeriesData[["value"]]) < 0)
  }
  return(negData)
}

#' @title isTimeSeriesInverted
#' 
#' @description Checks if a timeseries should be inverted.
#' 
#' @param timeseries The timseries to potentially be inverted.
#' 
#' @return Returns a logical value that is true if the timeseries should be inverted.
#' 
isTimeSeriesInverted <- function(timeseries) {
  invertedFlag <- timeseries[['inverted']]
  isInverted <- ifelse(is.null(invertedFlag), FALSE, invertedFlag)
  return(isInverted)
}


#' @title isLogged
#' 
#' @description To check if a timeseries can be plotted on a logged axis.
#' 
#' @param isVolFlow A logical value indicating whether the timeseries measures
#' a volumetric flow or not.
#' @param ts_data The timeseries data itself.
#' @param excludeZeroNegative A logical value indicating whether the timeseries
#' will exclude zero and negative values.
#' 
#' @return Return true if the timeseries can be plotted on a logged axis and false otherwise.
#'
isLogged <- function(ts_data, isVolFlow, excludeZeroNegative){
  
  zero_logic <- zeroValues(ts_data)
  neg_logic <- negValues(ts_data)
  loggingError <- (zero_logic || neg_logic) && 
    ( isEmptyOrBlank(excludeZeroNegative) || !excludeZeroNegative )
  
  if(is.null(isVolFlow) || !isVolFlow || loggingError){
    logAxis <- FALSE
  } else if(isVolFlow && !loggingError){  
    logAxis <- TRUE
  }
  
  return(logAxis)
}
