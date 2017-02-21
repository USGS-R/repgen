#' @title zeroValues
#' @description Function that returns the data equal to zero.
#' @param timeSeriesData The time series that is checked for data containing zero
#' @return A logical value that is true if there is any data that is zero.
zeroValues <- function(timeSeriesData){ 
  zeroData <- any(na.omit(timeSeriesData[["value"]]) == 0)
  return(zeroData)
}

#' @title negValues
#' @description Function that returns negative data
#' @param timeSeriesData The time series that is checked for negative data
#' @return A logical value that is true if there are any negative values.
negValues <- function(timeSeriesData){    
  negData <- any(na.omit(timeSeriesData[["value"]]) < 0)
  return(negData)
}

#' @title isTimeSeriesInverted
#' 
#' @description Checks if a time series is to be plotted on a revers \emph{y}-axis or not.
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
#' @param tsPoints The point data from the time series to check. 
#' If this is null but the other params are true, it returns true.
#' @param isVolFlow wheter or not the TS is representing volumetric flow
#' @param excludeZeroNegative A logical value indicating whether the timeseries
#' will exclude zero and negative values.
#' 
#' @return Return true if the timeseries can be plotted on a logged axis and false otherwise.
#'
isLogged <- function(tsPoints, isVolFlow=FALSE, excludeZeroNegative=FALSE){
  
  zero_logic <- zeroValues(tsPoints)
  neg_logic <- negValues(tsPoints)
  
  ## TODO: This logic seems odd to me: It's "If there are negatives or zeroes and we aren't excluding
  ## negatives and zeroes, make this false. Wouldn't we want this to be if there are any neg's or zeroes we don't log?
  loggingError <- (zero_logic || neg_logic) && 
    ( isEmptyOrBlank(excludeZeroNegative) || !excludeZeroNegative )
  
  if(is.null(isVolFlow) || !isVolFlow || loggingError){
    logAxis <- FALSE
  } else if(isVolFlow && !loggingError){  
    logAxis <- TRUE
  }
  
  return(logAxis)
}
