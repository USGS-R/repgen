#' @export
#finds if the plot data has any zero values
zeroValues <- function(data, val_nm){ 
  if(class(data) == "list"){
    zeroList <- lapply(data, function(x) {any(na.omit(x[[val_nm]]) == 0)})
    zeroData <- any(unlist(unname(zeroList)))
  } else {
    zeroData <- any(na.omit(data[[val_nm]]) == 0)
  }
  return(zeroData)
}

#' @export
#finds if the plot data has any zero values
negValues <- function(data, val_nm){    
  if(class(data) == "list"){
    negList <- lapply(data, function(x) {any(na.omit(x[[val_nm]]) < 0)})
    negData <- any(unlist(unname(negList)))
  } else {
    negData <- any(na.omit(data[[val_nm]]) < 0)
  }
  return(negData)
}

#' @export
isTimeSeriesInverted <- function(timeseries) {
  invertedFlag <- timeseries[['inverted']]
  isInverted <- ifelse(is.null(invertedFlag), FALSE, invertedFlag)
  return(isInverted)
}


#' @export
isLogged <- function(all_data, ts_data, series){
  
  isVolFlow <- all_data[[series]][['isVolumetricFlow']]
  zero_logic <- zeroValues(ts_data, "value")
  neg_logic <- negValues(ts_data, "value")
  ignoreZeroNegative <- getReportMetadata(all_data, 'excludeZeroNegative')
  loggingError <- (zero_logic || neg_logic) && 
    ( isEmptyOrBlank(ignoreZeroNegative) || !ignoreZeroNegative )
  
  if(is.null(isVolFlow) || !isVolFlow || loggingError){
    logAxis <- FALSE
  } else if(isVolFlow && !loggingError){  
    logAxis <- TRUE
  }
  
  return(logAxis)
}
