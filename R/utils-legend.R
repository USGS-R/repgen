#' @title Remove Duplicate Legend Items
#' 
#' @description Checks a plot object for duplicate keys in the legend.
#' @param object The plot object which contains the legend.
#' 
#' @return The changed object without duplicate legend keys.
#'
rmDuplicateLegendItems <- function(object){
  
  which.duplicated <- which(duplicated(object$legend$legend.auto$legend))
  if(length(which.duplicated) > 0){
    object$legend$legend.auto <- lapply(object$legend$legend.auto, function(legend.arg, which.duplicated) {
      legend.arg[-which.duplicated]
    }, which.duplicated = which.duplicated)
  }
  
  return(object)
}

#' @title Create Time Series Labels For the Legend
#' 
#' @description Returns the desired time series label for the time series
#' requested formatted for inclusion in the legend
#' 
#' @param ts The time series object which contains the time series information
#' @param field The type of time series data requested
#' 
#' @return The type of time series requested as well as the units
#' 
getTimeSeriesLabel<- function(ts, field){
  param <- ts[[field]]$type
  units <- ts[[field]]$unit
  
  if(!is.null(units)) {
    return(paste(param, " (", units, ")"))
  } else {
    return(param)
  }
}