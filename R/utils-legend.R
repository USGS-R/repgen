############ used in uvhydrograph-render, dvhydrograph-render, fiveyeargwsum-render ############ 
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