############ used in uvhydrograph-render, dvhydrograph-render, fiveyeargwsum-render ############ 

rm.duplicate.legend.items <- function(object){
  
  which.duplicated <- which(duplicated(object$legend$legend.auto$legend))
  if(length(which.duplicated) > 0){
    object$legend$legend.auto <- lapply(object$legend$legend.auto, function(legend.arg, which.duplicated) {
      legend.arg[-which.duplicated]
    }, which.duplicated = which.duplicated)
  }
  
  return(object)
}