############ used in sensorreading-data and sitevisitpeak-data ############ 

isEmpty <- function(val){
  result <- (is.null(val) || is.na(val))
  return(result)
}

############ used in various places ############ 

#' @export
isEmptyOrBlank <- function(val = NULL, listObjects = NULL, objectName = NULL){
  if(is.null(objectName)){
    result <- (length(val)==0 || isEmpty(val) || as.character(val)=="")
  } else {
    result <- !objectName %in% listObjects
  }
  return(result)
}

############ used in uvhydrograph-data, dvhydrograph-data, fiveyeargwsum-data ############ 

#' @export
isEmptyVar <- function(variable){
  result <- all(is.null(variable) || nrow(variable) == 0 || is.null(nrow(variable)), 
                is.null(variable) || length(variable$time[!is.na(variable$time)]) == 0)
  return(result)
}

isNullOrFalse <- function(variable) {
  return(is.null(variable) || 
           (!is.null(variable) && variable == FALSE))
}