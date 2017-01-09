############ used in sensorreading-data and sitevisitpeak-data ############ 

isEmpty <- function(val){
  result <- (is.null(val) || is.na(val))
  return(result)
}

############ used in various places ############ 

#' Test if a Value is the NULL Indicator, the NA Indicator, or the Empty String
#' 
#' @param val A value.
#' @param listObjects An R list.
#' @param objectName An object name.
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

#' Test Whether a Variable is Empty
#' 
#' @param variable A variable's value.
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

# as.numeric forces NULL to be NA
validParam <- function(val, param, required = FALSE, as.numeric = FALSE){
  if (is.null(val)){
    if (required){
      stop('required value ', param, ' missing.')
    }
    ifelse(as.numeric, return(as.numeric(NA)), return(""))
  } else {
    return(val)
  }
}

#if absolutely no data comes back after parsing - skip to render with a message
anyDataExist <- function(data){
  emptyData <- any(c(length(data) == 0, nrow(data) == 0, is.null(data)))
  notEmptyData <- !emptyData
  return(notEmptyData)
}