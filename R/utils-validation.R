############ used in sensorreading-data and sitevisitpeak-data ############ 

#' Check for empty values
#' 
#' @description Check if a value is NULL or NA.
#' 
#' @param val any R object.
#' 
#' @details Be careful what you pass in as \code{val},
#' e.g. a list will return a vector of logicals.
#'
isEmpty <- function(val){
  result <- (is.null(val) || is.na(val))
  return(result)
}

############ used in various places ############ 
#' Check for empty or blank values
#' 
#' @description Check if a value is NULL or NA or empty string or not in a list of objects.
#' 
#' @param val any R object.
#' @param listObjects R list of objects.
#' @param objectName R object.
#' 
#' @details Be careful what you pass in as \code{val},
#' e.g. a list will return a vector of logicals.
#' @seealso \code{\link{isEmpty}}
#'
isEmptyOrBlank <- function(val = NULL, listObjects = NULL, objectName = NULL){
  if(is.null(objectName)){
    result <- (length(val)==0 || isEmpty(val) || as.character(val)=="")
  } else {
    result <- !objectName %in% listObjects
  }
  return(result)
}

############ used in uvhydrograph-data, dvhydrograph-data, fiveyeargwsum-data ############ 

#' Check for empty variables
#' 
#' @description Check if a variable is NULL or has no rows.
#' 
#' @param variable R object, matrix, vector, array or data frame.
#'
isEmptyVar <- function(variable){
  result <- all(is.null(variable) || nrow(variable) == 0 || is.null(nrow(variable)), 
                is.null(variable) || length(variable$time[!is.na(variable$time)]) == 0)
  return(result)
}

#' Check for NULL or FALSE values
#' 
#' @description Check if a value is NULL or has a value of FALSE.
#' 
#' @param variable R object.
#'
isNullOrFalse <- function(variable) {
  return(is.null(variable) || 
           (!is.null(variable) && variable == FALSE))
}

#' Checks for valid parameter 
#' 
#' @description Check if a value is NULL.  If NULL and required, returns error message.  
#' if not required, forces a return of NA.  If not NULL, returns \code{val}.
#' 
#' @param val R object.
#' @param param string.
#' @param required boolean, defaults to FALSE
#' @param as.numeric, defaults to FALSE
#'
#as.numeric forces NULL to be NA
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

#' Checks that any data exists 
#' 
#' @description Check if data is NULL, has zero length or has no rows
#' 
#' @param data dataframe, list
#'
#if absolutely no data comes back after parsing - skip to render with a message
anyDataExist <- function(data){
  emptyData <- any(c(length(data) == 0, nrow(data) == 0, is.null(data)))
  notEmptyData <- !emptyData
  return(notEmptyData)
}