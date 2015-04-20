
#'@title get value from extremes json list
#'@description convienence function for accessing from the "values" block in 
#'extremes json
#'@param ts a list, can be the output of \code{\link[jsonlite]{fromJSON}}.
#'@param param the field name (e.g., 'locationNumber')
#'@param ... additional arguments passed to \code{repgen:::validParam}, 
#'such as \code{required}, or \code{as.numeric}
#'@return a value or array corresponding to the field specified by \code{param}
#'@export
getValue <- function(ts, param, ...){
  val <- ts$values[[param]]
  return(validParam(val, param, ...))
}

#'@title get input from extremes json list
#'@description convienence function for accessing from the "inputs" block in 
#'extremes json
#'@param ts a list, can be the output of \code{\link[jsonlite]{fromJSON}}.
#'@param param the field name (e.g., 'endDate')
#'@param ... additional arguments passed to \code{repgen:::validParam}, 
#'such as \code{required}, or \code{as.numeric}
#'@return a value or array corresponding to the field specified by \code{param}
#'@export
getInput <- function(ts, param, ...){
  val <- ts$inputs[[param]]
  return(validParam(val, param, ...))
}

numShifts <- function(ts){
  if (is.null(ts$ratingShifts)) {
    stop('required field ratingShifts is missing.')
  }
  return(nrow(ts$ratingShifts))
}

# as.numeric forces NULL to be NA
validParam <- function(val, param, required = FALSE, as.numeric = FALSE){
  if (is.null(val)){
    if (required){
      stop('required value ', param, ' missing.')
    }
    ifelse(as.numeric, return(as.numeric(NA)), return(" "))
  } else {
    return(val)
  }
}

getComputedUvDischarge <- function(ts, ...){
  param <- 'value'
  y <- ts$discharge$points[[param]]
  validParam(y, param, ...)
  param <- 'time'
  x <- ts$discharge$points[[param]]
  validParam(x, param, ...)
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
}

getRawUvDischarge <- function(ts, ...){
  param <- 'value'
  y <- ts$dischargeRaw$points[[param]]
  validParam(y, param, ...)
  param <- 'time'
  x <- ts$dischargeRaw$points[[param]]
  validParam(x, param, ...)
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
}

getDvDischarge <- function(ts, param, ...){
  param <- 'value'
  y <- ts$dailyDischarge$points[[param]]
  validParam(y, param, ...)
  param <- 'time'
  x <- ts$dailyDischarge$points[[param]]
  validParam(x, param, ...)
  time = as.POSIXct(strptime(x, "%FT%T"))
  return(data.frame(x=time, y=y))
}

getRatingShifts <- function(ts, param, ...){
  val <- ts$ratingShifts[[param]]
  return(validParam(val, param, ...))
}


getErrorBars <- function(ts, param, ...){
  val <- ts$errorBars[[param]]
  return(validParam(val, param, ...))
}


getMaxStage <- function(ts, ...){
  val <- as.numeric(ts$maximumStageHeight)
  return(validParam(val, param, ...))
}

getMinStage <- function(ts, ...){
  val <- as.numeric(ts$minimumStageHeight)
  return(validParam(val, param, ...))
}

