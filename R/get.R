
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
  return(validParam(val, ...))
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
  return(validParam(val, ...))
}

numShifts <- function(ts){
  if (is.null(ts$ratingShifts)) {
    stop('required field ratingShifts is missing.')
  }
  return(nrow(ts$ratingShifts))
}

# as.numeric forces NULL to be NA
validParam <- function(val, required = FALSE, as.numeric = FALSE){
  if (is.null(val)){
    if (required){
      stop('required value ', param, ' missing.')
    }
    ifelse(as.numeric, return(as.numeric(NA)), return(" "))
  } else {
    return(val)
  }
}



getRatingShifts <- function(ts, param, ...){
  val <- ts$ratingShifts[[param]]
  return(validParam(val, ...))
}


getErrorBars <- function(ts, param, ...){
  val <- ts$errorBars[[param]]
  return(validParam(val, ...))
}


getMaxStage <- function(ts, ...){
  val <- as.numeric(ts$maximumStageHeight)
  return(validParam(val, ...))
}

getMinStage <- function(ts, ...){
  val <- as.numeric(ts$minimumStageHeight)
  return(validParam(val, ...))
}

#'@importFrom httr GET add_headers verbose content http_status
getJSON = function(url, ..., verbose = FALSE){  
  checkAuth(...)
  
  response <- GET(url, 
                  config=list('ssl.verifypeer' = FALSE, 'verbose' = verbose), 
                  add_headers('Authorization' = getAuth(), 
                              'Connection'='keep-alive', Accept='application/json'))
  
  if (!http_status(response)$category == "success"){
    stop("GET failed on ", url)
  }
  json <- content(response, as = "parsed")
  return(json)
}