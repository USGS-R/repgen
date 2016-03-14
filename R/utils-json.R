
#'@title get value from extremes json list
#'@description convienence function for accessing from the "values" block in 
#'extremes json
#'@param ts a list, can be the output of \code{\link[jsonlite]{fromJSON}}.
#'@param param the field name (e.g., 'locationNumber')
#'@param ... additional arguments passed to \code{repgen:::validParam}, 
#'such as \code{required}, or \code{as.numeric}
#'@return a value or array corresponding to the field specified by \code{param}
#'@export
getReportMetadata <- function(ts, param, ...){
  val <- ts$reportMetadata[[param]]
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

getRatingShifts <- function(ts, param, ...){
  val <- ts$ratingShifts[[param]]
  return(validParam(val, param, ...))
}


getMeasurements <- function(ts, param, ...){
  val <- ts$measurements[[param]]
  return(validParam(val, param, ...))
}


getMaxStage <- function(ts, ...){
  val <- as.numeric(ts$maximumStageHeight)
  return(validParam(val, param = 'maximumStageHeight', ...))
}

getMinStage <- function(ts, ...){
  val <- as.numeric(ts$minimumStageHeight)
  return(validParam(val, param = 'minimumStageHeight', ...))
}

#finds if the plot data has any zero values
zeroValues <- function(dataList, val_nm){    
  logList <- lapply(dataList, function(x) {any(na.omit(x[[val_nm]]) == 0)})
  logVector <- any(unlist(unname(logList)))
}

#finds if the plot data has any zero values
negValues <- function(dataList, val_nm){    
  logList <- lapply(dataList, function(x) {any(na.omit(x[[val_nm]]) < 0)})
  logVector <- any(unlist(unname(logList)))
}

#if absolutely no data comes back after parsing - skip to render with a message
anyDataExist <- function(data){
  emptyData <- any(c(length(data) == 0, nrow(data) == 0, is.null(data)))
  notEmptyData <- !emptyData
  return(notEmptyData)
}

#'Import a JSON file to use for report
#'@importFrom jsonlite fromJSON
#'@param file incoming json file
#'@rdname json 
#'@export
json <- function(file){
  if (!file.exists(file)){
    stop(file, ' not found')
  }
  json = fromJSON(file)
  return(json)
}
