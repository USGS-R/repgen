#'@export
getStr <- function(ts, param, upper=FALSE){
  val <- getRaw(ts, param)
  if (upper){
    return(toupper(val))
  } 
  
  return(val)
  
}

#'@export
getNum <- function(ts, param, dec = NULL){
  val <- getRaw(ts, param)
  if (is.null(dec)){
    return(val)
  } else {
    fmt <- paste0('%1.',dec,'f')
    num <- as.numeric(val)
    val <- sprintf(fmt = fmt, num)
    return(val)
  }
}

#'@title get a date from json list in specified outputformat
#'@param ts a timeseries list that can be extracted w/ \code{\link{getRaw}}
#'@param param the json parameter name to match to (passed to \code{\link{getRaw}})
#'@param format string: the date format output.
#'@return a string date, or empty if the value returned from \code{\link{getRaw}} is empty
#'@seealso \code{\link{getRaw}}
#'@export
getDate <- function(ts, param, format = "%Y/%m/%d"){
  val <- getRaw(ts, param)
  if (val == " "){
    #empty response from getRaw (replaces NULL)
    return(val)
  } else {
    date <- as.POSIXct(val)
    return(strftime(date, format))
  }
}
#'@export
getTime <- function(ts, param, tz = FALSE){
  val <- getRaw(ts, param)
  if (!tz){
    val <- strsplit(val, '[-]')[[1]][1]
  }
  
  return(val)
}

#'@export
waterYear <- function(ts, format = "%m/%d/%Y", collapse = TRUE){
  
  if (!"startDate" %in% names(ts$inputs) | !"endDate" %in% names(ts$inputs)){
    stop('invalid json for EXTREMES report. Need "startDate" and "endDate".\nOne or both are missing.')
  }
  
  startDate <- as.POSIXct(ts$inputs[["startDate"]])
  endDate <- as.POSIXct(ts$inputs[["endDate"]])
  
  if (format(startDate,"%m-%d") != "10-01" | format(endDate,"%m-%d") != "09-30"){
    stop('invalid water year dates or parsing error, check start and end dates.\n start:', 
         startDate, ' end:', endDate)
  }
  startStr <- strftime(startDate,format = format)
  endStr <- strftime(endDate,format = format)
  wYr <- c(startStr, endStr)
  if (collapse){
    wYr <- paste(wYr,collapse = ' to ')
  } 
  
  return(wYr)
}

getRaw <- function(ts, param){
  baseParam <- strsplit(gsub("([A-Z])", " \\1", param[1]), " ")[[1]]
  param <- paste(unique(c(baseParam, param[-1])), collapse='')
  val <- ts$values[[param]]
  if (is.null(val)){
    return(" ")
  } else {
    return(val)
  }
}

#'@importFrom httr GET add_headers verbose content url_ok
#'@export
getJSON = function(url, auth){  
  
  response <- GET(url, 
                  config=list(ssl.verifypeer = FALSE), 
                  add_headers('Authorization' = auth, 
                              'Connection'='keep-alive', Accept='application/json'))
  
  url_ok(response$url)
  json <- content(response)
  return(json)
}