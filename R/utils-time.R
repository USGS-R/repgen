## Utility functions that deal with time, timezone, dates, etc

#'@title will attempt to parse a DV, UTC time, or offset time
#'@description convienence that will attempt to parse a DV, UTC time, or offset time
#'extremes json
#'@param x character vector of the date/time
#'@param timezone character vector of length one indicating a timezone code
#'@return time vector
#'@export
#'@importFrom lubridate parse_date_time
flexibleTimeParse <- function(x, timezone) {
  
  #first attempt utc
  format_str <- "Ymd HMOS z"
  time <- parse_date_time(x,format_str, tz=timezone,quiet = TRUE)
  
  #then attempt an offset time
  if(isEmptyOrBlank(time)) {
    format_str <- "Ymd T* z*"
    time <- parse_date_time(x,format_str, tz=timezone, quiet = TRUE)
  }
  
  #then attempt a DV
  if(isEmptyOrBlank(time)) {
    format_str <- "Ymd"
    time <- parse_date_time(x,format_str, tz=timezone,quiet = TRUE)
    time <- time + hours(12)
  }
  
  return(time)
}

#'given a datetime, will remove time and set 0000 as start
#'@importFrom lubridate hour minute
#'@param time datetime to shift to start
#'@rdname toEndOfDay 
#'@export
toStartOfDay <- function(time){
  hour(time) <- 0
  minute(time) <- 0
  return(time)
}

#'given a datetime, will remove time and set 2359
#'@importFrom lubridate hour minute
#'@param time datetime to shift to start
#'@rdname toStartOfDay 
#'@export
toEndOfDay <- function(time){
  hour(time) <- 23
  minute(time) <- 59
  return(time)
}

#'given a datetime, will change the date to the 1st, plus remove time and set 0000
#'this is used to set the labels for Five Year Groundwater Summary reports
#'@importFrom lubridate day hour minute
#'@param time datetime to shift to start
#'@rdname toEndOfMonth 
#'@export
toStartOfMonth <- function(time){
  day(time) <- 1
  hour(time) <- 0
  minute(time) <- 0
  return(time)
}

#'given a datetime, will change the date to the 30th, plus remove time and set 2359
#'this is used to set the labels for Five Year Groundwater Summary reports
#'@importFrom lubridate day hour minute
#'@param time datetime to shift to start
#'@rdname toStartOfMonth 
#'@export
toEndOfMonth <- function(time){
  day(time) <- 30
  hour(time) <- 23
  minute(time) <- 59
  return(time)
}

#' hacky fix for 9999 year issue which prevents the rectangles from displaying on the graphs 
#' apologies to the people of 2100 who have to revisit this
#' @importFrom lubridate year
#' @param time datetime to shift to "the end of time"
#' @export
toEndOfTime <- function(time){
  year(time) <- 2100
  return(reformatted_time)
}
