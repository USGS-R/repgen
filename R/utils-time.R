## Utility functions that deal with time, timezone, dates, etc

#' @title Will attempt to parse a DV, UTC time, or offset time.
#' 
#' @description A convienence function that will attempt to parse a (day
#'   point-type) date, UTC time, or offset time extremes JSON.
#' @param x A character vector of the date/time.
#' @param timezone A character vector of length one, indicating a time zone code.
#' @param shiftTimeToNoon Reference time to 12:00 p.m. if TRUE; interpret
#'        literally when FALSE.
#' @return time vector
#' @importFrom lubridate parse_date_time
#' @export
flexibleTimeParse <- function(x, timezone, shiftTimeToNoon = TRUE) {
  
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
    if (shiftTimeToNoon) {
      time <- time + hours(12)
    }
  }
  
  #If DV already has time, format using HMS (and leave them the same)
  if(isEmptyOrBlank(time)) {
    format_str <- "Ymd HMS"
    time <- parse_date_time(x,format_str, tz=timezone, quiet = TRUE)
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

#' Given a datetime, will remove time and set 23:59.
#' 
#' @param time datetime to shift to start
#' @rdname toStartOfDay
#' @importFrom lubridate hour<-
#' @importFrom lubridate minute<-
#' @export
toEndOfDay <- function(time) {
  hour(time) <- 23
  minute(time) <- 59
  return(time)
}

#' Given a datetime, will change the date to the 1st, plus remove time and set
#' 00:00. This is used to set the labels for Five Year Groundwater Summary
#' reports.
#' 
#' @param time datetime to shift to start
#' @rdname toEndOfMonth
#' @importFrom lubridate day<-
#' @importFrom lubridate hour<-
#' @importFrom lubridate minute<-
#' @export
toStartOfMonth <- function(time){
  day(time) <- 1
  hour(time) <- 0
  minute(time) <- 0
  return(time)
}

#' Given a datetime, will change the date to the 30th, remove time, and set 
#' 23:59. This is used to set the labels for Five Year Groundwater Summary 
#' reports.
#' 
#' @param time A datetime to shift to start.
#' @rdname toStartOfMonth
#' @importFrom lubridate day<-
#' @importFrom lubridate hour<-
#' @importFrom lubridate minute<-
#' @export
toEndOfMonth <- function(time) {
  day(time) <- 30
  hour(time) <- 23
  minute(time) <- 59
  return(time)
}

#' A hacky fix for 9999 year issue which prevents the rectangles from displaying
#' on the graphs. Apologies to the people of 2100 who have to revisit this.
#'
#' @param time datetime to shift to "the end of time"
#' @importFrom lubridate year<-
#' @export
toEndOfTime <- function(time){
  year(time) <- 2100
  return(time)
}

#' Checks to see if the date passed in is the first of the month
#' 
#' @description For a date passed into the function, returns true or false 
#' depending on if that date is the first of the month
#' 
#' @param date the date to check to see if it's the first of the month
#' @importFrom lubridate day<-
#' @return TRUE or FALSE depending on if the date is the first day of the month
isFirstDayOfMonth <- function(date) {
  isFirst <- day(date) == 1
  return(isFirst)
}

#' Calculate the number of days between two dates
#' 
#' @description For two dates passed into the function returns the number of days
#' between the two dates
#' 
#' @param startDate the start date for calculating the difference in days
#' @param endDate the end date for calculating the difference in days
#' @return days numeric number of days between the two dates
calculateTotalDays <- function(startDate, endDate) {
  days <- as.numeric(difftime(strptime(endDate, format="%Y-%m-%d"), strptime(startDate,format="%Y-%m-%d"), units="days"))
  return(days)
}

