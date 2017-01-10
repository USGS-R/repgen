#' Remove Zero or Negative Values
#' 
#' @description If negative or zero values are passed into function, turn them into NA 
#' so we can log the plot.
#' 
#' @param df A data frame with a column called \code{value} that it will check
#'   for negative or zeroes.
#' 
#' @details Make sure you have the column called \code{value} included in data
#'   frame.
#' @importFrom dplyr filter
removeZeroNegative <- function(df){
  df <- df
  value <- NULL # squelch "no visible binding for global variable" warnings
  df <- filter(df, value > 0)
  return(df)
}

#' Extract time value points list by a single month
#' 
#' @description Subset the data passed into the function by the month passed in
#' 
#' @param pts a list of time and value points
#' @param onlyMonth the numeric month to subset the list of time and value points by
#' 
subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}