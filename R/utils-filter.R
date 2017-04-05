#' Remove zero or negative values
#' 
#' @description If negative or zero values are passed into function, turn them into NA 
#' so we can log the plot
#' 
#' @param df any data frame with a column called value that it will check for negative or zeroes
#' 
#' @details make sure you have the column called value included in data frame
#' @importFrom dplyr filter
removeZeroNegative <- function(df, column='value'){
  df <- df
  df <- filter(df, df[[column]] > 0)
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