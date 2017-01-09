#' @export
# user specified option to treat negative/zero values as NA in order to have the plot logged
removeZeroNegative <- function(df){
  df <- df %>% 
    filter(value > 0)
  return(df)
}

subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}