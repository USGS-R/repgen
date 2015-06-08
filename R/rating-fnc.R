
#'@export
ratingPlot <- function(data){
  #for pagination by month, get all of the month strings for the primary series
  layout_rating()
  
}

add_rating_measurements <- function(){
  col = 'black'  
  bg = 'red'
  pch = 1
}

add_high_low_measurements <- function(){
  col = 'black'  
  bg = 'green'
  pch = 1
}

add_previous_ratings <- function(){
  lty = 2
  col = 'black'
}

add_max_min <- function(){ # should be generic for other plotters?
  col='red'
  lwd = 2
}

add_assoc_measurement <- function(){
  col='red'
  pch=8
  lwd=1.5
}

getRatingLims <- function(pts = NULL, xMinField = 'x', xMaxField = 'x', yMinField = 'y', yMaxField = 'y'){
  x_mx <- max(pts[[xMaxField]], na.rm = TRUE)
  x_mn <- min(pts[[xMinField]], na.rm = TRUE)
  y_mx <- max(pts[[yMaxField]], na.rm = TRUE)
  y_mn <- min(pts[[yMinField]], na.rm = TRUE)
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in points. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}