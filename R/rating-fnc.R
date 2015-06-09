
#'@export
ratingPlot <- function(data, page = "1"){
  #for pagination by month, get all of the month strings for the primary series
  layout_rating()
  
  currentRating <- getCurrentRating(data)
  previousRating <- getPreviousRating(data)
  lims <- getRatingLims(currentRating)
  createNewRatingPlot(lims, ylab = 'Stage in feet', xlab = 'Discharge in cubic feet per second', ylog = TRUE)
  
  add_current_ratings(currentRating)
  add_previous_ratings(previousRating)
  
  #discharge <- getHistoricalDischarge#getFieldMeasuredDischarge(data)
  tenYearHighs <- getTopTenGage(data, page)
  
  #add_rating_measurements(pts = discharge[c('x','y')], call_outs = discharge[['id']])
  add_top_ten(tenYearHighs[c('x','y')])
}

add_rating_measurements <- function(pts, call_outs, ...){
  points(pts$x, pts$y, bg = 'red', col='black', pch=21, lwd=1.5,...)
  text(pts$x,pts$y, labels = call_outs, pos = 3)
}

add_top_ten <- function(pts, ...){
  points(pts$x, pts$y, bg = 'green', col='black', pch=21, lwd=1.5,...)
}

add_current_ratings <- function(pts, ...){
  lines(pts$x, pts$y, col='black', lty=1, lwd=2, ...)
}

add_previous_ratings <- function(pts, ...){
  lines(pts$x, pts$y, col='black', lty=2, lwd=2, ...)
}

add_max_min <- function(){ # should be generic for other plotters?
  col='red'
  lwd = 2
}

add_assoc_measurement <- function(pts, call_outs, ...){
  
}

layout_rating <- function(){
  par(omi=c(0,0,0,0), mai = c(0.5, 0.5, 0.1, 0.1))
}


createNewRatingPlot <- function(lims, ylog = TRUE, xlog = TRUE, ylab, ...) {
  
  log=paste(c(ifelse(xlog,'x',''),ifelse(ylog,'y','')),collapse='')
  
  num_maj_x = 7
  num_min_x = 20
  num_maj_y = 7
  num_min_y = 20 #only used when ylog = F
  
  ymajor <- logTicks(lims$ylim, num_maj_y)
  yminor <- logTicks(lims$ylim, num_min_y)
  
  xmajor <- logTicks(lims$xlim, num_maj_x)
  xminor <- logTicks(lims$xlim, num_min_x)
  
  
  ticks <- list(xmajor=xmajor, xminor=xminor, 
                ymajor=ymajor, yminor=yminor, 
                xtickLabel=list(value = xmajor, 
                                text = xmajor),
                ytickLabel=list(value = ymajor,
                                text = ymajor))
  
  
  newGridPlot(lims, log=log, ylab=ylab, ticks=ticks, 
              ycol=c('minor'="lightgray", 'major'='lightgray'),
              xcol=c('minor'="lightgray", 'major'='lightgray'),
              xlty = c('minor'=4, 'major'=4),
              ylty = c('minor'=4, 'major'=4), ...)
  
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