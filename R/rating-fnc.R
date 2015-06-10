
#'@export
ratingPlot <- function(data, page = "1"){
  #for pagination by month, get all of the month strings for the primary series
  layout_rating()
  lims <- list()
  
  currentRating <- getCurrentRating(data)
  previousRating <- getPreviousRating(data)
  tenYearHighs <- getTopTenGage(data, page)
  minMaxStage <- getMaxMinStage(data, page)
  fieldVisits <- getFieldVisits(data, page)
  
  
  lims$xlim <- calcMinMax(currentRating$x, previousRating$x, tenYearHighs$x, fieldVisits$x)
  lims$ylim <- calcMinMax(currentRating$y, previousRating$y, tenYearHighs$y, fieldVisits$y, minMaxStage$y)

  createNewRatingPlot(lims, ylab = 'Stage in feet', xlab = 'Discharge in cubic feet per second', yaxs='r', xaxs='r')
  par(usr=c(0.7812262, 6, 0.2076904, 1.4924073))
  add_current_ratings(currentRating)
  add_previous_ratings(previousRating)
  add_rating_measurements(fieldVisits)
  add_top_ten(tenYearHighs)
  #
  add_min_max(pts = minMaxStage)
}

add_rating_measurements <- function(pts, ...){
  points(pts$x, pts$y, bg = 'red', col='black', pch=21, lwd=1.5, ...)
  text(pts$x,pts$y, labels = pts$id, pos = 3)
}

add_top_ten <- function(pts, ...){
  points(pts$x, pts$y, bg = 'green', col='black', pch=21, lwd=1.5, ...)
}

add_current_ratings <- function(pts, ...){
  lines(pts$x, pts$y, col='black', lty=1, lwd=2, ...)
}

add_previous_ratings <- function(pts, ...){
  lines(pts$x, pts$y, col='black', lty=2, lwd=2, ...)
}

add_min_max <- function(pts, ...){ # should be generic for other plotters?
  # get lims of plot, use for 2 line plots
  
  line_prc <- 0.05 # percent of width of plot
  line_strt <- 0.1 # percent from right side of axis
  
  x_usr <- par("usr")[1:2]
  x_span <- diff(x_usr)
  x1 <- x_usr[2]-x_span*line_strt
  x2 <- x_usr[2]-x_span*(line_strt-line_prc)
  x = c(x1,x2)
  if (par()$xlog){
    x <- 10^x
  }
    
  
  lines(x, y=pts$y, lwd=3, col='red')
}

add_assoc_measurement <- function(pts, call_outs, ...){
  
}

layout_rating <- function(){
  par(omi=c(0,0,0,0), mai = c(0.5, 0.5, 0.1, 0.1))
}


createNewRatingPlot <- function(lims, ylog = TRUE, xlog = TRUE, ylab, ...) {
  
  log=paste(c(ifelse(xlog,'x',''),ifelse(ylog,'y','')),collapse='')
  
  num_maj_x = 7
  num_min_x = 40
  num_maj_y = 7
  num_min_y = 40 #only used when ylog = F
  
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


calcMinMax <- function(...){
  
  
  return(c(min(..., na.rm = T),max(..., na.rm = T)))
}

resizePlot <- function(x,y){
  # checks par()$usr and adjusts if resizing is needed
  
}