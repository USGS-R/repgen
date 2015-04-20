
#'@export
uvhydrographPlot <- function(data){
  
  
  browser()
  uv_pts <- getComputedUvDischarge(data, required=TRUE)
  layout_uvhydro(getUvhLims(uv_pts))
  
  dv_pts <- getDvDischarge(data)
  dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
  
  
  add_computed_uv(uv_pts)
  add_approved_dv(dv_pts)
  
}


add_meas_w_error <- function(times, points){
  
  
  #baseline points
  
}
add_computed_uv <- function(pts){
  
  points(pts$x, pts$y, type = 'l', col = 'black', lty = 1)
}

add_estimated_uv <- function(times, points){
  
  col = 'orange'
  lty = 5
  type = 'l'
}

add_approved_dv <- function(points){
  pch = c(4, 15) # for x and box
  col = 'blue'
  type = 'p'
  points(points$x, points$y, pch = pch[1], type = type, col = col, lwd = 2)
  points(points$x, rep( 10 ^ par()$usr[3], length(points$y)), pch = pch[2], type = type, col = col)
}

add_review_dv <- function(times, points){
  pch = c(4, 15) # for x and box
  col = 'yellow'
  type = 'p'
  points(times, points, pch = pch[1], type = type, col = col, lwd = 2)
  points(times, rep( 10 ^ par()$usr[3], length(points)), pch = pch[2], type = type, col = col)
}

add_working_dv <- function(times, points){
  pch = c(4, 15) # for x and box
  col = 'yellow'
  type = 'p'
}


getUvhLims <- function(pts){
  
  
  
  x_mx <- max(pts$x, na.rm = TRUE)
  x_mn <- min(pts$x, na.rm = TRUE)
  y_mx <- max(pts$y, na.rm = TRUE)
  y_mn <- min(pts$y, na.rm = TRUE)
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in shiftPoints or stagePoints. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}
layout_uvhydro <- function(lims){
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  
  par(omi=c(0,0,0,0), mai = c(1, 1, 0.1, 0.0))
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, log = 'y',
       xlab=" ", ylab='Discharge in CFS', xaxt="n", yaxt="n", mgp=mgp$y)
  
  xticks <- seq(round(xaxis[1]), round(xaxis[2]), by = 'days')
  
  # gridlines
  abline(
    h   = c( seq( 1, 9, 1 ), seq( 10, 90, 10 ), seq( 100, 900, 100 ) , seq( 1000, 9000, 1000 ), seq( 10000, 100000, 10000 )),
    lty = 3, col = "lightgray")

  abline(v   = xticks,
    lty = 3, col = "lightgray")
  
  
  # major axes
  axis(side=1, at=xticks, cex.axis=0.5, tck=mj_tkL, mgp=mgp$x, labels=strftime(xticks, '%d'))
  axis(side=2, at=c(1,10,100,1000,10000), cex.axis=0.5, las=2, tck=mj_tkL, mgp=mgp$y, labels=c(1,10,100,1000,10000))
  
}