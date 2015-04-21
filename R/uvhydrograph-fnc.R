
#'@export
uvhydrographPlot <- function(data){
  
  
  uv_pts <- getUvHydro(data, "discharge" )
  layout_uvhydro()
  
  add_uvhydro_axes(getUvhLims(uv_pts), ylab = "Discharge in CFS", ylog = TRUE)
  
  dv_pts <- getUvHydro(data, "dailyDischarge" )
  dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
  
  
  add_computed_uv(uv_pts)
  add_approved_dv(dv_pts)
  
  gage_pts <- getUvHydro(data, "gageHeight")
  shift_pts <- getUvHydro(data, "effectiveShifts")
  add_uvhydro_axes(getUvhLims(gage_pts), ylab = "Gage height in feet", ylog = FALSE)
  
  add_computed_uv(gage_pts)
  shifted_pts <- gage_pts
  shifted_pts[['y']] <- shifted_pts[['y']] + shift_pts[['y']]
  add_edited_uv(shifted_pts)
}

add_edited_uv <- function(pts){
  points(pts$x, pts$y, type = 'l', col = 'blue', lty = 4)
}

add_meas_w_error <- function(times, points){
  
  #baseline points
}
add_computed_uv <- function(pts){
  
  points(pts$x, pts$y, type = 'l', col = 'black', lty = 1)
}

add_estimated_uv <- function(pts){
  
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

add_review_dv <- function(pts){
  pch = c(4, 15) # for x and box
  col = 'yellow'
  type = 'p'
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
    stop('missing or NA values in points. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}

add_uvhydro_axes <- function(lims, ylog = TRUE, ylab ){
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  ax_lab = 0.55 # scale
  num_maj_y = 7
  num_min_y = 15 #only used when ylog = F

  # main plot area
  
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, log = ifelse(ylog,'y',''),
       xlab=" ", ylab=ylab, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i')
  
  xticks <- seq(round(xaxis[1]), round(xaxis[2]), by = 'days')
  day1 <- xticks[strftime(xticks, format = '%d') == "01"]
  if (ylog){
    yticks <- .closestLogged(10^pretty(par()$usr[3:4], num_maj_y))
    yminor <- .betweenLogs(10^par()$usr[3:4])
  } else {
    yticks <- pretty(par()$usr[3:4], num_maj_y)
    yminor <- pretty(par()$usr[3:4], num_min_y)
  }
  
  # gridlines
  abline(h = yminor, lty = 3, col = "lightgray")
  
  abline(v = xticks, lty = 3, col = "lightgray")
  abline(v = day1, lty = 1, col = 'black')

  # major axes
  axis(side=1, at=xticks, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels=strftime(xticks, '%d'))
  axis(side=2, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks)
  axis(side=3, at=xticks, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels = NA)
  
}

layout_uvhydro <- function(lims){

  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  par(omi=c(0,0,0,0), mai = c(1, 1, 0.05, 0.05))
    
}

.closestLogged <- function(numbers){
  loggedNums <- .loggedNums()
  closestNums <- sapply(numbers, function(n) loggedNums[which.min(abs(n-loggedNums))])
  return(closestNums)
}

.betweenLogs <- function(range){
  loggedNums <- .loggedNums()
  return(loggedNums[loggedNums >= range[1] &  loggedNums <= range[2]])
}

.loggedNums <- function(lower = 1, upper = 19){
  powers <- seq(-10,10)
  loggedNums <- unique(as.vector(sapply(powers, function(p) (lower:upper)*10^p)))
  return(loggedNums)
}