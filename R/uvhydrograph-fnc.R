
#'@export
uvhydrographPlot <- function(data){
  uv_pts <- getUvHydro(data, "primarySeries" )
  layout_uvhydro()
  
  primary_lbl <- getUvLabel(data, "primarySeries")
  
  add_uvhydro_axes(getUvhLims(uv_pts), ylab = primary_lbl, ylog = TRUE)
  
  dv_pts <- getUvHydro(data, "derivedSeriesMean" )
  dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
  
  # discharge measurements and errors
  if(isSeriesOfType(data, "primarySeries", "Discharge")) {
    add_q_measurements(data)
  }
  
  add_computed_uv(uv_pts)
  add_approved_dv(dv_pts)
  
  secondary_lbl <- getUvLabel(data, "secondarySeries")
  
  uv2_pts <- getUvHydro(data, "secondarySeries")
  shift_pts <- getUvHydro(data, "effectiveShifts")
  add_uvhydro_axes(getUvhLims(uv2_pts), ylab = secondary_lbl, ylog = FALSE)
  
  add_computed_uv(uv2_pts)
  shifted_pts <- uv2_pts
  shifted_pts[['y']] <- shifted_pts[['y']] + shift_pts[['y']]
  add_edited_uv(shifted_pts)
  
  # gageHeight
  if(isSeriesOfType(data, "secondarySeries", "Gage height")) {
    add_stage_measurements(data)
  }
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
  abline(h = yminor, lty = 4, col = "lightgray")
  
  abline(v = xticks, lty = 4, col = "lightgray")
  abline(v = day1, lty = 1, col = 'black')

  # major axes
  axis(side=1, at=xticks, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels=strftime(xticks, '%d'))
  axis(side=2, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks)
  axis(side=3, at=xticks, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels = NA)
  
}

add_q_measurements <- function(data, ...){
  q <- getFieldVisitErrorBarsQPoints(data)
  if(!is.null(q) && nrow(q)>0) {
    minQ <- getFieldVisitErrorBars(data, 'errorMaxDischarge', as.numeric = TRUE)
    maxQ <- getFieldVisitErrorBars(data, 'errorMinDischarge', as.numeric = TRUE)
    call_text <- getFieldVisitErrorBars(data, 'measurementNumber', as.numeric = TRUE)
    
    arrows(q$x, minQ, q$x, maxQ, angle=90, lwd=.7, code=3, col = 'black', length=.05, ...)
    points(q$x, q$y, pch = 1, bg = 'black', col = 'black', cex = .5, ...)
    add_measurement_numbers(x=q$x, y=q$y, call_text=call_text)
  }
}

add_stage_measurements <- function(data, ...) {
  pts <- getMeanGageHeights(data)
  points(pts$x, pts$y, pch = 16, bg = 'red', col = 'red', cex = .75, ...)
  add_measurement_numbers(x=pts$x, y=pts$y, call_text=pts$n)
}

add_measurement_numbers <- function(x,y, call_text){
  if (length(x) > 0){
    ylim <- par()$usr[3:4]
    y_bmp = diff(ylim)*0.03
    for (i in 1:length(x)){
      text(x[i], y = y[i]+y_bmp, labels = call_text[i], pos = 2, cex = 1, col='red')
    }
  }
}

layout_uvhydro <- function(lims){

  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  par(omi=c(0,0,0,0), mai = c(0.75, 1, 0.05, 0.05))
    
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

.loggedNums <- function(lower = 1, upper = 19, powLims = c(-10,10)){
  powers <- seq(powLims[1], powLims[2])
  loggedNums <- unique(as.vector(sapply(powers, function(p) (lower:upper)*10^p)))
  return(loggedNums)
}