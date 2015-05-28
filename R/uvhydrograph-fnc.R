
#'@export
uvhydrographPlot <- function(data){
  layout_uvhydro()
  
  #legend vector, needs to be dynamically built up with plots
  primary_legend <- getNewLegendFrame()
  uv_pts <- getUvHydro(data, "primarySeries" )
  uv_appr <- getApprovals(data, "primarySeries" )
  uv_lims <- getUvhLims(uv_pts)
  primary_lbl <- getUvLabel(data, "primarySeries")
  
  createPlot(uv_lims, ylab = primary_lbl, ylog = TRUE)
  
  dv_pts <- getUvHydro(data, "derivedSeriesMean" )
  dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
  dv_appr <- getApprovals(data, "derivedSeriesMean" ) 
  
  add_computed_uv(uv_pts)
  add_series_approval(uv_pts, uv_appr)
  add_dv(dv_pts, dv_appr, 4)
  
  # discharge measurements and errors
  if(isSeriesOfType(data, "primarySeries", "Discharge")) {
    add_q_measurements(data)
  }
  add_uvhydro_axes(uv_lims, ylab = primary_lbl, ylog = TRUE)

  #start second plot
  uv2_pts <- getUvHydro(data, "secondarySeries")
  shift_pts <- getUvHydro(data, "effectiveShifts")
  secondary_lims <- getUvhLims(uv2_pts)
  
  secondary_lbl <- getUvLabel(data, "secondarySeries")
  tertiary_lbl <- getUvLabel(data, "effectiveShifts")
  
  createPlot(secondary_lims, ylab = secondary_lbl, ylog = FALSE)
  
  add_computed_uv(uv2_pts)
  shifted_pts <- uv2_pts
  shifted_pts[['y']] <- shifted_pts[['y']] + shift_pts[['y']]
  add_edited_uv(shifted_pts)
  
  add_uvhydro_axes(secondary_lims, ylab = secondary_lbl, ylog = FALSE)
  
  # gageHeight
  if(isSeriesOfType(data, "secondarySeries", "Gage height")) {
    add_stage_measurements(data)
    
    #add effective shift axis, timeseries, and shift measurements
    # NOTE: this is a third plot, so this has to come at the end of this method.
    #third axis
    measuredShifts <- getFieldVisitErrorBarsShifts(data)
    add_third_axes(secondary_lims = secondary_lims, tertiary_pts = shift_pts, tertiary_lbl = tertiary_lbl, measured_shift_pts = measuredShifts)
    add_shift_measurements(measuredShifts)
  }
}

add_edited_uv <- function(pts){
  points(pts$x, pts$y, type = 'l', col = 'blue', lty = 4)
}

add_computed_uv <- function(pts){
  points(pts$x, pts$y, type = 'l', col = 'black', lty = 1)
}

add_estimated_uv <- function(pts){
  # TODO
  col = 'orange'
  lty = 5
  type = 'l'
}

add_dv <- function(points, approvals, pch){
  approvalColors = c("red", "yellow", "blue")
  if(is.null(approvals)) { #default to working/red level for all points if no approvals found
    points(points$x, points$y, pch = pch, type = 'p', col = approvalColors[1], lwd = 2) 
  } else { #for each approval period, plot points in the time range using correct approval color
    for(i in 1:nrow(approvals)) {
      a <- approvals[i,]
      startTime <- a$startTime
      endTime <- a$endTime
      level <- a$level + 1
      pts_subset = points[points$x > startTime & points$x < endTime,]
      points(pts_subset$x, pts_subset$y, pch = pch, type = 'p', col = approvalColors[level], lwd = 2) 
    }
  }
}

add_series_approval <- function(points, approvals) {
  approvalColors = c("red", "yellow", "blue")
  
  if(is.null(approvals)) { #default to working/red level for all points if no approvals found
    points(points$x, rep( 10 ^ par()$usr[3], length(points$y)), pch = 15, type = 'p', col = approvalColors[1]) 
  } else { #for each approval period, plot points in the time range using correct approval color
    #TODO
    for(i in 1:nrow(approvals)) {
      a <- approvals[i,]
      startTime <- a$startTime
      endTime <- a$endTime
      level <- a$level + 1
      pts_subset = points[points$x > startTime & points$x < endTime,]
      points(pts_subset$x, rep( 10 ^ par()$usr[3], length(pts_subset$y)), pch = 15, type = 'p', col = approvalColors[level]) 
    }
    
  }
}


add_third_axes <- function(secondary_lims = NULL, tertiary_pts = NULL, tertiary_lbl = NULL, measured_shift_pts = NULL) {
  if(!is.null(tertiary_pts)) {
    par(new = TRUE)
    
    lims <- getUvhLims(tertiary_pts)
    xaxis <- lims$xlim
    yaxis <- lims$ylim
    
    #expand y limits if error bars bleed over
    if(!is.null(measured_shift_pts)) {
      errorBarLims <- getUvhLims(measured_shift_pts, yMinField = "minShift", yMaxField = "maxShift")
      combinedLims <- combineLims(lims, errorBarLims)
      yaxis = combinedLims$ylim
    }
    
    mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
    mn_tck = 50
    mn_tkL = 0.005
    mj_tck = 10
    mj_tkL = 0.01
    ax_lab = 0.55 # scale
    num_maj_y = 7
    num_min_y = 15 #only used when ylog = F
    
    # main plot area
    plot(type="n", x=NA, y=NA, xlim=secondary_lims$xlim, ylim=yaxis, log = '',
         xlab=" ", ylab='', xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i', axes=FALSE)
    
    xticks <- seq(round(xaxis[1]), round(xaxis[2]), by = 'days')
    day1 <- xticks[strftime(xticks, format = '%d') == "01"]

    yticks <- pretty(par()$usr[3:4], num_maj_y)
    yminor <- pretty(par()$usr[3:4], num_min_y)
    
    
    # major axes
    axis(side=4, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks, ylab=tertiary_lbl)
    
    mtext(side = 4, line = 1, tertiary_lbl)
    
    #points
    points(tertiary_pts$x, tertiary_pts$y, type = 'l', col = 'green', lty = 1)
  }
}

getUvhLims <- function(pts = NULL, xMinField = 'x', xMaxField = 'x', yMinField = 'y', yMaxField = 'y'){
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

combineLims<- function(lims1, lims2, ...) {
  x_mx <- max(lims1$xlim[2], lims2$xlim[2], na.rm = TRUE)
  x_mn <- min(lims1$xlim[1], lims2$xlim[1], na.rm = TRUE)
  y_mx <- max(lims1$ylim[2], lims2$ylim[2], na.rm = TRUE)
  y_mn <- min(lims1$ylim[1], lims2$ylim[1], na.rm = TRUE)
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in points. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}

createPlot <- function(lims, ylog = TRUE, ylab) {
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, log = ifelse(ylog,'y',''),
       xlab=" ", ylab=ylab, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i')
}

add_uvhydro_axes <- function(lims, ylog = TRUE, ylab){
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
  
  # label time axis
  mtext(text = paste(xaxis[1], " thru ", xaxis[2]), side = 1, line = .5, cex = .75)
}

add_q_measurements <- function(data, ...){
  q <- getFieldVisitErrorBarsQPoints(data)
  if(!is.null(q) && nrow(q)>0) {
    arrows(q$x, q$minQ, q$x, q$maxQ, angle=90, lwd=.7, code=3, col = 'black', length=.05, ...)
    points(q$x, q$y, pch = 1, bg = 'black', col = 'black', cex = .5, ...)
    add_label(x=q$x, y=q$y, call_text=q$n)
  }
}

add_shift_measurements <- function(shiftsMeasurements, ...){
  if(!is.null(shiftsMeasurements) && nrow(shiftsMeasurements)>0) {
    arrows(shiftsMeasurements$x, shiftsMeasurements$minShift, shiftsMeasurements$x, shiftsMeasurements$maxShift, angle=90, lwd=.7, code=3, col = 'green4', length=.05, ...)
    points(shiftsMeasurements$x, shiftsMeasurements$y, pch = 1, bg = 'green4', col = 'green4', cex = .5, ...)
  }
}

add_stage_measurements <- function(data, ...) {
  pts <- getMeanGageHeights(data)
  points(pts$x, pts$y, pch = 16, bg = 'red', col = 'red', cex = .75, ...)
  add_label(x=pts$x, y=pts$y, call_text=pts$n)
}

add_label <- function(x,y, call_text){
  if (length(x) > 0){
    ylim <- par()$usr[3:4]
    y_bmp = diff(ylim)*0.03
    for (i in 1:length(x)){
      text(x[i], y = y[i]+y_bmp, labels = call_text[i], pos = 2, cex = .5, col='red')
    }
  }
}

layout_uvhydro <- function(lims){

  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  par(omi=c(0,0,0,0), mai = c(1, .5, 0, 0.5))
    
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