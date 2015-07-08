
#'@export
uvhydrographPlot <- function(data){
  #for pagination by month, get all of the month strings for the primary series
  all_primary_pts <- getUvHydro(data, "primarySeries" )
  months <- unique(all_primary_pts$month, incomparables = FALSE)
  
  layout_uvhydro()
  
  #breaking up plot according to time period
  for (month in months){  
    #legend vector, needs to be dynamically built up with plots
    primary_legend <- getNewLegendFrame()
    addToPrimaryLegend <- function(newText, newSymbol, newColor, newLine) { 
      primary_legend <<- rbind(primary_legend, data.frame(text = newText, symbol = newSymbol, color = newColor, line = newLine, stringsAsFactors = FALSE))
    }
    
    uv_pts <- subsetByMonth(getUvHydro(data, "primarySeries" ), month)
    estimated_uv_pts <- subsetByMonth(getUvHydro(data, "primarySeries", estimatedOnly=TRUE), month)
    uv_pts_raw <- subsetByMonth(getUvHydro(data, "primarySeriesRaw" ), month)
    uv_appr <- getApprovals(data, "primarySeries" )
    uv_lims <- getUvhLims(uv_pts)
    primary_lbl <- getUvLabel(data, "primarySeries")
    uv_comp_pts <- subsetByMonth(getUvHydro(data, "comparisonSeries" ), month)
    uv_comp_lbl <- getUvName(data, "comparisonSeries")
    if(!is.null(uv_comp_pts) && nrow(uv_comp_pts)>0) {
      uv_lims <- combineLims(uv_lims, getUvhLims(uv_comp_pts))# expand lims for second graph
    }
    
    createNewUvHydrographPlot(uv_lims, ylab = primary_lbl, ylog = TRUE)
 
    primary_corrections <- getCorrections(data, "primarySeriesCorrections")
    if(!is.null(primary_corrections) && nrow(primary_corrections)>0) {
      primary_corrections <- subsetByMonth(primary_corrections, month)
      add_correction_lines(primary_corrections, addToPrimaryLegend, uv_lims$ylim[2])
    }
    
    add_corrected_uv(uv_pts, label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_estimated_uv(estimated_uv_pts, label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_uncorrected_uv(uv_pts_raw, label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_series_approval(uv_pts, uv_appr, label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_comparison_uv(uv_comp_pts, label=uv_comp_lbl, addToLegend=addToPrimaryLegend)
    add_mean(data, getApprovals(data, "derivedSeriesMean" ), month, "derivedSeriesMean", label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_max(data, getApprovals(data, "derivedSeriesMax" ), month, "derivedSeriesMax", label=primary_lbl, addToLegend=addToPrimaryLegend)
    add_min(data, getApprovals(data, "derivedSeriesMin" ), month, "derivedSeriesMin", label=primary_lbl, addToLegend=addToPrimaryLegend)
    
    # discharge measurements and errors
    if(isSeriesOfType(data, "primarySeries", "Discharge")) {
      add_q_measurements(data, month=month, addToLegend=addToPrimaryLegend)
    }
    
    add_wq_measurements(data, month=month, addToLegend=addToPrimaryLegend)
    
    add_uvhydro_axes(uv_lims, ylab = primary_lbl, ylog = TRUE)
    addLegend(primary_legend);
    
    #start second plot
    secondary_legend <- getNewLegendFrame()
    addToSecondaryLegend <- function(newText, newSymbol, newColor, newLine) { 
      secondary_legend <<- rbind(secondary_legend, data.frame(text = newText, symbol = newSymbol, color = newColor, line = newLine, stringsAsFactors = FALSE))
    }
    
    uv2_pts <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
    estimated_uv2_pts <- subsetByMonth(getUvHydro(data, "secondarySeries", estimatedOnly=TRUE), month)
    uv2_pts_raw <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
    secondary_lims <- getUvhLims(uv2_pts)
    
    secondary_lbl <- getUvLabel(data, "secondarySeries")
    
    createNewUvHydrographPlot(secondary_lims, ylab = secondary_lbl, ylog = FALSE)
    
    secondary_corrections <- getCorrections(data, "secondarySeriesCorrections")
    if(!is.null(secondary_corrections) && nrow(secondary_corrections)>0) {
      secondary_corrections <- subsetByMonth(secondary_corrections, month)
      add_correction_lines(secondary_corrections, addToSecondaryLegend, secondary_lims$ylim[2])
    }
    
    add_corrected_uv(uv2_pts, label=secondary_lbl, addToLegend=addToSecondaryLegend)
    add_estimated_uv(estimated_uv2_pts, label=secondary_lbl, addToLegend=addToSecondaryLegend)
    add_uncorrected_uv(uv2_pts_raw, label=secondary_lbl, addToLegend=addToSecondaryLegend)
    
    add_uvhydro_axes(secondary_lims, ylab = secondary_lbl, ylog = FALSE)
    
    # gageHeight
    if(isSeriesOfType(data, "secondarySeries", "Gage height")) {
      add_stage_measurements(data, month=month, addToLegend=addToSecondaryLegend)
    }
    
    #GW level
    if(isSeriesOfType(data, "secondarySeries", "WaterLevel, BelowLSD")) {
      add_gw_level_measurements(data, month=month, addToLegend=addToSecondaryLegend)
    }
    
    shift_pts <- subsetByMonth(getUvHydro(data, "effectiveShifts"), month)
    tertiary_lbl <- getUvLabel(data, "effectiveShifts")
    
    #add effective shift axis, timeseries, and shift measurements
    # NOTE: this is a third plot, so this has to come at the end of this method.
    #third axis
    measuredShifts <- subsetByMonth(getFieldVisitErrorBarsShifts(data), month)
    add_uv_shift(secondary_lims = secondary_lims, secondary_lbl=secondary_lbl, tertiary_pts = shift_pts, tertiary_lbl = tertiary_lbl, 
                 measured_shift_pts = measuredShifts, addToLegend=addToSecondaryLegend)
    
    add_shift_measurements(measuredShifts, addToLegend=addToSecondaryLegend)
    
    addLegend(secondary_legend);
  }
}

subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
}

add_uncorrected_uv <- function(pts, label, addToLegend){
  points(pts$x, pts$y, type = 'l', col = 'blue', lty = 4)
  addToLegend(paste("Uncorrected UV ", label), NA, 'blue', 4)
}

add_corrected_uv <- function(pts, label, addToLegend){
  points(pts$x, pts$y, type = 'l', col = 'black', lty = 1)
  addToLegend(paste("Corrected UV ", label), NA, "black", 1)
}

add_estimated_uv <- function(pts, label, addToLegend){
  if(!is.null(pts) && nrow(pts)>0) {
    points(pts$x, pts$y, type = 'l', col = 'orange', lty = 1, lwd=2)
    addToLegend(paste("Estimated UV ", label), NA, "Orange", 1)
  }
}

add_comparison_uv <- function(pts, label, addToLegend){
  if(!is.null(pts) && nrow(pts)>0) {
    points(pts$x, pts$y, type = 'l', col = 'green', lty = 1)
    addToLegend(paste("Comparison", label), NA, "green", 1)
  }
}

add_mean <- function(data, dv_appr, month, field, label, addToLegend) {
  dv_pts <- subsetByMonth(getUvHydro(data, field ), month)
  if(!is.null(dv_pts) && nrow(dv_pts)>0) {
    dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
    add_dv(dv_pts, dv_appr, 5, label=paste("Mean", label), addToLegend=addToLegend)
  }
}

add_max <- function(data, dv_appr, month, field, label, addToLegend) {
  dv_pts <- subsetByMonth(getUvHydro(data, field ), month)
  if(!is.null(dv_pts) && nrow(dv_pts)>0) {
    dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
    add_dv(dv_pts, dv_appr, 2, label=paste("Max", label), addToLegend=addToLegend)
  }
}

add_min <- function(data, dv_appr, month, field, label, addToLegend) {
  dv_pts <- subsetByMonth(getUvHydro(data, field ), month)
  if(!is.null(dv_pts) && nrow(dv_pts)>0) {
    dv_pts$x = dv_pts$x + 86400/2 # manual shift for now...
    add_dv(dv_pts, dv_appr, 6, label=paste("Min", label), addToLegend=addToLegend)
  }
}


add_dv <- function(points, approvals, pch, label, addToLegend){
  approvalColors = c("red", "yellow", "blue")
  approvalDescriptions = c("Working", "In-review", "Approved")
  if(is.null(approvals)) { #default to working/red level for all points if no approvals found
    points(points$x, points$y, pch = pch, type = 'p', col = approvalColors[1], lwd = 2) 
    addToLegend(paste("Working DV ", label, sep = ""), pch, approvalColors[1], NA)
  } else { #for each approval period, plot points in the time range using correct approval color
    for(i in 1:nrow(approvals)) {
      a <- approvals[i,]
      startTime <- a$startTime
      endTime <- a$endTime
      level <- a$level + 1
      pts_subset = points[points$x > startTime & points$x < endTime,]
      points(pts_subset$x, pts_subset$y, pch = pch, type = 'p', col = approvalColors[level], lwd = 1) 
      addToLegend(paste(approvalDescriptions[level], " DV ", label, sep = ""), pch, approvalColors[level], NA)
    }
  }
}

add_correction_lines <- function(corrections, addToLegend, yLowerLim) {
  if(!is.null(corrections) && nrow(corrections)>0) {
    abline(v=corrections$x, untf = FALSE, col="blue")
    add_label(x=corrections$x, y=rep(yLowerLim, nrow(corrections)), call_text=corrections$comment, srt=90)
    addToLegend("(vert. blue line) Data correction entry", 3, "blue", NA)
  }
}

add_series_approval <- function(points, approvals, label, addToLegend) {
  approvalColors = c("red", "yellow", "blue")
  approvalDescriptions = c("Working", "In-review", "Approved")
  
  if(is.null(approvals)) { #default to working/red level for all points if no approvals found
    points(points$x, rep( 10 ^ par()$usr[3], length(points$y)), pch = 15, type = 'p', col = approvalColors[1]) 
    addToLegend(paste("Working, UV ", label, sep = ""), 15, approvalColors[1], NA)
  } else { #for each approval period, plot points in the time range using correct approval color
    #TODO
    for(i in 1:nrow(approvals)) {
      a <- approvals[i,]
      startTime <- a$startTime
      endTime <- a$endTime
      level <- a$level + 1
      pts_subset = points[points$x > startTime & points$x < endTime,]
      points(pts_subset$x, rep( 10 ^ par()$usr[3], length(pts_subset$y)), pch = 15, type = 'p', col = approvalColors[level]) 
      addToLegend(paste(approvalDescriptions[level], " UV ", label, sep = ""), 15, approvalColors[level], NA)
    }
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

createNewUvHydrographPlot <- function(lims, ylog = TRUE, ylab) {
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
       xlab=" ", line = 2, ylab=ylab, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i')
  
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
}

add_uvhydro_axes <- function(lims, ylog = TRUE, ylab){
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  
  mgp = list(y=c(0,0,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  ax_lab = 0.75 # scale
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

  # major axes
  axis(side=1, at=xticks, cex.axis=ax_lab, tck=mj_tkL, mgp=mgp$x, labels=strftime(xticks, '%d'))
  axis(side=2, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks)
  
  # label time axis
  mtext(text = paste(xaxis[1], " thru ", xaxis[2]), side = 1, line = .75, cex = .75)
}

add_uv_shift <- function(secondary_lims = NULL, secondary_lbl = NULL, tertiary_pts = NULL, tertiary_lbl = NULL, measured_shift_pts = NULL, addToLegend) {
  if(!is.null(tertiary_pts) && nrow(tertiary_pts)>0) {
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
    ax_lab = 0.75 # scale
    num_maj_y = 7
    num_min_y = 15 #only used when ylog = F
    
    # main plot area
    par(new = TRUE)
    plot(type="l", x=tertiary_pts$x, y=tertiary_pts$y, xlim=secondary_lims$xlim, ylim=yaxis, log = '',
         xlab=NA, ylab=NA, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i', axes=FALSE, col = 'green3', lty = 1)
    
    yticks <- pretty(par()$usr[3:4], num_maj_y)
    yminor <- pretty(par()$usr[3:4], num_min_y)
    
    # major axes
    axis(side=4, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks, ylab=tertiary_lbl)
    mtext(side = 4, line = 2, tertiary_lbl, cex = .75)
    
    addToLegend(paste(secondary_lbl, tertiary_lbl), NA, "green3", 1)
  }
}

addLegend<- function(legendVector) {
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab=" ",ylab=" ")
  cols <- NROW(legendVector) %/% 3;
  if(NROW(legendVector) %% 3 > 0) {
    cols <- cols + 1
  }
  legend("bottom", legend=legendVector$text, xpd = TRUE, horiz = FALSE, inset = c(0, 0), bty = "n", 
         pch=legendVector$symbol, col=legendVector$color, lty=legendVector$line, cex = 1, ncol=cols) 
}

add_q_measurements <- function(data, month, addToLegend, ...){
  q <- subsetByMonth(getFieldVisitErrorBarsQPoints(data), month)
  if(!is.null(q) && nrow(q)>0) {
    arrows(q$x, q$minQ, q$x, q$maxQ, angle=90, lwd=.7, code=3, col = 'black', length=.05, ...)
    points(q$x, q$y, pch = 1, bg = 'black', col = 'black', cex = .8, ...)
    addToLegend("Discharge measurement and error", 1, 'black', NA)
    add_label(x=q$x, y=q$y, call_text=q$n)
  }
}

add_wq_measurements <- function(data, month, addToLegend, ...){
  q <- subsetByMonth(getWaterQualityMeasurements(data), month)
  if(!is.null(q) && nrow(q)>0) {
    points(q$x, q$y, pch = 8, bg = 'orange', col = 'orange', cex =1.2, ...)
    addToLegend("NWIS-RA WQ Measurement", 8, 'orange', NA)
  }
}

add_shift_measurements <- function(shiftsMeasurements, addToLegend, ...){
  if(!is.null(shiftsMeasurements) && nrow(shiftsMeasurements)>0) {
    arrows(shiftsMeasurements$x, shiftsMeasurements$minShift, shiftsMeasurements$x, shiftsMeasurements$maxShift, 
           angle=90, lwd=.7, code=3, col = 'green4', length=.05, ...)
    points(shiftsMeasurements$x, shiftsMeasurements$y, pch = 1, bg = 'green4', col = 'green4', cex = .8, ...)
    addToLegend("Effective shift and error", 1, 'green4', NA)
  }
}

add_stage_measurements <- function(data, month, addToLegend, ...) {
  pts <- subsetByMonth(getMeanGageHeights(data), month)
  points(pts$x, pts$y, pch = 1, bg = 'black', col = 'black', cex = .8, ...)
  addToLegend("Gage height measurement", 1, 'black', NA)
  add_label(x=pts$x, y=pts$y, call_text=pts$n)
}

add_gw_level_measurements <- function(data, month, addToLegend, ...) {
  pts <- subsetByMonth(getGroundWaterLevels(data), month)
  points(pts$x, pts$y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, ...)
  addToLegend("Measured Water Level (NWIS-RA)", 8, 'orange', NA)
}

add_label <- function(x,y, call_text, srt = 0){
  if (length(x) > 0){
    ylim <- par()$usr[3:4]
    y_bmp = diff(ylim)*0.03
    for (i in 1:length(x)){
      text(x[i], y = y[i]+y_bmp, labels = call_text[i], pos = 2, cex = .75, col='red', srt = srt)
    }
  }
}

layout_uvhydro <- function(months){
  panels <- matrix(c(1,2,3,4), nrow = 4)
  layout(panels, heights=c(4,1,4,1)) #2 plots, 2 legends
  par(omi=c(0,0,0,0), mai = c(0.25, .5, .1, 0.5))
    
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