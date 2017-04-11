#' Called from V diagram R Markdown files.
#' 
#' @param reportObject V diagram report data.
renderVDiagram <- function(reportObject) {
  
  options(scipen = 8)
  
  styles <- getVDiagramStyle()
  
  measurements <- parseFieldMeasurementData(reportObject)
  shifts <- parseRatingShiftsData(reportObject)
  
  maxStage <- fetchMaxStage(reportObject)
  validParam(maxStage, "maxStage")
  
  minStage <- fetchMinStage(reportObject)
  validParam(minStage, "minStage")
  
  #Check if we have any data to plot. If we don't, return NULL
  if(!hasEnoughVdiagramData(shifts)){
    return(NULL)
  }
  
  vplot <- gsplot(mar = c(7, 3, 4, 2), yaxs = "r", xaxs = "r") %>%
    points(NA, NA, axes = FALSE) %>% 
    view(ylab = styles$plot$ylab, xlab = styles$plot$xlab)
  
  vplot <- do.call(grid, append(list(object = vplot), styles$grid))
  
  # max./min. stage lines at top/bottom of plot
  vplot <- do.call(abline, append(
    list(object = vplot, a = maxStage), styles$maxStageLine
  ))
  vplot <- do.call(abline, append(
    list(object = vplot, a = minStage), styles$minStageLine
  ))
  
  vplot <- addMeasurementsAndError(vplot, measurements, styles)
  vplot <- addRatingShifts(vplot, shifts, styles)

  vplot <- testCallouts(vplot, xlimits = xlim(vplot)$side.1)
  
  ylims <- c(
      min(c(ylim(vplot)$side.2, minStage)),
      max(c(ylim(vplot)$side.2, maxStage))
      )
  xlims <- xlim(vplot)$side.1
  y_seq <- pretty(ylims, shrink.sml = 20)
  x_seq <- pretty(xlims, shrink.sml = 20)
  
  vplot <- axis(vplot, side = 1, at = x_seq) %>%
    axis(side=c(2,4), at = y_seq) %>% 
    view(side = 2, ylim = ylims)
  
  return(vplot)
}

#' Has Enough Vdiagram Data
#' @description returns true if we have enough data to plot
#' @param shifts the data in this report
#' return true or false if enough data exists to render a meaningful plot
hasEnoughVdiagramData <- function(shifts) {
  relevantShiftData <- unname(unlist(shifts[c("shiftId")]))
  relevantShiftData <- relevantShiftData[which(!is.na(relevantShiftData))]
  hasEnough <- !isEmptyOrBlank(relevantShiftData)
  
  return(hasEnough)
}


#' Add Measurements and Errors
#' Given a gsplot object, will add measurements to the plot. Measurements points and errors styled differently if they are historical or not
#' @param vplot the gsplot object for the vdiagram
#' @param measurements measurements information
#' @param styles a list of styles to be used for styling the points
addMeasurementsAndError <- function(vplot, measurements, styles) {
  histFlag <- measurements$histFlag
 
    if (any(histFlag)){
      # TODO replace with below when working
      #error_bar(gsNew, x=1:3, y=c(3,1,2), x.low=c(.2,NA,.2), x.high=.2, col="red",lwd=3)
      
      arrow_notNA <- intersect(which(!is.na(measurements$minShift)), which(!is.na(measurements$maxShift)))
      arrow_notNA_hist <- intersect(arrow_notNA, which(histFlag))
      minShift <- measurements$minShift[arrow_notNA_hist]
      maxShift <- measurements$maxShift[arrow_notNA_hist]
      obsGage <- measurements$obsGage[arrow_notNA_hist]
      if (!isEmptyOrBlank(maxShift) || !isEmptyOrBlank(minShift) || !isEmptyOrBlank(obsGage)) {
        vplot <- do.call(arrows, append(list(object=vplot, x0=minShift, y0=obsGage, 
                                             x1=maxShift, y1=obsGage), styles$err_lines_historic))
      }
      point_notNA_hist <- intersect(which(!is.na(measurements$obsShift)), which(histFlag))
      x <- measurements$obsShift[point_notNA_hist]
      y <- measurements$obsGage[point_notNA_hist]
      if (!isEmptyOrBlank(x) || !isEmptyOrBlank(y)) {
        vplot <- do.call(points, append(list(object=vplot, x=x, y=y), 
                                      styles$err_points_historic))
      }
    }
    
    if (any(!measurements$histFlag)){
      arrow_notNA <- intersect(which(!is.na(measurements$minShift)), which(!is.na(measurements$maxShift)))
      arrow_notNA_nothist <- intersect(arrow_notNA, which(!histFlag))
      minShift <- measurements$minShift[arrow_notNA_nothist]
      maxShift <- measurements$maxShift[arrow_notNA_nothist]
      obsGage <- measurements$obsGage[arrow_notNA_nothist]
      if (!isEmptyOrBlank(maxShift) || !isEmptyOrBlank(minShift) || !isEmptyOrBlank(obsGage)) {
        vplot <- do.call(arrows, append(list(object=vplot,x0=minShift, y0=obsGage, 
                                             x1=maxShift, y1=obsGage), styles$err_lines))
      }
      point_notNA_nothist <- intersect(which(!is.na(measurements$obsShift)), which(!histFlag))
      x <- measurements$obsShift[point_notNA_nothist]
      y <- measurements$obsGage[point_notNA_nothist]
      obsIDs <- measurements$obsIDs[point_notNA_nothist]
      measurementNumber <- measurements$measurementNumber[point_notNA_nothist]
      if (!isEmptyOrBlank(x) || !isEmptyOrBlank(y) || !isEmptyOrBlank(obsIDs) || !isEmptyOrBlank(measurementNumber)) {
        vplot <- do.call(points, append(list(object=vplot,x=x, y=y, 
                                             col = as.numeric(obsIDs)+1), styles$err_points))
        
        vplot <- do.call(callouts, list(object=vplot, x = x, y = y, labels=measurementNumber))
      }
    }
  return(vplot)
}

#' Add Rating Shifts
#' Given a gsplot object, will add rating shift points, arrows, lines with callouts to the plot (The "V's")
#' @param vplot the gsplot object for the vdiagram
#' @param shifts shift information
#' @param styles a list of styles to be used for styling the points/callouts
#' @importFrom utils head
#' @importFrom utils tail
addRatingShifts <- function(vplot, shifts, styles) {
  for (id in unique(shifts[['shiftId']])) {
    
    # if there are multiple shifts for the same ID, only want to plot the first occurrence
    # otherwise you get overplotting and it looks bad
    i <- which(shifts[['shiftId']] == id)[1]
    
    x <- shifts[['shiftPoints']][[i]]
    y <- shifts[['stagePoints']][[i]]
    ID <- as.numeric(shifts[['shiftId']][i])
    vplot <- do.call(callouts, list(object=vplot, x=x[2], y=y[2], labels=ID, cex = styles$rating_shift$callout_cex))
    vplot <- do.call(callouts, list(object=vplot, x=head(x,1), y=head(y,1), labels=ID, cex = styles$rating_shift$callout_cex))
    
    if (!is.null(styles$rating_shift$extendStageBy)){
      xlength = length(x)   
      vplot <- do.call(arrows, append(list(object=vplot, x0=x[xlength], y0=tail(y,1) + styles$rating_shift$extendStageBy, 
                                           x1=x[xlength], y1=y[xlength], col=ID), styles$rating_shift$from_segment))
      vplot <- do.call(arrows, append(list(object=vplot, x0=x[1], y0=y[1], x1=x[1], y1=y[1] - styles$rating_shift$extendStageBy, 
                                           col=ID), styles$rating_shift$to_segment))
    }
    
    vplot <- do.call(lines, append(list(object=vplot, x=x, y=y, type="o", col=ID), styles$rating_shift$shift_segment))
  }
  
  return(vplot)
}

#' Create V Diagram Table
#' 
#' @param reportObject A list of properly formatted V diagram report data.
#' @return A string properly formatted for HTML.
#' @importFrom knitr kable
#' @export
vdiagramTable <- function(reportObject){
  shifts <- parseRatingShiftsData(reportObject)
  
  startTime <- shifts[["startTime"]]
  numOfShifts <- shifts[["numOfShifts"]]

  df <- data.frame('Rating' = c(), 
                   'Date'= c(),
                   'Points' =  c(),
                   'Curve' = c(), check.names = F)
  for (i in 1:numOfShifts){
    dateF <- substring(startTime[i], 0, 10)
    timeF <- substring(startTime[i], 12, 19)
    tzF <- substring(startTime[i], 24)
    
    nPoints <- length(shifts[["stagePoints"]][[i]])
    points <- vector('numeric', length = nPoints * 2)
    points[seq(1, by = 2, length.out = nPoints)] <- format(round(shifts[["stagePoints"]][[i]], 2), nsmall = 2)
    points[seq(2, by = 2, length.out = nPoints)] <- format(round(shifts[["shiftPoints"]][[i]], 2), nsmall = 2)
    shftChar <- paste(points, collapse = ', ')
    df <- rbind(df, data.frame('Rating' = shifts[["rating"]][i], 
                               'Date'= paste(dateF, " at ", timeF, " (UTC ", tzF, ")", sep=''),
                               'Points' =  shftChar,
                               'Curve' = shifts[["shiftId"]][i]))
  }
  names(df) <- c('Rating', 'Date & Time', 'Variable Shift Points', 'Shift Curve #')
  addKableOpts(df, tableId = "vdiagram-table")
}

addKableOpts <- function(df, tableId){
  
  format <- 'html'
  alignVal = c('c', 'l', 'l', 'c')
  table_out <- kable( df, format=format, table.attr = sprintf("id=\"%s\" border=\"1\"", tableId), align=alignVal)

  return(table_out)
} 
