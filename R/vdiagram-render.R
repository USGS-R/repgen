#' Create R Markdown and Run Rendering
#' 
#' @param rmd_dir Path to R Markdown directory.
#' @param reportObject Report data structure.
#' @param wd Path to working directory.
makeVDiagramRmd <- function(rmd_dir, reportObject, wd) {
  rmdName <- 'vdiagram.Rmd'
  rmd_file <- file.path(rmd_dir, rmdName)
  
  newPage = '------'
  tempRmd <- tempfile(pattern = 'vdiagram', fileext = '.Rmd', tmpdir = wd)
  
  con <- file(rmd_file)
  rawText <- readLines(con)
  close(con)
  replacePlot <- "renderVDiagram(data)"
  replaceTable <- "vdiagramTable(data)"
  
  nPages <- length(reportObject$pages)
  metaData <- vector(mode = 'list', length = nPages) #lol
  # Creates multi Rmd pages for output, truncates plots and tables. Returns
  # metaData list globally, which would be nice to avoid should probably break
  # this up into two calls. One that returns Rmd handle, the other w/ reportObject.
  for (i in 1:nPages){
    pageName <- names(reportObject$pages)[i]
    pageData <- reportObject$pages[[pageName]]
    metaData[[i]] <- pageData
    pageText <- rawText
    pageText[pageText == replacePlot] <- sprintf('renderVDiagram(metaData[[%s]])', i)
    pageText[pageText == replaceTable] <- sprintf('vdiagramTable(metaData[[%s]])', i)
    cat(c(pageText,newPage), file = tempRmd, sep = '\n', append = TRUE)
  }
  metaData <<- metaData
  return(tempRmd)
}

#' Called from V diagram R Markdown files.
#' 
#' @param reportObject V diagram report data.
renderVDiagram <- function(reportObject) {
  if (!is.null(reportObject$pages)){
    for (i in 1:length(names(reportObject$pages))){
      pageName <- names(reportObject$pages)[i]
      createVdiagram(reportObject$pages[[pageName]])
    }
  } else {
    createVdiagram(reportObject)
  }
}

createVdiagram <- function(reportObject) {
  options(scipen = 8)
  
  styles <- getVDiagramStyle()
  
  vdiagramData <- parseVDiagramData(reportObject)
  
  vplot <- gsplot(mar = c(7, 3, 4, 2), yaxs = "r", xaxs = "r") %>%
    points(NA, NA, ylab = styles$plot$ylab, xlab = styles$plot$xlab)
  
  vplot <- do.call(grid, append(list(object = vplot), styles$grid))
  
  # max./min. stage lines at top/bottom of plot
  vplot <- do.call(abline, append(
    list(object = vplot, a = vdiagramData$maxStage), styles$maxStageLine
  ))
  vplot <- do.call(abline, append(
    list(object = vplot, a = vdiagramData$minStage), styles$minStageLine
  ))
  
  vplot <- addMeasurementsAndError(vplot, vdiagramData, styles)
  vplot <- addRatingShifts(vplot, vdiagramData, styles)

  vplot <- testCallouts(vplot, xlimits = xlim(vplot)$side.1)
  
  ylims <- c(
      min(c(ylim(vplot)$side.2, vdiagramData$minStage)),
      max(c(ylim(vplot)$side.2, vdiagramData$maxStage))
      )
  xlims <- xlim(vplot)$side.1
  y_seq <- pretty(ylims, shrink.sml = 20)
  x_seq <- pretty(xlims, shrink.sml = 20)
  
  vplot <-
    do.call(abline, append(list(object = vplot, h = y_seq), styles$ablines))
  vplot <-
    do.call(abline, append(list(object = vplot, v = x_seq), styles$ablines))
  
  vplot <-
    axis(vplot, side = c(1, 2, 4), at = c(x_seq, y_seq, y_seq)) %>%
    view(side = 2, ylim = ylims)
  
  print(vplot)
}

addMeasurementsAndError <- function(vplot, vdiagramData, styles) {
  histFlag <- vdiagramData$histFlag
 
    if (any(histFlag)){
      # TODO replace with below when working
      #error_bar(gsNew, x=1:3, y=c(3,1,2), x.low=c(.2,NA,.2), x.high=.2, col="red",lwd=3)
      
      arrow_notNA <- intersect(which(!is.na(vdiagramData$minShift)), which(!is.na(vdiagramData$maxShift)))
      arrow_notNA_hist <- intersect(arrow_notNA, which(histFlag))
      minShift <- vdiagramData$minShift[arrow_notNA_hist]
      maxShift <- vdiagramData$maxShift[arrow_notNA_hist]
      obsGage <- vdiagramData$obsGage[arrow_notNA_hist]
      if (!isEmptyOrBlank(maxShift) || !isEmptyOrBlank(minShift) || !isEmptyOrBlank(obsGage)) {
        vplot <- do.call(arrows, append(list(object=vplot, x0=minShift, y0=obsGage, 
                                             x1=maxShift, y1=obsGage), styles$err_lines_historic))
      }
      point_notNA_hist <- intersect(which(!is.na(vdiagramData$obsShift)), which(histFlag))
      x <- vdiagramData$obsShift[point_notNA_hist]
      y <- vdiagramData$obsGage[point_notNA_hist]
      if (!isEmptyOrBlank(x) || !isEmptyOrBlank(y)) {
        vplot <- do.call(points, append(list(object=vplot, x=x, y=y), 
                                      styles$err_points_historic))
      }
    }
    
    if (any(!vdiagramData$histFlag)){
      arrow_notNA <- intersect(which(!is.na(vdiagramData$minShift)), which(!is.na(vdiagramData$maxShift)))
      arrow_notNA_nothist <- intersect(arrow_notNA, which(!histFlag))
      minShift <- vdiagramData$minShift[arrow_notNA_nothist]
      maxShift <- vdiagramData$maxShift[arrow_notNA_nothist]
      obsGage <- vdiagramData$obsGage[arrow_notNA_nothist]
      if (!isEmptyOrBlank(maxShift) || !isEmptyOrBlank(minShift) || !isEmptyOrBlank(obsGage)) {
        vplot <- do.call(arrows, append(list(object=vplot,x0=minShift, y0=obsGage, 
                                             x1=maxShift, y1=obsGage), styles$err_lines))
      }
      point_notNA_nothist <- intersect(which(!is.na(vdiagramData$obsShift)), which(!histFlag))
      x <- vdiagramData$obsShift[point_notNA_nothist]
      y <- vdiagramData$obsGage[point_notNA_nothist]
      obsIDs <- vdiagramData$obsIDs[point_notNA_nothist]
      obsCallOut <- vdiagramData$obsCallOut[point_notNA_nothist]
      if (!isEmptyOrBlank(x) || !isEmptyOrBlank(y) || !isEmptyOrBlank(obsIDs) || !isEmptyOrBlank(obsCallOut)) {
        vplot <- do.call(points, append(list(object=vplot,x=x, y=y, 
                                             col = as.numeric(obsIDs)+1), styles$err_points))
        
        vplot <- do.call(callouts, list(object=vplot, x = x, y = y, labels=obsCallOut))
      }
    }
  return(vplot)
}

#' @importFrom utils head
#' @importFrom utils tail
addRatingShifts <- function(vplot, vdiagramData, styles) {
  for (id in unique(vdiagramData$shiftId)) {
    
    # if there are multiple shifts for the same ID, only want to plot the first occurrence
    # otherwise you get overplotting and it looks bad
    i <- which(vdiagramData$shiftId == id)[1]
    
    x <- vdiagramData$shiftPoints[[i]]
    y <- vdiagramData$stagePoints[[i]]
    ID <- as.numeric(vdiagramData$shiftId[i])
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
  ratingShifts <- fetchRatingShifts(reportObject)
  
  shiftPoints <- ratingShifts$shiftPoints
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- ratingShifts$stagePoints
  validParam(stagePoints, "stagePoints")
  
  shiftId <- ratingShifts$shiftNumber
  validParam(stagePoints, "shiftNumber")
  
  startTime <- ratingShifts$applicableStartDateTime
  validParam(stagePoints, "applicableStartDateTime")
  
  rating <- ratingShifts$curveNumber
  validParam(stagePoints, "curveNumber")
  
  nShift = sizeOf(ratingShifts)
  
  df <- data.frame('Rating' = c(), 
                   'Date'= c(),
                   'Points' =  c(),
                   'Curve' = c(), check.names = F)
  for (i in 1:nShift){
    dateF <- substring(startTime[i], 0, 10)
    timeF <- substring(startTime[i], 12, 19)
    tzF <- substring(startTime[i], 24)
    
    nPoints <- length(stagePoints[[i]])
    points <- vector('numeric', length = nPoints * 2)
    points[seq(1, by = 2, length.out = nPoints)] <- format(round(stagePoints[[i]], 2), nsmall = 2)
    points[seq(2, by = 2, length.out = nPoints)] <- format(round(shiftPoints[[i]], 2), nsmall = 2)
    shftChar <- paste(points, collapse = ', ')
    df <- rbind(df, data.frame('Rating' = rating[i], 
                               'Date'= paste(dateF, " at ", timeF, " (UTC ", tzF, ")", sep=''),
                               'Points' =  shftChar,
                               'Curve' = shiftId[i]))
  }
  names(df) <- c('Rating', 'Date & Time', 'Variable Shift Points', 'Shift Curve #')
  addKableOpts(df, tableId = "vdiagram-table")
}

addKableOpts <- function(df, tableId){
  format <- 'html'
  alignVal = c('c', 'l', 'l', 'c')
  if (format == 'html'){
    table_out <- kable( df, format=format, table.attr = sprintf("id=\"%s\" border=\"1\"", tableId), align=alignVal)
  } else {
    table_out <- kable( df, format=format, align=alignVal) # tex and other options handled here
  }
  return(table_out)
} 
