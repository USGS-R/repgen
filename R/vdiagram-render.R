#
# Starting point, creates RMD and runs rendering
#
startVDiagramRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- makeVDiagramRmd(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}

makeVDiagramRmd <- function(rmd_dir, data, output, wd){
  rmdName <- 'vdiagram.Rmd'
  rmd_file <- file.path(rmd_dir, rmdName)
  
  newPage = ifelse(output == "pdf", '$\\pagebreak$', '------')
  tempRmd <- tempfile(pattern = 'vdiagram', fileext = '.Rmd', tmpdir = wd)
  
  con <- file(rmd_file)
  rawText <- readLines(con)
  close(con)
  replacePlot <- "renderVDiagram(data)"
  replaceTable <- "vdiagramTable(data, output)"
  
  nPages <- length(data$pages)
  metaData <- vector(mode = 'list', length = nPages) #lol
  # creates multi Rmd pages for output, truncates plots and tables. Returns metaData list globally, which would be nice to avoid
  # should probably break this up into two calls. One that returns Rmd handle, the other w/ data
  for (i in 1:nPages){
    pageName <- names(data$pages)[i]
    pageData <- data$pages[[pageName]]
    metaData[[i]] <- pageData
    pageText <- rawText
    pageText[pageText == replacePlot] <- sprintf('renderVDiagram(metaData[[%s]])', i)
    pageText[pageText == replaceTable] <- sprintf('vdiagramTable(metaData[[%s]], output)', i)
    cat(c(pageText,newPage), file = tempRmd, sep = '\n', append = TRUE)
  }
  metaData <<- metaData
  return(tempRmd)
}

#
# Called from VDiagram RMD files
#
renderVDiagram <- function(data){
  if (!is.null(data$pages)){
    for (i in 1:length(names(data$pages))){
      pageName <- names(data$pages)[i]
      createVdiagram(data$pages[[pageName]])
    }
  } else {
    createVdiagram(data)
  }
}

createVdiagram <- function(data) {
  styles <- getVDiagramStyle()
  
  vdiagramData <- parseVDiagramData(data)
  
  vplot <- gsplot(mar=c(7, 3, 4, 2), yaxs = "r", xaxs = "r") %>%
    points(NA,NA, ylab=styles$plot$ylab, xlab=styles$plot$xlab,
           ylim = c(vdiagramData$minStage, vdiagramData$maxStage))
  
  vplot <- do.call(grid, append(list(object=vplot), styles$grid))
  vplot <- do.call(axis, append(list(object=vplot), styles$axis))
  vplot <- do.call(abline, append(list(object=vplot, a=vdiagramData$maxStage), styles$maxStageLine))
  vplot <- do.call(abline, append(list(object=vplot, a=vdiagramData$minStage), styles$minStageLine))
  vplot <- addMeasurementsAndError(vplot, vdiagramData, styles)
  vplot <- addRatingShifts(vplot, vdiagramData, styles)

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
    
    vplot <- do.call(arrows, append(list(object=vplot, x0=minShift, y0=obsGage, 
                                         x1=maxShift, y1=obsGage), styles$err_lines_historic))
    
    point_notNA_hist <- intersect(which(!is.na(vdiagramData$obsShift)), which(histFlag))
    x <- vdiagramData$obsShift[point_notNA_hist]
    y <- vdiagramData$obsGage[point_notNA_hist]
    
    vplot <- do.call(points, append(list(object=vplot, x=x, y=y), 
                                    styles$err_points_historic))
  }
  
  if (any(!vdiagramData$histFlag)){
    
    arrow_notNA <- intersect(which(!is.na(vdiagramData$minShift)), which(!is.na(vdiagramData$maxShift)))
    arrow_notNA_nothist <- intersect(arrow_notNA, which(!histFlag))
    minShift <- vdiagramData$minShift[arrow_notNA_nothist]
    maxShift <- vdiagramData$maxShift[arrow_notNA_nothist]
    obsGage <- vdiagramData$obsGage[arrow_notNA_nothist]
    
    vplot <- do.call(arrows, append(list(object=vplot,x0=minShift, y0=obsGage, 
                                         x1=maxShift, y1=obsGage), styles$err_lines))
   
    point_notNA_nothist <- intersect(which(!is.na(vdiagramData$obsShift)), which(!histFlag))
    x <- vdiagramData$obsShift[point_notNA_nothist]
    y <- vdiagramData$obsGage[point_notNA_nothist]
    obsIDs <- vdiagramData$obsIDs[point_notNA_nothist]
    obsCallOut <- vdiagramData$obsCallOut[point_notNA_nothist]
    
    vplot <- do.call(points, append(list(object=vplot,x=x, y=y, 
                                         col = as.numeric(obsIDs)+1), styles$err_points))
    
    vplot <- do.call(callouts, list(object=vplot, x = x, y = y, labels=obsCallOut))
  }
  
  return(vplot)
}

addRatingShifts <- function(vplot, vdiagramData, styles) {
  for (i in 1:vdiagramData$numOfShifts) {
    x <- vdiagramData$shiftPoints[[i]]
    y <- vdiagramData$stagePoints[[i]]
    ID <- as.numeric(vdiagramData$shiftId[i]) + 1
    
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

#'@title v-diagram table from data inputs
#'@param data a list of properly formatted v-diagram data
#'@param output output type for table. ('html','pdf', others supported by \code{\link[knitr]{kable}]})
#'@return a string properly formatted for the specified output type
#'@importFrom knitr kable
#'@export
vdiagramTable <- function(data, output){
  shiftPoints <- getRatingShifts(data, 'shiftPoints', required = TRUE)
  stagePoints <- getRatingShifts(data, 'stagePoints', required = TRUE)
  
  shiftId <- getRatingShifts(data, 'shiftNumber', required = TRUE)
  startTime <- getRatingShifts(data,"applicableStartDateTime", required = TRUE)
  rating <- getRatingShifts(data, "curveNumber", required = TRUE)
  nShift = numShifts(data)
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
  addKableOpts(df,output, tableId = "vdiagram-table")
}

addKableOpts <- function(df, output, tableId){
  if (missing(output)){
    output = 'markdown' # print to screen
  }
  format <- ifelse(output =='pdf','latex','html')
  alignVal = c('c', 'l', 'l', 'c')
  if (format == 'html'){
    table_out <- kable( df, format=format, table.attr = sprintf("id=\"%s\" border=\"1\"", tableId), align=alignVal)
  } else {
    table_out <- kable( df, format=format, align=alignVal) # tex and other options handled here
  }
  return(table_out)
} 