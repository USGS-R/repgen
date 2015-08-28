vplot <- function(vdiagramData) {
  extendStageBy = 0.5
  vplot <- gsplot() %>%
    points(NA,NA, ylab='Stage, in feet', xlab='Shift, in feet') %>%
    callouts(x=c(0,0),y=c(vdiagramData$minStage, vdiagramData$maxStage), labels="", 
             col = 'red', lwd = 3, angle=0, legend.name="Max and min gage height for the period shown") %>%
    grid(lty = "dotted") %>%
    axis(side=c(2,4)) %>%
    addVdiagErrorBars(x = vdiagramData$obsShift, y = vdiagramData$obsGage, 
                      xError0 = vdiagramData$minShift, xError1 = vdiagramData$maxShift, 
                      vdiagramData$histFlag, IDs = vdiagramData$obsIDs)
  
  for (i in 1:vdiagramData$numOfShifts) {
    vplot <- addRatingShifts(vplot, vdiagramData$shiftPoints[[i]], vdiagramData$stagePoints[[i]], 
                             ID = vdiagramData$shiftId[i], extendStageBy = extendStageBy) #skip black as a color
  }
  
  if (any(!is.na(vdiagramData$obsShift)) && any(!vdiagramData$histFlag)){
    vplot <- callouts(vplot,x = vdiagramData$obsShift[!vdiagramData$histFlag], y = vdiagramData$obsGage[!vdiagramData$histFlag], 
                      labels=vdiagramData$obsCallOut[!vdiagramData$histFlag], cex=0.6)
  }
  par(mar=c(7, 3, 4, 2))
  print(vplot) 
}

addRatingShifts <- function(gsplot, x, y, ID, extendStageBy = NULL, callOuts = TRUE) {
  curve_pch = 8
  
  if (callOuts){
    gsplot <- callouts(gsplot, x=x[2], y=y[2], labels=ID, cex = 0.5) %>%
      callouts(x=head(x,1), y=head(y,1), labels=ID, cex = 0.5)
  }
  
  
  if (!is.null(extendStageBy)){
    xlength = length(x)     
    gsplot <- arrows(gsplot, x[xlength], tail(y,1) + extendStageBy, x[xlength], y[xlength], col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch, angle=30, code=1, length=0.1) %>%
      arrows(x[1], y[1], x[1], y[1] - extendStageBy, col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch, angle=30, code=2, length=0.1)
  }
  
  invisible(lines(gsplot, x, y, type="o", col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch))
}

addVdiagErrorBars <- function(gsplot, x, y, xError0, xError1, histFlag, IDs, ...){
  if (length(histFlag)==1 && histFlag == " "){
    histFlag <- rep(TRUE, length(x))
  }
  if (any(histFlag)){
    gsplot <- arrows(gsplot, xError0[histFlag], y[histFlag], xError1[histFlag], y[histFlag], 
                     angle=90, lwd=1.25, code=3, col = 'blue', length=0.05, ...) %>%
      points(x[histFlag], y[histFlag], 
             pch = 21, bg = 'black', col = 'black', cex = 0.7, ...)
  }
  
  if (any(!histFlag)){
    gsplot <- arrows(gsplot,xError0[!histFlag], y[!histFlag], xError1[!histFlag], y[!histFlag], 
                     angle=90, lwd=1.25, code=3, col = 'black', length=0.1, ...) %>%
      points(x[!histFlag], y[!histFlag], pch = 21, bg = 'white', col = as.numeric(IDs)+1, legend.name="Historical measurements from the last 2 years", ...)
  }
  invisible(gsplot)
}


echo <- function(string) {
  print(string, quote=FALSE)
}

percentError <- function(MeasurementGrade) {
  percents = rep(0, length(MeasurementGrade))
  percents[grep("fair", MeasurementGrade)] = 0.08
  # other options to be added
  return(percents)
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

