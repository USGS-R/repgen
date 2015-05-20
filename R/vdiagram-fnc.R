

set_up_plot <- function(lims) {
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  par(omi=c(0,0,0,0), mai = c(0.5, 0.5, 0.1, 0.5))
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  
  yText = "Stage, in feet"
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, xlab=yText, ylab="Stage, in feet", xaxt="n", yaxt="n", mgp=mgp$y)
  mtext(yText, side=4, line=1)
  actualX = par("usr")[1:2]
  actualY = par("usr")[3:4]
  
  # gridlines
  abline(v=pretty(xaxis,mn_tck), h=pretty(yaxis,mn_tck), lty="solid", col="lightgray")
  abline(v=pretty(xaxis,mj_tck), h=pretty(yaxis,mj_tck), lty="solid", col="darkgray")
  
  # minor axes
  axis(side=1, at=pretty(xaxis,mn_tck), tck=mn_tkL, labels=FALSE)
  axis(side=2, at=pretty(xaxis,mn_tck), tck=mn_tkL, labels=FALSE)
  axis(side=4, at=pretty(xaxis,mn_tck), tck=mn_tkL, labels=FALSE)
  
  # major axes
  majorX = pretty(xaxis,mj_tck)
  majorY = pretty(yaxis,mj_tck)
  axis(side=1, at=majorX, cex.axis=0.5, tck=mj_tkL, mgp=mgp$x, labels=sprintf("%.2f", majorX))
  axis(side=2, at=majorY, cex.axis=0.5, las=2, tck=mj_tkL, mgp=mgp$y, labels=sprintf("%.2f", majorY))
  axis(side=4, at=majorY, cex.axis=0.5, las=2, tck=mj_tkL, mgp=mgp$y, labels=sprintf("%.2f", majorY))
  
  # edges
  axis(side=1, at=actualX, cex.axis=0.5, tck=0, mgp=mgp$x, labels=sprintf("%.2f", actualX))
  axis(side=2, at=actualY, cex.axis=0.5, las=2, tck=0, mgp=mgp$y, labels=sprintf("%.2f", actualY))
  
  # zero line
  abline(v=0, lwd=1.5)
}

addRatingShifts <- function(x, y, ID, extendStageBy = NULL, callOuts = TRUE) {
  curve_pch = 8

  if (callOuts){
    text(x[2], y[2], ID, cex = 0.5, pos = 2)
    text(head(x,1), head(y,1), ID, cex = 0.5, pos = 2)
  }
  
  
  if (!is.null(extendStageBy)){
    xlength = length(x)     
    arrows(x[xlength], tail(y,1) + extendStageBy, x[xlength], y[xlength], col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch, angle=30, code=1, length=0.1)
    arrows(x[1], y[1], x[1], y[1] - extendStageBy, col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch, angle=30, code=2, length=0.1)
  }
  
  lines(x, y, type="o", col=as.numeric(ID)+1, lwd=1.5, pch=curve_pch)
}

addVdiagErrorBars <- function(x, y, xError0, xError1, histFlag, IDs, ...){
  if (length(histFlag)==1 && histFlag == " "){
    histFlag <- rep(TRUE, length(x))
  }
  if (any(histFlag)){
    arrows(xError0[histFlag], y[histFlag], xError1[histFlag], y[histFlag], 
           angle=90, lwd=1.25, code=3, col = 'blue', length=0.05, ...)
    points(x[histFlag], y[histFlag], 
           pch = 21, bg = 'black', col = 'black', cex = 0.7, ...)
  }

  if (any(!histFlag)){
    arrows(xError0[!histFlag], y[!histFlag], xError1[!histFlag], y[!histFlag], 
           angle=90, lwd=1.25, code=3, col = 'black', length=0.1, ...)
    points(x[!histFlag], y[!histFlag], 
           pch = 21, bg = 'white', col = as.numeric(IDs)+1, ...)
  }
  
}


add_call_out <- function(x,y, call_text){
  if (length(x) > 0){
    xlim <- par()$usr[1:2]
    ylim <- par()$usr[3:4]
    x_bmp = diff(xlim)*0.05
    y_bmp = diff(ylim)*0.03
    for (i in 1:length(x)){
      lines(c(x[i],x[i]-x_bmp),c(y[i],y[i]+y_bmp), type = 'l',col='black')
      lines(c(x[i]-x_bmp, x[i]-x_bmp*2),c(y[i]+y_bmp,y[i]+y_bmp), type = 'l',col='black')
      text(x[i]-x_bmp*2, y = y[i]+y_bmp, labels = call_text[i], pos = 2, cex = 0.5)
    }
  }
}
echo <- function(string) {
  print(string, quote=FALSE)
}

#'@title add min max horizontal lines to plot
#'@keywords internal
#'@param minStage y values for min and max lines
#'@param maxStage x values for min and max lines
#'@param ... additional arguments passed to \code{lines}
addMinMax <- function(minStage, maxStage, ...){
  
  xRange <- par()$usr[1:2]
  lwd = 3
  col = 'red'
  barWidth <- diff(xRange) * 0.08 # % of plot width
  x1 <- ifelse(-barWidth < xRange[1], xRange[1], -barWidth)
  x2 <- ifelse(barWidth > xRange[2], xRange[2], barWidth)

  lines(x = c(x1, x2), c(minStage, minStage), ...)
  lines(x = c(x1, x2), c(maxStage,maxStage), ...)
}

percentError <- function(MeasurementGrade) {
  percents = rep(0, length(MeasurementGrade))
  percents[grep("fair", MeasurementGrade)] = 0.08
  # other options to be added
  return(percents)
}

getVdiagLims <- function(shiftPoints, stagePoints, maxShift, minShift, maxStage, minStage, obsShift, obsGage, extendStageBy = 0){

  # shiftPoints and stagePoints are required, and should not be NA. 
  # maxShift and minShift, if missing from the json, are NA
  x_mx <- max(c(sapply(shiftPoints, FUN = max), maxShift, obsShift), na.rm = TRUE)
  x_mn <- min(c(sapply(shiftPoints, FUN = min), minShift, obsShift), na.rm = TRUE)
  y_mx <- max(c(sapply(stagePoints, FUN = max)) + extendStageBy, maxStage, obsGage, na.rm = TRUE)
  y_mn <- min(c(sapply(stagePoints, FUN = min)) - extendStageBy, minStage, obsGage, na.rm = TRUE)
  
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in shiftPoints or stagePoints. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))

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
  
  if (format == 'html'){
    table_out <- kable( df, format=format, table.attr = sprintf("id=\"%s\" border=\"1\"", tableId))
  } else {
    table_out <- kable( df, format=format) # tex and other options handled here
  }
  return(table_out)
}


pagingVdiagram <- function(rmd_dir, data, output, wd){
  

  rmdName <- 'vdiagram.Rmd'
  rmd_file <- file.path(rmd_dir, rmdName)
  
  newPage = ifelse(output == "pdf", '$\\pagebreak$', '------')
  tempRmd <- tempfile(pattern = 'vdiagram', fileext = '.Rmd', tmpdir = wd)
  
  con <- file(rmd_file)
  rawText <- readLines(con)
  close(con)
  replacePlot <- "vdiagram(data)"
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
    pageText[pageText == replacePlot] <- sprintf('vdiagram(metaData[[%s]])', i)
    pageText[pageText == replaceTable] <- sprintf('vdiagramTable(metaData[[%s]], output)', i)
    cat(c(pageText, newPage), file = tempRmd, sep = '\n', append = TRUE)
  }
  metaData <<- metaData
  return(tempRmd)
}

