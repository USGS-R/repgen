
addRatingShifts <- function(gsplot, x, y, ID, extendStageBy = NULL, callOuts = TRUE) {
  curve_pch = 8

  if (callOuts){
    gsplot <- text(gsplot, x[2], y[2], ID, cex = 0.5, pos = 2) %>%
      text(head(x,1), head(y,1), ID, cex = 0.5, pos = 2)
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
      points(x[!histFlag], y[!histFlag], pch = 21, bg = 'white', col = as.numeric(IDs)+1, ...)
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
    cat(c(pageText,newPage), file = tempRmd, sep = '\n', append = TRUE)
  }
  metaData <<- metaData
  return(tempRmd)
}

