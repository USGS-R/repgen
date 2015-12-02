#
# Starting point, creates RMD and runs rendering
#
startdvhydrographRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('dvhydrograph', 'dvhydrograph.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}

dvhydrographPlot <- function(data) {
  options(scipen=5)
  dvhplot <- createDvhydrographPlot(data)
  secRefPlot <- createSecRefPlot(data)
  terRefPlot <- createTerRefPlot(data)
  quaRefPlot <- createQuaRefPlot(data)
  return(list(dvhplot=dvhplot, secRefPlot=secRefPlot, terRefPlot=terRefPlot, quaRefPlot=quaRefPlot))
}

createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  dvInfo <- parseDVSupplemental(data, dvData, zeroValues(dvData))
  
  startDate <- formatDates(data$reportMetadata$startDate)
  endDate <- formatDates(data$reportMetadata$endDate)
  
  dvhplot <- gsplot(ylog=TRUE, yaxs='r') %>% 
    lines(as.POSIXct(NA), NA, 
          xlim=c(startDate, endDate)) %>% 
  grid(nx=NA, ny=NULL, lwd=2, lty=1, col="gray") %>%  
    abline(v=seq(from=startDate, to=endDate, by="days"), col="gray", lwd=1) %>% 
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1) %>% 
    #I don't know what this is for
    #abline(h=dvInfo$horizontalGrid, col="black", lwd=2) %>% 
    abline(v=seq(from=startDate, to=endDate, by="month"), col="black", lwd=2) %>%
    legend(location="below", title="", cex=0.8) 
  #not sure how to label the y axis yet because we have three possibly different datasets?
  #title(main="", xlab=paste(data$reportMetadata$siteNumber, "-", data$reportMetadata$stationName),ylab=paste(data$firstDownChain$type, " ", data$firstDownChain$units))
    #grid(nx=NA, ny=NULL, lwd=2, lty=1, col="darkgreen") %>%  
  
  for (i in 1:length(dvData)) {
    
    dvStyles <- getDvStyle(dvData[i])
    for (j in seq_len(length(dvStyles))) {
      dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
    }
  }
  
  return(dvhplot)
  
}

createSecRefPlot <- function(data) {
  if (length(data$secondaryReferenceTimeSeries)>0) {
    
  refData <- parseSecRefData(data)
  
    secRefStartDate <- formatDates(data$secondaryReferenceTimeSeries$startDate)
    secRefEndDate <- formatDates(data$secondaryReferenceTimeSeries$endDate)
    
    secRefPlot <- gsplot(ylog=FALSE) %>%
      lines(as.POSIXct(NA), NA, 
            xlim=c(secRefStartDate, secRefEndDate)) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=1, col="gray") %>%
      legend(location="below", title="", cex=0.8)
  }
  
  for (i in 1:length(refData)) {
    refStyles <- getDvStyle(refData[i])
    for (j in seq_len(length(refStyles))) {
      secRefPlot <- do.call(names(refStyles[j]), append(list(object=secRefPlot), refStyles[[j]]))
    }
  }
  
  
  return(secRefPlot)
}

createTerRefPlot <- function(data) {
  
  if (length(data$tertiaryReferenceTimeSeries)>0) {
    
    refData <- parseTerRefData(data)
    
    terRefStartDate <- formatDates(data$tertiaryReferenceTimeSeries$startDate)
    terRefEndDate <- formatDates(data$tertiaryReferenceTimeSeries$endDate)
    
    terRefPlot <- gsplot(ylog=FALSE) %>%
      lines(as.POSIXct(NA), NA, 
            xlim=c(terRefStartDate, terRefEndDate)) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=1, col="gray") %>%
      legend(location="below", title="", cex=0.8)
  }
  
  for (i in 1:length(refData)) {
    refStyles <- getDvStyle(refData[i])
    for (j in seq_len(length(refStyles))) {
      terRefPlot <- do.call(names(refStyles[j]), append(list(object=terRefPlot), refStyles[[j]]))
    }
  }
  
  return(terRefPlot)
}

createQuaRefPlot <- function(data) {
  
  refData <- parseQuaRefData(data)
  
  if (length(data$quaternaryReferenceTimeSeries)>0) {
    quaRefStartDate <- formatDates(data$quaternaryReferenceTimeSeries$startDate)
    quaRefEndDate <- formatDates(data$quaternaryReferenceTimeSeries$endDate)
    
    quaRefPlot <- gsplot(ylog=FALSE) %>%
      lines(as.POSIXct(NA), NA, 
            xlim=c(quaRefStartDate, quaRefEndDate)) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=1, col="gray") %>%
      legend(location="below", title="", cex=0.8)
  }
  
  for (i in 1:length(refData)) {

    refStyles <- getDvStyle(refData[i])
    for (j in seq_len(length(refStyles))) {
      quaRefPlot <- do.call(names(refStyles[j]), append(list(object=quaRefPlot), refStyles[[j]]))
    }
  }
  
  return(quaRefPlot)
}
