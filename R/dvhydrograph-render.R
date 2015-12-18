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

createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  dvInfo <- parseDVSupplemental(data, dvData, zeroValues(dvData, "value"))
  startDate <- formatDates(data$reportMetadata$startDate)
  endDate <- formatDates(data$reportMetadata$endDate)
  plotDates <- seq(startDate, endDate, by=7*24*60*60)
  
  dvhplot <- gsplot(ylog=dvInfo$logAxis, yaxs='r') %>% 
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>%
    axis(1, at=plotDates, labels=format(plotDates, "%b\n%d"), padj=0.5) %>%
    lines(as.POSIXct(NA), NA, xlim=c(startDate, endDate)) %>% 
    abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray") %>%
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1) %>% 
    legend(location="below", title="", cex=0.8) %>%
    title(main="DVHydrograph", ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units))

  for (i in 1:length(dvData)) {
    
    dvStyles <- getDvStyle(dvData[i])
    for (j in seq_len(length(dvStyles))) {
      dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
    }
  }
  
  return(dvhplot)
  
}

createSecRefPlot <- function(data) {
  if (!length(data$secondaryReferenceTimeSeries$points)==0) {
    
    refData <- parseSecRefData(data)
    
    secRefStartDate <- formatDates(data$secondaryReferenceTimeSeries$startTime)
    secRefEndDate <- formatDates(data$secondaryReferenceTimeSeries$endTime)
    secRefPlotDates <- seq(secRefStartDate, secRefEndDate, by=7*24*60*60)
    
    secRefPlot <- gsplot(ylog=FALSE) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=3, col="gray") %>%
      axis(1, at=secRefPlotDates, labels=format(secRefPlotDates, "%b\n%d"), padj=0.5) %>%
      lines(as.POSIXct(NA), NA, xlim=c(secRefStartDate, secRefEndDate)) %>%
      abline(v=seq(from=secRefStartDate, to=secRefEndDate, by="days"), lty=3, col="gray") %>%
      abline(v=seq(from=secRefStartDate, to=secRefEndDate, by="weeks"), col="darkgray", lwd=1) %>% 
      title(main="Secondary Reference Time Series", ylab = paste(data$secondaryReferenceTimeSeries$type, data$secondaryReferenceTimeSeries$units))
    
    
    for (i in 1:length(refData)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        secRefPlot <- do.call(names(refStyles[j]), append(list(object=secRefPlot), refStyles[[j]]))
      }
      
    }
    return(secRefPlot)
  }
}

createTerRefPlot <- function(data) {
  
  if (!length(data$tertiaryReferenceTimeSeries$points)==0) {
    
    refData <- parseTerRefData(data)
    
    terRefStartDate <- formatDates(data$tertiaryReferenceTimeSeries$startTime)
    terRefEndDate <- formatDates(data$tertiaryReferenceTimeSeries$endTime)
    terRefPlotDates <- seq(terRefStartDate, terRefEndDate, by=7*24*60*60)
    
    terRefPlot <- gsplot(ylog=FALSE) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=3, col="gray") %>%
      axis(1, at=terRefPlotDates, labels=format(terRefPlotDates, "%b\n%d"), padj=0.5) %>%
      lines(as.POSIXct(NA), NA, xlim=c(terRefStartDate, terRefEndDate)) %>%
      abline(v=seq(from=terRefStartDate, to=terRefEndDate, by="days"), lty=3, col="gray") %>%
      abline(v=seq(from=terRefStartDate, to=terRefEndDate, by="weeks"), col="darkgray", lwd=1) %>% 
      title(main="Tertiary Reference Time Series", ylab = paste(data$tertiaryReferenceTimeSeries$type, data$tertiaryReferenceTimeSeries$units))
    
    
    for (i in 1:length(refData)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        terRefPlot <- do.call(names(refStyles[j]), append(list(object=terRefPlot), refStyles[[j]]))
      }
    }
    return(terRefPlot)
  }
  
}

createQuaRefPlot <- function(data) {
  
  if (!length(data$quaternaryReferenceTimeSeries$points)==0) {
    
    refData <- parseQuaRefData(data)
    
    quaRefStartDate <- formatDates(data$quaternaryReferenceTimeSeries$startTime)
    quaRefEndDate <- formatDates(data$quaternaryReferenceTimeSeries$endTime)
    quaRefPlotDates <- seq(quaRefStartDate, quaRefEndDate, by=7*24*60*60)
    
    quaRefPlot <- gsplot(ylog=FALSE) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=3, col="gray") %>%
      axis(1, at=quaRefPlotDates, labels=format(quaRefPlotDates, "%b\n%d"), padj=0.5) %>%
      lines(as.POSIXct(NA), NA, xlim=c(quaRefStartDate, quaRefEndDate)) %>%
      abline(v=seq(from=quaRefStartDate, to=quaRefEndDate, by="days"), lty=3, col="gray") %>%
      abline(v=seq(from=quaRefStartDate, to=quaRefEndDate, by="weeks"), col="darkgray", lwd=1) %>% 
      title(main="Quaternary Reference Time Series", ylab = paste(data$quaternaryReferenceTimeSeries$type, data$quaternaryReferenceTimeSeries$units))
    
    for (i in 1:length(refData)) {
      
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        quaRefPlot <- do.call(names(refStyles[j]), append(list(object=quaRefPlot), refStyles[[j]]))
      }
    }
    return(quaRefPlot)
  }
  
}
