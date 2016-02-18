#
# Starting point, creates RMD and runs rendering
#

createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  isInverted <- data$reportMetadata$isInverted
  
  if(anyDataExist(dvData)){
    dvInfo <- parseDVSupplemental(data, dvData, zeroValues(dvData, "value"), negValues(dvData, "value"))
    startDate <- formatDates(data$reportMetadata$startDate)
    endDate <- formatDates(data$reportMetadata$endDate)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    #semantics for min/max are swapped on inverted plots
    maxLabel = "Max. Instantaneous"
    minLabel = "Min. Instantaneous";
    if(isInverted) {
      maxLabel = "Min. Instantaneous"
      minLabel = "Max. Instantaneous";
    }
    
    dvhplot <- gsplot(ylog=dvInfo$logAxis, yaxs='r') %>% 
      grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>%
      axis(1, at=plotDates, labels=format(plotDates, "%b\n%d"), padj=0.5) %>%
      axis(2, reverse=isInverted) %>%
      lines(as.POSIXct(NA), NA, xlim=c(startDate, endDate)) %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray") %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1) %>% 
      legend(location="below", cex=0.8) %>%
      title(main="DVHydrograph", ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units))
    
    for (i in 1:length(dvData)) {
      
      dvStyles <- getDvStyle(dvData[i], dvInfo, maxLabel=maxLabel, minLabel=minLabel)
      for (j in seq_len(length(dvStyles))) {
        dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
      }
    }
    
    return(dvhplot)
  } else {
    dvhplot <- NULL
  }
}

createRefPlot <- function(data, series) {
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  # capitalize the reference series name
  ref_name_letters <- strsplit(ref_name, "")[[1]]
  ref_name_letters[1] <- LETTERS[which(letters == ref_name_letters[1])]
  ref_name_capital <- paste0(ref_name_letters, collapse = "")
  
  if (!length(data[[ref_name]]$points)==0) {
    
    refData <- parseRefData(data, series)
    isInverted <- data$reportMetadata$isInverted
    
    startDate <- formatDates(data[[ref_name]]$startTime)
    endDate <- formatDates(data[[ref_name]]$endTime)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    refPlot <- gsplot(ylog=FALSE) %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=3, col="gray") %>%
      axis(1, at=plotDates, labels=format(plotDates, "%b\n%d"), padj=0.5) %>%
      axis(2, reverse=isInverted) %>%
      lines(as.POSIXct(NA), NA, xlim=c(startDate, endDate)) %>%
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray") %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1) %>% 
      title(main=paste(ref_name_capital, "Reference Time Series"), 
            ylab = paste(data[[ref_name]]$type, data[[ref_name]]$units)) %>% 
      legend(location="below", cex=0.8)
    
    
    for (i in 1:length(refData)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        refPlot <- do.call(names(refStyles[j]), append(list(object=refPlot), refStyles[[j]]))
      }
      
    }
    return(refPlot)
  }
}
