#
# Starting point, creates RMD and runs rendering
#

createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  isInverted <- data$reportMetadata$isInverted
  
  if(anyDataExist(dvData)){
    dvInfo <- parseDVSupplemental(data, dvData)
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
      title(main="DV Hydrograph", ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units))
    
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
  
  # capitalize the reference series name for plot titles
  ref_name_letters <- strsplit(series, "")[[1]]
  ref_name_letters[1] <- LETTERS[which(letters == ref_name_letters[1])]
  ref_name_capital <- paste0(ref_name_letters, collapse = "")
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  
  if (!length(data[[ref_name]]$points)==0) {
    
    refData <- parseRefData(data, series)
    isInverted <- data$reportMetadata$isInverted
    logAxis <- isLogged(refData, ref_name)
    
    startDate <- formatDates(data$reportMetadata$startDate)
    endDate <- formatDates(data$reportMetadata$endDate)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    refPlot <- gsplot(ylog=logAxis, yaxs='r') %>%
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
