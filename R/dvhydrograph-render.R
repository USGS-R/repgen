

dvhydrographPlot <- function(data) {
  plot_object <- createDvhydrographPlot(data)
  return(plot_object)
}

createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  isInverted <- data$reportMetadata$isInverted
  
  #semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous";
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous";
  }
  
  if(anyDataExist(dvData)){
    dvInfo <- parseDVSupplemental(data, dvData)
    startDate <- formatDates(data$reportMetadata$startDate) 
    endDate <- formatDates(data$reportMetadata$endDate) + hours(23) + minutes(45)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plot_object <- gsplot(ylog=dvInfo$logAxis, yaxs='r') %>% 
      grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>%
      axis(1, at=plotDates, labels=format(plotDates, "%b\n%d"), padj=0.5) %>%
      axis(2, reverse=isInverted) %>%
      view(xlim=c(startDate, endDate)) %>% 
      legend(location="below", cex=0.8) %>%
      title(main="DV Hydrograph", ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units))
    
    for (i in 1:length(dvData)) {
      
      dvStyles <- getDvStyle(dvData[i], dvInfo, maxLabel=maxLabel, minLabel=minLabel)
      for (j in seq_len(length(dvStyles))) {
        plot_object <- do.call(names(dvStyles[j]), append(list(object=plot_object), dvStyles[[j]]))
      }
    }

    plot_object <- rm.duplicate.legend.items(plot_object)
    
    #custom gridlines below approval bar
    plot_object <- plot_object %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
    
    return(plot_object)
  } else {
    plot_object <- NULL
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
    logAxis <- isLogged(data, refData, ref_name)
    
    startDate <- formatDates(data$reportMetadata$startDate)
    endDate <- formatDates(data$reportMetadata$endDate) + hours(23) + minutes(45)
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plot_object <- gsplot(ylog=logAxis, yaxs='r') %>%
      grid(nx=NA, ny=NULL, lwd=2, lty=3, col="gray") %>%
      axis(1, at=plotDates, labels=format(plotDates, "%b\n%d"), padj=0.5) %>%
      axis(2, reverse=isInverted) %>%
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", legend.name="verticalGrids") %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, legend.name="verticalGrids") %>% 
      view(xlim=c(startDate, endDate)) %>%
      title(main=paste(ref_name_capital, "Reference Time Series"), 
            ylab = paste(data[[ref_name]]$type, data[[ref_name]]$units)) %>% 
      legend(location="below", cex=0.8)
    
    for (i in 1:length(refData)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        plot_object <- do.call(names(refStyles[j]), append(list(object=plot_object), refStyles[[j]]))
      }
      
    }

    orderLegend <- c("verticalGrids", "Working", "In Review", "Approved") #top --> bottom
    plot_object <- reorderPlot(plot_object, "view.1.2", "legend.name", orderLegend)
    plot_object <- reorderPlot(plot_object, "legend", "legend", orderLegend)
    plot_object <- rm.duplicates(plot_object, "view.1.2", "legend.name")
    plot_object <- rm.duplicates(plot_object, "legend", "legend")
    
    return(plot_object)
  }
}
