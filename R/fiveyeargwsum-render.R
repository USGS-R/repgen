

fiveyeargwsumPlot <- function(data) {
  options(scipen=5)
  fiveyrplot <- createfiveyeargwsumPlot(data)
  return(fiveyrplot)
}

createfiveyeargwsumPlot <- function(data){
  
  isInverted <- data$reportMetadata$isInverted
  fiveyrData <- parseFiveYrData(data)
  
  #semantics for min/max are swapped on inverted plots
  maxLabel = "Max. Instantaneous"
  minLabel = "Min. Instantaneous";
  if(isInverted) {
    maxLabel = "Min. Instantaneous"
    minLabel = "Max. Instantaneous";
  }
  
  if(anyDataExist(fiveyrData)){
    
    fiveyrInfo <- parseFiveYrSupplemental(data, fiveyrData, zeroValues(fiveyrData, "value"))
    
    fiveyrPlot <- gsplot(yaxs='r', xaxt="n", mar=c(8, 4, 4, 2) + 0.1) %>% 
      axis(side=1, at=fiveyrInfo$date_seq_mo, labels=FALSE) %>%
      lines(as.POSIXct(NA), NA, xlim=c(fiveyrInfo$startDate, fiveyrInfo$endDate)) %>% 
      abline(v=fiveyrInfo$date_seq_yr, col="gray47", lwd=2) %>%
      mtext(text=fiveyrInfo$month_label, at=fiveyrInfo$month_label_location, cex=0.5, side=1) %>% 
      mtext(text=year(fiveyrInfo$date_seq_yr), at=fiveyrInfo$date_seq_yr+(60*60*24*30*6), line=1, side=1) %>% 
      legend(location="below", cex=0.8, ncol=2) %>% 
      axis(side=2, reverse=isInverted) %>% 
      grid(col="lightgrey", lty=1) %>% 
      title(main=data$reportMetadata$title,
            ylab="Water Level, Below LSD (feet)")
    
    for (i in 1:length(fiveyrData)) {
      
      fiveyrStyles <- getFiveyearStyle(fiveyrData[i], fiveyrInfo, maxLabel=maxLabel, minLabel=minLabel)
      for (j in seq_len(length(fiveyrStyles))) {
        fiveyrPlot <- do.call(names(fiveyrStyles[j]), append(list(object=fiveyrPlot), fiveyrStyles[[j]]))
      }
    }
    
    fiveyrPlot <- reorder_approvals(fiveyrPlot)
    fiveyrPlot <- rm.duplicates(fiveyrPlot, "view.1.2", "legend.name")
    fiveyrPlot <- rm.duplicates(fiveyrPlot, "legend", "legend")
    
  } else {
    fiveyrPlot <- NULL
  }

  return(fiveyrPlot)
  
}
