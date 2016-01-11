

fiveyeargwsumPlot <- function(data) {
  options(scipen=5)
  fiveyrplot <- createfiveyeargwsumPlot(data)
  return(fiveyrplot)
}

createfiveyeargwsumPlot <- function(data){
  
  isInverted <- data$reportMetadata$isInverted
  dvData <- parseFiveYrData(data)
  
  if(anyDataExist(dvData)){
    
    dvInfo <- parseFiveYrSupplemental(data, dvData, zeroValues(dvData, "value"))
    
    dvhplot <- gsplot(yaxs='r', xaxt="n", mar=c(8, 4, 4, 2) + 0.1) %>% 
      axis(side=1, at=dvInfo$date_seq_mo, labels=FALSE) %>%
      lines(as.POSIXct(NA), NA, xlim=c(dvInfo$startDate, dvInfo$endDate)) %>% 
      abline(v=dvInfo$date_seq_yr, col="gray47", lwd=2) %>%
      mtext(text=dvInfo$month_label, at=dvInfo$month_label_location, cex=0.5, side=1) %>% 
      mtext(text=year(dvInfo$date_seq_yr), at=dvInfo$date_seq_yr+(60*60*24*30*6), line=1, side=1) %>% 
      legend(location="below", cex=0.8, ncol=2) %>% 
      axis(side=2, reverse=isInverted) %>% 
      grid(col="lightgrey", lty=1) %>% 
      title(main=data$reportMetadata$title,
            ylab="Water Level, Below LSD (feet)")
    
    for (i in 1:length(dvData)) {
      
      dvStyles <- getFiveyearStyle(dvData[i], dvInfo, invertedPlot=isInverted)
      for (j in seq_len(length(dvStyles))) {
        dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
      }
    }
    
    dvhplot <- reorder_approvals(dvhplot)
  } else {
    dvhplot <- NULL
  }

  return(dvhplot)
  
}
