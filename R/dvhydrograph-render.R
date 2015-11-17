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
  return(dvhplot)
}
  
createDvhydrographPlot <- function(data){
  
  dvData <- parseDVData(data)
  dvInfo <- parseDVSupplemental(data, dvData, zeroValues(dvData))
  
  startDate <- formatDates(data$reportMetadata$startDate)
  endDate <- formatDates(data$reportMetadata$endDate)
  
  dvhplot <- gsplot(ylog=dvInfo$logAxis, yaxs='r') %>% 
    lines(as.POSIXct(NA), NA, 
          xlim=c(startDate, endDate)) %>% 
    grid(nx=NA, ny=NULL, lwd=2, lty=1, col="darkgreen") %>%  
    abline(v=seq(from=startDate, to=endDate, by="days"), col="lightgreen", lwd=1) %>% 
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgreen", lwd=1) %>% 
    abline(h=dvInfo$horizontalGrid, col="darkgreen", lwd=2) %>% 
    abline(v=seq(from=startDate, to=endDate, by="month"), col="purple", lwd=2) %>%
    legend(location="below", title="", cex=0.8) %>%
    title(main="", xlab=paste(data$reportMetadata$siteNumber, "-", data$reportMetadata$stationName), 
        ylab=paste(data$primaryTimeSeries$type, " ", data$primaryTimeSeries$units))
    
  
  for (i in 1:length(dvData)) {
    x <- dvData[[i]]$time
    y <- dvData[[i]]$value
    
    dvStyles <- getDvStyle(dvData[i], x, y)
    for (j in seq_len(length(dvStyles))) {
      dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
    }
  }
  
  return(dvhplot)
  
}
  
  
 

