#'Starting point, creates RMD and runs rendering
#'@param data coming in to create a plot
#'@param output format (either html or pdf)
#'@param author name of person generating the report
#'@rdname startFiveYearRender 
#'@export
startFiveYearRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('fiveyeargwsum', 'fiveyeargwsum.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}

fiveyeargwsumPlot <- function(data) {
  options(scipen=5)
  fiveyrplot <- createfiveyeargwsumPlot(data)
  return(dvhplot)
}

createfiveyeargwsumPlot <- function(data){
  
  dvData <- parseDVData(data)
  dvInfo <- parseDVSupplemental(data, dvData, zeroValues(dvData))
  
  startDate <- formatDates_fiveyr(data$reportMetadata$startDate, "start")
  endDate <- formatDates_fiveyr(data$reportMetadata$endDate, "end")
  
  date_seq_mo <- seq(from=startDate, to=endDate, by="month")
  first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
  date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
  month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
  month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
  month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  
  dvhplot <- gsplot(yaxs='r', xaxt="n") %>% 
    axis(side=1, at=date_seq_mo, labels=FALSE) %>%
    lines(as.POSIXct(NA), NA, xlim=c(startDate, endDate)) %>% 
    abline(v=date_seq_yr, col="gray47", lwd=2) %>%
    mtext(text=month_label, at=month_label_location, cex=0.5, side=1) %>% 
    mtext(text=year(date_seq_yr), at=date_seq_yr+(60*60*24*30*6), line=1, side=1) %>% 
    legend(cex=0.5) %>% 
    axis(side=2, reverse=TRUE) %>% 
    grid(col="lightgrey", lty=1)
  
  for (i in 1:length(dvData)) {
    
    dvStyles <- getFiveyearStyle(dvData[i])
    for (j in seq_len(length(dvStyles))) {
      dvhplot <- do.call(names(dvStyles[j]), append(list(object=dvhplot), dvStyles[[j]]))
    }
  }

  return(dvhplot)
  
}
