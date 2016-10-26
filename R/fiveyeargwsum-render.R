fiveyeargwsumPlot <- function(data) {
  plot_object <- createfiveyeargwsumPlot(data)
  return(plot_object)
}

createfiveyeargwsumPlot <- function(data){
  
  isInverted <- data$reportMetadata$isInverted
  fiveyrData <- parseFiveYrData(data)
  
  #semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous"
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous"
  }
  
  if(anyDataExist(fiveyrData)){
    
    fiveyrInfo <- parseFiveYrSupplemental(data, fiveyrData)
    
    plot_object <- gsplot(yaxs = 'i', xaxt = "n") %>%
      axis(side = 1, at = fiveyrInfo$date_seq_mo, labels = FALSE) %>%
      view(xlim = c(fiveyrInfo$startDate, fiveyrInfo$endDate)) %>%
      legend(location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = isInverted) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(main = data$reportMetadata$title, ylab = "Water Level, Below LSD (feet)")
    
    plot_object <- XAxisLabels(plot_object,
                               fiveyrInfo$month_label,
                               fiveyrInfo$month_label_location,
                               fiveyrInfo$date_seq_yr)
    
    for (i in grep("^appr_.*_uv$", names(fiveyrData), invert = TRUE)) {
      fiveyrStyles <-
        getFiveyearStyle(fiveyrData[i], fiveyrInfo, maxLabel = maxLabel, minLabel = minLabel)
      for (j in seq_len(length(fiveyrStyles))) {
        plot_object <-
          do.call(names(fiveyrStyles[j]), append(list(object = plot_object), fiveyrStyles[[j]]))
      }
    }
    
    plot_object <- plot_object %>% 
      abline(v=fiveyrInfo$date_seq_yr, col="gray47", lwd=2, where='first')
    
    plot_object <- ApplyApprovalBarStyles(plot_object, fiveyrData)
    
    plot_object <- rm.duplicate.legend.items(plot_object)
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)
  }
  else {
    plot_object <- NULL
  }

  return(plot_object)
  
}
