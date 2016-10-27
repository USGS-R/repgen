fiveyeargwsumPlot <- function(data) {
  options(scipen=8)
  plot_object <- createfiveyeargwsumPlot(data)
  return(plot_object)
}

createfiveyeargwsumPlot <- function(data){
  
  isInverted <- data$reportMetadata$isInverted
  fiveyrData <- parseFiveYrData(data)
  
  #semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous";
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous";
  }
  
  if(anyDataExist(fiveyrData)){
    
    fiveyrInfo <- parseFiveYrSupplemental(data, fiveyrData)
    
    plot_object <- gsplot(yaxs = 'i', xaxt = "n", mar = c(8, 4, 4, 2) + 0.1) %>%
      axis(side = 1, at = fiveyrInfo$date_seq_mo, labels = FALSE) %>%
      view(xlim = c(fiveyrInfo$startDate, fiveyrInfo$endDate)) %>%
      mtext(
        text = fiveyrInfo$month_label,
        at = fiveyrInfo$month_label_location,
        cex = 0.5, side = 1
      ) %>%
      mtext(
        text = year(fiveyrInfo$date_seq_yr),
        at = fiveyrInfo$date_seq_yr + (60 * 60 * 24 * 30 * 6),
        line = 1, side = 1
      ) %>%
      legend(
        location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = isInverted) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(main = data$reportMetadata$title, ylab = "Water Level, Below LSD (feet)")
    
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

    #Add Min/Max labels if we aren't plotting min and max
    if(!is.null(fiveyrData$max_iv_label) && !is.null(fiveyrData$min_iv_label)){
      plot_object <- plot_object %>% 
          mtext(paste0(maxLabel, " ", fiveyrInfo$type, ": ", fiveyrData$max_iv_label$value, " ", data$firstDownChain$units, " (", format(as.POSIXct(fiveyrData$max_iv_label$time), "%b %d, %Y %H:%M:%S %Z") , ")"),
                              side = 3, axes=FALSE, cex=0.625, line = 0.85, adj = 0) %>%
          mtext(paste0(minLabel, " ", fiveyrInfo$type, ": ", fiveyrData$min_iv_label$value, " ", data$firstDownChain$units, " (", format(as.POSIXct(fiveyrData$min_iv_label$time), "%b %d, %Y %H:%M:%S %Z") , ")"),
                              side = 3, axes=FALSE, cex=0.625, line = 0.1, adj = 0)
    }
  }
  else {
    plot_object <- NULL
  }

  return(plot_object)
  
}
