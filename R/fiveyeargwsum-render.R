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
  minLabel <- "Min. Instantaneous"
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous"
  }
  
  if(anyDataExist(fiveyrData)){
    
    fiveyrInfo <- parseFiveYrSupplemental(data, fiveyrData)
    
    plot_object <- gsplot(yaxs = 'i', xaxt = "n", mar = c(8, 4, 4, 2) + 0.1) %>%
      axis(side = 1, at = fiveyrInfo$date_seq_mo, labels = FALSE) %>%
      view(xlim = c(fiveyrInfo$startDate, fiveyrInfo$endDate)) %>%
      legend(location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = isInverted) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(ylab = "Water Level, Below LSD (feet)")

    plot_object <-
      XAxisLabels(plot_object,
                  fiveyrInfo$month_label,
                  fiveyrInfo$month_label_location,
                  fiveyrInfo$date_seq_yr + months(6))
    
    for (i in grep("^appr_.*_uv$", names(fiveyrData), invert = TRUE)) {
      fiveyrPlotConfig <-
        getFiveyearPlotConfig(fiveyrData[i], fiveyrInfo, maxLabel = maxLabel, minLabel = minLabel)
      for (j in seq_len(length(fiveyrPlotConfig))) {
        plot_object <-
          do.call(names(fiveyrPlotConfig[j]), append(list(object = plot_object), fiveyrPlotConfig[[j]]))
      }
    }
    
    # add vertical lines to delineate calendar year boundaries
    plot_object <- DelineateYearBoundaries(plot_object, fiveyrInfo$date_seq_yr)
    
    plot_object <- ApplyApprovalBarStyles(plot_object, fiveyrData)
    
    plot_object <- rmDuplicateLegendItems(plot_object)
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)

    #Add Min/Max labels if we aren't plotting min and max
    if(!is.null(fiveyrData$max_iv_label) && !is.null(fiveyrData$min_iv_label)){
      #Extract Timezone
      tzf <- format(as.POSIXct(fiveyrData$max_iv_label$time), "%z")
      #Insert ":" before 2nd to last character
      tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf) 
      plot_object <- plot_object %>% 
          mtext(paste0(maxLabel, " ", fiveyrInfo$type, ": ", fiveyrData$max_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$max_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                              side = 3, axes=FALSE, cex=0.625, line = 0.85, adj = 0) %>%
          mtext(paste0(minLabel, " ", fiveyrInfo$type, ": ", fiveyrData$min_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$min_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                              side = 3, axes=FALSE, cex=0.625, line = 0.1, adj = 0)
    }
  }
  else {
    plot_object <- NULL
  }

  return(plot_object)
  
}

getFiveyearPlotConfig <- function(reportObject, info=NULL, ...) {
  styles <- getFiveyearStyle()
  
  x <- reportObject[[1]]$time
  y <- reportObject[[1]]$value
  legend.name <- reportObject[[1]]$legend.name
  args <- list(...)
  
  styles <- switch(names(reportObject), 
      stat = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$stat_lines)
          ),
      est_stat = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$est_stat_lines)
          ),
      max_iv = list(
          points = append(list(x=x, y=y, legend.name=paste(args$maxLabel, info$type, ":", y)), styles$max_iv_points)
          ),
      min_iv = list(
          points = append(list(x=x, y=y, legend.name=paste(args$minLabel, info$type, ":", y)), styles$min_iv_points)
          ),
      gw_level = list(
          points = append(list(x=x,y=y), styles$gw_level_points)
      )
  )
  
  return(styles)
}
