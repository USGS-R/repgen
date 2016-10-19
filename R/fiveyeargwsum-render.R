fiveyeargwsumPlot <- function(data) {
  options(scipen=5)
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
    plot_object <- FiveYearGWSumRescaleYTop(plot_object)
  }
  else {
    plot_object <- NULL
  }

  return(plot_object)
  
}

#' Rescale top of y-axis to create ~4% margin between vertical top extent of 
#' plot objects and top edge of plot. This is an inaccurate emulation of (the 
#' top-end-of-plot behavior of) R graphics::par's "yaxs = 'r'" state, because we
#' have to use "yaxs = 'i'" in spots, but still want the ~4% margin at the top 
#' of the plot, so we adjust the y-axis endpoint accordingly after we do what we
#' need.
#' @param object A gsplot, plot object.
#' @return The passed-in gsplot object, with y-axis top augmented (upwards).
FiveYearGWSumRescaleYTop <- function(object) {
  ylog <- par("ylog")
  reverse <- object$side.2$reverse
  
  # Desired top margin, in NDCs. See also "yaxs" parameter domain in
  # graphics::par.
  m <- 0.04
  e <- ylim(object)$side.2
  e.length <- abs(e[1] - e[2])
  
  if (ylog) {
    # if the y-axis is inverted
    if (reverse) {
      object$side.2$lim[1] <- 10^((1 - m) * log10(e[2]))
    }
    else {
      object$side.2$lim[2] <- 10^((1 + m) * log10(e[2]))
    }
  }
  else {
    # if the y-axis is inverted
    if (reverse) {
      object$side.2$lim[1] <- e[2] - m * e.length
    }
    else {
      object$side.2$lim[2] <- e[2] + m * e.length
    }
  }
  
  return(object)
}

