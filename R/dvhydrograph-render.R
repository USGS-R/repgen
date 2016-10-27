dvhydrographPlot <- function(data) {
  plot_object <- createDvhydrographPlot(data)
  return(plot_object)
}

createDvhydrographPlot <- function(data) {
  
  dvData <- parseDVData(data)
  isInverted <- data$reportMetadata$isInverted
  
  # semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous"
  if (isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous"
  }
  
  if(anyDataExist(dvData)){
    dvInfo <- parseDVSupplemental(data, dvData)
    startDate <- flexibleTimeParse(data$reportMetadata$startDate, timezone=data$reportMetadata$timezone) 
    endDate <- toEndOfDay(flexibleTimeParse(data$reportMetadata$endDate, timezone=data$reportMetadata$timezone))
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plotDates <- toStartOfDay(plotDates)
    
    plot_object <- gsplot(ylog = dvInfo$logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      legend(location = "below", cex = 0.8, y.intersp = 1.5) %>%
      title(
        main = "DV Hydrograph",
        ylab = paste0(data$firstDownChain$type, ", ", data$firstDownChain$units),
        line = 3
      )
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(dvData), invert = TRUE)) {
      dvStyles <- getDvStyle(dvData[i], dvInfo, maxLabel = maxLabel, minLabel = minLabel)
      for (j in names(dvStyles)) {
        dvStyles[[j]] <- extendStep(dvStyles[[j]])
        plot_object <- do.call(names(dvStyles[j]), append(list(object = plot_object), dvStyles[[j]]))
      }
    }

    # approval bar styles are applied last, because it makes it easier to align
    # them with the top of the x-axis line
    plot_object <- ApplyApprovalBarStyles(plot_object, dvData)
    
    plot_object <- rm.duplicate.legend.items(plot_object)
    
    # custom gridlines below approval bar
    plot_object <- plot_object %>% 
      abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="weeks"), col="darkgray", lwd=1, where='first')
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)

    #Add Min/Max labels if we aren't plotting min and max
    if(!is.null(dvData$max_iv_label) && !is.null(dvData$min_iv_label)){
      plot_object <- plot_object %>% 
          mtext(paste0(maxLabel, " ", dvInfo$type, ": ", dvData$max_iv_label$value, " ", data$firstDownChain$units, " (", format(dvData$max_iv_label$time, "%b %d, %Y") , ")"), 
                              side = 3, axes=FALSE, cex=0.85, line = 1.33, adj = 0) %>%
          mtext(paste0(minLabel, " ", dvInfo$type, ": ", dvData$min_iv_label$value, " ", data$firstDownChain$units, " (", format(dvData$min_iv_label$time, "%b %d, %Y") , ")"), 
                              side = 3, axes=FALSE, cex=0.85, line = 0.33, adj = 0)
    }
    
    return(plot_object)
  }
  else {
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
    
    startDate <- flexibleTimeParse(data$reportMetadata$startDate, timezone=data$reportMetadata$timezone)
    endDate <- toEndOfDay(flexibleTimeParse(data$reportMetadata$endDate, timezone=data$reportMetadata$timezone))
    plotDates <- seq(startDate, endDate, by=7*24*60*60)
    
    plotDates <- toStartOfDay(plotDates)
    plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = NA, ny = NULL, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = isInverted) %>%
      view(xlim = c(startDate, endDate)) %>%
      title(
        main = paste(ref_name_capital, "Reference Time Series"),
        ylab = paste(data[[ref_name]]$type, data[[ref_name]]$units),
        line = 3
      ) %>%
      legend(location = "below", cex = 0.8, y.intersp = 1.5)
    
    # for non-approval-bar objects
    for (i in grep("^appr_", names(refData), invert = TRUE)) {
      refStyles <- getDvStyle(refData[i])
      for (j in seq_len(length(refStyles))) {
        plot_object <- do.call(names(refStyles[j]), append(list(object = plot_object), refStyles[[j]]))
      }
    }
    
    plot_object <- ApplyApprovalBarStyles(plot_object, refData)
    
    plot_object <- rm.duplicate.legend.items(plot_object)
    
    plot_object <- plot_object %>% 
      abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
      abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)
    
    return(plot_object)
  }
}
