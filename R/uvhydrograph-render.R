#'Create a uvhydrograph
#'@export
#'@param data coming in to create a plot
#'@rdname uvhydrographPlot

uvhydrographPlot <- function(data) {
  options(scipen = 5) # less likely to give scientific notation
  
  months <- getMonths(data)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  if (!is.null(months)) {
    for (month in months) {
      primaryPlotTable <- createPrimaryPlot(data, month)
      if (!is.null(primaryPlotTable$plot)) {
        secondaryPlotTable <- createSecondaryPlot(data, month)
      } else {
        secondaryPlotTable <- list()
      }
      
      renderList[[month]] <-
        list(
          plot1 = primaryPlotTable$plot, table1 = primaryPlotTable$table,
          status_msg1 = primaryPlotTable$status_msg,
          plot2 = secondaryPlotTable$plot, table2 = secondaryPlotTable$table,
          status_msg2 = secondaryPlotTable$status_msg
        )
    }
  } else {
    renderList[[1]] <- list(plot1 = NULL, table1 = NULL, plot2 = NULL, table2 = NULL)
  }
  
  return(renderList)
}

createPrimaryPlot <- function(data, month) {
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  primaryData <- parseUVData(data, "primary", month)
  
  correctedExist <- 'corr_UV' %in% names(primaryData)
  
  if (correctedExist) {
    primaryInfo <- parseUVSupplemental(data, "primary", primaryData)
    
    plotEndDate <- tail(primaryInfo$plotDates, 1) + hours(23) + minutes(45)
    plotStartDate <- primaryInfo$plotDates[1]
    
    y.origin <- YOrigin(primaryData$corr_UV$value, primaryData$uncorr_UV$value)
    y.endpoint <- YEndpoint(primaryData$corr_UV$value, primaryData$uncorr_UV$value)
    
    plot_object <-
      gsplot(ylog = primaryInfo$logAxis,
             yaxs = 'r',
             xaxs = 'r') %>%
      lines(
        as.POSIXct(NA), as.numeric(NA),
        xlim = c(plotStartDate, plotEndDate), ylim = c(y.origin, y.endpoint)
      ) %>%
      axis(
        side = 1, at = primaryInfo$plotDates,
        labels = as.character(primaryInfo$days)
      ) %>%
      axis(side = 2, reverse = primaryInfo$isInverted, las = 0) %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray",
        legend.name = "horizontalGrids"
      ) %>%
      abline(
        v = primaryInfo$plotDates, lty = 3, col = "gray",
        legend.name = "verticalGrids"
      ) %>%
      title(
        main = format(primaryInfo$plotDates[1], "%B %Y"),
        xlab = paste("UV Series:", primaryInfo$date_lbl),
        ylab = primaryInfo$primary_lbl
      )
    
    for (i in 1:length(primaryData)) {
      correctionLabels <- parseLabelSpacing(primaryData[i], primaryInfo)
      primaryStyles <-
        getUvStyle(primaryData[i], primaryInfo, correctionLabels, "primary")
      
      for (j in seq_len(length(primaryStyles))) {
        plot_object <-
          do.call(names(primaryStyles[j]), append(list(object = plot_object), primaryStyles[[j]]))
      }
    }
    
    orderLegend <-
      c("verticalGrids", "Working UV", "In Review UV", "Approved UV", "horizontalGrids")
    plot_object <- reorderPlot(plot_object, "view.1.2", "legend.name", orderLegend)
    plot_object <- reorderPlot(plot_object, "legend", "legend", orderLegend)
    plot_object <- rm.duplicates(plot_object, "view.1.2", "legend.name")
    plot_object <- rm.duplicates(plot_object, "legend", "legend")
    
    ncol <- ifelse(length(plot_object$legend) > 3, 2, 1)
    leg_lines <- ifelse(ncol == 2, ceiling((length(plot_object$legend) - 6) / 2), 0)
    legend_offset <- ifelse(ncol == 2, 0.3 + (0.005 * leg_lines), 0.3)
    plot_object <- legend(
        plot_object, location = "below", title = "", ncol = ncol,
        legend_offset = legend_offset, cex = 0.8
      )
    
    plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
    
    ##HACKY FIX FOR OVERLAPPING LABELS###
    plot_object$view.1.2$window$xlim <- as.numeric(xlim(plot_object)$side.1)
    plot_object$view.1.2$window$ylim <- as.numeric(ylim(plot_object)$side.2)
    
    table <- correctionsTable(primaryData)
    
  } else {
    status_msg <- paste('Corrected data missing for', data$reportMetadata$primaryParameter)
  }
  
  return(list(plot = plot_object, table = table, status_msg = status_msg))
}

createSecondaryPlot <- function(data, month) {
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  isSecondarySeries <- any(grepl("downsampledSecondarySeries", names(data)))
  
  if (isSecondarySeries) {
    secondaryData <- parseUVData(data, "secondary", month)
    
    correctedExist <- 'corr_UV2' %in% names(secondaryData)
    if (correctedExist) {
      secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData)
      
      plotEndDate <- tail(secondaryInfo$plotDates, 1) + hours(23) + minutes(45)
      plotStartDate <- secondaryInfo$plotDates[1]
      
      y.origin <- YOrigin(secondaryData$corr_UV2$value, secondaryData$uncorr_UV2$value)
      y.endpoint <- YEndpoint(secondaryData$corr_UV2$value, secondaryData$uncorr_UV2$value)
      
      plot_object <- gsplot(yaxs = 'r', xaxs = 'r') %>%
        lines(
          as.POSIXct(NA), as.numeric(NA),
          xlim = c(plotStartDate, plotEndDate), ylim = c(y.origin, y.endpoint)
        ) %>%
        grid(
          nx = 0, ny = NULL,
          equilogs = FALSE, lty = 3, col = "gray"
        ) %>%
        abline(
          v = secondaryInfo$plotDates, lty = 3, col = "gray",
          legend.name = "verticalGrids"
        ) %>%
        title(
          main = "",
          xlab = paste("UV Series:", secondaryInfo$date_lbl2),
          ylab = secondaryInfo$secondary_lbl
        )
      
      for (i in 1:length(secondaryData)) {
        correctionLabels <- parseLabelSpacing(secondaryData[i], secondaryInfo)
        secondaryStyles <-
          getUvStyle(secondaryData[i], secondaryInfo, correctionLabels, "secondary")
        
        for (j in seq_len(length(secondaryStyles))) {
          plot_object <- do.call(names(secondaryStyles[j]),
                                 append(list(object = plot_object), secondaryStyles[[j]]))
        }
      }
      
      orderLegend <- c("verticalGrids", "Working", "In Review", "Approved")
      plot_object <- reorderPlot(plot_object, "view.1.2", "legend.name", orderLegend)
      plot_object <- reorderPlot(plot_object, "legend", "legend", orderLegend)
      plot_object <- rm.duplicates(plot_object, "view.1.2", "legend.name")
      plot_object <- rm.duplicates(plot_object, "legend", "legend")
      
      ncol <- ifelse(length(plot_object$legend) > 3, 2, 1)
      leg_lines <- ifelse(ncol == 2, ceiling((length(plot_object$legend) - 6) / 2), 0)
      legend_offset <- ifelse(ncol == 2, 0.3 + (0.05 * leg_lines), 0.3)
      
      plot_object <-
        legend(
          plot_object, location = "below", title = "", ncol = ncol,
          legend_offset = legend_offset, cex = 0.8
        ) %>%
        axis(
          side = 1, at = secondaryInfo$plotDates,
          labels = as.character(secondaryInfo$days)
        ) %>%
        axis(side = 2, reverse = secondaryInfo$isInverted, las = 0)
      
      isShift <- length(grep("shift", names(secondaryData))) > 0
      if (isShift) {
        plot_object <- plot_object %>%
          mtext(
            paste0(
              secondaryInfo$tertiary_lbl, " (", secondaryInfo$sec_units, ")"
            ),
            side = 4, line = 1.5
          ) %>%
          axis(side = 4, las = 0)
      }
      
      plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
      
      table <- correctionsTable(secondaryData)
      
      ##HACKY FIX FOR OVERLAPPING LABELS###
      plot_object$view.1.2$window$xlim <-
        as.numeric(xlim(plot_object)$side.1)
      plot_object$view.1.2$window$ylim <-
        as.numeric(ylim(plot_object)$side.2)
      if (isShift) {
        plot_object$view.1.4$window$xlim <- xlim(plot_object)$side.1
      }
      
    } else {
      status_msg <- paste('Corrected data missing for', data$reportMetadata$secondaryParameter)
    }
  }
  
  return(list(plot = plot_object, table = table, status_msg = status_msg))
}

YOrigin <- function (corr.value.sequence, uncorr.value.sequence) {
  # Compute the y-axis origin, based on a heuristic. See also JIRA issue
  # AQCU-769.
  # 
  # Args:
  #   corr.value.sequence: An array of corrected time series values.
  #   uncorr.value.sequence: An array of uncorrected time series values.
  #
  # Returns:
  #   The y-axis origin value.
  min.corr.value <- min(corr.value.sequence)
  min.uncorr.value <- min(uncorr.value.sequence)
  
  # if minimum corrected value is below or equal to minimum uncorrected, or if
  # the minimum uncorrected value is less than 70% of the minimum corrected
  # value
  if (min.corr.value <= min.uncorr.value || min.uncorr.value < 0.70 * min.corr.value)
    y.origin <- min.corr.value # use minimum corrected value as y-axis origin
  else
    y.origin <- min.uncorr.value # use minimum uncorrected value as y-axis origin

  return(y.origin)
}

YEndpoint <- function (corr.value.sequence, uncorr.value.sequence) {
  # Compute the y-axis endpoint, based on a heuristic. See also JIRA issue
  # AQCU-769.
  # 
  # Args:
  #   corr.value.sequence: An array of corrected time series values.
  #   uncorr.value.sequence: An array of uncorrected time series values.
  #
  # Returns:
  #   The y-axis endpoint value.
  max.corr.value <- max(corr.value.sequence)
  max.uncorr.value <- max(uncorr.value.sequence)
  
  # if maximum corrected value is greater than or equal to the maxium
  # uncorrected value, or if the maximum uncorrected value is greater than 130%
  # of the maximum corrected value
  if (max.corr.value >= max.uncorr.value || max.uncorr.value > 1.30 * max.corr.value)
    y.endpoint <- max.corr.value   # use corrected time series' maximum as y-axis endpoint
  else
    y.endpoint <- max.uncorr.value # use uncorrected time series' maxium as y-axis endpoint
  
  return(y.endpoint)
}
