#'Create a uvhydrograph
#'@export
#'@param data coming in to create a plot
#'@rdname uvhydrographPlot
uvhydrographPlot <- function(data) {
  options(scipen=5) # less likely to give scientific notation

  months <- getMonths(data)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  if(!is.null(months)){
    for (month in months) {
      primaryPlotTable <- createPrimaryPlot(data, month)
      if(!is.null(primaryPlotTable$plot)){
        secondaryPlotTable <- createSecondaryPlot(data, month)
      } else {
        secondaryPlotTable <- list()
      }
      
      renderList[[month]] <- list(plot1=primaryPlotTable$plot, table1=primaryPlotTable$table, 
                                  status_msg1=primaryPlotTable$status_msg,
                                  plot2=secondaryPlotTable$plot, table2=secondaryPlotTable$table,
                                  status_msg2=secondaryPlotTable$status_msg)
    }
  } else {
    renderList[[1]] <- list(plot1=NULL, table1=NULL, plot2=NULL, table2=NULL)
  }
  
  return(renderList)
  
}

createPrimaryPlot <- function(data, month){ 
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  primaryData <- parseUVData(data, "primary", month)

  correctedExist <- 'corr_UV' %in% names(primaryData)
  referenceExist <- 'corr_UV_Qref' %in% names(primaryData)
  comparisonExist <- 'comp_UV' %in% names(primaryData)
  
  if(correctedExist){

    primaryInfo <- parseUVSupplemental(data, "primary", primaryData)
    
    plotEndDate <- tail(primaryInfo$plotDates,1) + hours(23) + minutes(45)
    plotStartDate <- primaryInfo$plotDates[1]

    ylimPrimaryData <- unname(unlist(sapply(primaryData[grepl("^corr_UV$", names(primaryData))], function (x) x['value'])))
    ylimReferenceData <- unname(unlist(sapply(primaryData[grepl("^corr_UV_Qref$", names(primaryData))], function (x) x['value'])))
    ylimCompData <- unname(unlist(sapply(primaryData[grepl("^comp_UV$", names(primaryData))], function (x) x['value'])))

    primarySide <- 2
    referenceSide <- 4
    comparisonSide <- 6

    #Setup limits and sides based on TS properties
    if(referenceExist){
      if(primaryInfo$primary_type == primaryInfo$reference_type){
        referenceSide <- 2
      }
    }

    if(comparisonExist){
        if(primaryInfo$comp_UV_type == primaryInfo$primary_type){
          comparisonSide <- primarySide
          ylimPrimaryData <- append(ylimPrimaryData, ylimCompData)
          ylimCompData <- ylimPrimaryData
        } else if(referenceExist && primaryInfo$comp_UV_type == primaryInfo$reference_type) {
          comparisonSide <- referenceSide
          ylimReferenceData <- append(ylimReferenceData, ylimCompData)
          ylimCompData <- ylimReferenceData
        } else if(!referenceExist) {
          comparisonSide <- referenceSide
        }
    }

    ylims <- data.frame(primary=YAxisInterval(ylimPrimaryData, data$uncorr_UV$value), reference=YAxisInterval(ylimReferenceData, data$uncorr_UV2$value), comparison=YAxisInterval(ylimCompData, ylimCompData))
    sides <- data.frame(primary=primarySide, reference=referenceSide, comparison=comparisonSide)

    plot_object <- gsplot(ylog = primaryInfo$logAxis, yaxs = 'r') %>%
      view(xlim = c(plotStartDate, plotEndDate)) %>%
      axis(side = 1, at = primaryInfo$plotDates, labels = as.character(primaryInfo$days)) %>%
      axis(side = 2, las = 0) %>%
      lines(x=0, y=0, side = 2, reverse = primaryInfo$isInverted) %>%
      title(
        main = format(primaryInfo$plotDates[1], "%B %Y"),
        xlab = paste("UV Series:", primaryInfo$date_lbl)
      )

      if(referenceExist || comparisonExist){
        plot_object <- lines(plot_object, x=0, y=0, side = 4, reverse = primaryInfo$isInverted) %>%
        axis(side = 4, las = 0)
      }
      
    for (i in grep("^appr_.+_uv", names(primaryData), invert = TRUE)) {
      
      correctionLabels <- parseLabelSpacing(primaryData[i], primaryInfo)
      primaryStyles <- getUvStyle(primaryData[i], primaryInfo, correctionLabels, "primary", dataSides=sides, dataLimits=ylims)
      
      for (j in seq_len(length(primaryStyles))) {
        plot_object <-
          do.call(names(primaryStyles[j]), append(list(object = plot_object), primaryStyles[[j]]))
      }
      
      which_error_bars <- grep('error_bar', names(primaryStyles))
      for (err in which_error_bars) {
        plot_object <- extendYaxisLimits(plot_object, primaryStyles[[err]])
      }
      
    }

    # approval bar styles are applied last, because it makes it easier to align
    # them with the top of the x-axis line
    plot_object <- ApplyApprovalBarStyles(plot_object, primaryData)
    
    plot_object <- rm.duplicate.legend.items(plot_object)
    
    legend_items <- plot_object$legend$legend.auto$legend
    ncol <- ifelse(length(legend_items) > 3, 2, 1)
    leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
    legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
    plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                      legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
      grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray", where='first') %>%
      abline(v=primaryInfo$plotDates, lty=3, col="gray", where='first')
    
    plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
    
    table <- correctionsTable(primaryData)
    
  } else {
    status_msg <- paste('Corrected data missing for', data$reportMetadata$primaryParameter)
  }
    
  return(list(plot=plot_object, table=table, status_msg=status_msg))
}

createSecondaryPlot <- function(data, month){
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  isReferenceSeries <- any(grepl("downsampledReferenceSeries", names(data)))
  isUpchainSeries <- any(grepl("downsampledUpchainSeries", names(data)))
  
  if((isReferenceSeries && !any(grepl("Discharge", getReportMetadata(data,'primaryParameter')))) || isUpchainSeries) {
    secondaryData <- parseUVData(data, "secondary", month)
    
    correctedExist <- 'corr_UV2' %in% names(secondaryData)
    if(correctedExist){
    
      secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData)
      
      plotEndDate <- tail(secondaryInfo$plotDates,1) + hours(23) + minutes(45)
      plotStartDate <- secondaryInfo$plotDates[1]

      ylimSecondaryData <- unname(unlist(sapply(secondaryData[grepl("^corr_UV2$", names(secondaryData))], function (x) x['value'])))

      plot_object <- gsplot(yaxs = 'r') %>%
        view(
          xlim = c(plotStartDate, plotEndDate),
          ylim = YAxisInterval(ylimSecondaryData, secondaryData$uncorr_UV2$value)
        ) %>%
        axis(side = 1, at = secondaryInfo$plotDates, labels = as.character(secondaryInfo$days)) %>%
        axis(side = 2, reverse = secondaryInfo$isInverted, las = 0) %>%
        title(
          main = "",
          xlab = paste("UV Series:", secondaryInfo$date_lbl),
          ylab = secondaryInfo$secondary_lbl
        )
      
      for (i in grep("^appr_.+_uv", names(secondaryData), invert = TRUE)) {
        
        correctionLabels <- parseLabelSpacing(secondaryData[i], secondaryInfo)
        secondaryStyles <- getUvStyle(secondaryData[i], secondaryInfo, correctionLabels, "secondary")
        
        for (j in seq_len(length(secondaryStyles))) {
          plot_object <- do.call(names(secondaryStyles[j]), append(list(object=plot_object), secondaryStyles[[j]]))
        }
        
        which_error_bars <- grep('error_bar', names(secondaryStyles))
        for(err in which_error_bars){
          plot_object <- extendYaxisLimits(plot_object, secondaryStyles[[err]])
        }
        
      }
      
      plot_object <- ApplyApprovalBarStyles(plot_object, secondaryData)
      
      plot_object <- rm.duplicate.legend.items(plot_object)
      
      legend_items <- plot_object$legend$legend.auto$legend
      ncol <- ifelse(length(legend_items) > 3, 2, 1)
      leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
      legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
    
      plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                                legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
        grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
        abline(v=secondaryInfo$plotDates, lty=3, col="gray")
      
      isShift <- length(grep("shift", names(secondaryData))) > 0
      if(isShift){
        plot_object <- plot_object %>% 
          mtext(paste0(secondaryInfo$tertiary_lbl, " (", secondaryInfo$sec_units, ")"), 
                              side = 4, line = 1.5) %>% 
          axis(side=4, las=0)
      }
      
      plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
      
      table <- correctionsTable(secondaryData)
    
    } else {
      status_msg <- paste('Corrected data missing for', data$reportMetadata$secondaryParameter)
    }
  } 

  return(list(plot=plot_object, table=table, status_msg=status_msg))
}

YAxisInterval <- function(corr.value.sequence, uncorr.value.sequence) {
  # Compute the y-axis real interval, based on a heuristic.
  # 
  # Args:
  #   corr.value.sequence: An array of corrected time series values.
  #   uncorr.value.sequence: An array of uncorrected time series values.
  #
  # Returns:
  #   The y-axis real interval, as order-pair vector.
  return(c(
      YOrigin(corr.value.sequence, uncorr.value.sequence),
      YEndpoint(corr.value.sequence, uncorr.value.sequence)
  ))
}

YOrigin <- function (corr.value.sequence, uncorr.value.sequence) {
  # Compute the y-axis origin, based on a heuristic.
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
  # Compute the y-axis endpoint, based on a heuristic.
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
