#'Create a uvhydrograph
#'@export
#'@param data coming in to create a plot
#'@rdname uvhydrographPlot
uvhydrographPlot <- function(data) {
  options(scipen=8) # less likely to give scientific notation
  
  useDownsampled <- FALSE
  if(!isEmptyOrBlank(data$reportMetadata$useDownsampling) && data$reportMetadata$useDownsampling == "true") {
    useDownsampled <- TRUE
  }
  
  months <- getMonths(data, useDownsampled=useDownsampled)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  if(!is.null(months)){
    for (month in months) {
      primaryPlotTable <- createPrimaryPlot(data, month, useDownsampled=useDownsampled)
      if(!is.null(primaryPlotTable$plot)){
        secondaryPlotTable <- createSecondaryPlot(data, month, useDownsampled=useDownsampled)
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

createPrimaryPlot <- function(data, month, useDownsampled=FALSE){ 
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  primaryData <- parseUVData(data, "primary", month, useDownsampled=useDownsampled)

  correctedExist <- 'corr_UV' %in% names(primaryData)
  referenceExist <- 'corr_UV_Qref' %in% names(primaryData)
  comparisonExist <- 'comp_UV' %in% names(primaryData)
  
  if(correctedExist){

    primaryInfo <- parseUVSupplemental(data, "primary", primaryData, useDownsampled=useDownsampled)
    
    plotEndDate <- tail(primaryInfo$plotDates,1) + hours(23) + minutes(45)
    plotStartDate <- primaryInfo$plotDates[1]

    primaryInfo$plotEndDate <- plotEndDate
    primaryInfo$plotStartDate <- plotStartDate

    ylimPrimaryData <- unname(unlist(sapply(primaryData[grepl("^corr_UV$", names(primaryData))], function (x) x['value'])))
    ylimReferenceData <- unname(unlist(sapply(primaryData[grepl("^corr_UV_Qref$", names(primaryData))], function (x) x['value'])))
    ylimCompData <- unname(unlist(sapply(primaryData[grepl("^comp_UV$", names(primaryData))], function (x) x['value'])))

    primarySide <- 2
    referenceSide <- 4
    comparisonSide <- 6

    #Setup limits and sides based on TS properties
    if(referenceExist){
      if(primaryInfo$primary_type == primaryInfo$reference_type){
        referenceSide <- primarySide
        ylimPrimaryData <- append(ylimPrimaryData, ylimReferenceData)
        ylimReferenceData <- ylimPrimaryData
      }
    } else {
      referenceSide <- 0
    }

    if(comparisonExist){
        if(primaryInfo$comp_UV_type == primaryInfo$primary_type){
          comparisonSide <- primarySide
          ylimPrimaryData <- append(ylimPrimaryData, ylimCompData)
          ylimCompData <- ylimPrimaryData
          
          if(referenceSide == primarySide){
            ylimReferenceData <- ylimPrimaryData
          }
        } else if(referenceExist && primaryInfo$comp_UV_type == primaryInfo$reference_type) {
          comparisonSide <- referenceSide
          ylimReferenceData <- append(ylimReferenceData, ylimCompData)
          ylimCompData <- ylimReferenceData
        } else if(!referenceExist || referenceSide == primarySide) {
          comparisonSide <- 4
        }
    } else {
      comparisonSide <- 0
    }

    ylims <- data.frame(primary=YAxisInterval(ylimPrimaryData, primaryData$uncorr_UV$value), reference=YAxisInterval(ylimReferenceData, primaryData$uncorr_UV2$value), comparison=YAxisInterval(ylimCompData, ylimCompData))
    sides <- data.frame(primary=primarySide, reference=referenceSide, comparison=comparisonSide)

    plot_object <- gsplot(ylog = primaryInfo$logAxis, yaxs = 'r') %>%
      view(xlim = c(plotStartDate, plotEndDate)) %>%
      axis(side = 1, at = primaryInfo$plotDates, labels = as.character(primaryInfo$days)) %>%
      axis(side = 2, reverse = primaryInfo$isInverted, las = 0) %>%
      lines(x=0, y=0, side = 2, reverse = primaryInfo$isInverted) %>%
      title(
        main = format(primaryInfo$plotDates[1], "%B %Y"),
        xlab = paste("UV Series:", primaryInfo$date_lbl)
      )

    #Don't add the right-side axis if we aren't actually plotting anything onto it
    if((referenceExist && referenceSide == 4) || (comparisonExist && comparisonSide == 4)){
      plot_object <- lines(plot_object, x=0, y=0, side = 4, reverse = primaryInfo$isInverted) %>%
      axis(side = 4, las = 0, reverse = primaryInfo$isInverted)
    }
    
    # still need gsplot to handle side 1 vs side 2 logging. See issue #414
    # once that is working, use view to log the axes when appropriate
    # could be added using logic in if statement above
    # if(isLogged(data, ylimPrimaryData, primaryInfo$primarySeriesName)){
    #   plot_object <- view(plot_object, side=primarySide, log='y')
    # }
    # if(comparisonExist && isLogged(data, ylimComparisonData, primaryInfo$comparisonSeriesName)){
    #   plot_object <- view(plot_object, side=comparisonSide, log='y')
    # }
    # if(referenceExist && isLogged(data, ylimReferenceData, primaryInfo$referenceSeriesName)){
    #   plot_object <- view(plot_object, side=referenceSide, log='y')
    # }
      
    # reorder so that uncorrected is below corrected (plot uncorrected first)
    primaryData <-
      primaryData[c(grep("^uncorr_UV$", names(primaryData)),
                    grep("^uncorr_UV$", names(primaryData), invert = TRUE))]

    # reorder so that corrections are plotted last
    primaryData <-
      primaryData[c(grep("^series_corr$", names(primaryData), invert = TRUE),
                    grep("^series_corr$", names(primaryData)))]
    
    # add data to plot
    for (i in grep("^appr_.+_uv", names(primaryData), invert = TRUE)) {
      plot_object <-
        PlotUVHydrographObject(plot_object, primaryData[i], primaryInfo,
                               "primary", sides, ylims)
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

createSecondaryPlot <- function(data, month, useDownsampled=FALSE){
  # assume everything is NULL unless altered
  plot_object <- NULL
  table <- NULL
  status_msg <- NULL
  
  if(useDownsampled) {
    referenceSeriesName <- "downsampledReferenceSeries"
    upchainSeriesName <- "downsampledUpchainSeries"
  } else {
    referenceSeriesName <- "referenceSeries"
    upchainSeriesName <- "upchainSeries"
  }
  
  isReferenceSeries <- any(grepl(referenceSeriesName, names(data)))
  isUpchainSeries <- any(grepl(upchainSeriesName, names(data)))
  
  if((isReferenceSeries && !any(grepl("Discharge", getReportMetadata(data,'primaryParameter')))) || isUpchainSeries) {
    secondaryData <- parseUVData(data, "secondary", month, useDownsampled=useDownsampled)
    
    correctedExist <- 'corr_UV2' %in% names(secondaryData)
    if(correctedExist){
    
      secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData, useDownsampled=useDownsampled)
      
      plotEndDate <- tail(secondaryInfo$plotDates,1) + hours(23) + minutes(45)
      plotStartDate <- secondaryInfo$plotDates[1]

      secondaryInfo$plotStartDate <- plotStartDate
      secondaryInfo$plotEndDate <- plotEndDate

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
      
      # reorder so that uncorrected is below corrected (plot uncorrected first)
      secondaryData <-
        secondaryData[c(grep("^uncorr_UV2$", names(secondaryData)),
                        grep("^uncorr_UV2$", names(secondaryData), invert = TRUE))]

      # reorder so that corrections are last to be added
      secondaryData <- 
        secondaryData[c(grep("^series_corr2$", names(secondaryData), invert = TRUE),
                        grep("^series_corr2$", names(secondaryData)))]
      
      # add data to plot
      for (i in grep("^appr_.+_uv", names(secondaryData), invert = TRUE)) {
        plot_object <-
          PlotUVHydrographObject(plot_object, secondaryData[i], secondaryInfo,
                                 "secondary", sides, ylims)
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
      
      # add this in once gsplot can handle logging different sides.
      # if(secondaryInfo$sec_logAxis){
      #   plot_object <- view(plot_object, side=2, log='y')
      # }
      
      isShift <- length(grep("shift", names(secondaryData))) > 0
      if(isShift){
        yMax = max(secondaryData$effect_shift$value)
        yMin = min(secondaryData$effect_shift$value)
        y_seq <- pretty(c(yMin, yMax), shrink.sml = 20)
        plot_object <- plot_object %>% 
          mtext(paste0(secondaryInfo$tertiary_lbl, " (", secondaryInfo$sec_units, ")"), 
                              side = 4, line = 1.5) %>% 
          axis(side=4, las=0, at=y_seq, reverse = secondaryInfo$isInverted)
        
        # add this in once gsplot can handle logging different sides.
        # if(secondaryInfo$tertiary_logAxis){
        #   plot_object <- view(plot_object, side=4, log='y')
        # }
      }
      
      plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
      
      table <- correctionsTable(secondaryData)
    
    } else {
      status_msg <- paste('Corrected data missing for', data$reportMetadata$secondaryParameter)
    }
  } 

  return(list(plot=plot_object, table=table, status_msg=status_msg))
}

#' Add UV hydrograph objects to a plot object.
#' @param object A gsplot, plot object.
#' @param data Time series data to add to hydrograph.
#' @param info Meta-data about time series data in "data" parameter.
#' @param plotName Plot name string. Presently an element of
#'                 domain {primary,secondary}.
#' @param dataSides Sides data frame. See getUvStyle().
#' @param dataLimits Limits data frame. See getUvStyle().
#' @return A modified gsplot, plot object, with UV hydrograph objects included.
PlotUVHydrographObject <- function(object, data, info, plotName, dataSides, dataLimits) {
  correctionLabels <- parseLabelSpacing(data, info)
  styles <-
    getUvStyle(data, info, correctionLabels, plotName, dataSides, dataLimits)
  
  for (j in seq_len(length(styles))) {
    object <-
      do.call(names(styles[j]), append(list(object = object), styles[[j]]))
  }
  
  error_bars <- grep('error_bar', names(styles))
  for (err in error_bars) {
    object <- extendYaxisLimits(object, styles[[err]])
  }
  
  return(object)
}

#' Compute the y-axis real interval, based on a heuristic.
#' @param corr.value.sequence A sequence of corrected time series points.
#' @param uncorr.value.sequence A sequence of uncorrected time series points.
#' @return The y-axis real interval, as ordered-pair vector.
YAxisInterval <- function(corr.value.sequence, uncorr.value.sequence) {
  return(c(
      YOrigin(corr.value.sequence, uncorr.value.sequence),
      YEndpoint(corr.value.sequence, uncorr.value.sequence)
  ))
}

#' Compute the y-axis origin, based on a heuristic.
#' @param corr.value.sequence A sequence of corrected time series points.
#' @param uncorr.value.sequence A sequence of uncorrected time series points.
#' @return The y-axis real interval origin.
YOrigin <- function (corr.value.sequence, uncorr.value.sequence) {
  min.corr.value <- min(corr.value.sequence, na.rm = TRUE)
  min.uncorr.value <- min(uncorr.value.sequence, na.rm = TRUE)
  
  # if minimum corrected value is below or equal to minimum uncorrected, or if
  # the minimum uncorrected value is less than 70% of the minimum corrected
  # value
  if (min.corr.value <= min.uncorr.value || min.uncorr.value < 0.70 * min.corr.value)
    y.origin <- min.corr.value # use minimum corrected value as y-axis origin
  else
    y.origin <- min.uncorr.value # use minimum uncorrected value as y-axis origin

  return(y.origin)
}

#' Compute the y-axis endpoint, based on a heuristic.
#' @param corr.value.sequence A sequence of corrected time series points.
#' @param uncorr.value.sequence A sequence of uncorrected time series points.
#' @return The y-axis real interval endpoint.
YEndpoint <- function (corr.value.sequence, uncorr.value.sequence) {
  max.corr.value <- max(corr.value.sequence, na.rm = TRUE)
  max.uncorr.value <- max(uncorr.value.sequence, na.rm = TRUE)
  
  # if maximum corrected value is greater than or equal to the maxium
  # uncorrected value, or if the maximum uncorrected value is greater than 130%
  # of the maximum corrected value
  if (max.corr.value >= max.uncorr.value || max.uncorr.value > 1.30 * max.corr.value)
    y.endpoint <- max.corr.value   # use corrected time series' maximum as y-axis endpoint
  else
    y.endpoint <- max.uncorr.value # use uncorrected time series' maxium as y-axis endpoint
  
  return(y.endpoint)
}
