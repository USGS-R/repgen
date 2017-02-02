#' Create DV Hydrograph Plot
#'
#' @description Given a full report JSON object, extracts
#' relevant data, formats it, and then creates a DV Hydrogrpah
#' plot from it.
#' @param reportObject the full report JSON object
createDVHydrographPlot <- function(reportObject){
  #Rendering Options
  options(scipen=8)

  #Validate Report Metadata
  metaData <- fetchReportMetadata(reportObject)

  requiredMetadataFields <- c(
    'startDate',
    'endDate',
    'isInverted',
    'timezone',
    'stationId',
    'stationName',
    'title'
  )

  #Get Necessary Report Metadata
  if(validateFetchedData(metaData, "metadata", requiredMetadataFields)){
    timezone <- fetchReportMetadataField(reportObject, 'timezone')
    excludeZeroNegativeFlag <- parseReportMetadataField(reportObject, 'excludeZeroNegative', FALSE)
    excludeMinMaxFlag <- parseReportMetadataField(reportObject, 'excludeMinMax', FALSE)
    invertedFlag <- parseReportMetadataField(reportObject, 'isInverted', FALSE)
    startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
    endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
    plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))
  }

  #Get Basic Plot data
  stat1TimeSeries <- parseTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone, isDV=TRUE)
  stat1TimeSeriesEst <- parseTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone, estimated=TRUE, isDV=TRUE)
  stat2TimeSeries <- parseTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone, isDV=TRUE)
  stat2TimeSeriesEst <- parseTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone, estimated=TRUE, isDV=TRUE)
  stat3TimeSeries <- parseTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone, isDV=TRUE)
  stat3TimeSeriesEst <- parseTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone, estimated=TRUE, isDV=TRUE)
  comparisonTimeSeries <- parseTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesDescriptions', timezone, isDV=TRUE)
  comparisonTimeSeriesEst <- parseTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesDescriptions', timezone, estimated=TRUE, isDV=TRUE)

  #Validate Basic Plot Data
  if(all(isEmptyOrBlank(c(stat1TimeSeries, stat1TimeSeriesEst, stat2TimeSeries, stat2TimeSeriesEst, stat3TimeSeries, stat3TimeSeriesEst)))){
    return(NULL)
  }

  #Get Estimated / Non-Estimated Edges
  estimated1Edges <- getEstimatedEdges(stat1TimeSeries, stat1TimeSeriesEst, excludeZeroNegativeFlag)
  estimated2Edges <- getEstimatedEdges(stat2TimeSeries, stat2TimeSeriesEst, excludeZeroNegativeFlag)
  estimated3Edges <- getEstimatedEdges(stat3TimeSeries, stat3TimeSeriesEst, excludeZeroNegativeFlag)
  comparisonEdges <- getEstimatedEdges(comparisonTimeSeries, comparisonTimeSeriesEst, excludeZeroNegativeFlag)
  
  #Get Additional Plot Data
  groundWaterLevels <- parseGroundWaterLevels(reportObject)
  fieldVisitMeasurements <- parseFieldVisitMeasurements(reportObject)
  minMaxIVs <- parseMinMaxIVs(reportObject, timezone, stat1TimeSeries[['type']], invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag)
  minMaxLabels <- NULL
  minMaxPoints <- NULL
  minMaxCanLog <- TRUE

  if(!isEmptyOrBlank(minMaxIVs)){
    minMaxLabels <- minMaxIVs[grepl("label", names(minMaxIVs))]
    minMaxPoints <- minMaxIVs[!grepl("label", names(minMaxIVs))]
    minMaxCanLog <- minMaxIVs[['canLog']]
  }

  #Note: After work in AQC-961 this should get approvals from the primary TS, not the primary existant stat time series
  approvals <- readApprovalBar(stat1TimeSeries, timezone, legend_nm=stat1TimeSeries[['legend.name']], snapToDayBoundaries=TRUE)
  logAxis <- isLogged(stat1TimeSeries[['points']], stat1TimeSeries[['isVolumetricFlow']], excludeZeroNegativeFlag) && minMaxCanLog
  yLabel <- paste0(stat1TimeSeries[['type']], ", ", stat1TimeSeries[['units']])

  #Create Base Plot Object
  plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = invertedFlag, las=0) %>%
      view(xlim = c(startDate, endDate))
    
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, stat1TimeSeries, 'stat1TimeSeries', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat2TimeSeries, 'stat2TimeSeries', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat3TimeSeries, 'stat3TimeSeries', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat1TimeSeriesEst, 'stat1TimeSeriesEst', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat2TimeSeriesEst, 'stat2TimeSeriesEst', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat3TimeSeriesEst, 'stat3TimeSeriesEst', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, comparisonTimeSeries, 'comparisonTimeSeries', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, comparisonTimeSeriesEst, 'comparisonTimeSeriesEst', timezone, getDVHydrographPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)

  #Plot Other Items
  plot_object <- plotItem(plot_object, estimated1Edges, 'estimated1Edges', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated2Edges, 'estimated2Edges', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated3Edges, 'estimated3Edges', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, comparisonEdges, 'comparisonEdges', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, groundWaterLevels, 'groundWaterLevels', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, fieldVisitMeasurements, 'fieldVisitMeasurements', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['min_iv']], 'min_iv', getDVHydrographPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['max_iv']], 'max_iv', getDVHydrographPlotConfig, isDV=TRUE)

  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- ApplyApprovalBarStyles(plot_object, approvals)
  
  #Remove any duplicate legend items
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # custom gridlines below approval bar
  plot_object <- plot_object %>% 
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="weeks"), col="darkgray", lwd=1, where='first')
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Legend
  #If the time period is greater than 1 year additional x-axis labels are added so we must move the legend down further
  #Legend offset also needs to be calculated based on the number of lines and columns to be consistent in position
  legend_items <- plot_object$legend$legend.auto$legend
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), length(legend_items))
  legend_offset <- ifelse(ncol==2, 0.09+(0.025*leg_lines), (0.025*leg_lines))
  legend_offset <- ifelse(as.period(interval(startDate, endDate, tzone = attr(startDate, timezone))) < years(1), legend_offset+0.03, legend_offset+0.08)
  plot_object <- legend(plot_object, location="below", cex=0.8, legend_offset=legend_offset, y.intersp=1.5, ncol=ncol)

  #Add Min/Max labels if we aren't plotting min and max
  line <- 0.33
  for(ml in na.omit(names(minMaxLabels))){
    #Extract Timezone
    tzf <- format(as.POSIXct(minMaxLabels[[ml]][['time']]), "%z")
    #Insert ":" before 2nd to last character
    tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf)
    formatted_label <- paste0(minMaxLabels[[ml]][['legend.name']], stat1TimeSeries[['units']], 
                              format(as.POSIXct(minMaxLabels[[ml]][['time']]), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")")
    
    plot_object <- mtext(plot_object, formatted_label, side = 3, axes=FALSE, cex=0.85, line = line, adj = 0)
    
    line <- line + 1
  }
  
  return(plot_object)
}

#' Create DV Hdyrograph Reference Plot
#'
#' @description Given the full report object, a series field name, and
#' a series description field name, creates a DV Hydrograph Reference
#' plot using the specific parameters.
#' @param reportObject the full report JSON object
#' @param series the series field name to extract from the JSON
#' @param descriptions the description field name to extract from the JSON
createDVHydrographRefPlot <- function(reportObject, series, descriptions) {
  #Rendering Options
  options(scipen=8)

  #Get Necessary Report Metadata
  ref_name_capital <- switch(series,
    'secondaryReferenceTimeSeries' = 'Secondary',
    'tertiaryReferenceTimeSeries' = 'Tertiary',
    'quaternaryReferenceTimeSeries' = 'Quaternary'
  )
  seriesEst <- paste0(series, 'Est', setp="")
  seriesEstEdges <- paste0(series, 'EstEdges', setp="")
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMax')
  invertedFlag <- fetchReportMetadataField(reportObject, 'isInverted')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
  endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
  plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))

  #Get Basic Plot Data
  referenceSeries <- parseTimeSeries(reportObject, series, descriptions, timezone, isDV=TRUE)
  referenceSeriesEst <- parseTimeSeries(reportObject, series, descriptions, timezone, estimated=TRUE, isDV=TRUE)

  #Validate Basic Plot Data
  if(all(isEmptyOrBlank(c(referenceSeries, referenceSeriesEst)))){
    return(NULL)
  }

  #Get Estimated / Non-Estimated Edges
  estEdges <- getEstimatedEdges(referenceSeries, referenceSeriesEst, excludeZeroNegativeFlag)

  #Get Additional Plot Data
  logAxis <- isLogged(referenceSeries[['points']], referenceSeries[['isVolumetricFlow']], excludeZeroNegativeFlag)
  yLabel <- paste0(referenceSeries[['type']], ", ", referenceSeries[['units']])
  approvals <- readApprovalBar(referenceSeries, timezone, legend_nm=referenceSeries[['legend.name']], snapToDayBoundaries=TRUE)

  #Do Plotting
  plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
    grid(nx = NA, ny = NULL, lty = 3, col = "gray") %>%
    axis(2, reverse = invertedFlag, las=0) %>%
    view(xlim = c(startDate, endDate)) %>%
    title(main = paste(ref_name_capital, "Reference Time Series"))
  
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, referenceSeries, series, timezone, getDVHydrographRefPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, referenceSeriesEst, paste0(series, "Est"), timezone, getDVHydrographRefPlotConfig, yLabel=yLabel, excludeZeroNegativeFlag, isDV=TRUE)

  #Plot Other Items
  plot_object <- plotItem(plot_object, estEdges, paste0(series, "EstEdges"), getDVHydrographRefPlotConfig)
  
  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- ApplyApprovalBarStyles(plot_object, approvals)
  
  #Remove any duplicate legend items
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # custom gridlines below approval bar
  plot_object <- plot_object %>% 
    abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Legend
  legend_items <- plot_object$legend$legend.auto$legend
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), length(legend_items))
  legend_offset <- ifelse(ncol==2, 0.09+(0.025*leg_lines), (0.025*leg_lines))
  legend_offset <- ifelse(as.period(interval(startDate, endDate, tzone = attr(startDate, timezone))) < years(1), legend_offset+0.03, legend_offset+0.08)
  plot_object <- legend(plot_object, location="below", cex=0.8, legend_offset=legend_offset, y.intersp=1.5, ncol=ncol)
  
  return(plot_object)
}

#' Get DV Hydrograph Plot Config
#'
#' @description Given an item to plot, fetch the associated
#' plot feature(s) and their styles. 
#' @param plotItem the item to fetch styles and plot features for
#' @param plotItemName the string to use for matching the configuration and styles
#' @param yLabel the string to use for the Y-Axis label for this object (if applicable) (Default: "")
#' @param ... any additional parameters to pass into the function
getDVHydrographPlotConfig <- function(plotItem, plotItemName, yLabel="", ...){
  styles <- getDvHydrographStyles()

  x <- plotItem[['time']]
  y <- plotItem[['value']]

  if('legend.name' %in% names(plotItem)){
    legend.name <- plotItem$legend.name
  } else {
    legend.name <- ""
  }

  args <- list(...)
  
  plotConfig <- switch(plotItemName, 
    stat1TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat1_lines)
    ),
    stat2TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat2_lines)
    ),
    stat3TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat3_lines)
    ),
    comparisonTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$comp_lines)
    ),
    stat1TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat1e_lines)
    ),
    stat2TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat2e_lines)
    ),
    stat3TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$stat3e_lines)
    ),
    comparisonTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$compe_lines)
    ),
    estimated1Edges = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']],
                           lty=ifelse(plotItem[['newSet']] == "est", 1, 2), col=ifelse(plotItem[['newSet']] == "est", "blue", "red1")),
                      styles$est_lines)
    ),
    estimated2Edges = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']],
                           lty=ifelse(plotItem[['newSet']] == "est", 1, 3), col=ifelse(plotItem[['newSet']] == "est", "maroon", "red2")),
                      styles$est_lines)
    ),
    estimated3Edges = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']], 
                           lty=ifelse(plotItem[['newSet']] == "est", 1, 6), col=ifelse(plotItem[['newSet']] == "est", "orange", "red3")),
                      styles$est_lines)
    ),
    comparisonEdges = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']], 
                           lty=ifelse(plotItem[['newSet']] == "est", 1, 6), col=ifelse(plotItem[['newSet']] == "est", "green", "red4")),
                      styles$est_lines)
    ),
    fieldVisitMeasurements = list(
      points = append(list(x=x, y=y, legend.name="Measured Discharge"), styles$meas_q_points),
      callouts = append(list(x=x, y=y, labels = plotItem[['n']]), styles$meas_q_callouts)
    ),	
    groundWaterLevels = list(
      points = append(list(x=x, y=y, legend.name="Measured Water Level (GWSI)"), styles$gw_level_points)
    ),
    max_iv = list(
      points = append(list(x=x, y=y, legend.name=legend.name), styles$max_iv_points)
    ),
    min_iv = list(
      points = append(list(x=x, y=y, legend.name=legend.name), styles$min_iv_points)
    ),
    stop(paste(" Plotting configuration could not be found within DVHydrograph for element:", names(plotItem)))
  )
  
  return(plotConfig)
}

#' Get DV Hydrograph Reference Plot Config
#'
#' @description Given an item to plot, fetch the associated
#' plot feature(s) and their styles. 
#' @param plotItem the item to fetch styles and plot features for
#' @param plotItemName the string to use for matching the configuration and styles
#' @param yLabel the string to use for the Y-Axis label for this object (if applicable) (Default: "")
#' @param ... any additional parameters to pass into the function
getDVHydrographRefPlotConfig <- function(plotItem, plotItemName, yLabel, ...){
  styles <- getDvHydrographStyles()

  x <- plotItem[['time']]
  y <- plotItem[['value']]
  
  if('legend.name' %in% names(plotItem)){
    legend.name <- plotItem$legend.name
  } else {
    legend.name <- ""
  }
  
  args <- list(...)

  plotConfig <- switch(plotItemName, 
    secondaryReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$sref_lines)
    ),
    tertiaryReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$tref_lines)
    ),
    quaternaryReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$qref_lines)
    ),
    secondarReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$srefe_lines)
    ),
    tertiaryReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$trefe_lines)
    ),
    quaternaryReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$qrefe_lines)
    ),
    secondarReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1,
                           lty=ifelse(plotItem$newSet == "est", 1, 2), col=ifelse(plotItem$newSet == "est", "blue", "red1")),
                      styles$est_lines)
    ),
    tertiaryReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1,
                           lty=ifelse(plotItem$newSet == "est", 1, 3), col=ifelse(plotItem$newSet == "est", "orange", "red2")),
                      styles$est_lines)
    ),
    quaternaryReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1, 
                           lty=ifelse(plotItem$newSet == "est", 1, 6), col=ifelse(plotItem$newSet == "est", "maroon", "red3")),
                      styles$est_lines)
    )
  )

  return(plotConfig)
}