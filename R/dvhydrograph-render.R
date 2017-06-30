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
  stat1TimeSeries <- parseTimeSeries(reportObject, 'firstStatDerived', 'firstStatDerivedLabel', timezone, isDV=TRUE)
  stat2TimeSeries <- parseTimeSeries(reportObject, 'secondStatDerived', 'secondStatDerivedLabel', timezone, isDV=TRUE)
  stat3TimeSeries <- parseTimeSeries(reportObject, 'thirdStatDerived', 'thirdStatDerivedLabel', timezone, isDV=TRUE)
  stat4TimeSeries <- parseTimeSeries(reportObject, 'fourthStatDerived', 'fourthStatDerivedLabel', timezone, isDV=TRUE)
  stat1TimeSeriesEst <- parseTimeSeries(reportObject, 'firstStatDerived', 'firstStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)
  stat2TimeSeriesEst <- parseTimeSeries(reportObject, 'secondStatDerived', 'secondStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)
  stat3TimeSeriesEst <- parseTimeSeries(reportObject, 'thirdStatDerived', 'thirdStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)
  stat4TimeSeriesEst <- parseTimeSeries(reportObject, 'fourthStatDerived', 'fourthStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)

  #Validate Basic Plot Data
  if(all(isEmptyOrBlank(c(stat1TimeSeries, stat1TimeSeriesEst, stat2TimeSeries, stat2TimeSeriesEst, stat3TimeSeries, stat3TimeSeriesEst, stat4TimeSeries, stat4TimeSeriesEst)))){
    return(NULL)
  }

  #Find the highest priority TS that has data
  priorityTS <- list(stat1TimeSeries, stat2TimeSeries, stat3TimeSeries, stat4TimeSeries, stat1TimeSeriesEst, stat2TimeSeriesEst, stat3TimeSeriesEst, stat4TimeSeriesEst)
  priorityTS <- priorityTS[unlist(lapply(priorityTS, function(ts){!isEmptyOrBlank(ts)}))][[1]]

  #Get Additional Plot Data
  comparisonTimeSeries <- parseTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesLabel', timezone, isDV=TRUE)
  comparisonTimeSeriesEst <- parseTimeSeries(reportObject, 'comparisonSeries', 'comparisonSeriesLabel', timezone, estimated=TRUE, isDV=TRUE)
  groundWaterLevels <- parseGroundWaterLevels(reportObject)
  waterQualityData <- parseWaterQualityMeasurements(reportObject)
  fieldVisitMeasurements <- parseFieldVisitMeasurements(reportObject)
  minMaxIVs <- parseMinMaxIVs(reportObject, timezone, priorityTS[['type']], invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag)
  minMaxLabels <- NULL
  minMaxEst <- list()
  minMaxPoints <- NULL
  minMaxCanLog <- TRUE

  if(!isEmptyOrBlank(minMaxIVs)){
    primarySeriesQualifiers <- parsePrimarySeriesQualifiers(reportObject, filterCode = 'E')
    minMaxEst[['max_iv']] <- any((minMaxIVs$max_iv$time >= primarySeriesQualifiers$startDate) & (minMaxIVs$max_iv$time <= primarySeriesQualifiers$endDate))
    minMaxEst[['min_iv']] <- any((minMaxIVs$min_iv$time >= primarySeriesQualifiers$startDate) & (minMaxIVs$min_iv$time <= primarySeriesQualifiers$endDate))
    minMaxLabels <- minMaxIVs[grepl("label", names(minMaxIVs))]
    minMaxPoints <- minMaxIVs[!grepl("label", names(minMaxIVs))]
    minMaxCanLog <- minMaxIVs[['canLog']]
  }

  primarySeriesApprovals <- parsePrimarySeriesApprovals(reportObject, startDate, endDate)
  primarySeriesLegend <- fetchReportMetadataField(reportObject, 'primarySeriesLabel')
  approvals <- readApprovalBar(primarySeriesApprovals, timezone, legend_nm=primarySeriesLegend, snapToDayBoundaries=TRUE)
  primaryTSCanLog <- isLogged(priorityTS[['points']], priorityTS[['isVolumetricFlow']], excludeZeroNegativeFlag)
  if (!isEmptyOrBlank(fieldVisitMeasurements)) {
    fieldVisitMeasurementsCanLog <- isLogged(fieldVisitMeasurements, TRUE, excludeZeroNegativeFlag) 
  }
  else {
    fieldVisitMeasurementsCanLog <- TRUE
  }
  
  logAxis <- primaryTSCanLog && fieldVisitMeasurementsCanLog && minMaxCanLog
  yLabel <- paste0(priorityTS[['type']], ", ", priorityTS[['units']])

  #Get Estimated / Non-Estimated Edges
  estimated1Edges <- getEstimatedEdges(stat1TimeSeries, stat1TimeSeriesEst, excludeZeroNegativeFlag)
  estimated2Edges <- getEstimatedEdges(stat2TimeSeries, stat2TimeSeriesEst, excludeZeroNegativeFlag)
  estimated3Edges <- getEstimatedEdges(stat3TimeSeries, stat3TimeSeriesEst, excludeZeroNegativeFlag)
  estimated4Edges <- getEstimatedEdges(stat4TimeSeries, stat4TimeSeriesEst, excludeZeroNegativeFlag)
  comparisonEdges <- getEstimatedEdges(comparisonTimeSeries, comparisonTimeSeriesEst, excludeZeroNegativeFlag)
  
  estimated1EdgesDf <- as.data.frame(estimated1Edges, stringsAsFactors = FALSE)
  estimated2EdgesDf <- as.data.frame(estimated2Edges, stringsAsFactors = FALSE)
  estimated3EdgesDf <- as.data.frame(estimated3Edges, stringsAsFactors = FALSE)
  estimated4EdgesDf <- as.data.frame(estimated4Edges, stringsAsFactors = FALSE)
  comparisonEdgesDf <- as.data.frame(comparisonEdges, stringsAsFactors = FALSE)
  
  #remove zeros from field measurements, if present:
  if (logAxis) {
    fieldVisitMeasurements <- removeZeroNegative(fieldVisitMeasurements)
  }

  #Subset the estimated/non estimated edges so we can style them differently
  estimated1EdgesEst <- as.list(estimated1EdgesDf[which(estimated1EdgesDf$newSet=='est'), ])
  estimated1EdgesStat <- as.list(estimated1EdgesDf[which(estimated1EdgesDf$newSet=='stat'), ])
  estimated2EdgesEst <- as.list(estimated2EdgesDf[which(estimated2EdgesDf$newSet=='est'), ])
  estimated2EdgesStat <- as.list(estimated2EdgesDf[which(estimated2EdgesDf$newSet=='stat'), ])
  estimated3EdgesEst <- as.list(estimated3EdgesDf[which(estimated3EdgesDf$newSet=='est'), ])
  estimated3EdgesStat <- as.list(estimated3EdgesDf[which(estimated3EdgesDf$newSet=='stat'), ])
  estimated4EdgesEst <- as.list(estimated4EdgesDf[which(estimated4EdgesDf$newSet=='est'), ])
  estimated4EdgesStat <- as.list(estimated4EdgesDf[which(estimated4EdgesDf$newSet=='stat'), ])
  comparisonEdgesEst <- as.list(comparisonEdgesDf[which(comparisonEdgesDf$newSet=='est'), ])
  comparisonEdgesStat <- as.list(comparisonEdgesDf[which(comparisonEdgesDf$newSet=='stat'), ])
  

  #Create Base Plot Object
  plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = invertedFlag, las=0) %>%
      view(xlim = c(startDate, endDate))
    
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, stat1TimeSeries, 'stat1TimeSeries', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat2TimeSeries, 'stat2TimeSeries', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat3TimeSeries, 'stat3TimeSeries', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat4TimeSeries, 'stat4TimeSeries', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat1TimeSeriesEst, 'stat1TimeSeriesEst', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat2TimeSeriesEst, 'stat2TimeSeriesEst', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat3TimeSeriesEst, 'stat3TimeSeriesEst', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, stat4TimeSeriesEst, 'stat4TimeSeriesEst', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, comparisonTimeSeries, 'comparisonTimeSeries', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, comparisonTimeSeriesEst, 'comparisonTimeSeriesEst', timezone, getDVHydrographPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  
  #Plot Other Items
  plot_object <- plotItem(plot_object, estimated1EdgesEst, getDVHydrographPlotConfig, list(estimated1EdgesEst, 'estimated1EdgesEst'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated1EdgesStat, getDVHydrographPlotConfig, list(estimated1EdgesStat, 'estimated1EdgesStat'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated2EdgesEst, getDVHydrographPlotConfig, list(estimated2EdgesEst, 'estimated2EdgesEst'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated2EdgesStat, getDVHydrographPlotConfig, list(estimated2EdgesStat, 'estimated2EdgesStat'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated3EdgesEst, getDVHydrographPlotConfig, list(estimated3EdgesEst, 'estimated3EdgesEst'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated3EdgesStat, getDVHydrographPlotConfig, list(estimated3EdgesStat, 'estimated3EdgesStat'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated4EdgesEst, getDVHydrographPlotConfig, list(estimated4EdgesEst, 'estimated4EdgesEst'), isDV=TRUE)
  plot_object <- plotItem(plot_object, estimated4EdgesStat, getDVHydrographPlotConfig, list(estimated4EdgesStat, 'estimated4EdgesStat'), isDV=TRUE)
  plot_object <- plotItem(plot_object, comparisonEdgesEst, getDVHydrographPlotConfig, list(comparisonEdgesEst, 'comparisonEdgesEst'), isDV=TRUE)
  plot_object <- plotItem(plot_object, comparisonEdgesStat, getDVHydrographPlotConfig, list(comparisonEdgesStat, 'comparisonEdgesStat'), isDV=TRUE)
  plot_object <- plotItem(plot_object, groundWaterLevels, getDVHydrographPlotConfig, list(groundWaterLevels, 'groundWaterLevels'), isDV=TRUE)
  plot_object <- plotItem(plot_object, waterQualityData, getDVHydrographPlotConfig, list(waterQualityData, 'waterQualityData'), isDV=TRUE)
  plot_object <- plotItem(plot_object, fieldVisitMeasurements, getDVHydrographPlotConfig, list(fieldVisitMeasurements, 'fieldVisitMeasurements'), isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['min_iv']], getDVHydrographPlotConfig, list(minMaxPoints[['min_iv']], 'min_iv', minMaxEst=minMaxEst[['min_iv']]), isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['max_iv']], getDVHydrographPlotConfig, list(minMaxPoints[['max_iv']], 'max_iv', minMaxEst=minMaxEst[['max_iv']]), isDV=TRUE)

  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- addToGsplot(plot_object, getApprovalBarConfig(approvals, ylim(plot_object, side = 2), logAxis))
  
  #Remove any duplicate legend items
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # custom gridlines below approval bar
  plot_object <- plot_object %>% 
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="weeks"), col="darkgray", lwd=1, where='first')
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Add approval explanation label to the top of the plot
  plot_object <- mtext(plot_object, text = "Displayed approval level(s) are from the source TS that statistics are derived from.", side=3, cex=0.85, line=0.33, adj=1, axes=FALSE)

  #Legend
  initialOffset <- getDvHydrographStyles()[['primary_legend_offset']]
  plot_object <- plotDVHydroLegend(plot_object, startDate, endDate, timezone, initialOffset)

  #Add Min/Max lbaels if we aren't plotting the min and max 
  formattedLabels <- lapply(minMaxLabels, function(l) {formatMinMaxLabel(l, priorityTS[['units']])})
  plot_object <- plotItem(plot_object, formattedLabels[['min_iv_label']], getDVHydrographPlotConfig, list(formattedLabels[['min_iv_label']], 'min_iv_label'), isDV=TRUE)
  plot_object <- plotItem(plot_object, formattedLabels[['max_iv_label']], getDVHydrographPlotConfig, list(formattedLabels[['max_iv_label']], 'max_iv_label', ylabel="", minMaxEst=FALSE, maxIvLabelOnTop=length(minMaxLabels) > 1), isDV=TRUE)

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
  series_number <- switch(series,
    'firstReferenceTimeSeries' = '1',
    'secondReferenceTimeSeries' = '2',
    'thirdReferenceTimeSeries' = '3'
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
    title(main = paste("\n\n", "Reference Time Series", series_number))
  
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, referenceSeries, series, timezone, getDVHydrographRefPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, referenceSeriesEst, paste0(series, "Est"), timezone, getDVHydrographRefPlotConfig, list(yLabel=yLabel), excludeZeroNegativeFlag, isDV=TRUE)

  #Plot Other Items
  plot_object <- plotItem(plot_object, estEdges, getDVHydrographRefPlotConfig, list(estEdges, paste0(series, "EstEdges")))
  
  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- addToGsplot(plot_object, getApprovalBarConfig(approvals, ylim(plot_object, side = 2), logAxis))
  
  #Remove any duplicate legend items
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # custom gridlines below approval bar
  plot_object <- plot_object %>% 
    abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Legend
  initialOffset <- getDvHydrographStyles()[['ref_legend_offset']]
  plot_object <- plotDVHydroLegend(plot_object, startDate, endDate, timezone, initialOffset)
  
  return(plot_object)
}

#' Get DV Hydrograph Plot Config
#'
#' @description Given an item to plot, fetch the associated
#' plot feature(s) and their styles. 
#' @param plotItem the item to fetch styles and plot features for
#' @param plotItemName the string to use for matching the configuration and styles
#' @param yLabel the string to use for the Y-Axis label for this object (if applicable) (Default: "")
#' @param maxIvLabelOnTop for the maximum IV point styling, set if label is aboe or below point (if applicable) (Default: FALSE)
#' @param minMaxEst Whether or not the min/max IV point is estimated
#' @param ... any additional parameters to pass into the function
getDVHydrographPlotConfig <- function(plotItem, plotItemName, yLabel="", minMaxEst=FALSE, maxIvLabelOnTop=FALSE, ...){
  styles <- getDvHydrographStyles()

  if(length(plotItem) > 1 || (!is.null(nrow(plotItem)) && nrow(plotItem) > 1)){
    x <- plotItem[['time']]
    y <- plotItem[['value']]
    legend.name <- nullMask(plotItem[['legend.name']])
  }
  
  args <- list(...)
  
  plotConfig <- switch(plotItemName, 
    stat1TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Stat 1:", legend.name)), styles$stat1_lines)
    ),
    stat2TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Stat 2:", legend.name)), styles$stat2_lines)
    ),
    stat3TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Stat 3:", legend.name)), styles$stat3_lines)
    ),
    stat4TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Stat 4:", legend.name)), styles$stat4_lines)
    ),
    comparisonTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=legend.name), styles$comp_lines)
    ),
    stat1TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Estimated Stat 1:", legend.name)), styles$stat1e_lines)
    ),
    stat2TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Estimated Stat 2:", legend.name)), styles$stat2e_lines)
    ),
    stat3TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Estimated Stat 3:", legend.name)), styles$stat3e_lines)
    ),
    stat4TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Estimated Stat 4:", legend.name)), styles$stat4e_lines)
    ),
    comparisonTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=yLabel, legend.name=paste("Estimated", legend.name)), styles$compe_lines)
    ),
    estimated1EdgesEst = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated1EdgesEst)
    ),
    estimated1EdgesStat = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated1EdgesStat)
    ),
    estimated2EdgesEst = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated2EdgesEst)
    ),
    estimated2EdgesStat = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated2EdgesStat)
    ),
    estimated3EdgesEst = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated3EdgesEst)
    ),
    estimated3EdgesStat = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated3EdgesStat)
    ),
    estimated4EdgesEst = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated4EdgesEst)
    ),
    estimated4EdgesStat = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$estimated4EdgesStat)
    ),
    comparisonEdgesEst = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$comparisonEdgesEst)
    ),
    comparisonEdgesStat = list(
      arrows = append(list(x0=plotItem[['time']], x1=plotItem[['time']], y0=plotItem[['y0']], y1=plotItem[['y1']]), styles$comparisonEdgesStat)
    ),
    fieldVisitMeasurements = list(
      points = append(list(x=x, y=y, legend.name="Measured Discharge"), styles$meas_q_points),
      callouts = append(list(x=x, y=y, labels = plotItem[['n']]), styles$meas_q_callouts)
    ),	
    groundWaterLevels = list(
      points = append(list(x=x, y=y, legend.name="Measured Water Level (GWSI)"), styles$gw_level_points)
    ),
    waterQualityData = list(
      points = append(list(x=x, y=y, legend.name="Measured Value (QWDATA)"), styles$wq_data_points)
    ),
    max_iv = list(
      points = append(list(x=x, y=y, legend.name=ifelse(minMaxEst, paste("(Estimated)", legend.name), legend.name), col=ifelse(minMaxEst, "red", "blue")), styles$max_iv_points)
    ),
    min_iv = list(
      points = append(list(x=x, y=y, legend.name=ifelse(minMaxEst, paste("(Estimated)", legend.name), legend.name), col=ifelse(minMaxEst, "red", "blue")), styles$min_iv_points)
    ),
    min_iv_label = list(
      mtext = append(list(ifelse(minMaxEst, paste("(Estimated)", plotItem), plotItem)), styles$bottom_iv_label)
    ),
    max_iv_label = list(
      mtext = append(list(ifelse(minMaxEst, paste("(Estimated)", plotItem), plotItem)), if(maxIvLabelOnTop) styles$top_iv_label else styles$bottom_iv_label)
    ),
    stop(paste("Plotting configuration could not be found within DVHydrograph for element:", names(plotItem)))
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
  
  legend.name <- nullMask(plotItem[['legend.name']])
  
  args <- list(...)

  plotConfig <- switch(plotItemName, 
    firstReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Reference 1:", legend.name)), styles$sref_lines)
    ),
    secondReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Reference 2:", legend.name)), styles$tref_lines)
    ),
    thirdReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Reference 3:", legend.name)), styles$qref_lines)
    ),
    firstReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Est. Reference 1:", legend.name)), styles$srefe_lines)
    ),
    secondReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Est. Reference 2:", legend.name)), styles$trefe_lines)
    ),
    thirdReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=paste("Est. Reference 3:", legend.name)), styles$qrefe_lines)
    ),
    firstReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1,
                           lty=ifelse(plotItem$newSet == "est", 1, 2), col=ifelse(plotItem$newSet == "est", "blue", "red1")),
                      styles$est_lines)
    ),
    secondReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1,
                           lty=ifelse(plotItem$newSet == "est", 1, 3), col=ifelse(plotItem$newSet == "est", "orange", "red2")),
                      styles$est_lines)
    ),
    thirdReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem$time, x1=plotItem$time, y0=plotItem$y0, y1=plotItem$y1, 
                           lty=ifelse(plotItem$newSet == "est", 1, 6), col=ifelse(plotItem$newSet == "est", "maroon", "red3")),
                      styles$est_lines)
    )
  )

  return(plotConfig)
}

#' Plot DV Hydro Legend
#'
#' @description Given the plot object and additional necessary parameters, calculates the
#' legend offset and then adds the legend to the plot.
#' @param plot_object The gsplot object to add the legend to
#' @param startDate The start date of the report
#' @param endDate The end date of the report
#' @param timezone The timezone of the report
#' @param initialOffset The initial amount to offset the legend by
#' @param modOffset [Default: 1] An optional amount to multiply the final calculated offset by
#' @return gsplot object with legend added
#' @importFrom lubridate years
plotDVHydroLegend <- function(plot_object, startDate, endDate, timezone, initialOffset, modOffset=1){
  legend_items <- plot_object$legend$legend.auto$legend
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  
  #Legend offset needs to be calculated based on the number of lines and columns to be consistent in position
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), length(legend_items))
  legend_offset <- ifelse(ncol==2, initialOffset+(0.025*leg_lines), initialOffset/2+(0.025*leg_lines))
  legend_offset <- legend_offset * modOffset

  #Add space to legend so that it doesn't overlap with dates.
  legend_offset <- legend_offset + 0.08

  #Add Legend to the plot
  plot_object <- legend(plot_object, location="below", cex=0.8, legend_offset=legend_offset, y.intersp=1.5, ncol=ncol)

  return(plot_object)
}