createfiveyeargwsumPlot <- function(reportObject){
  #Rendering Options
  options(scipen=8)
  
  #Validation Options
  requiredTimeSeriesFields <- c(
    "points",
    "approvals",
    "qualifiers",
    "startTime",
    "endTime",
    "isVolumetricFlow",
    "units",
    "grades",
    "type",
    "gaps",
    "gapTolerances",
    "name"
  )
  requiredMetadataFields <- c(
    'startDate',
    'endDate',
    'isInverted',
    'timezone',
    'stationId',
    'title'
  )

  #Validate Report Metadata
  metaData <- fetchReportMetadata(reportObject)

  #Get Necessary Report Metadata
  if(validateFetchedData(metaData, "metadata", requiredMetadataFields)){
    timezone <- fetchReportMetadataField(reportObject, 'timezone')
    excludeMinMaxFlag <- parseReportMetadataField(reportObject, 'excludeMinMax', FALSE)
    invertedFlag <- parseReportMetadataField(reportObject, 'isInverted', FALSE)
    startDate <- toStartOfMonth(flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone))
    endDate <- toEndOfMonth(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
    date_seq_mo <- seq(from=startDate, to=endDate, by="month")
    first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
    date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
    month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
    month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
    month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  }

  #Get Priority Stat Time Series
  priorityStat <- getPriorityStat(reportObject)

  #Get Basic Plot data
  statTimeSeries <- parseTimeSeries(reportObject, priorityStat[['data_nm']], priorityStat[['descr_nm']], timezone, isDV=TRUE)
  statTimeSeriesEst <- parseTimeSeries(reportObject, priorityStat[['data_nm']], priorityStat[['descr_nm']], timezone, estimated=TRUE, isDV=TRUE)

  #Validate Basic Plot Data
  if(is.null(c(statTimeSeries, statTimeSeriesEst))){
    return(NULL)
  }

  #Get Additional Plot Data
  groundWaterLevels <- parseGroundWaterLevels(reportObject)
  minMaxIVs <- parseMinMaxIVs(reportObject, timezone, statTimeSeries[['type']], invertedFlag, excludeMinMaxFlag, FALSE)
  minMaxLabels <- NULL
  minMaxEst <- list()
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
  logAxis <- isLogged(statTimeSeries[['points']], statTimeSeries[['isVolumetricFlow']], FALSE) && minMaxCanLog

  #Create the Base Plot Object
  plot_object <- gsplot(yaxs = 'i', ylog=logAxis, xaxt = "n", mar = c(8, 4, 4, 2) + 0.1) %>%
      axis(side = 1, at = date_seq_mo, labels = FALSE) %>%
      view(xlim = c(startDate, endDate)) %>%
      legend(location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = invertedFlag) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(ylab = "Water Level, Below LSD (feet)")

  plot_object <- 
    XAxisLabels(plot_object, month_label, month_label_location, date_seq_yr + months(6))

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, statTimeSeries, 'statTimeSeries', timezone, getFiveYearPlotConfig, list(), isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, statTimeSeriesEst, 'statTimeSeriesEst', timezone, getFiveYearPlotConfig, list(), isDV=TRUE)

  #Plot Other Items
  plot_object <- plotItem(plot_object, groundWaterLevels, getFiveYearPlotConfig, list(groundWaterLevels, 'gw_level'), isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['min_iv']], getFiveYearPlotConfig, list(minMaxPoints[['min_iv']], 'min_iv', minMaxEst=minMaxEst[['min_iv']]), isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['max_iv']], getFiveYearPlotConfig, list(minMaxPoints[['max_iv']], 'max_iv', minMaxEst=minMaxEst[['max_iv']]), isDV=TRUE)

  # add vertical lines to delineate calendar year boundaries
  plot_object <- DelineateYearBoundaries(plot_object, date_seq_yr)
  
  # add approval bars
  plot_object <- addToGsplot(plot_object, getApprovalBarConfig(approvals, ylim(plot_object, side = 2), logAxis))
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Add approval explanation label to the top of the plot
  plot_object <- mtext(plot_object, text = "Displayed approval level(s) are from the source TS that statistics are derived from.", side=3, cex=0.6, line=0.1, adj=1, axes=FALSE)

  #Add Min/Max labels if we aren't plotting min and max
  formattedLabels <- lapply(minMaxLabels, function(l) {formatMinMaxLabel(l, statTimeSeries[['units']])})
  plot_object <- plotItem(plot_object, formattedLabels[['min_iv_label']], getFiveYearPlotConfig, list(formattedLabels[['min_iv_label']], 'min_iv_label'), isDV=TRUE)
  plot_object <- plotItem(plot_object, formattedLabels[['max_iv_label']], getFiveYearPlotConfig, list(formattedLabels[['max_iv_label']], 'max_iv_label', ylabel="", lineOffset=length(minMaxLabels)), isDV=TRUE)

  return(plot_object)
}

getFiveYearPlotConfig <- function(plotItem, plotItemName, minMaxEst=FALSE, lineOffset=1, ...) {
  styles <- getFiveyearStyle()
  
  if(length(plotItem) > 1 || (!is.null(nrow(plotItem)) && nrow(plotItem) > 1)){
    x <- plotItem[['time']]
    y <- plotItem[['value']]
    legend.name <- nullMask(plotItem[['legend.name']])
  }

  args <- list(...)
  
  styles <- switch(plotItemName, 
      statTimeSeries = list(
        lines = append(list(x=x, y=y, legend.name=legend.name), styles$stat_lines)
      ),
      statTimeSeriesEst = list(
        lines = append(list(x=x, y=y, legend.name=legend.name), styles$est_stat_lines)
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
          mtext = append(list(ifelse(minMaxEst, paste("(Estimated)", plotItem), plotItem)), if(lineOffset > 1) styles$top_iv_label else styles$bottom_iv_label)
      ),
      gw_level = list(
          points = append(list(x=x,y=y), styles$gw_level_points)
      )
  )
  
  return(styles)
}
