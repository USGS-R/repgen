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
    'stationName',
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
  minMaxCanLog <- TRUE

  if(!isEmptyOrBlank(minMaxIVs)){
    minMaxLabels <- minMaxIVs[grepl("label", names(minMaxIVs))]
    minMaxPoints <- minMaxIVs[!grepl("label", names(minMaxIVs))]
    minMaxCanLog <- minMaxIVs[['canLog']]
  }

  primarySeriesApprovals <- parsePrimarySeriesApprovals(reportObject, startDate, endDate)
  primarySeriesLegend <- fetchReportMetadataField(reportObject, 'primaryDescriptions')
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
  plot_object <- plotItem(plot_object, minMaxPoints[['min_iv']], getFiveYearPlotConfig, list(minMaxPoints[['min_iv']], 'min_iv'), isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['max_iv']], getFiveYearPlotConfig, list(minMaxPoints[['max_iv']], 'max_iv'), isDV=TRUE)

  # add vertical lines to delineate calendar year boundaries
  plot_object <- DelineateYearBoundaries(plot_object, date_seq_yr)
  
  plot_object <- ApplyApprovalBarStyles(plot_object, approvals)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Add approval explanation label to the top of the plot
  plot_object <- mtext(plot_object, text = "Displayed approval level(s) are from the source TS that statistics are derived from.", side=3, cex=0.6, line=0.1, adj=1, axes=FALSE)

  #Add Min/Max labels if we aren't plotting min and max
  line <- 0.1
  for(ml in na.omit(names(minMaxLabels))){
   formatted_label <- formatMinMaxLabel(minMaxLabels[[ml]], statTimeSeries[['units']])
    
    plot_object <- mtext(plot_object, formatted_label, side = 3, axes=FALSE, cex=0.6, line = line, adj = 0)
    
    line <- line + 0.75
  }

  return(plot_object)
}

getFiveYearPlotConfig <- function(plotItem, plotItemName, ...) {
  styles <- getFiveyearStyle()
  
  x <- plotItem[['time']]
  y <- plotItem[['value']]

  legend.name <- nullMask(plotItem[['legend.name']])

  args <- list(...)
  
  styles <- switch(plotItemName, 
      statTimeSeries = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$stat_lines)
          ),
      statTimeSeriesEst = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$est_stat_lines)
          ),
      max_iv = list(
          points = append(list(x=x, y=y, legend.name=legend.name), styles$max_iv_points)
          ),
      min_iv = list(
          points = append(list(x=x, y=y, legend.name=legend.name), styles$min_iv_points)
          ),
      gw_level = list(
          points = append(list(x=x,y=y), styles$gw_level_points)
      )
  )
  
  return(styles)
}
