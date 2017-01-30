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
    startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
    endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
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

  logAxis <- isLogged(statTimeSeries[['points']], statTimeSeries[['isVolumetricFlow']], FALSE) && minMaxCanLog
  approvals <- readApprovalBar(statTimeSeries, timezone, legend_nm=statTimeSeries[['legend.name']], snapToDayBoundaries=TRUE)

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
  plot_object <- plotTimeSeries(plot_object, statTimeSeries, 'statTimeSeries', timezone, getFiveYearPlotConfig, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, statTimeSeriesEst, 'statTimeSeriesEst', timezone, getFiveYearPlotConfig, isDV=TRUE)

  #Plot Other Items
  plot_object <- plotItem(plot_object, groundWaterLevels, 'gw_level', getFiveYearPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['min_iv']], 'min_iv', getFiveYearPlotConfig, isDV=TRUE)
  plot_object <- plotItem(plot_object, minMaxPoints[['max_iv']], 'max_iv', getFiveYearPlotConfig, isDV=TRUE)

  # add vertical lines to delineate calendar year boundaries
  plot_object <- DelineateYearBoundaries(plot_object, date_seq_yr)
  
  plot_object <- ApplyApprovalBarStyles(plot_object, approvals)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # Add space to the top of the Y Axis
  plot_object <- RescaleYTop(plot_object)

  #Add Min/Max labels if we aren't plotting min and max
  line <- 0.33
  for(ml in na.omit(names(minMaxLabels))){
    #Extract Timezone
    tzf <- format(as.POSIXct(minMaxLabels[[ml]]$time), "%z")
    #Insert ":" before 2nd to last character
    tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf)
    formatted_label <- paste0(minMaxLabels[[ml]]$legend.name, statTimeSeries[['units']], 
                              format(as.POSIXct(minMaxLabels[[ml]]$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")")
    
    plot_object <- mtext(plot_object, formatted_label, side = 3, axes=FALSE, cex=0.85, line = line, adj = 0)
    
    line <- line + 1
  }

  return(plot_object)
}

getFiveYearPlotConfig <- function(plotItem, plotItemName, ...) {
  styles <- getFiveyearStyle()
  
  x <- plotItem$time
  y <- plotItem$value

  if('legend.name' %in% names(plotItem)){
    legend.name <- plotItem$legend.name
  } else {
    legend.name <- ""
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
