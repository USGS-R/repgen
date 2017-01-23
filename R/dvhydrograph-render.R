dvhydrographPlot <- function(data) {
  plot_object <- createDvhydrographPlot(data)
  return(plot_object)
}

createDvhydrographPlot <- function(reportObject){
  #Rendering Options
  options(scipen=8)

  #Get Necessary Report Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMax')
  invertedFlag <- fetchReportMetadataField(reportObject, 'isInverted')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
  endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
  plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))

  #Get Basic Plot data
  dvData <- list()

  dvData[['stat1TimeSeries']] <- parseDVTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone, excludeZeroNegativeFlag)
  dvData[['stat1TimeSeriesEst']] <- parseDVTimeSeries(reportObject, 'firstDownChain', 'downChainDescriptions1', timezone, excludeZeroNegativeFlag, estimated=TRUE)
  dvData[['stat2TimeSeries']] <- parseDVTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone, excludeZeroNegativeFlag)
  dvData[['stat2TimeSeriesEst']] <- parseDVTimeSeries(reportObject, 'secondDownChain', 'downChainDescriptions2', timezone, excludeZeroNegativeFlag, estimated=TRUE)
  dvData[['stat3TimeSeries']] <- parseDVTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone, excludeZeroNegativeFlag)
  dvData[['stat3TimeSeriesEst']] <- parseDVTimeSeries(reportObject, 'thirdDownChain', 'downChainDescriptions3', timezone, excludeZeroNegativeFlag, estimated=TRUE)
  dvData[['comparisonTimeSeries']] <- parseDVTimeSeries(reportObject, 'comparisonSeries', 'comparisonDescriptions', timezone, excludeZeroNegativeFlag)
  dvData[['comparisonTimeSeriesEst']] <- parseDVTimeSeries(reportObject, 'comparisonSeries', 'comparisonDescriptions', timezone, excludeZeroNegativeFlag, estimated=TRUE)
  primaryPresentTS <- names(dvData)[1]

  #Validate Basic Plot Data
  if(is.null(primaryPresentTS)){
    return(NULL)
  }

  #Get Estimated / Non-Estimated Edges
  if(!is.null(dvData[['stat1TimeSeries']]) && !is.null(dvData[['stat1TimeSeriesEst']])){
    dvData[['estimated1Edges']] <- getEstimatedEdges(dvData[['stat1TimeSeries']][['points']], dvData[['stat1TimeSeriesEst']][['points']])
  }

  if(!is.null(dvData[['stat2TimeSeries']]) && !is.null(dvData[['stat2TimeSeriesEst']])){
    dvData[['estimated2Edges']] <- getEstimatedEdges(dvData[['stat2TimeSeries']][['points']], dvData[['stat2TimeSeriesEst']][['points']])
  }

  if(!is.null(dvData[['stat3TimeSeries']]) && !is.null(dvData[['stat3TimeSeriesEst']])){
    dvData[['estimated3Edges']] <- getEstimatedEdges(dvData[['stat3TimeSeries']][['points']], dvData[['stat3TimeSeries']][['points']])
  }

  if(!is.null(dvData[['comparisonTimeSeries']]) && !is.null(dvData[['comparisonTimeSeriesEst']])){
    dvData[['comparisonEdges']] <- getEstimatedEdges(dvData[['comparisonTimeSeries']][['points']], dvData[['comparisonTimeSeriesEst']][['points']])
  }

  #Get Additional Plot Data
  dvData[['gw_level']] <- parseDVGroundWaterLevels(reportObject)
  dvData[['meas_Q']] <- parseDVFieldVisitMeasurements(reportObject)
  dvData <- append(dvData, parseDVMinMaxIVs(reportObject, dvData[[primaryPresentTS]][['type']], timezone, invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag))
  dvData <- append(dvData, parseDVApprovals(dvData[[primaryPresentTS]], timezone))
  logAxis <- isLogged(dvData[[primaryPresentTS]][['points']], dvData[[primaryPresentTS]][['isVolumetricFlow']], excludeZeroNegativeFlag)
  yLabel <- paste0(dvData[[primaryPresentTS]][['type']], ", ", dvData[[primaryPresentTS]][['units']])

  #Do plotting
  plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = invertedFlag, las=0) %>%
      view(xlim = c(startDate, endDate))
    
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)

  # for non-approval-bar objects
  for (i in grep("^appr_", names(dvData), invert = TRUE)) {
    if(grepl("TimeSeries", names(dvData[i]))){
      dvData[i][[names(dvData[i])]]<- formatTimeSeriesForPlotting(dvData[i][[names(dvData[i])]], excludeZeroNegativeFlag)
    }

    dvConfig <- getDVHydrographPlotConfig(dvData[i], yLabel=yLabel)
    for (j in names(dvConfig)) {
      dvConfig[[j]] <- extendStep(dvConfig[[j]])
      plot_object <- do.call(names(dvConfig[j]), append(list(object = plot_object), dvConfig[[j]]))
    }
  }

  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- ApplyApprovalBarStyles(plot_object, dvData)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # custom gridlines below approval bar
  plot_object <- plot_object %>% 
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=toStartOfDay(startDate), to=toStartOfDay(endDate), by="weeks"), col="darkgray", lwd=1, where='first')
  
  # patch up top extent of y-axis
  plot_object <- RescaleYTop(plot_object)

  #Legend
  legend_items <- plot_object$legend$legend.auto$legend
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
  plot_object <- legend(plot_object, location="below", cex=0.8, legend_offset=0.2, y.intersp=1.5, ncol=ncol)

  #Add Min/Max labels if we aren't plotting min and max
  minmax_labels <- append(dvData['max_iv_label'], dvData['min_iv_label'])
  line <- 0.33
  for(ml in na.omit(names(minmax_labels))){
    #Extract Timezone
    tzf <- format(as.POSIXct(dvData[[ml]]$time), "%z")
    #Insert ":" before 2nd to last character
    tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf)
    formatted_label <- paste0(dvData[[ml]]$legend.name, data$firstDownChain$units, 
                              format(as.POSIXct(dvData[[ml]]$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")")
    
    plot_object <- mtext(plot_object, formatted_label, side = 3, axes=FALSE, cex=0.85, line = line, adj = 0)
    
    line <- line + 1
  }
  
  return(plot_object)
}

createRefPlot <- function(reportObject, series, descriptions) {
  #Rendering Options
  options(scipen=8)

  #Get Necessary Report Metadata
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, 'excludeZeroNegative')
  excludeMinMaxFlag <- fetchReportMetadataField(reportObject, 'excludeMinMax')
  invertedFlag <- fetchReportMetadataField(reportObject, 'isInverted')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
  endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
  plotDates <- toStartOfDay(seq(startDate, endDate, by = 7 * 24 * 60 * 60))

  #Get Basic Plot Data
  refData <- list()

  refData[[series]] <- parseDVTimeSeries(reportObject, series, descriptions, timezone, excludeZeroNegativeFlag)
  refData[[paste(series, 'Est', setp='')]] <- parseDVTimeSeries(reportObject, series, descriptions, timezone, excludeZeroNegativeFlag, estimated=TRUE)

  #Validate Basic Plot Data
  if(is.null(refData[[series]]) || is.null(refData[[paste(series, 'Est', setp='')]])){
    return(NULL)
  }

  #Get Estimated / Non-Estimated Edges
  if(!is.null(dvData[[series]]) && !is.null(dvData[[paste(series, 'Est', setp='')]])){
    dvData[[paste(series, 'EstEdges', setp='')]] <- getEstimatedEdges(dvData[[series]], dvData[[paste(series, 'Est', setp='')]])
  }

  #Get Additional Plot Data
  logAxis <- isLogged(dvData[[series]][['points']], dvData[[series]][['isVolumetricFlow']], excludeZeroNegativeFlag)
  yLabel <- paste0(dvData[[series]][['type']], ", ", dvData[[series]][['units']])

  #Do Plotting
  plot_object <- gsplot(ylog = logAxis, yaxs = 'i') %>%
    grid(nx = NA, ny = NULL, lty = 3, col = "gray") %>%
    axis(2, reverse = invertedFlag, las=0) %>%
    view(xlim = c(startDate, endDate)) %>%
    title(main = paste(ref_name_capital, "Reference Time Series")) %>%
    legend(location = "below", cex = 0.8, y.intersp = 1.5)
  
  plot_object <-
    XAxisLabelStyle(plot_object, startDate, endDate, timezone, plotDates)
  
  # for non-approval-bar objects
  for (i in grep("^appr_", names(refData), invert = TRUE)) {
    if(grepl("TimeSeries", names(refData[i]))){
      refData[i][[names(refData[i])]] <- formatTimeSeriesForPlotting(refData[i][[names(refData[i])]], excludeZeroNegativeFlag)
    }

    refConfig <- getDVHydrographRefPlotConfig(refData[i], yLabel=yLabel)
    for (j in seq_len(length(refConfig))) {
      plot_object <- do.call(names(refConfig[j]), append(list(object = plot_object), refConfig[[j]]))
    }
  }
  
  plot_object <- ApplyApprovalBarStyles(plot_object, refData)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  plot_object <- plot_object %>% 
    abline(v=seq(from=startDate, to=endDate, by="days"), lty=3, col="gray", where='first') %>%
    abline(v=seq(from=startDate, to=endDate, by="weeks"), col="darkgray", lwd=1, where='first')
  
  # patch up top extent of y-axis
  plot_object <- RescaleYTop(plot_object)
  
  return(plot_object)
}

getDVHydrographPlotConfig <- function(plotItem, ...){
  styles <- getDvHydrographStyles()

  x <- plotItem[[1]]$time
  y <- plotItem[[1]]$value
  legend.name <- plotItem[[1]]$legend.name
  args <- list(...)
  
  styles <- switch(names(plotItem), 
    stat1TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$stat1_lines)
    ),
    stat2TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), style$stat2_lines)
    ),
    stat3TimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), style$stat3_lines)
    ),
    comparisonTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$comp_lines)
    ),
    stat1TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$stat1e_lines)
    ),
    stat2TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$stat2e_lines)
    ),
    stat3TimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$stat3e_lines)
    ),
    comparisonTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), styles$compe_lines)
    ),
    estimated1Edges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1,
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 2), col=ifelse(plotItem[[1]]$newSet == "est", "blue", "red1")),
                      styles$est_lines)
    ),
    estimated2Edges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1,
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 3), col=ifelse(plotItem[[1]]$newSet == "est", "maroon", "red2")),
                      styles$est_lines)
    ),
    estimated3Edges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1, 
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 6), col=ifelse(plotItem[[1]]$newSet == "est", "orange", "red3")),
                      styles$est_lines)
    ),
    comparisonEdges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1, 
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 6), col=ifelse(plotItem[[1]]$newSet == "est", "green", "red4")),
                      styles$est_lines)
    ),
    meas_Q = list(
      points = append(list(x=x, y=y, legend.name="Measured Discharge"), styles$meas_q_points),
      callouts = append(list(x=x, y=y, labels = plotItem$meas_Q$n), styles$meas_q_callouts)
    ),	
    gw_level = list(
      points = append(list(x=x, y=y, legend.name="Measured Water Level (GWSI)"), styles$gw_level_points)
    ),
    max_iv = list(
      points = append(list(x=x, y=y, legend.name=legend.name), styles$max_iv_points)
    ),
    min_iv = list(
      points = append(list(x=x, y=y, legend.name=legend.name), styles$min_iv_points)
    )
  )
  
  return(styles)
}

getDVHydrographRefPlotConfig <- function(plotItem, info = NULL, ...){
  styles <- getDvHydrographStyles()

  x <- plotItem[[1]]$time
  y <- plotItem[[1]]$value
  legend.name <- plotItem[[1]]$legend.name
  args <- list(...)

  styles <- switch(names(plotItem), 
    secondarReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), sref_lines)
    ),
    tertiaryReferenceTimeseries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), tref_lines)
    ),
    quaternaryReferenceTimeSeries = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), qref_lines)
    ),
    secondarReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), srefe_lines)
    ),
    tertiaryReferenceTimeseriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), trefe_lines)
    ),
    quaternaryReferenceTimeSeriesEst = list(
      lines = append(list(x=x, y=y, ylab=args$yLabel, legend.name=legend.name), qrefe_lines)
    ),
    secondarReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1,
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 2), col=ifelse(plotItem[[1]]$newSet == "est", "blue", "red1")),
                      styles$est_lines)
    ),
    tertiaryReferenceTimeseriesEstEdges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1,
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 3), col=ifelse(plotItem[[1]]$newSet == "est", "maroon", "red2")),
                      styles$est_lines)
    ),
    quaternaryReferenceTimeSeriesEstEdges = list(
      arrows = append(list(x0=plotItem[[1]]$time, x1=plotItem[[1]]$time, y0=plotItem[[1]]$y0, y1=plotItem[[1]]$y1, 
                           lty=ifelse(plotItem[[1]]$newSet == "est", 1, 6), col=ifelse(plotItem[[1]]$newSet == "est", "orange", "red3")),
                      styles$est_lines)
    ),
  )
}

#' @importFrom lubridate interval
#' @importFrom lubridate as.period
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate floor_date
#' @importFrom lubridate %m+%
#' @importFrom lubridate %m-%
#' @importFrom lubridate day
#' @importFrom lubridate days
#' @importFrom stats median
XAxisLabelStyle <- function(object, start, end, timezone, plotDates) {
  i <- interval(start, end, tzone = attr(start, timezone))
  
  # if chart interval is less than 1 year
  if (as.period(i) < years(1)) {
    # x-axis
    object <- axis(
      object,
      1, at = plotDates,
      labels = format(plotDates, "%b\n%d"),
      padj = 0.5
    )
  }
  else {
    # if start date day is not the 1st of the month
    if (day(start) != 1) {
      # begin month letter labeling at next adjacent month
      from <- floor_date(start %m+% months(1), "month")
    }
    else {
      from <- start
    }
    
    # if end date day is not the last day of the month
    if (day(end) != days_in_month(end)) {
      # end month letter labeling at preceding adjacent month
      to <- ceiling_date(end %m-% months(1), "month")
    }
    else {
      to <- end
    }
    
    months <-
      seq(
        from = ceiling_date(start, "month"),
        to = floor_date(end, "month"),
        by = "month"
      )
    
    # [start:end] is interval here, because [from:to] above could be abbreviated
    # to omit month-letter-labeling of partial months at beginning/end of x-axis
    years <- seq(from = floor_date(start, "year"), to = floor_date(end, "year"), by = "year")

    object <- axis(object, side = 1, at = months, labels = FALSE) # x-axis
    
    month_label_split <- strsplit(as.character(month(months, label = TRUE)), "")
    text <- unlist(lapply(month_label_split, function(x) { x[1] }))
    
    at.months <- months + days(15) # position label at 15th of month
    
    at.years <-
      do.call(c, lapply(year(years), function(y, plotDates) {
        which.yr.dates <- which(year(plotDates) == y)
        return(median(plotDates[which.yr.dates]))
      }, plotDates = plotDates))
    
    # add year labels to x-axis
    object <- XAxisLabels(object, text, at.months, at.years)
    
    # add vertical lines to delineate calendar year boundaries
    object <- DelineateYearBoundaries(object, years)
  }
  
  return(object)
}