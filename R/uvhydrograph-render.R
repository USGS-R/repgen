#' Create a Unit Values Hydrograph
#'
#' @param reportObject full UV hydro report data structure.
#' @return list of all elements to be individually rendered
uvhydrographPlot <- function(reportObject) {
  options(scipen=8) # less likely to give scientific notation
  
  timezone <- fetchReportMetadataField(reportObject, "timezone") 
  
  #default to false if not included in report
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, "excludeZeroNegative") 
  if(isEmptyOrBlank(excludeZeroNegativeFlag)) {
    excludeZeroNegativeFlag <- FALSE
  }
  
  months <- getMonths(reportObject, timezone)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  
  if(!is.null(months)){
    for (month in months) {
      primary <- getPrimaryReportElements(reportObject, month, timezone, excludeZeroNegativeFlag)
      
      if(!is.null(primary[['plot']]) && useSecondaryPlot(reportObject)){
        secondary <- getSecondaryReportElements(reportObject, month, timezone, excludeZeroNegativeFlag)
      } else {
        secondary <- list()
      }
      
      renderList[[month]] <- list(plot1=primary[['plot']],
                                  table1=primary[['table']],
                                  ratingShiftTable=primary[['ratingShiftTable']],
                                  status_msg1=primary[['status_msg']],
                                  plot2=secondary[['plot']], 
                                  table2=secondary[['table']],
                                  status_msg2=secondary[['status_msg']])
    }
  } else {
    renderList[[1]] <- list(plot1=NULL, table1=NULL, plot2=NULL, table2=NULL)
  }
  
  return(renderList)
  
}

#' Use Secondary Plot
#' @description Determines where or not a report should use a secondary plot 
#' @param reportObject the report to be rendered
#' @return boolean true or false of whether a secondary series is appropriate for report
useSecondaryPlot <- function(reportObject) {
  hasRef <- hasReferenceSeries(reportObject)
  hasUpchain <- hasUpchainSeries(reportObject)
  isDisc <- isPrimaryDischarge(reportObject)
  
  useSecondaryPlot <- (hasRef && !isDisc) || hasUpchain
  
  return(useSecondaryPlot)
}

#' Get Primary Report Elements
#' @description contructs the gsPlot, a table of correction information, and a status message for data associated with the primary plot
#' @param reportObject UV hydro report
#' @param month the month to plot for all data
#' @param timezone timezone to parse all data into
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @return named list of report elements (plot, table status_msg)
getPrimaryReportElements <- function(reportObject, month, timezone, excludeZeroNegativeFlag) {
  primary_status_msg <- NULL
  primaryPlot <- NULL
  primaryTable <- NULL
  ratingShiftTable <- NULL
  
  corrections <- parseCorrectionsByMonth(reportObject, "primarySeriesCorrections", month)
  ratingShifts <- parseRatingShiftsByMonth(reportObject, month)
  primarySeriesList <- parsePrimarySeriesList(reportObject, month, timezone)
  
  if ((!isEmptyOrBlank(primarySeriesList[['corrected']]) && !isEmptyVar(primarySeriesList[['corrected']][['points']])) || primarySeriesList[['useEstimated']]){ #if primary corrected UV exists
    primaryPlot <- createPrimaryPlot(
        readTimeSeriesUvInfo(reportObject, "primarySeries"),
        readTimeSeriesUvInfo(reportObject, "referenceSeries"),
        readTimeSeriesUvInfo(reportObject, "comparisonSeries"),
        fetchReportMetadataField(reportObject, "comparisonStationId"),
        primarySeriesList,
        parsePrimaryDvList(reportObject, month, timezone),
        readUvQMeasurements(reportObject, month),
        readUvWq(reportObject, month),
        readUvGwLevel(reportObject, month),
        readAllUvReadings(reportObject, month, "primaryReadings"),
        readPrimaryUvHydroApprovalBars(reportObject, timezone, month), 
        corrections,
        ratingShifts,
        isPrimaryDischarge(reportObject),
        timezone,
        excludeZeroNegativeFlag)
    primaryTable <- parseCorrectionsAsTable(corrections)
    ratingShiftTable <- parseRatingShiftsAsTable(ratingShifts)
  } else {
    primary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'primaryParameter'))
  }
  return(list(plot=primaryPlot, table=primaryTable, ratingShiftTable=ratingShiftTable, status_msg=primary_status_msg))
}

#' Get Secondary Report Elements
#' @description contructs the gsPlot, a table of correction information, and a status message for data associated with the secondary plot
#' @param reportObject UV hydro report
#' @param month the month to plot for all data
#' @param timezone timezone to parse all data into
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @return named list of report elements (plot, table status_msg)
getSecondaryReportElements <- function(reportObject, month, timezone, excludeZeroNegativeFlag) {
  secondary_status_msg <- NULL
  secondaryPlot <- NULL
  secondaryTable <- NULL
  
  secondarySeriesList <- parseSecondarySeriesList(reportObject, month, timezone)
  if((!isEmptyOrBlank(secondarySeriesList[['corrected']]) && !isEmptyVar(secondarySeriesList[['corrected']][['points']])) || secondarySeriesList[['useEstimated']]) { #if corrected data exists
    corrections <- parseSecondaryCorrectionsByMonth(reportObject, month)
    secondaryPlot <- createSecondaryPlot( 
        readSecondaryTimeSeriesUvInfo(reportObject),
        secondarySeriesList, 
        readSecondaryUvHydroApprovalBars(reportObject, timezone), 
        readEffectiveShifts(reportObject, timezone, month),
        readUvMeasurementShifts(reportObject, month),
        readUvGageHeight(reportObject, month),
        readAllUvReadings(reportObject, month, "upchainReadings"),
        corrections, 
        timezone, 
        excludeZeroNegativeFlag, 
        tertiary_label=getTimeSeriesLabel(reportObject, "effectiveShifts"))
    secondaryTable <- parseCorrectionsAsTable(corrections)
  } else {
    secondary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'secondaryParameter'))
  }
  
  return(list(plot=secondaryPlot, table=secondaryTable, status_msg=secondary_status_msg))
}

#' Create Primary Plot
#' Will create and configure gsPlot object using provided data
#' @param uvInfo main timeseries information for the plot
#' @param refInfo timeseries information for reference series
#' @param compInfo timeseries information for comparios series
#' @param comparisonStation station id where comparison info comes from
#' @param primarySeriesList named list of timeseries to be plotted
#' @param dailyValues named list of daily values (lists of points) to be plotted. Each name tells what approval level the DV is at.
#' @param meas_Q Q measurement data to be plotted (list of points)
#' @param water_qual wq measurement data to be plotted (list of points)
#' @param gw_level ground water level measurements data to be plotted (list of points)
#' @param readings named list of different readings series (ref/csg/hwm readings)
#' @param approvals bars to be plotted which show approval level of data
#' @param corrections data correction information be plotted (time/correction pairs)
#' @param ratingShifts rating shift information to be plotted
#' @param isDischarge true if the plot has discharge as it's main corrected series
#' @param timezone timezone to plot/display in
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @return fully configured gsPlot object ready to be plotted
createPrimaryPlot <- function( 
    uvInfo, refInfo, compInfo, comparisonStation, 
    primarySeriesList, dailyValues, 
    meas_Q, water_qual, gw_level, readings, approvals, corrections, ratingShifts, isDischarge, timezone, excludeZeroNegativeFlag){ 
  # assume everything is NULL unless altered
  plot_object <- NULL
  
  dataAndSides <- sortDataAndSides(primarySeriesList, uvInfo, refInfo, compInfo)
  if(!isEmptyOrBlank(dataAndSides[['data']][['primary']])){
    primaryLims <- calculateLims(dataAndSides[['data']][['primary']])
    primaryLims[['ylim']] <- bufferLims(primaryLims[['ylim']], primarySeriesList[['uncorrected']][['points']][['value']])
  } else {
    primaryLims <- calculateLims(primarySeriesList[['uncorrected']][['points']])
  }
  referenceLims <- calculateLims(dataAndSides[['data']][['reference']])
  
  timeInformation <- parseUvTimeInformationFromLims(primaryLims, timezone)
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]

  plot_object <- gsplot(yaxs = 'r') %>%
    view(xlim = c(startDate, endDate)) %>%
    axis(side = 1, at = timeInformation[['dates']], labels = as.character(timeInformation[['days']])) %>%
    lines(x=0, y=0, side = 2, reverse = primarySeriesList[['inverted']]) %>%
    axis(side = 2, reverse = primarySeriesList[['inverted']], las = 0) %>%
    title(
      main = format(timeInformation[['dates']][1], "%B %Y"),
      xlab = paste("UV Series:", paste(primaryLims[['xlim']][1], "through", primaryLims[['xlim']][2]))
    )

  #Don't add the right-side axis if we aren't actually plotting anything onto it
  if((dataAndSides[['sides']][['reference']] == 4) || (dataAndSides[['sides']][['comparison']] == 4)){
     plot_object <- lines(plot_object, x=0, y=0, side = 4, reverse = primarySeriesList[['inverted']]) %>%
        axis(side = 4, las = 0, reverse = primarySeriesList[['inverted']])
  }
  
  #uncorrected data
  if(!isEmptyOrBlank(primarySeriesList[['uncorrected']]) && !isEmptyVar(primarySeriesList[['uncorrected']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['uncorrected']], "uncorrected", 
                                  timezone, getPrimaryPlotConfig, list(uvInfo[['label']], primaryLims[['ylim']], 
                                                                       dataAndSides[['sides']][['primary']], ylab=paste0(primarySeriesList[['uncorrected']][['type']], " (", primarySeriesList[['uncorrected']][['units']],")")), excludeZeroNegativeFlag)
  }
  
  #corrected data
  # primary axis is logged based on corrected data; if no corrected data, should be FALSE
  # gsplot object logged inside of plotTimeSeries >> plotItem
  
  if (primarySeriesList[['useEstimated']]) {
    # Plot the estimated data in place of the corrected data, but add TS label
    logPrimary <- isLogged(primarySeriesList[['estimated']][['points']], primarySeriesList[['corrected']][['isVolumetricFlow']], excludeZeroNegativeFlag)
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated']], "estimatedPrimary", 
                                  timezone, getPrimaryPlotConfig, list(uvInfo[['label']], primaryLims[['ylim']], dataAndSides[['sides']][['primary']], ylab=paste0(primarySeriesList[['estimated']][['type']], " (", primarySeriesList[['estimated']][['units']],")")), excludeZeroNegativeFlag)
  } else {
    #Plot estimated data as usual when there is also corrected data
    if(!isEmptyOrBlank(primarySeriesList[['estimated']]) && !isEmptyVar(primarySeriesList[['estimated']][['points']])) {
      plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated']], "estimated", 
                                    timezone, getPrimaryPlotConfig, list(uvInfo[['label']], primaryLims[['ylim']], dataAndSides[['sides']][['primary']], ylab=paste0(primarySeriesList[['estimated']][['type']], " (", primarySeriesList[['estimated']][['units']],")")), excludeZeroNegativeFlag)
    }
    
    logPrimary <- isLogged(primarySeriesList[['corrected']][['points']], primarySeriesList[['corrected']][['isVolumetricFlow']], excludeZeroNegativeFlag)
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['corrected']], "corrected", 
                                timezone, getPrimaryPlotConfig, list(uvInfo[['label']], primaryLims[['ylim']], dataAndSides[['sides']][['primary']], ylab=paste0(primarySeriesList[['corrected']][['type']], " (", primarySeriesList[['corrected']][['units']],")")), excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['corrected_reference']]) && !isEmptyVar(primarySeriesList[['corrected_reference']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['corrected_reference']], "corrected_reference", 
        timezone, getPrimaryPlotConfig, list(refInfo[['label']], referenceLims[['ylim']], dataAndSides[['sides']][['reference']], ylab=paste0(primarySeriesList[['corrected_reference']][['type']], " (", primarySeriesList[['corrected_reference']][['units']], ")")), excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['estimated_reference']]) && !isEmptyVar(primarySeriesList[['estimated_reference']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated_reference']], "estimated_reference", 
        timezone, getPrimaryPlotConfig, list(refInfo[['label']], referenceLims[['ylim']], dataAndSides[['sides']][['reference']], ylab=paste0(primarySeriesList[['estimated_reference']][['type']], " (", primarySeriesList[['estimated_reference']][['units']],")")), excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['comparison']]) && !isEmptyVar(primarySeriesList[['comparison']][['points']])) {
    comparisonLims <- calculateLims(dataAndSides[['data']][['comparison']])
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['comparison']], "comparison", 
        timezone, getPrimaryPlotConfig, 
        list(paste("Comparison", compInfo[['label']], "@", comparisonStation), comparisonLims[['ylim']], dataAndSides[['sides']][['comparison']], comparisonOnIndependentAxes=dataAndSides[['sides']][['comparison']]==6, ylab=paste0(primarySeriesList[['comparison']][['type']], " (", primarySeriesList[['comparison']][['units']],")")), 
        excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['estimated_comparison']]) && !isEmptyVar(primarySeriesList[['estimated_comparison']][['points']])) {
    comparisonLims <- calculateLims(dataAndSides[['data']][['comparison']])
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated_comparison']], "estimatedComparison", 
                                  timezone, getPrimaryPlotConfig, 
                                  list(paste("Estimated Comparison", compInfo[['label']], "@", comparisonStation), comparisonLims[['ylim']], dataAndSides[['sides']][['comparison']], comparisonOnIndependentAxes=dataAndSides[['sides']][['comparison']]==6, ylab=paste0(primarySeriesList[['estimated_comparison']][['type']], " (",primarySeriesList[['estimated_comparison']][['units']],")")), 
                                  excludeZeroNegativeFlag)
  }
  
  #Remove values from other data that could inhibit logging
  if(logPrimary){
    readings[['reference']] <- removeZeroNegative(readings[['reference']])
    readings[['create_stage_gage']] <- removeZeroNegative(readings[['crest_stage_gage']])
    readings[['high_water_mark']] <- removeZeroNegative(readings[['high_water_mark']])
    water_qual <- removeZeroNegative(water_qual)
    meas_Q <- removeZeroNegative(meas_Q)
    gw_level <- removeZeroNegative(gw_level)
  }

  #daily values
  for (i in 1:length(dailyValues)) {
    level <- names(dailyValues[i])
    if(!isEmptyOrBlank(level)) {
      plot_object <-
          addToGsplot(plot_object, getDvPlotConfig(level, dailyValues[[i]]))
    }
  }

  #reference readings
  if(!isEmptyVar(readings[['reference']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("ref", readings[['reference']]))
  }

  #CSG readings
  if(!isEmptyVar(readings[['crest_stage_gage']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("csg",readings[['crest_stage_gage']]))
  }

  #HWM readings
  if(!isEmptyVar(readings[['high_water_mark']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("hwm", readings[['high_water_mark']]))
  }
  
  #wq
  if(!isEmptyVar(water_qual)){
    plot_object <-
        addToGsplot(plot_object, getWqPlotConfig(water_qual))
  }
  
  #discharge measurements
  if(!isEmptyVar(meas_Q)){
    meas_Q_true <- meas_Q[which(meas_Q[['publish']]=='TRUE'),]
    meas_Q_false <- meas_Q[which(meas_Q[['publish']]=='FALSE'),]
    if(!isEmptyVar(meas_Q_true)) {
      plot_object <-
          addToGsplot(plot_object, getMeasQPlotConfig(meas_Q_true, "meas_Q_true"))
    }
    if(!isEmptyVar(meas_Q_false)) {
      plot_object <-
        addToGsplot(plot_object, getMeasQPlotConfig(meas_Q_false, "meas_Q_false"))
      }
  }
  
  #gw_level
  if(!isEmptyVar(gw_level)){
    plot_object <-
        addToGsplot(plot_object, getGwPlotConfig(gw_level))
  }
    
  plot_object <- addToGsplot(plot_object, getCorrectionsPlotConfig(corrections, timeInformation[['start']], timeInformation[['end']], 
        uvInfo[['label']], primaryLims))
  
  plot_object <- addToGsplot(plot_object, getRatingShiftsPlotConfig(ratingShifts, timeInformation[['start']], timeInformation[['end']], 
                                                                   uvInfo[['label']], primaryLims))

  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- addToGsplot(plot_object, 
                             getApprovalBarConfig(approvals, ylim=ylim(plot_object, side = 2), ylog=logPrimary))
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  legend_items <- plot_object[['legend']][['legend.auto']][['legend']]
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
  plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                    legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray", where='first') %>%
    abline(v=timeInformation[['dates']], lty=3, col="gray", where='first')
  
  
  plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)[['side.1']])
  
  return(plot_object)
}

#' Create Secondary Plot
#' Will create and configure gsPlot object using provided data
#' @param uvInfo main timeseries information for the plot
#' @param secondarySeriesList named list of timeseries to be plotted. Also includes log/invert axis info.
#' @param approvals bars to be plotted which show approval level of data
#' @param effective_shift_pts effective shift data to be plotted (list of points)
#' @param meas_shift measured shift data to be plotted (list of points)
#' @param gage_height measured gage height data to be plotted (list of points)
#' @param readings named list of different readings series (ref/csg/hwm readings)
#' @param corrections data correction information be plotted (time/correction pairs)
#' @param timezone timezone to plot/display in
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @param tertiary_label label for the 3rd data item plotted
#' @return fully configured gsPlot object ready to be plotted
createSecondaryPlot <- function(uvInfo, secondarySeriesList, 
    approvals, effective_shift_pts, 
    meas_shift, gage_height, readings,
    corrections, timezone, excludeZeroNegativeFlag, tertiary_label=""){
  plot_object <- NULL
  
  invertPlot <- secondarySeriesList[['inverted']]

  allCorrectedData <- rbind(secondarySeriesList[['corrected']][['points']], secondarySeriesList[['estimated']][['points']])
  
  if(!isEmptyOrBlank(allCorrectedData )){
    lims <- calculateLims(allCorrectedData )
    lims[['ylim']] <- bufferLims(lims[['ylim']], secondarySeriesList[['uncorrected']][['points']][['value']])
  } else {
    lims <- calculateLims(secondarySeriesList[['uncorrected']][['points']])
  }
  
  timeInformation <- parseUvTimeInformationFromLims(lims, timezone)
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]
  
  plot_object <- gsplot(yaxs = 'r') %>%
    view(
      xlim = c(startDate, endDate),
      ylim = bufferLims(lims[['ylim']], secondarySeriesList[['uncorrected']][['points']][['value']])
    ) %>%
    axis(side = 1, at = timeInformation[['dates']], labels = as.character(timeInformation[['days']])) %>%
    axis(side = 2, reverse = invertPlot, las = 0) %>%
    title(
      main = "",
      xlab = paste("UV Series:", paste(lims[['xlim']][1], "through", lims[['xlim']][2])),
      ylab = uvInfo[['label']]
    )

  #uncorrected data
  if(!isEmptyOrBlank(secondarySeriesList[['uncorrected']]) && !isEmptyVar(secondarySeriesList[['uncorrected']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, secondarySeriesList[['uncorrected']], "uncorrected", 
        timezone, getSecondaryPlotConfig, list(uvInfo[['label']], lims$ylim), excludeZeroNegativeFlag)
  }
  
  #estimated data
  if(!isEmptyOrBlank(secondarySeriesList[['estimated']]) && !isEmptyVar(secondarySeriesList[['estimated']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, secondarySeriesList[['estimated']], "estimated", 
        timezone, getSecondaryPlotConfig, list(uvInfo[['label']], lims$ylim), excludeZeroNegativeFlag)
  }
  
  #corrected data
  # primary axis is logged based on corrected data; if no corrected data, should be FALSE
  # gsplot object logged inside of plotTimeSeries >> plotItem
  if (secondarySeriesList[['useEstimated']]) {
    # Plot estimated corrected data, in place of non-estimated corrected data
    doLog <- isLogged(secondarySeriesList[['estimated']][['points']], secondarySeriesList[['estimated']][['isVolumetricFlow']], excludeZeroNegativeFlag)

  } else {
    #Plot estimated data if non-estimated corrected data exists
    doLog <- isLogged(secondarySeriesList[['corrected']][['points']], secondarySeriesList[['corrected']][['isVolumetricFlow']], excludeZeroNegativeFlag)
    plot_object <- plotTimeSeries(plot_object, secondarySeriesList[['corrected']], "corrected", 
        timezone, getSecondaryPlotConfig, list(uvInfo[['label']], lims$ylim), excludeZeroNegativeFlag)
  }

  #effective shift
  if(!isEmptyVar(effective_shift_pts)){
    plot_object <- 
        addToGsplot(plot_object, 
            getEffectiveShiftPlotConfig(effective_shift_pts, uvInfo[['label']], tertiary_label)
        )
  }
  
  #Remove values from other data that could inhibit logging
  if(doLog){
    readings[['reference']] <- removeZeroNegative(readings[['reference']])
    readings[['create_stage_gage']] <- removeZeroNegative(readings[['crest_stage_gage']])
    readings[['high_water_mark']] <- removeZeroNegative(readings[['high_water_mark']])
    gage_height <- removeZeroNegative(gage_height)
    meas_shift <- removeZeroNegative(meas_shift)
  }
  
  if(!isEmptyVar(gage_height)){
    plot_object <- 
        addToGsplot(plot_object, 
            getGageHeightPlotConfig(gage_height)
        )
  }
  
  if(!isEmptyVar(meas_shift)){
    plot_object <- 
        addToGsplot(plot_object, 
            getMeasuredShiftPlotConfig(meas_shift)
        )
  }

  #reference readings
  if(!isEmptyVar(readings[['reference']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("ref", readings[['reference']]))
  }

  #CSG readings
  if(!isEmptyVar(readings[['crest_stage_gage']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("csg",readings[['crest_stage_gage']]))
  }

  #HWM readings
  if(!isEmptyVar(readings[['high_water_mark']])){
    plot_object <-
      addToGsplot(plot_object, getReadingsPlotConfig("hwm", readings[['high_water_mark']]))
  }
  
  plot_object <- addToGsplot(plot_object, getCorrectionsPlotConfig(corrections, startDate, endDate, 
          uvInfo[['label']], lims))
  
  plot_object <- addToGsplot(plot_object, getApprovalBarConfig(approvals, ylim(plot_object, side = 2), ylog=doLog))
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  legend_items <- plot_object[['legend']][['legend.auto']][['legend']]
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)

  plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                            legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=timeInformation[['dates']], lty=3, col="gray")
  
  
  isShift <- !isEmptyVar(effective_shift_pts)
  
  if(isShift){
    yMax = max(effective_shift_pts[['value']])
    yMin = min(effective_shift_pts[['value']])
    y_seq <- pretty(c(yMin, yMax), shrink.sml = 20)
    plot_object <- plot_object %>% 
        mtext(paste0(tertiary_label, " (", uvInfo[['units']], ")"), 
            side = 4, line = 1.5) %>% 
        axis(side=4, las=0, at=y_seq, reverse = invertPlot)
  }
  
  plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)[['side.1']])
  
  return(plot_object)
}

#' Sort data and sides
#' @description Depending on the configuration, reference and comparison series will be plotted on different sides. This constructs ylims and what side each series should be on
#' @param primarySeriesList list of timeseries available for reporting
#' @param uvInfo main timeseries information for the plot
#' @param refInfo timeseries information for reference series
#' @param compInfo timeseries information for comparios series
#' @return named list with two items, sides and ylims. Each item has a named list with items for each timeseries. (EG: side for reference would be returnedObject$sides$reference)
sortDataAndSides <- function(primarySeriesList, uvInfo, refInfo, compInfo) {
  referenceExist <- !isEmptyOrBlank(primarySeriesList[['corrected_reference']])
  comparisonExist <- !isEmptyOrBlank(primarySeriesList[['comparison']])
  
  ylimPrimaryData <- data.frame(time=c(), value=c())
  
  ylimCorrectedData <- rbind(primarySeriesList[['corrected']][['points']],
                             primarySeriesList[['estimated']][['points']])
  
  ylimReferenceData <- primarySeriesList[['corrected_reference']][['points']]
  ylimCompData <- rbind(primarySeriesList[['comparison']][['points']],
                        primarySeriesList[['estimated_comparison']][['points']])
  
  primarySide <- 2
  referenceSide <- 4
  comparisonSide <- 6
  
  ## Setup sides based on TS properties
  
  # add corrected data
  ylimPrimaryData <- rbind(ylimPrimaryData, ylimCorrectedData)
  
  if(referenceExist){
    if(uvInfo[['type']] == refInfo[['type']]){
      referenceSide <- primarySide
      ylimPrimaryData <- rbind(ylimPrimaryData, ylimReferenceData)
      ylimReferenceData <- ylimPrimaryData
    }
  } else {
    referenceSide <- 0
  }
  
  if(comparisonExist){
    if(compInfo[['type']] == uvInfo[['type']]){
      comparisonSide <- primarySide
      ylimPrimaryData <- rbind(ylimPrimaryData, ylimCompData)
      ylimCompData <- ylimPrimaryData
      
      if(referenceSide == primarySide){
        ylimReferenceData <- ylimPrimaryData
      }
    } else if(referenceExist && compInfo[['type']] == refInfo[['type']]) {
      comparisonSide <- referenceSide
      ylimReferenceData <- rbind(ylimReferenceData, ylimCompData)
      ylimCompData <- ylimReferenceData
    } else if(!referenceExist || referenceSide == primarySide) {
      comparisonSide <- 4
    }
  } else {
    comparisonSide <- 0
  }
  
  data <- list(primary=ylimPrimaryData, 
               reference=ylimReferenceData, 
               comparison=ylimCompData)
  
  sides <- data.frame(primary=primarySide, reference=referenceSide, comparison=comparisonSide)
  
  return(list(data=data, sides=sides))
}


#' Get Primary Plot Config
#' @description Given a list of TS points, some information about the plot to build, will return a named list of gsplot elements to call
#' @param timeseries list of TS points relavant to primary plot
#' @param name name of item being plotted
#' @param label label of time series
#' @param ylim y range that the time series covers
#' @param dataSide optional for reference and comparison series, integer of what side the time series' y value is on
#' @param comparisonOnIndependentAxes set to false if being plotted on the same axes as another
#' @param doLog Whether or not the item should be placed on a logarithmic axis
#' @param ylab the y-axis label (not to be confused with what is used in legend)
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getPrimaryPlotConfig <- function(timeseries, name, label, 
    ylim, dataSide=0, comparisonOnIndependentAxes=TRUE, doLog=FALSE, ylab) {
  styles <- getUvStyles()
  
  x <- timeseries[['time']]
  y <- timeseries[['value']]
  
  compAxes <- TRUE
  compAnnotations <- TRUE
  
  if(comparisonOnIndependentAxes){
    compAxes <- FALSE
    compAnnotations <- FALSE
  }
  
  if(doLog){
    doLog = 'y'
  } else {
    doLog = ''
  }
  
  plotConfig <- switch(name,
      corrected = list(
          lines = append(list(x=x, y=y, side=2, ylim=ylim, ylab=ylab, legend.name=paste(styles[['corr_UV_lbl']], label)), styles[['corr_UV_lines']]),
          view = list(side=2, log=doLog)
          ),
      estimatedPrimary = list(
          lines = append(list(x=x, y=y, side=2, ylim=ylim, ylab=ylab, legend.name=paste(styles[['est_UV_lbl']], label)), styles[['est_UV_lines']]),
          view = list(side=2, log=doLog)
          ),
      estimated = list(
          lines = append(list(x=x, y=y, side=2, ylim=ylim, ylab=ylab, legend.name=paste(styles[['est_UV_lbl']], label)), styles[['est_UV_lines']]),
          view = list(side=2, log=doLog)
          ),
      uncorrected = list(
          lines = append(list(x=x, y=y, side=2, ylim=ylim, ylab=ylab, legend.name=paste(styles[['uncorr_UV_lbl']], label)), styles[['uncorr_UV_lines']]),
          view = list(side=2, log=doLog)
          ),
      comparison = list(
          lines = append(list(x=x, y=y, ylim=ylim, side=dataSide, axes=compAxes, ylab=ylab, ann=compAnnotations, legend.name=label), styles[['comp_UV_lines']]),
          view = list(side=dataSide, log=doLog)
          ), 
      estimatedComparison = list(
        lines = append(list(x=x, y=y, ylim=ylim, side=dataSide, axes=compAxes, ylab=ylab, ann=compAnnotations, legend.name=label), styles[['est_comp_UV_lines']]),
        view = list(side=dataSide, log=doLog)
      ), 
      corrected_reference = list(
          lines = append(list(x=x,y=y, ylim=ylim, side=dataSide, ylab=ylab, legend.name=paste(styles[['corr_UV_Qref_lbl']], label)), styles[['corr_UV_Qref_lines']]),
          view = list(side=dataSide, log=doLog)
          ),
      estimated_reference = list(
          lines = append(list(x=x,y=y, side=dataSide, ylab=ylab, legend.name=paste(styles[['est_UV_Qref_lbl']], label)), styles[['est_UV_Qref_lines']]),
          view = list(side=dataSide, log=doLog)
          ),
      stop(paste(name, " config not found for primary plot"))
  )
  
  return(plotConfig)
}

#' Get Secondary Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param timeseries the timeseries to be plotted
#' @param name name of style to be applied to given x/y points (corrected, estimated, or uncorrected)
#' @param legend_label label to be applied to points in legend
#' @param ylim The y-axis limits of the item to get the configuration for
#' @param doLog Whether or not the item should be placed on a logarithmic axis (DEFAULT: FALSE)
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getSecondaryPlotConfig <- function(timeseries, name, legend_label, ylim, doLog=FALSE) {
  styles <- getUvStyles()
  
  x <- timeseries[['time']]
  y <- timeseries[['value']]
  
  if(doLog){
    doLog = 'y'
  } else {
    doLog = ''
  }
  
  plotConfig <- switch(name,
      corrected = list(
          lines = append(list(x=x,y=y,ylim=ylim, legend.name=paste(styles[['corr_UV_lbl']], legend_label)), styles[['corr_UV2_lines']]),
          view = list(side=2, log=doLog)
      ), 
      estimated = list(
          lines = append(list(x=x,y=y,ylim=ylim,legend.name=paste(styles[['est_UV_lbl']], legend_label)), styles[['est_UV2_lines']]),
          view = list(side=2, log=doLog)
      ),
      uncorrected = list(
          lines = append(list(x=x,y=y,ylim=ylim, legend.name=paste(styles[['uncorr_UV_lbl']], legend_label)), styles[['uncorr_UV_lines']]),
          view = list(side=2, log=doLog)
      ),                
      stop(paste(name, " config not found for secondary plot"))
  )
  
  return(plotConfig)
}

#' Get Water Quality Plot Config
#' @description Given a list of wq objects, will return a named list of gsplot elements to call
#' @param water_qual list of water_qual objects 
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getWqPlotConfig <- function(water_qual) {
  styles <- getUvStyles()
  
  x <- water_qual[['time']]
  y <- water_qual[['value']]
  
  plotConfig <-  list(
          points = append(list(x=x, y=y), styles[['water_qual_points']])
      )
  
  return(plotConfig)
}

#' Get Discharge Measurement Plot Config
#' @description Given a list of discharge measurements, will return a named list of gsplot elements to call
#' @param meas_Q list of discharge measurements
#' @param name the name of the style to be given to the discharge measurements (meas_Q_true or meas_Q_false)
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getMeasQPlotConfig <- function(meas_Q, name) {
  styles <- getUvStyles()
  
  x <- meas_Q[['time']]
  y <- meas_Q[['value']]
  
  plotConfig <- switch(name,
                       meas_Q_true = list(
                         error_bar=append(list(x=x, y=y, offset.down=(y-meas_Q[['minQ']]), offset.up=(meas_Q[['maxQ']]-y)), styles[['meas_Q_error_bars_true']]),
                         points=append(list(x=x, y=y), styles[['meas_Q_points_true']]),
                         callouts=append(list(x=x, y=y, labels = meas_Q[['n']]), styles[['meas_Q_callouts_true']])
                       ),
                       meas_Q_false = list(
                         error_bar=append(list(x=x, y=y, offset.down=(y-meas_Q[['minQ']]), offset.up=(meas_Q[['maxQ']]-y)), styles[['meas_Q_error_bars_false']]),
                         points=append(list(x=x, y=y), styles[['meas_Q_points_false']]),
                         callouts=append(list(x=x, y=y, labels = meas_Q[['n']]), styles[['meas_Q_callouts_false']])
                       ),
                       stop(paste(name, " config not found for discharge measurements"))
  )
  
  return(plotConfig)
}

#' Get GW level Plot Config
#' @description Given a gw level readings, will return a named list of gsplot elements to call
#' @param gw_level list of GW level objects 
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getGwPlotConfig <- function(gw_level) {
  styles <- getUvStyles()
  
  x <- gw_level[['time']]
  y <- gw_level[['value']]
  
  plotConfig <- list(points = append(list(x=x,y=y), styles[['gw_level_points']]))
  
  return(plotConfig)
}


#' Get Readings Plot Config
#' @description Given a readings list, will pull the desired plotting calls and styling
#' @param reading_type name of reading type to get style for (ref, csg, hwm)
#' @param readings list of readings objects relavant to primary plot
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getReadingsPlotConfig <- function(reading_type, readings) {
  styles <- getUvStyles()
  
  x <- readings[['time']]
  y <- readings[['value']]
  
  plotConfig <- switch(reading_type,
      ref = list(
          points=append(list(x=x, y=y), styles[['ref_readings_points']]), 
          error_bar=append(list(x=x, y=y, offset.down=readings[['uncertainty']], offset.up=readings[['uncertainty']]), styles[['ref_readings_error_bars']])
      ),
      csg = list(
          points=append(list(x=x, y=y), styles[['csg_readings_points']]), 
          error_bar=append(list(x=x, y=y, offset.down=readings[['uncertainty']], offset.up=readings[['uncertainty']]), styles[['csg_readings_error_bars']])
      ),
      hwm = list(
          points=append(list(x=x, y=y), styles[['hwm_readings_points']]), 
          error_bar=append(list(x=x, y=y, offset.down=readings[['uncertainty']], offset.up=readings[['uncertainty']]), styles[['hwm_readings_error_bars']])
      ),
      stop(paste(reading_type, " config not found for reference reading plotting"))
  )
  
  return(plotConfig)
}

#' Get DV Plot Config
#' @description Given a DV plot item, will return plotting config styled to the given level
#' @param level the appr level label (approved_dv, analyzed_dv, working_dv)
#' @param dvPlotItem list of data objects relavant to primary plot
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getDvPlotConfig <- function(level, dvPlotItem) {
  styles <- getUvStyles()
  
  x <- dvPlotItem[['time']]
  y <- dvPlotItem[['value']]
  
  legend.name <- dvPlotItem[['legend.name']]
  point_type <- dvPlotItem[['point_type']]
  
  plotConfig <- switch(level,
      approved_dv = list(
          points = append(list(x=x, y=y, pch=point_type, legend.name=legend.name), styles[['approved_dv_points']])
      ),
      analyzed_dv = list(
          points = append(list(x=x, y=y, pch=point_type, legend.name=legend.name), styles[['analyzed_dv_points']])
      ),
      working_dv = list(
          points = append(list(x=x, y=y, pch=point_type, legend.name=legend.name), styles[['working_dv_points']])
      ),
      stop(paste(level, " config not found for DV plot"))
  )
  
  return(plotConfig)
}

#' Get Effective Shift Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param effective_shift list of effective shift points
#' @param secondary_lbl label of secondary time series
#' @param tertiary_lbl label of tertiary time series
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getEffectiveShiftPlotConfig <- function(effective_shift, secondary_lbl, tertiary_lbl) {
  styles <- getUvStyles()
  
  x <- effective_shift[['time']]
  y <- effective_shift[['value']]
  
  effective_shift_config = list(
      lines=append(list(x=x,y=y, legend.name=paste(secondary_lbl, tertiary_lbl)), styles[['effect_shift_lines']]),
      text=append(list(x=x[1], y=y[1]), styles[['effect_shift_text']]) #this is a bit of hack to make sure the right axis appears
  )
  
  return(effective_shift_config)
}

#' Get Gage Height Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param gage_height list of gage height records
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getGageHeightPlotConfig <- function(gage_height) {
  styles <- getUvStyles()
  
  x <- gage_height[['time']]
  y <- gage_height[['value']]
  
  gage_height_config = list(
      points=append(list(x=x, y=y), styles[['gage_height_points']]),
      callouts=list(x=x, y=y, labels=gage_height[['n']])
  )
  
  return(gage_height_config)
}

#' Get Measured shifts Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param meas_shift list of measured shift objects
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getMeasuredShiftPlotConfig <- function(meas_shift) {
  styles <- getUvStyles()
  
  x <- meas_shift[['time']]
  y <- meas_shift[['value']]
  
  meas_shift_config = list(
      points=append(list(x=x, y=y), styles[['meas_shift_points']]),
      error_bar=append(list(x=x, y=y, offset.down=(y-meas_shift[['minShift']]), offset.up=(meas_shift[['maxShift']]-y)), styles[['meas_shift_error_bars']])
  )
  
  return(meas_shift_config)
}

#' Get Corrections Plot Config
#' @description Given corrections, some information about the plot to build, will return a named list of gsplot elements to call
#' @param corrections list of data objects relavant to plotting corrections data
#' @param plotStartDate start date of this plot 
#' @param plotEndDate end date of this plot
#' @param label label of that these corrections are associated with
#' @param limits list of lims for all of the data which will be on here
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getCorrectionsPlotConfig <- function(corrections, plotStartDate, plotEndDate, label, 
    limits) {
  
  if(isEmptyOrBlank(corrections) || nrow(corrections) <= 0) {
    return(list())
  }
  styles <- getUvStyles()
  
  corrPositions <- getVerticalFlagPositions(corrections)
  correctionLabels <- parseVerticalFlagLabelSpacing(corrections, limits)
  corrArrows <- getVerticalFlagArrows(correctionLabels)
  
  plotConfig <- list(
      lines=append(list(x=0, y=0, xlim = c(plotStartDate, plotEndDate)), styles[['corrections_lines']]),
      abline=append(list(v=corrPositions, legend.name=paste(styles[['corrections_correction_lbl']], label)), styles[['corrections_ablines']]),
      arrows=append(list(x0=corrArrows[['xorigin']], x1=corrArrows[['x']], y0=corrArrows[['y']], y1=corrArrows[['y']]), styles[['corrections_arrows']]),
      points=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], cex=correctionLabels[['r']]), styles[['corrections_points']]),
      text=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], labels=correctionLabels[['label']]), styles[['corrections_text']])
  )
  
  return(plotConfig)
}

#' Get Rating Shifts Plot Config
#' @description Given rating shifts, some information about the plot to build, will return a named list of gsplot elements to call
#' @param ratingShifts list of data objects relavant to plotting corrections data
#' @param plotStartDate start date of this plot 
#' @param plotEndDate end date of this plot
#' @param label label of that these corrections are associated with
#' @param limits list of lims for all of the data which will be on here
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getRatingShiftsPlotConfig <- function(ratingShifts, plotStartDate, plotEndDate, label, 
                                     limits) {
  
  if(isEmptyOrBlank(ratingShifts) || nrow(ratingShifts) <= 0) {
    return(list())
  }
  styles <- getUvStyles()
  
  ratingShiftsPositions <- getVerticalFlagPositions(ratingShifts)
  ratingShiftsLabels <- parseVerticalFlagLabelSpacing(ratingShifts, limits)
  ratingShiftsArrows <- getVerticalFlagArrows(ratingShiftsLabels)
  
  plotConfig <- list(
    lines=append(list(x=0, y=0, xlim = c(plotStartDate, plotEndDate)), styles[['ratingShifts_lines']]),
    abline=append(list(v=ratingShiftsPositions, legend.name=paste(styles[['ratingShifts_lbl']], label)), styles[['ratingShifts_ablines']]),
    arrows=append(list(x0=ratingShiftsArrows[['xorigin']], x1=ratingShiftsArrows[['x']], y0=ratingShiftsArrows[['y']], y1=ratingShiftsArrows[['y']]), styles[['ratingShifts_arrows']]),
    points=append(list(x=ratingShiftsLabels[['x']], y=ratingShiftsLabels[['y']], cex=ratingShiftsLabels[['r']]), styles[['ratingShifts_points']]),
    text=append(list(x=ratingShiftsLabels[['x']], y=ratingShiftsLabels[['y']], labels=ratingShiftsLabels[['label']]), styles[['ratingShifts_text']])
  )
  
  return(plotConfig)
}
