#' Create a Unit Values Hydrograph
#'
#' @param reportObject full UV hydro report data structure.
#' @return list of all elements to be individually rendered
uvhydrographPlot <- function(reportObject) {
  options(scipen=8) # less likely to give scientific notation
  
  timezone <- fetchReportMetadataField(reportObject, "timezone") 
  
  #default to false if not included in report
  excludeZeroNegativeFlag <- fetchReportMetadataField(reportObject, "excludeZeroNegativeFlag") 
  if(isEmptyOrBlank(excludeZeroNegativeFlag)) {
    excludeZeroNegativeFlag <- FALSE
  }
  
  months <- getMonths(reportObject, timezone)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  
  if(!is.null(months)){
    for (month in months) {
      primary <- getPrimaryReportElements(reportObject, month, timezone, excludeZeroNegativeFlag)
      
      if(!is.null(primary[['plot']])){
        secondary <- getSecondaryReportElements(reportObject, month, timezone, excludeZeroNegativeFlag)
      } else {
        secondary <- list()
      }
      
      renderList[[month]] <- list(plot1=primary[['plot']],
                                  table1=primary[['table']], 
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
  
  corrections <- parseCorrectionsByMonth(reportObject, "primarySeriesCorrections", month)
  primarySeriesList <- parsePrimarySeriesList(reportObject, month, timezone)
  
  if(!isEmptyOrBlank(primarySeriesList[['corrected']]) && !isEmptyVar(primarySeriesList[['corrected']][['points']])){ #if primary corrected UV exists
    primaryLims <- calculatePrimaryLims(primarySeriesList, isPrimaryDischarge(reportObject))
    
    primaryPlot <- createPrimaryPlot(
        readTimeSeriesUvInfo(reportObject, "primarySeries"),
        readTimeSeriesUvInfo(reportObject, "referenceSeries"),
        readTimeSeriesUvInfo(reportObject, "comparisonSeries"),
        fetchReportMetadataField(reportObject, "comparisonStationId"),
        parseUvTimeInformationFromLims(primaryLims, timezone),
        primarySeriesList,
        parsePrimaryDvList(reportObject, month, timezone),
        readUvQMeasurements(reportObject, month),
        readUvWq(reportObject, month),
        readUvGwLevel(reportObject, month),
        readAllUvReadings(reportObject, month),
        readPrimaryUvHydroApprovalBars(reportObject, timezone, month), 
        corrections, 
        primaryLims,
        timezone,
        excludeZeroNegativeFlag)
    primaryTable <- parseCorrectionsAsTable(corrections)
  } else {
    primary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'primaryParameter'))
  }
  return(list(plot=primaryPlot, table=primaryTable, status_msg=primary_status_msg))
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
  
  hasReferenceSeries <- hasReferenceSeries(reportObject)
  hasUpchainSeries <- hasUpchainSeries(reportObject)
  
  if((hasReferenceSeries && !isPrimaryDischarge(reportObject)) || hasUpchainSeries) {
    if(hasReferenceSeries) {
      corrections <- parseCorrectionsByMonth(reportObject, "referenceSeriesCorrections", month)
    } else {
      corrections <- parseCorrectionsByMonth(reportObject, "upchainSeriesCorrections", month)
    }
    secondarySeriesList <- parseSecondarySeriesList(reportObject, month, timezone)
    if(!isEmptyOrBlank(secondarySeriesList[['corrected']]) && !isEmptyVar(secondarySeriesList[['corrected']][['points']])){ #if corrected data exists
      secondaryLims <- calculateLims(secondarySeriesList[['corrected']][['points']])
      secondaryPlot <- createSecondaryPlot( 
          readSecondaryTimeSeriesUvInfo(reportObject),
          parseUvTimeInformationFromLims(secondaryLims, timezone),
          secondarySeriesList, 
          readSecondaryUvHydroApprovalBars(reportObject, timezone), 
          readEffectiveShifts(reportObject, timezone, month),
          readUvMeasurementShifts(reportObject, month),
          readUvGageHeight(reportObject, month),
          corrections, 
          secondaryLims,
          timezone, 
          excludeZeroNegativeFlag, 
          tertiary_label=getTimeSeriesLabel(reportObject, "effectiveShifts"))
      secondaryTable <- parseCorrectionsAsTable(corrections)
    } else {
      secondary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'secondaryParameter'))
    }
  }
  
  return(list(plot=secondaryPlot, table=secondaryTable, status_msg=secondary_status_msg))
}

#' Create Primary Plot
#' Will create and configure gsPlot object using provided data
#' @param uvInfo main timeseries information for the plot
#' @param refInfo timeseries information for reference series
#' @param compInfo timeseries information for comparios series
#' @param comparisonStation station id where comparison info comes from
#' @param timeInformation time information about the data on the plot, see parseUvTimeInformationFromLims for information
#' @param primarySeriesList named list of timeseries to be plotted
#' @param dailyValues named list of daily values (lists of points) to be plotted. Each name tells what approval level the DV is at.
#' @param meas_Q Q measurement data to be plotted (list of points)
#' @param water_qual wq measurement data to be plotted (list of points)
#' @param gw_level ground water level measurements data to be plotted (list of points)
#' @param readings named list of different readings series (ref/csg/hwm readings)
#' @param approvalBars bars to be plotted which show approval level of data
#' @param corrections data correction information be plotted (time/correction pairs)
#' @param lims x/y lims which should contain all data above
#' @param timezone timezone to plot/display in
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @return fully configured gsPlot object ready to be plotted
createPrimaryPlot <- function( 
    uvInfo, refInfo, compInfo, comparisonStation, timeInformation, 
    primarySeriesList, dailyValues, 
    meas_Q, water_qual, gw_level, readings, approvalBars, corrections, lims, timezone, excludeZeroNegativeFlag){ 
  # assume everything is NULL unless altered
  plot_object <- NULL
  
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]

  plot_object <- gsplot(ylog = primarySeriesList[['loggedAxis']], yaxs = 'r') %>%
    view(xlim = c(startDate, endDate)) %>%
    axis(side = 1, at = timeInformation[['dates']], labels = as.character(timeInformation[['days']])) %>%
    axis(side = 2, reverse = primarySeriesList[['inverted']], las = 0) %>%
    lines(x=0, y=0, side = 2, reverse = primarySeriesList[['inverted']]) %>%
    title(
      main = format(timeInformation[['dates']][1], "%B %Y"),
      xlab = paste("UV Series:", paste(lims[['xlim']][1], "through", lims[['xlim']][2]))
    )

  limsAndSides <- calculateLimitsAndSides(primarySeriesList, uvInfo, refInfo, compInfo)

  #Don't add the right-side axis if we aren't actually plotting anything onto it
  if((limsAndSides[['sides']][['reference']] == 4) || (limsAndSides[['sides']][['comparison']] == 4)){
    plot_object <- lines(plot_object, x=0, y=0, side = 4, reverse = primarySeriesList[['inverted']]) %>%
        axis(side = 4, las = 0, reverse = primarySeriesList[['inverted']])
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['corrected_reference']]) && !isEmptyVar(primarySeriesList[['corrected_reference']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['corrected_reference']], "corrected_reference", 
        timezone, getPrimaryPlotConfig, list(NULL, refInfo[['label']], NULL, limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['estimated_reference']]) && !isEmptyVar(primarySeriesList[['estimated_reference']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated_reference']], "estimated_reference", 
        timezone, getPrimaryPlotConfig, list(NULL, refInfo[['label']], NULL, limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)
  }
  
  if(!isEmptyOrBlank(primarySeriesList[['comparison']]) && !isEmptyVar(primarySeriesList[['comparison']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['comparison']], "comparison", 
        timezone, getPrimaryPlotConfig, list(NULL, NULL, paste("Comparison", compInfo[['label']], "@", comparisonStation), limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)
  }
  
  #uncorrected data
  if(!isEmptyOrBlank(primarySeriesList[['uncorrected']]) && !isEmptyVar(primarySeriesList[['uncorrected']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['uncorrected']], "uncorrected", 
        timezone, getPrimaryPlotConfig, list(uvInfo[['label']], NULL, NULL, limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)
  }
  
  #estimated data
  if(!isEmptyOrBlank(primarySeriesList[['estimated']]) && !isEmptyVar(primarySeriesList[['estimated']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, primarySeriesList[['estimated']], "estimated", 
        timezone, getPrimaryPlotConfig, list(uvInfo[['label']], NULL, NULL, limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)
  }

  #corrected data
  plot_object <- plotTimeSeries(plot_object, primarySeriesList[['corrected']], "corrected", 
      timezone, getPrimaryPlotConfig, list(uvInfo[['label']], NULL, NULL, limsAndSides$sides, limsAndSides$ylims), excludeZeroNegativeFlag)

  # still need gsplot to handle side 1 vs side 2 logging. See issue #414
  # once that is working, use view to log the axes when appropriate
  # could be added using logic in if statement above
  # if(isLogged(data, ylimPrimaryData, primaryInfo[['primarySeries']])){
  #   plot_object <- view(plot_object, side=primarySide, log='y')
  # }
  # if(comparisonExist && isLogged(data, ylimComparisonData, primaryInfo[['comparisonSeries']])){
  #   plot_object <- view(plot_object, side=comparisonSide, log='y')
  # }
  # if(referenceExist && isLogged(data, ylimReferenceData, primaryInfo[['referenceSeries']])){
  #   plot_object <- view(plot_object, side=referenceSide, log='y')
  # }

  #daily values
  for (i in 1:length(dailyValues)) {
    level <- names(dailyValues[i])
    if(!isEmptyOrBlank(level)) {
      plot_object <-
          AddToGsplot(plot_object, getDvPlotConfig(level, dailyValues[[i]]))
    }
  }

  #reference readings
  if(!isEmptyVar(readings[['reference']])){
    plot_object <-
      AddToGsplot(plot_object, getReadingsPlotConfig("ref", readings[['reference']]))
  }

  #CSG readings
  if(!isEmptyVar(readings[['crest_stage_gage']])){
    plot_object <-
      AddToGsplot(plot_object, getReadingsPlotConfig("csg",readings[['crest_stage_gage']]))
  }

  #HWM readings
  if(!isEmptyVar(readings[['high_water_mark']])){
    plot_object <-
      AddToGsplot(plot_object, getReadingsPlotConfig("hwm", readings[['high_water_mark']]))
  }
  
  #wq
  if(!isEmptyVar(water_qual)){
    plot_object <-
        AddToGsplot(plot_object, getWqPlotConfig(water_qual))
  }
  
  #discharge measurement
  if(!isEmptyVar(meas_Q)){
    plot_object <-
        AddToGsplot(plot_object, getMeasQPlotConfig(meas_Q))
  }
  
  #gw_level
  if(!isEmptyVar(gw_level)){
    plot_object <-
        AddToGsplot(plot_object, getGwPlotConfig(gw_level))
  }
    
  # corrections have been pulled out of primaryData and are their own top level object. Need to get it's own style info
  # and add to plot. NOTE: this is out of original order for now.
  plot_object <- AddToGsplot(plot_object, getCorrectionsPlotConfig(corrections, timeInformation[['start']], timeInformation[['end']], 
        uvInfo[['label']], lims))
  
  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- ApplyApprovalBarStyles(plot_object, approvalBars)
  
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
#' @param timeInformation time information about the data on the plot, see parseUvTimeInformationFromLims for information
#' @param secondarySeriesList named list of timeseries to be plotted. Also includes log/invert axis info.
#' @param approvalBars bars to be plotted which show approval level of data
#' @param effective_shift_pts effective shift data to be plotted (list of points)
#' @param meas_shift measured shift data to be plotted (list of points)
#' @param gage_height measured gage height data to be plotted (list of points)
#' @param corrections data correction information be plotted (time/correction pairs)
#' @param lims x/y lims which should contain all data above
#' @param timezone timezone to plot/display in
#' @param excludeZeroNegativeFlag flag to exclude ploting of values <= 0
#' @param tertiary_label label for the 3rd data item plotted
#' @return fully configured gsPlot object ready to be plotted
createSecondaryPlot <- function(uvInfo, timeInformation, secondarySeriesList, 
    approvalBars, effective_shift_pts, 
    meas_shift, gage_height,
    corrections, lims, timezone, excludeZeroNegativeFlag, tertiary_label=""){
  plot_object <- NULL
  
  invertPlot <- secondarySeriesList[['inverted']]
  
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]
  
  plot_object <- gsplot(yaxs = 'r') %>%
    view(
      xlim = c(startDate, endDate),
      ylim = calculateYLim(secondarySeriesList[['corrected']][['points']][['value']], 
          secondarySeriesList[['uncorrected']][['points']][['value']])
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
        timezone, getSecondaryPlotConfig, list(uvInfo[['label']]), excludeZeroNegativeFlag)
  }
  
  #estimated data
  if(!isEmptyOrBlank(secondarySeriesList[['estimated']]) && !isEmptyVar(secondarySeriesList[['estimated']][['points']])) {
    plot_object <- plotTimeSeries(plot_object, secondarySeriesList[['estimated']], "estimated", 
        timezone, getSecondaryPlotConfig, list(uvInfo[['label']]), excludeZeroNegativeFlag)
  }
  
  #corrected data
  plot_object <- plotTimeSeries(plot_object, secondarySeriesList[['corrected']], "corrected", 
      timezone, getSecondaryPlotConfig, list(uvInfo[['label']]), excludeZeroNegativeFlag)

  #effective shift
  if(!isEmptyVar(effective_shift_pts)){
    plot_object <- 
        AddToGsplot(plot_object, 
            getEffectiveShiftPlotConfig(effective_shift_pts, uvInfo[['label']], tertiary_label)
        )
  }
  
  if(!isEmptyVar(gage_height)){
    plot_object <- 
        AddToGsplot(plot_object, 
            getGageHeightPlotConfig(gage_height)
        )
  }
  
  if(!isEmptyVar(meas_shift)){
    plot_object <- 
        AddToGsplot(plot_object, 
            getMeasuredShiftPlotConfig(meas_shift)
        )
  }
  
  # corrections have been pulled out of primaryData and are their own top level object. Need to get it's own style info
  # and add to plot. NOTE: this is out of original order for now.
  plot_object <- AddToGsplot(plot_object, getCorrectionsPlotConfig(corrections, startDate, endDate, 
          uvInfo[['label']], lims))
  
  plot_object <- ApplyApprovalBarStyles(plot_object, approvalBars)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  legend_items <- plot_object[['legend']][['legend.auto']][['legend']]
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)

  plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                            legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=timeInformation[['dates']], lty=3, col="gray")
  
  # add this in once gsplot can handle logging different sides.
  # sec_logAxis <- isLogged(pts, reportObject[["secondDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  # if(sec_logAxis){
  #   plot_object <- view(plot_object, side=2, log='y')
  # }
  
  isShift <- !isEmptyVar(effective_shift_pts)
  if(isShift){
    yMax = max(effective_shift_pts[['value']])
    yMin = min(effective_shift_pts[['value']])
    y_seq <- pretty(c(yMin, yMax), shrink.sml = 20)
    plot_object <- plot_object %>% 
        mtext(paste0(tertiary_label, " (", uvInfo[['units']], ")"), 
            side = 4, line = 1.5) %>% 
        axis(side=4, las=0, at=y_seq, reverse = invertPlot)
    
    # add this in once gsplot can handle logging different sides.
    # tertiary_logAxis <- isLogged(pts, reportObject[["thirdDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
    # if(tertiary_logAxis){
    #   plot_object <- view(plot_object, side=4, log='y')
    # }
  }
  
  plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)[['side.1']])
  
  return(plot_object)
}

#' Compute the y lims, y-lims will ensure all of the corrected points are shown, but not necessarily all of the uncorrected points
#' @param corr.value.sequence A sequence of corrected time series points.
#' @param uncorr.value.sequence A sequence of uncorrected time series points.
#' @return The y-lim
calculateYLim <- function(corr.value.sequence, uncorr.value.sequence) {
  bufferPercent <- .30 #percent of corrected range allowed to extend to include uncorrected points. If lims of uncorrected points within percent. 
  
  min.corr.value <- min(corr.value.sequence, na.rm = TRUE)
  min.uncorr.value <- min(uncorr.value.sequence, na.rm = TRUE)
  
  if (min.corr.value <= min.uncorr.value || min.uncorr.value < (1 - bufferPercent) * min.corr.value) {
    y.bottom <- min.corr.value 
  } else {
    y.bottom <- min.uncorr.value 
  }
  
  max.corr.value <- max(corr.value.sequence, na.rm = TRUE)
  max.uncorr.value <- max(uncorr.value.sequence, na.rm = TRUE)
  
  if (max.corr.value >= max.uncorr.value || max.uncorr.value > (1 + bufferPercent) * max.corr.value) {
    y.top <- max.corr.value   
  } else {
    y.top <- max.uncorr.value 
  }
  
  return(c(y.bottom, y.top))
}

#' Calculate Limits And Sides
#' @description Depending on the configuration, reference and comparison series will be plotted on different sides. This constructs ylims and what side each series should be on
#' @param primarySeriesList list of timeseries available for reporting
#' @param uvInfo main timeseries information for the plot
#' @param refInfo timeseries information for reference series
#' @param compInfo timeseries information for comparios series
#' @return named list with two items, sides and ylims. Each item has a named list with items for each timeseries. (EG: side for reference would be returnedObject$sides$reference)
calculateLimitsAndSides <- function(primarySeriesList, uvInfo, refInfo, compInfo) {
  referenceExist <- !isEmptyOrBlank(primarySeriesList[['corrected_reference']])
  comparisonExist <- !isEmptyOrBlank(primarySeriesList[['comparison']])
  
  ylimPrimaryData <- primarySeriesList[['corrected']][['points']][['value']]
  ylimReferenceData <- primarySeriesList[['corrected_reference']][['points']][['value']]
  ylimCompData <- primarySeriesList[['comparison']][['points']][['value']]
  
  primarySide <- 2
  referenceSide <- 4
  comparisonSide <- 6
  
  #Setup limits and sides based on TS properties
  if(referenceExist){
    if(uvInfo[['type']] == refInfo[['type']]){
      referenceSide <- primarySide
      ylimPrimaryData <- append(ylimPrimaryData, ylimReferenceData)
      ylimReferenceData <- ylimPrimaryData
    }
  } else {
    referenceSide <- 0
  }
  
  if(comparisonExist){
    if(compInfo[['type']] == uvInfo[['type']]){
      comparisonSide <- primarySide
      ylimPrimaryData <- append(ylimPrimaryData, ylimCompData)
      ylimCompData <- ylimPrimaryData
      
      if(referenceSide == primarySide){
        ylimReferenceData <- ylimPrimaryData
      }
    } else if(referenceExist && compInfo[['type']] == refInfo[['type']]) {
      comparisonSide <- referenceSide
      ylimReferenceData <- append(ylimReferenceData, ylimCompData)
      ylimCompData <- ylimReferenceData
    } else if(!referenceExist || referenceSide == primarySide) {
      comparisonSide <- 4
    }
  } else {
    comparisonSide <- 0
  }
  
  ylims <- data.frame(primary=calculateYLim(ylimPrimaryData, primarySeriesList[['uncorrected']][['points']][['value']]), 
      reference=calculateYLim(ylimReferenceData, primarySeriesList[['corrected_reference']][['points']][['value']]), 
      comparison=calculateYLim(ylimCompData, ylimCompData))
  sides <- data.frame(primary=primarySide, reference=referenceSide, comparison=comparisonSide)
  
  return(list(ylims=ylims, sides=sides))
}


#' Get Primary Plot Config
#' @description Given a list of TS points, some information about the plot to build, will return a named list of gsplot elements to call
#' @param timeseries list of TS points relavant to primary plot
#' @param name name of item being plotted
#' @param primary_lbl label of primary time series
#' @param reference_lbl label of refernce time series
#' @param comp_lbl label of comparison time series
#' @param dataSides named list of integers describing what side of the plot correspondingly named series goes on
#' @param dataLimits named list of limits for name series
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getPrimaryPlotConfig <- function(timeseries, name, primary_lbl, 
    reference_lbl, comp_lbl, dataSides, dataLimits) {
  styles <- getUvStyles()
  
  x <- timeseries[['time']]
  y <- timeseries[['value']]
  
  compAxes <- TRUE
  compAnnotations <- TRUE
  compLabel <- primary_lbl
  
  if(dataSides[['comparison']] == 6){
    compAxes <- FALSE
    compAnnotations <- FALSE
    compLabel <- comp_lbl
  } else if(dataSides[['comparison']] == 4 && (dataSides[['reference']] != 4)){
    compLabel <- comp_lbl
  }
  
  plotConfig <- switch(name,
      corrected = list(
          lines = append(list(x=x, y=y, ylim=dataLimits[['primary']], ylab=primary_lbl, legend.name=paste(styles[['corr_UV_lbl']], primary_lbl)), styles[['corr_UV_lines']])
          ),
      estimated = list(
          lines = append(list(x=x, y=y, legend.name=paste(styles[['est_UV_lbl']], primary_lbl)), styles[['est_UV_lines']])
          ),
      uncorrected = list(
          lines = append(list(x=x, y=y, legend.name=paste(styles[['uncorr_UV_lbl']], primary_lbl)), styles[['uncorr_UV_lines']])
          ),
      comparison = list(
          lines = append(list(x=x, y=y, ylim=dataLimits[['comparison']], side=dataSides[['comparison']], axes=compAxes, ylab=compLabel, ann=compAnnotations, legend.name=comp_lbl), styles[['comp_UV_lines']])
          ), 
      corrected_reference = list(
          lines = append(list(x=x,y=y, ylim=dataLimits[['reference']], side=dataSides[['reference']], ylab=reference_lbl, legend.name=paste(styles[['corr_UV_Qref_lbl']], reference_lbl)), styles[['corr_UV_Qref_lines']])
          ),
      estimated_reference = list(
          lines = append(list(x=x,y=y, side=dataSides[['reference']], legend.name=paste(styles[['est_UV_Qref_lbl']], reference_lbl)), styles[['est_UV_Qref_lines']])
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
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getSecondaryPlotConfig <- function(timeseries, name, legend_label) {
  styles <- getUvStyles()
  
  x <- timeseries[['time']]
  y <- timeseries[['value']]
  
  plotConfig <- switch(name,
      corrected = list(
          lines = append(list(x=x,y=y, legend.name=paste(styles[['corr_UV_lbl']], legend_label)), styles[['corr_UV2_lines']])
      ), 
      estimated = list(
          lines = append(list(x=x,y=y,legend.name=paste(styles[['est_UV_lbl']], legend_label)), styles[['est_UV2_lines']])
      ),
      uncorrected = list(
          lines = append(list(x=x,y=y, legend.name=paste(styles[['uncorr_UV_lbl']], legend_label)), styles[['uncorr_UV2_lines']])
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
#' @return named list of gsplot calls. The name is the plotting call to make, and it points to a list of config params for that call
getMeasQPlotConfig <- function(meas_Q) {
  styles <- getUvStyles()
  
  x <- meas_Q[['time']]
  y <- meas_Q[['value']]
  
  plotConfig <- list(
          error_bar=append(list(x=x, y=y, y.low=(y-meas_Q[['minQ']]), y.high=(meas_Q[['maxQ']]-y)), styles[['meas_Q_error_bars']]),
          points=append(list(x=x, y=y), styles[['meas_Q_points']]),
          callouts=append(list(x=x, y=y, labels = meas_Q[['n']]), styles[['meas_Q_callouts']])
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
          error_bar=append(list(x=x, y=y, y.low=readings[['uncertainty']], y.high=readings[['uncertainty']]), styles[['ref_readings_error_bars']])
      ),
      csg = list(
          points=append(list(x=x, y=y), styles[['csg_readings_points']]), 
          error_bar=append(list(x=x, y=y, y.low=readings[['uncertainty']], y.high=readings[['uncertainty']]), styles[['csg_readings_error_bars']])
      ),
      hwm = list(
          points=append(list(x=x, y=y), styles[['hwm_readings_points']]), 
          error_bar=append(list(x=x, y=y, y.low=readings[['uncertainty']], y.high=readings[['uncertainty']]), styles[['hwm_readings_error_bars']])
      ),
      stop(paste(reading_type, " config not found for reference reading plotting"))
  )
  
  return(plotConfig)
}

#' Get DV Plot Config
#' @description Given a DV plot item, will return plotting config styled to the given level
#' @param level the appr level label (approved_dv, inreview_dv, working_dv)
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
      inreview_dv = list(
          points = append(list(x=x, y=y, pch=point_type, legend.name=legend.name), styles[['inreview_dv_points']])
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
      error_bar=append(list(x=x, y=y, y.low=(y-meas_shift[['minShift']]), y.high=(meas_shift[['maxShift']]-y)), styles[['meas_shift_error_bars']])
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
  
  corrPositions <- getCorrectionPositions(corrections)
  correctionLabels <- parseCorrectionsLabelSpacing(corrections, limits)
  corrArrows <- getCorrectionArrows(correctionLabels)
  
  plotConfig <- list(
      lines=append(list(x=0, y=0, xlim = c(plotStartDate, plotEndDate)), styles[['corrections_lines']]),
      abline=append(list(v=corrPositions, legend.name=paste(styles[['corrections_correction_lbl']], label)), styles[['corrections_ablines']]),
      arrows=append(list(x0=corrArrows[['xorigin']], x1=corrArrows[['x']], y0=corrArrows[['y']], y1=corrArrows[['y']]), styles[['corrections_arrows']]),
      points=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], cex=correctionLabels[['r']]), styles[['corrections_points']]),
      text=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], labels=correctionLabels[['label']]), styles[['corrections_text']])
  )
  
  return(plotConfig)
}
