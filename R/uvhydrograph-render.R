#' Create a Unit Values Hydrograph
#'
#' @param reportObject full UV hydro report data structure.
#' @export
#' @rdname uvhydrographPlot
uvhydrographPlot <- function(reportObject) {
  options(scipen=8) # less likely to give scientific notation
  
  timezone <- fetchReportMetadataField(reportObject, "timezone") 
  
  months <- getMonths(reportObject, timezone)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  
  if(!is.null(months)){
    for (month in months) {
      primary <- getPrimaryReportElements(reportObject, month, timezone)
      
      if(!is.null(primary[['plot']])){
        secondary <- getSecondaryReportElements(reportObject, month, timezone)
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

#' TODO documentation
getPrimaryReportElements <- function(reportObject, month, timezone) {
  primary_status_msg <- NULL
  primaryPlot <- NULL
  primaryTable <- NULL
  
  corrections <- readCorrectionsByMonth(reportObject, "primarySeriesCorrections", month)
  primarySeriesList <- getPrimarySeriesList(reportObject, month, timezone)
  
  if(!isEmptyOrBlank(primarySeriesList[['corrected']]) && !isEmptyVar(primarySeriesList[['corrected']])){ #if primary corrected UV exists
    primaryLims <- calculatePrimaryLims(primarySeriesList, isPrimaryDischarge(reportObject))
    
    primaryPlot <- createPrimaryPlot(
        getTimeSeriesUvInfo(reportObject, "primarySeries"),
        getTimeSeriesUvInfo(reportObject, "referenceSeries"),
        getTimeSeriesUvInfo(reportObject, "comparisonSeries"),
        getUvTimeInformationFromLims(primaryLims, timezone),
        primarySeriesList,
        getPrimaryDvList(reportObject, month, timezone),
        readUvQMeasurements(reportObject, month),
        readUvWq(reportObject, month),
        readUvGwLevel(reportObject, month),
        readAllUvReadings(reportObject, month),
        readPrimaryUvHydroApprovalBars(reportObject, timezone, month), 
        corrections, 
        primaryLims)
    primaryTable <- correctionsAsTable(corrections)
  } else {
    primary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'primaryParameter'))
  }
  return(list(plot=primaryPlot, table=primaryTable, status_msg=primary_status_msg))
}

#' TODO documentation
getSecondaryReportElements <- function(reportObject, month, timezone) {
  secondary_status_msg <- NULL
  secondaryPlot <- NULL
  secondaryTable <- NULL
  
  hasReferenceSeries <- hasReferenceSeries(reportObject)
  hasUpchainSeries <- hasUpchainSeries(reportObject)
  
  if((hasReferenceSeries && !isPrimaryDischarge(reportObject)) || hasUpchainSeries) {
    if(hasReferenceSeries) {
      corrections <- readCorrectionsByMonth(reportObject, "referenceSeriesCorrections", month)
    } else {
      corrections <- readCorrectionsByMonth(reportObject, "upchainSeriesCorrections", month)
    }
    secondarySeriesList <- getSecondarySeriesList(reportObject, month, timezone)
    if(!isEmptyOrBlank(secondarySeriesList[['corrected']])){ #if corrected data exists
      secondaryLims <- calculateLims(secondarySeriesList[['corrected']])
      secondaryPlot <- createSecondaryPlot( 
          getSecondaryTimeSeriesUvInfo(reportObject, timezone, month),
          getUvTimeInformationFromLims(secondaryLims, timezone),
          secondarySeriesList, 
          readSecondaryUvHydroApprovalBars(reportObject, timezone, month), 
          readEffectiveShifts(reportObject, timezone, month),
          readUvMeasurementShifts(reportObject, month),
          readUvGageHeight(reportObject, month),
          corrections, 
          secondaryLims, 
          secondarySeriesList[['inverted']],
          tertiary_label=getTimeSeriesLabel(reportObject, "effectiveShifts"))
      secondaryTable <- correctionsAsTable(corrections)
    } else {
      secondary_status_msg <- paste('Corrected data missing for', fetchReportMetadataField(reportObject, 'secondaryParameter'))
    }
  }
  
  return(list(plot=secondaryPlot, table=secondaryTable, status_msg=secondary_status_msg))
}

#' TODO documentation
#' @importFrom lubridate hours
#' @importFrom lubridate minutes
createPrimaryPlot <- function( 
    uvInfo, refInfo, compInfo, timeInformation, 
    primarySeriesList, dailyValues, 
    meas_Q, water_qual, gw_level, readings, approvalBars, corrections, lims){ 
  # assume everything is NULL unless altered
  plot_object <- NULL
  
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]

  referenceExist <- !isEmptyVar(primarySeriesList[['corrected_reference']])
  comparisonExist <- !isEmptyVar(primarySeriesList[['comparison']])
  
  ylimPrimaryData <- primarySeriesList[['corrected']][['value']]
  ylimReferenceData <- primarySeriesList[['corrected_reference']][['value']]
  ylimCompData <- primarySeriesList[['comparison']][['value']]

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

  ylims <- data.frame(primary=YAxisInterval(ylimPrimaryData, primarySeriesList[['uncorrected']][['value']]), reference=YAxisInterval(ylimReferenceData, primarySeriesList[['corrected_reference']][['value']]), comparison=YAxisInterval(ylimCompData, ylimCompData))
  sides <- data.frame(primary=primarySide, reference=referenceSide, comparison=comparisonSide)

  plot_object <- gsplot(ylog = primarySeriesList[['loggedAxis']], yaxs = 'r') %>%
    view(xlim = c(startDate, endDate)) %>%
    axis(side = 1, at = timeInformation[['dates']], labels = as.character(timeInformation[['days']])) %>%
    axis(side = 2, reverse = primarySeriesList[['inverted']], las = 0) %>%
    lines(x=0, y=0, side = 2, reverse = primarySeriesList[['inverted']]) %>%
    title(
      main = format(timeInformation[['dates']][1], "%B %Y"),
      xlab = paste("UV Series:", paste(lims[['xlim']][1], "through", lims[['xlim']][2]))
    )

  #Don't add the right-side axis if we aren't actually plotting anything onto it
  if((referenceExist && referenceSide == 4) || (comparisonExist && comparisonSide == 4)){
    plot_object <- lines(plot_object, x=0, y=0, side = 4, reverse = primarySeriesList[['inverted']]) %>%
        axis(side = 4, las = 0, reverse = primarySeriesList[['inverted']])
  }
  
  if(!isEmptyVar(primarySeriesList[['corrected_reference']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getPrimaryPlotConfig(list(corrected_reference=primarySeriesList[['corrected_reference']]), 
                startDate, endDate, 
                NULL, refInfo[['label']], NULL, sides, ylims, lims)
        )
  }
  
  if(!isEmptyVar(primarySeriesList[['estimated_reference']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getPrimaryPlotConfig(list(estimated_reference=primarySeriesList[['estimated_reference']]), 
                startDate, endDate, 
                NULL, refInfo[['label']], NULL, sides, ylims, lims)
        )
  }
  
  if(!isEmptyVar(primarySeriesList[['comparison']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getPrimaryPlotConfig(list(comparison=primarySeriesList[['comparison']]), 
                startDate, endDate, 
                NULL, NULL, paste("Comparison", compInfo[['label']], "@", comparisonStation), sides, ylims, lims)
        )
  }
  #uncorrected data
  if(!isEmptyVar(primarySeriesList[['uncorrected']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getPrimaryPlotConfig(list(uncorrected=primarySeriesList[['uncorrected']]),
                startDate, endDate, 
                uvInfo[['label']], NULL, NULL, sides, ylims, lims)
        )
  }
  
  #estimated data
  if(!isEmptyVar(primarySeriesList[['estimated']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getPrimaryPlotConfig(list(estimated=primarySeriesList[['estimated']]), 
                startDate, endDate, 
                uvInfo[['label']], NULL, NULL, sides, ylims, lims)
        )
  }

  #corrected data
  plot_object <- 
    AddToGsplot(plot_object, 
        getPrimaryPlotConfig(list(corrected=primarySeriesList[['corrected']]), 
            startDate, endDate, 
            uvInfo[['label']], NULL, NULL, sides, ylims, lims)
    )

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
        AddToGsplot(plot_object, getPrimaryPlotConfig(list(water_qual=water_qual), startDate, endDate, 
                NULL, NULL, NULL,
                sides, ylims, lims))
  }
  
  #discharge measurement
  if(!isEmptyVar(meas_Q)){
    plot_object <-
        AddToGsplot(plot_object, getPrimaryPlotConfig(list(meas_Q=meas_Q), startDate, endDate, 
                NULL, NULL, NULL,
                sides, ylims, lims))
  }
  
  #gw_level
  if(!isEmptyVar(gw_level)){
    plot_object <-
        AddToGsplot(plot_object, getPrimaryPlotConfig(list(gw_level=gw_level), startDate, endDate, 
                NULL, NULL, NULL,
                sides, ylims, lims))
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

#' TODO documentation
#' @importFrom lubridate hours
#' @importFrom lubridate minutes
createSecondaryPlot <- function(uvInfo, timeInformation, secondarySeriesList, 
    approvalBars, effective_shift_pts, 
    meas_shift, gage_height,
    corrections, lims, invertPlot, tertiary_label=""){
  plot_object <- NULL
  
  startDate <- timeInformation[['start']]
  endDate <- timeInformation[['end']]
  
  plot_object <- gsplot(yaxs = 'r') %>%
    view(
      xlim = c(startDate, endDate),
      ylim = YAxisInterval(secondarySeriesList[['corrected']][['value']], secondarySeriesList[['uncorrected']][['value']])
    ) %>%
    axis(side = 1, at = timeInformation[['dates']], labels = as.character(timeInformation[['days']])) %>%
    axis(side = 2, reverse = invertPlot, las = 0) %>%
    title(
      main = "",
      xlab = paste("UV Series:", paste(lims[['xlim']][1], "through", lims[['xlim']][2])),
      ylab = uvInfo[['label']]
    )

  #uncorrected data
  if(!isEmptyVar(secondarySeriesList[['uncorrected']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getSecondaryPlotConfig("uncorrected", secondarySeriesList[['uncorrected']][['time']], secondarySeriesList[['uncorrected']][['value']], uvInfo[['label']])
        )
  }
  
  #estimated data
  if(!isEmptyVar(secondarySeriesList[['estimated']])) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getSecondaryPlotConfig("estimated", secondarySeriesList[['estimated']][['time']], secondarySeriesList[['estimated']][['value']], uvInfo[['label']])
        )
  }
  
  #corrected data
  plot_object <- 
      AddToGsplot(plot_object, 
          getSecondaryPlotConfig("corrected", secondarySeriesList[['corrected']][['time']], secondarySeriesList[['corrected']][['value']], uvInfo[['label']])
      )

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


#' Get Primary Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param primaryPlotItem list of data objects relavant to primary plot
#' @param plotStartDate start date of this plot 
#' @param plotEndDate end date of this plot
#' @param primary_lbl label of primary time series
#' @param reference_lbl label of refernce time series
#' @param comp_lbl label of comparison time series
#' @param dataSides named list of integers describing what side of the plot correspondingly named series goes on
#' @param dataLimits named list of limits for name series
#' @param limits list of limits for all data on the plot
#' @param limits list of lims for all of the data which will be on here
#' @importFrom grDevices rgb
getPrimaryPlotConfig <- function(primaryPlotItem, plotStartDate, plotEndDate, primary_lbl, 
    reference_lbl, comp_lbl, dataSides, dataLimits, limits) {
  styles <- getUvStyles()
  
  x <- primaryPlotItem[[1]][['time']]
  y <- primaryPlotItem[[1]][['value']]
  
  legend.name <- primaryPlotItem[[1]][['legend.name']]
  
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
  
  plotConfig <- switch(names(primaryPlotItem),
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
      water_qual = list(
          points = append(list(x=x, y=y), styles[['water_qual_points']])
          ), 
      meas_Q = list(
          error_bar=append(list(x=x, y=y, y.low=(y-primaryPlotItem[['meas_Q']][['minQ']]), y.high=(primaryPlotItem[['meas_Q']][['maxQ-y']])), styles[['meas_Q_error_bars']]),
          points=append(list(x=x, y=y), styles[['meas_Q_points']]),
          callouts=append(list(x=x, y=y, labels = primaryPlotItem[['meas_Q']][['n']]), styles[['meas_Q_callouts']])
          ),
      gw_level = list(points = append(list(x=x,y=y), styles[['gw_level_points']])), 
      stop(paste(names(primaryPlotItem), " config not found for primary plot"))
  )
  
  return(plotConfig)
}

#' Get Readings Plot Config
#' @description Given a readings list, will pull the desired plotting calls and styling
#' @param reading_type name of reading type to get style for (ref, csg, hwm)
#' @param readings list of readings objects relavant to primary plot
#' @importFrom grDevices rgb
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
#' @param primaryPlotItem list of data objects relavant to primary plot
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

#' Get Secondary Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param name name of style to be applied to given x/y points (corrected, estimated, or uncorrected)
#' @param x the x/time values to put into the gsplot calls
#' @param y the y/time values to put into the gsplot calls
#' @param legend_label label to be applied to points in legend
getSecondaryPlotConfig <- function(name, x, y, legend_label) {
  styles <- getUvStyles()
  
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

#' Get Effective Shift Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param effect_shift list of effective shift points
#' @param secondary_lbl label of secondary time series
#' @param tertiary_lbl label of tertiary time series
getEffectiveShiftPlotConfig <- function(effect_shift, secondary_lbl, tertiary_lbl) {
  styles <- getUvStyles()
  
  x <- effective_shift_pts[['time']]
  y <- effective_shift_pts[['value']]
  
  effective_shift_config = list(
      lines=append(list(x=x,y=y, legend.name=paste(secondary_lbl, tertiary_lbl)), styles[['effect_shift_lines']]),
      text=append(list(x=x[1], y=y[1]), styles[['effect_shift_text']])
  )
  
  return(effective_shift_config)
}

#' Get Gage Height Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param gage_height list of gage height records
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
#' @importFrom grDevices rgb
getCorrectionsPlotConfig <- function(corrections, plotStartDate, plotEndDate, label, 
    limits) {
  
  if(nrow(corrections) <= 0) {
    return(list())
  }
  styles <- getUvStyles()
  
  x <- corrections[1,][['time']]
  y <- corrections[1,][['value']]
  correctionLabels <- parseCorrectionsLabelSpacing(corrections, limits)
  
  corrArrowPositions <- getCorrectionArrowPositions(x, y, correctionLabels)
  corrAblinePositions <- getCorrectionAbLinesPositions(corrections)
  
  plotConfig <- list(
      lines=append(list(x=0, y=0, xlim = c(plotStartDate, plotEndDate)), styles[['corrections_lines']]),
      abline=append(list(v=corrAblinePositions[['time']], legend.name=paste(styles[['corrections_correction_lbl']], label)), styles[['corrections_ablines']]),
      arrows=append(list(x0=corrArrowPositions[['xorigin']], x1=corrArrowPositions[['x']], y0=corrArrowPositions[['y']], y1=corrArrowPositions[['y']]), styles[['corrections_arrows']]),
      points=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], cex=correctionLabels[['r']]), styles[['corrections_points']]),
      text=append(list(x=correctionLabels[['x']], y=correctionLabels[['y']], labels=correctionLabels[['label']]), styles[['corrections_text']])
  )
  
  return(plotConfig)
}

#' Get Correction Arrows and Labels
#' Given a list of times, values, and labels, will create data to draw arrows to the labels
#' @param time list of time(x) values
#' @param value list of values(y)
#' @param correcionLabels list of labels associated with each point
#' @return list of data describing how to draw lines to corresponding labels
getCorrectionArrowPositions <- function(time, value, correctionLabels) {
  x <- time
  y <- value
  
  corrArrowPositions <- list()
  
  #Make the correction label lines connect to the outside of the bounding box and not to the center of the label
  if(!isEmptyOrBlank(correctionLabels)){
    # work around irrelevant warnings from devtools::check()
    xorigin <- 0
    r <- NULL
    
    corrArrowPositions <- correctionLabels %>% as.data.frame() %>% select(x, xorigin, r, y) %>%
        mutate(x = ifelse(x > xorigin, x - 60 * 60 * 2.85 * correctionLabels[['r']], x + 60 * 60 * 2.85 * correctionLabels[['r']])) %>% 
        as.list()
  }
  
  return(corrArrowPositions)
}

#' Get AB line positions for Corrections
#' Given a list of corrections, will return the time(x) position of each
#' @param corrections a list of corrections
#' @return list of time/x for each correction 
getCorrectionAbLinesPositions <- function(corrections) {
  corrAblinePositions <- list()
  
  #Remove overlapping correction ablinesmy assum
  if(!isEmptyOrBlank(corrections)){
    corrAblinePositions <- corrections[which(!duplicated(corrections[['time']])),]
  }
  
  return(corrAblinePositions)
}
