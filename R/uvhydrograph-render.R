#' Create a Unit Values Hydrograph
#'
#' @param reportObject full UV hydro report data structure.
#' @export
#' @rdname uvhydrographPlot
uvhydrographPlot <- function(reportObject) {
  options(scipen=8) # less likely to give scientific notation
  
  months <- getMonths(reportObject)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  timezone <- fetchReportMetadataField(reportObject, "timezone") 
  
  if(!is.null(months)){
    for (month in months) {
      primary <- getPrimaryReportElements(reportObject, month)
      
      if(!is.null(primary$plot)){
        secondary <- getSecondaryReportElements(reportObject, month, timezone)
      } else {
        secondary <- list()
      }
      
      renderList[[month]] <- list(plot1=primary$plot,
                                  table1=primary$table, 
                                  status_msg1=primary$status_msg,
                                  plot2=secondary$plot, 
                                  table2=secondary$table,
                                  status_msg2=secondary$status_msg)
    }
  } else {
    renderList[[1]] <- list(plot1=NULL, table1=NULL, plot2=NULL, table2=NULL)
  }
  
  return(renderList)
  
}

#' TODO documentation
getPrimaryReportElements <- function(reportObject, month) {
  primary_status_msg <- NULL
  primaryPlot <- NULL
  primaryTable <- NULL
  
  primaryData <- parsePrimaryUVData(reportObject, month)
  
  primaryLims <- calculatePrimaryLims(primaryData, isPrimaryDischarge(reportObject))
  
  primaryInfo <- parseSupplementalPrimaryInfo(reportObject, primaryData, primaryLims)
  
  if('corr_UV' %in% names(primaryData)){ #if primary corrected UV exists
    corrections <- readCorrectionsByMonth(reportObject, "primarySeriesCorrections", month)
    primaryPlot <- createPrimaryPlot(primaryData, primaryInfo, corrections, primaryLims)
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
  
  hasReferenceSeries <- any(grepl("referenceSeries", names(reportObject)))
  hasUpchainSeries <- any(grepl("upchainSeries", names(reportObject)))
  
  if((hasReferenceSeries && !any(grepl("Discharge", fetchReportMetadataField(reportObject,'primaryParameter')))) || hasUpchainSeries) {
    if(hasReferenceSeries) {
      corrections <- readCorrectionsByMonth(reportObject, "referenceSeriesCorrections", month)
    } else {
      corrections <- readCorrectionsByMonth(reportObject, "upchainSeriesCorrections", month)
    }
    secondaryData <- parseSecondaryUVData(reportObject, month)
    secondarySeriesList <- getSecondarySeriesList(reportObject, month, timezone)
    if(!isEmptyOrBlank(secondarySeriesList$corrected)){ #if corrected data exists
      secondaryLims <- calculateLims(secondarySeriesList$corrected)
      secondaryInfo <- parseSecondarySupplementalInfo(reportObject, secondaryData, secondaryLims)
      secondaryPlot <- createSecondaryPlot(secondaryData, secondaryInfo, secondarySeriesList, corrections, secondaryLims)
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
createPrimaryPlot <- function(primaryData, primaryInfo, corrections, lims){ 
  # assume everything is NULL unless altered
  plot_object <- NULL

  correctedExist <- 'corr_UV' %in% names(primaryData)
  referenceExist <- 'corr_UV_Qref' %in% names(primaryData)
  comparisonExist <- 'comp_UV' %in% names(primaryData)
  
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
  # if(isLogged(data, ylimPrimaryData, primaryInfo$primarySeries)){
  #   plot_object <- view(plot_object, side=primarySide, log='y')
  # }
  # if(comparisonExist && isLogged(data, ylimComparisonData, primaryInfo$comparisonSeries)){
  #   plot_object <- view(plot_object, side=comparisonSide, log='y')
  # }
  # if(referenceExist && isLogged(data, ylimReferenceData, primaryInfo$referenceSeries)){
  #   plot_object <- view(plot_object, side=referenceSide, log='y')
  # }
    
  # reorder so that uncorrected is below corrected (plot uncorrected first)
  primaryData <-
    primaryData[c(grep("^uncorr_UV$", names(primaryData)),
                  grep("^uncorr_UV$", names(primaryData), invert = TRUE))]
  
  # add data to plot
  # TODO: Once we've fully deconstructed primaryData into individual calls, this loop might become single explicit calls
  for (i in grep("^appr_.+_uv", names(primaryData), invert = TRUE)) {
    plot_object <-
        AddToGsplot(plot_object, getPrimaryPlotConfig(primaryData[i], primaryInfo$plotStartDate, primaryInfo$plotEndDate, 
                primaryInfo$primary_lbl, primaryInfo$reference_lbl, paste("Comparison", primaryInfo$comp_UV_TS_lbl, "@", primaryInfo$comp_UV_lbl),
                sides, ylims, lims))
  }
  
  # corrections have been pulled out of primaryData and are their own top level object. Need to get it's own style info
  # and add to plot. NOTE: this is out of original order for now.
  plot_object <- AddToGsplot(plot_object, getCorrectionsPlotConfig(corrections, primaryInfo$plotStartDate, primaryInfo$plotEndDate, 
        primaryInfo$primary_lbl, lims))
  
  # approval bar styles are applied last, because it makes it easier to align
  # them with the top of the x-axis line
  plot_object <- ApplyApprovalBarStyles(plot_object, primaryData)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  legend_items <- plot_object$legend$legend.auto$legend
  ncol <- ifelse(length(legend_items) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(legend_items) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
  plot_object <- legend(plot_object, location="below", title="", ncol=ncol, 
                    legend_offset=legend_offset, cex=0.8, y.intersp=1.5) %>% 
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray", where='first') %>%
    abline(v=primaryInfo$plotDates, lty=3, col="gray", where='first')
  
  plot_object <- testCallouts(plot_object, xlimits = xlim(plot_object)$side.1)
  
  return(plot_object)
}

#' TODO documentation
#' @importFrom lubridate hours
#' @importFrom lubridate minutes
createSecondaryPlot <- function(secondaryData, secondaryInfo, secondarySeriesList, corrections, lims){
  plot_object <- NULL
  
  plotEndDate <- tail(secondaryInfo$plotDates,1) + hours(23) + minutes(45)
  plotStartDate <- secondaryInfo$plotDates[1]

  secondaryInfo$plotStartDate <- plotStartDate
  secondaryInfo$plotEndDate <- plotEndDate

  plot_object <- gsplot(yaxs = 'r') %>%
    view(
      xlim = c(plotStartDate, plotEndDate),
      ylim = YAxisInterval(secondarySeriesList$corrected$value, secondarySeriesList$uncorrected$value)
    ) %>%
    axis(side = 1, at = secondaryInfo$plotDates, labels = as.character(secondaryInfo$days)) %>%
    axis(side = 2, reverse = secondaryInfo$isInverted, las = 0) %>%
    title(
      main = "",
      xlab = paste("UV Series:", secondaryInfo$date_lbl),
      ylab = secondaryInfo$secondary_lbl
    )
  
  # add data to plot
  for (i in grep("^appr_.+_uv", names(secondaryData), invert = TRUE)) {
    # TODO: try to factor out NULL arguments to PlotUVHydrographObject() below
    plot_object <-
        AddToGsplot(plot_object, 
            getSecondaryPlotConfig(secondaryData[i],   
                secondaryData[i][[1]]$time,
                secondaryData[i][[1]]$value, 
                secondaryInfo$plotStartDate, secondaryInfo$plotEndDate, 
                secondaryInfo$secondary_lbl, secondaryInfo$tertiary_lbl, lims)
            )
  }
  
  #corrected data
  plot_object <- 
      AddToGsplot(plot_object, 
          getSecondaryPlotConfig(list(corrected=secondarySeriesList$corrected), 
              secondarySeriesList$corrected$time, secondarySeriesList$corrected$value,
              secondaryInfo$plotStartDate, secondaryInfo$plotEndDate, 
              secondaryInfo$secondary_lbl, secondaryInfo$tertiary_lbl, lims)
      )
  
  #estimated data
  if(!isEmptyVar(secondarySeriesList$estimated)) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getSecondaryPlotConfig(list(estimated=secondarySeriesList$estimated), 
                secondarySeriesList$estimated$time, secondarySeriesList$estimated$value,
                secondaryInfo$plotStartDate, secondaryInfo$plotEndDate, 
                secondaryInfo$secondary_lbl, secondaryInfo$tertiary_lbl, lims)
        )
  }
  
  #uncorrected data
  if(!isEmptyVar(secondarySeriesList$uncorrected)) {
    plot_object <- 
        AddToGsplot(plot_object, 
            getSecondaryPlotConfig(list(uncorrected=secondarySeriesList$uncorrected),
                secondarySeriesList$uncorrected$time, secondarySeriesList$uncorrected$value,
                secondaryInfo$plotStartDate, secondaryInfo$plotEndDate, 
                secondaryInfo$secondary_lbl, secondaryInfo$tertiary_lbl, lims)
        )
  }

  # corrections have been pulled out of primaryData and are their own top level object. Need to get it's own style info
  # and add to plot. NOTE: this is out of original order for now.
  plot_object <- AddToGsplot(plot_object, getCorrectionsPlotConfig(corrections, secondaryInfo$plotStartDate, secondaryInfo$plotEndDate, 
          secondaryInfo$secondary_lbl, lims))
  
  plot_object <- ApplyApprovalBarStyles(plot_object, secondaryData)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
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
  
  x <- primaryPlotItem[[1]]$time
  y <- primaryPlotItem[[1]]$value
  
  legend.name <- primaryPlotItem[[1]]$legend.name
  
  compAxes <- TRUE
  compAnnotations <- TRUE
  compLabel <- primary_lbl
  
  if(dataSides$comparison == 6){
    compAxes <- FALSE
    compAnnotations <- FALSE
    compLabel <- comp_lbl
  } else if(dataSides$comparison == 4 && (dataSides$reference != 4)){
    compLabel <- comp_lbl
  }
  
  plotConfig <- switch(names(primaryPlotItem),
      corr_UV = list(
          lines = append(list(x=x, y=y, ylim=dataLimits$primary, ylab=primary_lbl, legend.name=paste(styles$corr_UV_lbl, primary_lbl)), styles$corr_UV_lines)
          ),
      est_UV = list(
          lines = append(list(x=x, y=y, legend.name=paste(styles$est_UV_lbl, primary_lbl)), styles$est_UV_lines)
          ),
      uncorr_UV = list(
          lines = append(list(x=x, y=y, legend.name=paste(styles$uncorr_UV_lbl, primary_lbl)), styles$uncorr_UV_lines)
          ),
      comp_UV = list(
          lines = append(list(x=x, y=y, ylim=dataLimits$comparison, side=dataSides$comparison, axes=compAxes, ylab=compLabel, ann=compAnnotations, legend.name=comp_lbl), styles$comp_UV_lines)
          ), 
      corr_UV_Qref = list(
          lines = append(list(x=x,y=y, ylim=dataLimits$reference, side=dataSides$reference, ylab=reference_lbl, legend.name=paste(styles$corr_UV_Qref_lbl, reference_lbl)), styles$corr_UV_Qref_lines)
          ),
      est_UV_Qref = list(
          lines = append(list(x=x,y=y, side=dataSides$reference, legend.name=paste(styles$est_UV_Qref_lbl, reference_lbl)), styles$est_UV_Qref_lines)
          ),
      water_qual = list(
          points = append(list(x=x, y=y), styles$water_qual_points)
          ), 
      meas_Q = list(
          error_bar=append(list(x=x, y=y, y.low=(y-primaryPlotItem$meas_Q$minQ), y.high=(primaryPlotItem$meas_Q$maxQ-y)), styles$meas_Q_error_bars),
          points=append(list(x=x, y=y), styles$meas_Q_points),
          callouts=append(list(x=x, y=y, labels = primaryPlotItem$meas_Q$n), styles$meas_Q_callouts)
          ),
      ref_readings = list(
          points=append(list(x=x, y=y), styles$ref_readings_points), 
          error_bar=append(list(x=x, y=y, y.low=primaryPlotItem$ref_readings$uncertainty, y.high=primaryPlotItem$ref_readings$uncertainty), styles$ref_readings_error_bars)
          ),
      csg_readings = list(
          points=append(list(x=x, y=y), styles$csg_readings_points), 
          error_bar=append(list(x=x, y=y, y.low=primaryPlotItem$csg_readings$uncertainty, y.high=primaryPlotItem$csg_readings$uncertainty), styles$csg_readings_error_bars)
          ),
      hwm_readings = list(
          points=append(list(x=x, y=y), styles$hwm_readings_points), 
          error_bar=append(list(x=x, y=y, y.low=primaryPlotItem$hwm_readings$uncertainty, y.high=primaryPlotItem$hwm_readings$uncertainty), styles$hwm_readings_error_bars)
          ),
      appr_approved_dv = list(
          points = append(list(x=x, y=y, pch=primaryPlotItem[[1]]$point_type, legend.name=legend.name), styles$appr_approved_dv_points)
          ),
      appr_inreview_dv = list(
          points = append(list(x=x, y=y, pch=primaryPlotItem[[1]]$point_type, legend.name=legend.name), styles$appr_inreview_dv_points)
          ),
      appr_working_dv = list(
          points = append(list(x=x, y=y, pch=primaryPlotItem[[1]]$point_type, legend.name=legend.name), styles$appr_working_dv_points)
          ),
      stop(paste(names(primaryPlotItem), " config not found for primary plot"))
  )
  
  return(plotConfig)
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
  
  x <- corrections[1,]$time
  y <- corrections[1,]$value
  correctionLabels <- parseCorrectionsLabelSpacing(corrections, limits)
  
  corrArrowPositions <- getCorrectionArrowPositions(x, y, correctionLabels)
  corrAblinePositions <- getCorrectionAbLinesPositions(corrections)
  
  plotConfig <- list(
    lines=append(list(x=0, y=0, xlim = c(plotStartDate, plotEndDate)), styles$corrections_lines),
    abline=append(list(v=corrAblinePositions$time, legend.name=paste(styles$corrections_correction_lbl, label)), styles$corrections_ablines),
    arrows=append(list(x0=corrArrowPositions$xorigin, x1=corrArrowPositions$x, y0=corrArrowPositions$y, y1=corrArrowPositions$y), styles$corrections_arrows),
    points=append(list(x=correctionLabels$x, y=correctionLabels$y, cex=correctionLabels$r), styles$corrections_points),
    text=append(list(x=correctionLabels$x, y=correctionLabels$y, labels=correctionLabels$label), styles$corrections_text)
  )
  
  return(plotConfig)
}

#' Get Secondary Plot Config
#' @description Given a report object, some information about the plot to build, will return a named list of gsplot elements to call
#' @param secondaryPlotItem plot item, must be in named list to match with the style
#' @param x the x/time values to put into the gsplot calls
#' @param y the y/time values to put into the gsplot calls
#' @param plotStartDate start date of this plot 
#' @param plotEndDate end date of this plot
#' @param secondary_lbl label of secondary time series
#' @param tertiary_lbl label of tertiary time series
#' @param limits list of lims for all of the data which will be on here
#' @importFrom grDevices rgb
getSecondaryPlotConfig <- function(secondaryPlotItem, x, y, plotStartDate, plotEndDate, secondary_lbl, tertiary_lbl, limits) {
  styles <- getUvStyles()
  
  plotConfig <- switch(names(secondaryPlotItem),
      corrected = list(
          lines = append(list(x=x,y=y, legend.name=paste(styles$corr_UV2_lbl, secondary_lbl)), styles$corr_UV2_lines)
          ), 
      estimated = list(
          lines = append(list(x=x,y=y,legend.name=paste(styles$est_UV2_lbl, secondary_lbl)), styles$est_UV2_lines)
          ),
      uncorrected = list(
          lines = append(list(x=x,y=y, legend.name=paste(styles$uncorr_UV2_lbl, secondary_lbl)), styles$uncorr_UV2_lines)
          ),                
      effect_shift = list(
          lines=append(list(x=x,y=y, legend.name=paste(secondary_lbl, tertiary_lbl)), styles$effect_shift_lines),
          text=append(list(x=x[1], y=y[1]), styles$effect_shift_text)
          ),
      gage_height = list(
          points=append(list(x=x, y=y), styles$gage_height_points),
          callouts=list(x=x, y=y, labels=secondaryPlotItem$gage_height$n)
          ),
      gw_level = list(points = append(list(x=x,y=y), styles$gw_level_points)), 
      meas_shift = list(
          points=append(list(x=x, y=y), styles$meas_shift_points),
          error_bar=append(list(x=x, y=y, y.low=(y-secondaryPlotItem$meas_shift$minShift), y.high=(secondaryPlotItem$meas_shift$maxShift-y)), styles$meas_shift_error_bars)
          ),
      stop(paste(names(secondaryPlotItem), " config not found for secondary plot"))
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
        mutate(x = ifelse(x > xorigin, x - 60 * 60 * 2.85 * correctionLabels$r, x + 60 * 60 * 2.85 * correctionLabels$r)) %>% 
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
    corrAblinePositions <- corrections[which(!duplicated(corrections$time)),]
  }
  
  return(corrAblinePositions)
}
