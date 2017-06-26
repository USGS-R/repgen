#'@aliases parseCustomDataElementsForTemplate
#'@rdname parseCustomDataElementsForTemplate
setMethod("parseCustomDataElementsForTemplate", signature(reportData = "timeseriessummary"), 
    definition = function(reportData) {
      return(parseCustomDataElementsForTemplateForTimeSeriesSummary(reportData))
    }
)

#' parseCustomDataElementsForTemplateForTimeSeriesSummary
#' @description Will return a list of tables for the renderer
#' @param reportData full report data structure 
#' @return list of data elements for template
#' @importFrom jsonlite toJSON
parseCustomDataElementsForTemplateForTimeSeriesSummary <- function(reportData) {
  timezone <- fetchReportMetadataField(reportData, 'timezone')
  
  relatedSeriesTable <- formatDataTable(parseTSSRelatedSeries(reportData))
  
  gapsTable <- list()
  gapsTable[['gaps']] <- formatDataTable(parseTSSGaps(reportData, timezone))
  gapsTable[['tolerances']] <- formatDataTable(parseTSSGapTolerances(reportData, timezone))
  addNaNote <- any(do.call("rbind", gapsTable[['gaps']])[['startTime']] == "n/a*") ||  any(do.call("rbind", gapsTable[['gaps']])[['endTime']] == "n/a*")
  
  thresholdsTable <- formatDataTable(parseTSSThresholds(reportData, timezone))

  correctionsTable <- list()
  correctionsTable[['pre']] <- formatDataTable(parseTSSProcessingCorrections(reportData, "pre", timezone))
  correctionsTable[['normal']] <- formatDataTable(parseTSSProcessingCorrections(reportData, "normal", timezone))
  correctionsTable[['post']] <- formatDataTable(parseTSSProcessingCorrections(reportData, "post", timezone))
  
  corrUrl <- fetchCorrReportURL(reportData)
  
  thresholdsTable <- formatDataTable(parseTSSThresholds(reportData, timezone))
  
  ratingsTable <- list()
  ratingsTable[['curves']] <- formatDataTable(parseTSSRatingCurves(reportData, timezone))
  ratingsTable[['shifts']] <- formatDataTable(parseTSSRatingShifts(reportData, timezone))
  
  metadataTable <- list()
  metadataTable <- mergeLists(parseTSSQualifiers(reportData, timezone),parseTSSNotes(reportData, timezone))
  metadataTable <- mergeLists(metadataTable, parseTSSGrades(reportData, timezone))
  metadataTable <- data.frame(metadataTable)
  metadataTable <- formatDataTable(metadataTable)
  
  approvalsTable <- formatDataTable(parseTSSApprovals(reportData, timezone))
  
  return(list(
      relatedSeries = list(hasData=!isEmptyOrBlank(relatedSeriesTable), data=relatedSeriesTable),
      gaps = list(hasData=(!isEmptyOrBlank(gapsTable[['gaps']]) || !isEmptyOrBlank(gapsTable[['tolerances']])), data=gapsTable, addNaNote=addNaNote),
      corrections = list(hasData=(!isEmptyOrBlank(correctionsTable[['pre']]) || !isEmptyOrBlank(correctionsTable[['normal']]) || !isEmptyOrBlank(correctionsTable[['post']])), data=correctionsTable, corrUrl=corrUrl),
      thresholds = list(hasData=!isEmptyOrBlank(thresholdsTable), data=thresholdsTable),
      ratings = list(hasData=(!isEmptyOrBlank(ratingsTable[['curves']]) || !isEmptyOrBlank(ratingsTable[['shifts']])), data=ratingsTable),
      metadata = list(hasData=!isEmptyOrBlank(metadataTable), data=metadataTable),
      approvals = list(hasData=!isEmptyOrBlank(approvalsTable), data=approvalsTable)
  ))
}

#' Format Data Table
#'
#' @description Formats a given dataframe into a whisker table
#' @param inputData the data to format
#' @importFrom whisker rowSplit
formatDataTable <- function(inputData){
  returnData <- data.frame()
  inputData <- as.data.frame(inputData)
  
  if(!isEmptyOrBlank(inputData) || (!is.null(inputData) && nrow(inputData) > 0)){
    returnData <- unname(rowSplit(inputData))
  }
  
  return(returnData)
}

#' Parse TSS Thresholds
#'
#' @description Given the full report JSON object reads the
#' thresholds and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSThresholds <- function(reportData, timezone){
  thresholds <- tryCatch({
    readThresholds(reportData)
  }, error=function(e){
    warning(paste("Returning NULL for TSS thresholds. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(thresholds)){
    thresholds[['periods']] <- lapply(thresholds[['periods']], function(p){
      p[['startTime']] <- formatOpenDateLabel(flexibleTimeParse(p[['startTime']], timezone))
      p[['endTime']] <- formatOpenDateLabel(flexibleTimeParse(p[['endTime']], timezone))
      return(p)
    })
    
    thresholds <- attachFullDataToSubFrame(thresholds, 'periods')
  }
  
  return(thresholds)
}

#' Parse TSS Related Series
#'
#' @description Given the full report JSON object reads the
#' related series and handles read errors.
#' @param reportData the full report JSON object
parseTSSRelatedSeries <- function(reportData){
  upchain <- tryCatch({
    readUpchainSeries(reportData)
  }, error=function(e){
    warning(paste("Returning list() for TSS Related Upchain. Error:", e))
    return(list())
  })
  
  downchain <- tryCatch({
    readDownchainSeries(reportData)
  }, error=function(e){
    warning(paste("Returning list() for TSS Related Upchain. Error:", e))
    return(list())
  })
  
  upchainIds = upchain[['identifier']]
  upchainURLs = upchain[['url']]
  downchainIds = downchain[['identifier']]
  downchainURLs = downchain[['url']]

  maxSeriesLength <- max(length(upchainIds), length(downchainIds))
  
  if(maxSeriesLength > 0){
    if(isEmptyOrBlank(upchainIds)){
      upchainIds <- c(NA)
    }
    
    if(isEmptyOrBlank(downchainIds)){
      downchainIds <- c(NA)
    }
    
    if(isEmptyOrBlank(upchainURLs)){
      upchainURLs <- c(NA)
    }
    
    if(isEmptyOrBlank(downchainURLs)){
      downchainURLs <- c(NA)
    }
    
    relatedSeriesRows <- seq(maxSeriesLength)
    relatedSeriesList <- data.frame(upchainIds[relatedSeriesRows], upchainURLs[relatedSeriesRows], downchainIds[relatedSeriesRows], downchainURLs[relatedSeriesRows], stringsAsFactors = FALSE)
    relatedSeriesList[is.na(relatedSeriesList)] <- ""
    colnames(relatedSeriesList) <- c("upchain", "upchainURL", "downchain", "downchainURL")
  } else {
    relatedSeriesList <- list()
  }
  
  return(relatedSeriesList)
}

#' Parse TSS Rating Curves
#'
#' @description Given the full report JSON object reads the
#' ratings curves and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSRatingCurves <- function(reportData, timezone){
  curves <- tryCatch({
    readRatingCurves(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Rating Curves. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(curves)){
    colnames(curves)[which(colnames(curves) == 'remarks')] <- "curveRemarks"
    curves <- curves[order(curves[['startOfPeriod']]),]
    curves[['applicablePeriods']] <- lapply(curves[['applicablePeriods']], function(p){
      p[['startTime']] <- formatOpenDateLabel(flexibleTimeParse(p[['startTime']], timezone))
      p[['endTime']] <- formatOpenDateLabel(flexibleTimeParse(p[['endTime']], timezone))
      return(p)
    })
    
    curves <- curves[-which(names(curves) == "ratingShifts")]
    curves <- attachFullDataToSubFrame(curves, 'applicablePeriods')
  }
  
  
  return(curves)
}

#' Parse TSS Rating Shifts
#'
#' @description Given the full report JSON object reads the
#' ratings curves and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSRatingShifts <- function(reportData, timezone){
  shifts <- tryCatch({
    readRatingShifts(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Rating Shifts. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(shifts)){
    shifts[['variablePoints']] <- apply(shifts, 1, function(x) {paste(paste(x[['stagePoints']], x[['shiftPoints']], sep=", "), collapse="; ")})
    shifts <- shifts[order(shifts[['applicableStartDateTime']]),]
    shifts[['applicableStartDateTime']] <- formatOpenDateLabel(shifts[['applicableStartDateTime']])
    shifts[['applicableEndDateTime']] <- formatOpenDateLabel(shifts[['applicableEndDateTime']])
  }
  
  return(shifts)
}

#' Parse TSS Qualifiers
#'
#' @description Given the full report JSON object reads the
#' ratings curves and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSQualifiers <- function(reportData, timezone){
  qualifiers <- tryCatch({
    readQualifiers(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Qualifiers. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(qualifiers)){
    qualifiers <- as.data.frame(qualifiers, stringsAsFactors=FALSE)
    colnames(qualifiers)[which(colnames(qualifiers) == 'identifier')] <- "value"
    qualifiers[['metaType']] <- 'Qualifier'
    qualifiers <- qualifiers[order(qualifiers[['startDate']]),]
    qualifiers[['startDate']] <- formatOpenDateLabel(qualifiers[['startDate']])
    qualifiers[['endDate']] <- formatOpenDateLabel(qualifiers[['endDate']])
  }
  
  return(qualifiers)
}

#' Parse TSS Notes
#'
#' @description Given the full report JSON object reads the
#' notes and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSNotes <- function(reportData, timezone){
  notes <- tryCatch({
    readNotes(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Notes Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(notes)){
    notes <- as.data.frame(notes, stringsAsFactors=FALSE)
    colnames(notes)[which(colnames(notes) == 'note')] <- "value"
    notes[['metaType']] <- 'Note'
    notes <- notes[order(notes[['startDate']]),]
    notes[['startDate']] <- formatOpenDateLabel(notes[['startDate']])
    notes[['endDate']] <- formatOpenDateLabel(notes[['endDate']])
  }
  
  return(notes)
}

#' Parse TSS Grades
#'
#' @description Given the full report JSON object reads the
#' grades and handles read errors.
#' @param reportData the full report JSON object
#' @param timezone the timezone of the report
parseTSSGrades <- function(reportData, timezone){
  grades <- tryCatch({
    readGrades(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Grades Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(grades)){
    grades <- as.data.frame(grades, stringsAsFactors=FALSE)
    colnames(grades)[which(colnames(grades) == 'code')] <- "value"
    grades[['metaType']] <- 'Grade'
    grades <- grades[order(grades[['startDate']]),]
    grades[['startDate']] <- formatOpenDateLabel(grades[['startDate']])
    grades[['endDate']] <- formatOpenDateLabel(grades[['endDate']])
  }
  
  return(grades)
}

#' Parse Processing Corrections
#'
#' @description TSS wrapper for the readProcessingCorrections function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object 
#' @param processOrder The processing order to fetch data for
#' @param timezone The timezone to parse data into
parseTSSProcessingCorrections <- function(reportData, processOrder, timezone){
  corrections <- tryCatch({
    readProcessingCorrections(reportData, processOrder, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for", processOrder, "corrections. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(corrections)){
    corrections <- corrections[order(corrections[['startTime']]),]
    corrections[['startTime']] <- formatOpenDateLabel(corrections[['startTime']])
    corrections[['endTime']] <- formatOpenDateLabel(corrections[['endTime']])
    corrections <- unNestCorrectionParameters(corrections, timezone)
  }
  
  return(corrections)
}

#' Parse Gaps
#'
#' @description TSS wrapper for the readGaps function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSGaps <- function(reportData, timezone){
  gaps <- tryCatch({
    readGaps(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for gaps. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(gaps)){
    gaps <- gaps[order(gaps[['startTime']]),]
    gaps[['startTime']] <- formatOpenDateLabel(gaps[['startTime']])
    gaps[['endTime']] <- formatOpenDateLabel(gaps[['endTime']])
    
    #Handle Gaps that are not fully contained within the report period
    if(nrow(gaps[which(gaps[['gapExtent']] == "OVER_START"),]) > 0){
      gaps[which(gaps[['gapExtent']] == "OVER_START"),][['startTime']] <- "n/a*"
      gaps[which(gaps[['gapExtent']] == "OVER_START"),][['durationInHours']] <- ""
    }
    
    if(nrow(gaps[which(gaps[['gapExtent']] == "OVER_END"),]) > 0){
      gaps[which(gaps[['gapExtent']] == "OVER_END"),][['endTime']] <- "n/a*"
      gaps[which(gaps[['gapExtent']] == "OVER_END"),][['durationInHours']] <- ""
    }
  }
  
  return(gaps)
}

#' Parse TSS Approvals
#'
#' @description TSS wrapper for the readApprovals function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSApprovals <- function(reportData, timezone){
  approvals <- tryCatch({
    readApprovals(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for approvals. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(approvals)){
    approvals <- approvals[order(approvals[['startTime']]),]
    approvals[['startTime']] <- formatOpenDateLabel(approvals[['startTime']])
    approvals[['endTime']] <- formatOpenDateLabel(approvals[['endTime']])
  }
  
  return(approvals)
}

#' Parse TSS Gap Tolerances
#'
#' @description TSS wrapper for the readGapTolerances function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSGapTolerances <- function(reportData, timezone){
  gapTolerances <- tryCatch({
    readGapTolerances(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for gap tolerances. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(gapTolerances)){
    gapTolerances <- gapTolerances[order(gapTolerances[['startTime']]),]
    gapTolerances[['startTime']] <- formatOpenDateLabel(gapTolerances[['startTime']])
    gapTolerances[['endTime']] <- formatOpenDateLabel(gapTolerances[['endTime']])
  }
  
  return(gapTolerances)
}

#' Unnest the correction parameters
#' @description Takes the corrections and unnests them so they can 
#' more easily be processed for each type
#' @param corrections the corrections for the report
#' @param timezone the timezone to parse data into
#' @return corrections formatted better for report display
#' @importFrom dplyr rowwise mutate ungroup
unNestCorrectionParameters <- function(corrections, timezone) {
  
  type <- ".dplyr"
  driftPoints <- ".dplyr"
  value <- ".dplyr"
  startShiftPoints <- ".dplyr"
  endShiftPoints <- ".dplyr"
  usgsType <- ".dplyr"
  upperThresholdPoints <- ".dplyr"
  resamplePeriod <- ".dplyr"
  gapLimit <- ".dplyr"
  deviationValue <- ".dplyr"
  deviationType <- ".dplyr"
  windowSizeInMinutes <- ".dplyr"
  ungroup <- ".dplyr"
  params <- corrections$parameters
  corrections$parameters <- NULL
  corrections <- cbind(corrections, params)
  
  corrections_formatted <- corrections %>%
    rowwise() %>%
    mutate(timezone=timezone) %>%
    mutate(
      formattedParameters = switch(type,
                                   "Offset" = formatCorrectionsParamOffset(offset),
                                   "Drift" = formatCorrectionsParamDrift(driftPoints, timezone),
                                   "SinglePoint" = formatCorrectionsParamSinglePoint(value),
                                   "USGSMultiPoint" = formatCorrectionsParamUSGSMultiPoint(startShiftPoints, endShiftPoints, usgsType),
                                   "AdjustableTrim" = formatCorrectionsParamAdjustableTrim(upperThresholdPoints, timezone),
                                   "FillGaps" = formatCorrectionsParamFillGaps(resamplePeriod, gapLimit),
                                   "Deviation" = formatCorrectionsParamDeviation(deviationValue, deviationType, windowSizeInMinutes),
                                    type)) %>% 
    ungroup()
  return(corrections_formatted)
}

#' formats the offset correction parameters
#' @description formats the offset correction parameters
#' @param offset the offset value for the parameter offset
#' @return formatted string of offset parameters for report display
formatCorrectionsParamOffset <- function(offset) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(offset)) {
    formattedParameters <- paste0("Offset ", offset)
  }
  return(formattedParameters)
}

#' formats the drift correction parameters
#' @description formats the drift correction parameters
#' @param driftPoints the driftPoints values in a list including
#' date/times and the difference values in feet
#' @param timezone the timezone for the drift parameters
#' @return formatted string of drift parameters for report display
formatCorrectionsParamDrift <- function(driftPoints, timezone) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(driftPoints) && !isEmptyOrBlank(timezone)) {
    driftPoints <- unlist(driftPoints)
    formattedParameters <- paste0("Drift correction of (date/time, diff): (", flexibleTimeParse(driftPoints['time1'], timezone, FALSE), ", ", driftPoints['offset1'][1], "ft), (", flexibleTimeParse(driftPoints['time2'], timezone, FALSE), ", ", driftPoints['offset2'], "ft)")
  }
  return(formattedParameters)
}

#' formats the single point correction parameters
#' @description formats the single point correction parameters
#' @param value the value for the parameter single point
#' @return formatted string of single point parameter for report display
formatCorrectionsParamSinglePoint <- function(value) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(value)) {
    formattedParameters <- paste0("SinglePoint ", value)
  }
  return(formattedParameters)
}

#' formats the USGSMultiPoint correction parameters
#' @description formats the USGSMultiPoint correction parameters
#' @param startShiftPoints startShift points values and offset as a list
#' @param endShiftPoints endShift points values and offset as a list
#' @return formatted string of USGSMultiPoint parameters for report display
formatCorrectionsParamUSGSMultiPoint <- function(startShiftPoints, endShiftPoints, usgsType) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(startShiftPoints) && !isEmptyOrBlank(endShiftPoints) && !isEmptyOrBlank(usgsType)) {
    startShiftPoints <- unlist(startShiftPoints)
    endShiftPoints <- unlist(endShiftPoints)
    formattedParameters <- paste0("USGSMultiPoint Start shift points value ", startShiftPoints['value'], ", offset ", startShiftPoints['offset'], ". End shift points value ", endShiftPoints['value'], ", offset ", endShiftPoints['offset'], ", ", usgsType)
  }
  return(formattedParameters)
}

#' formats the adjustable trim correction parameters
#' @description formats the adjustable trim correction parameters
#' @param upperThresholdPoints upper threshold points date/times and values as a list
#' @param timezone the timezone for the drift parameters
#' #' @return formatted string of adjustable trim parameters for report display
formatCorrectionsParamAdjustableTrim <- function(upperThresholdPoints, timezone) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(upperThresholdPoints) && !isEmptyOrBlank(timezone)) {
    upperThresholdPoints <- unlist(upperThresholdPoints)  
    formattedParameters <- paste0("Adjustable trim with Upper threshold: (", flexibleTimeParse(upperThresholdPoints[['time1']], timezone, FALSE), ", ", round(as.numeric(upperThresholdPoints['value1']), 3), "ft), (", flexibleTimeParse(upperThresholdPoints['time2'], timezone, FALSE), ", ", round(as.numeric(upperThresholdPoints['value2']), 3), "ft)")
  }
  return(formattedParameters)
}

#' formats the fill gaps correction parameters
#' @description formats the fill gaps correction parameters
#' @param resamplePeriod a description of the resample period
#' @param gapLimit a description of the gap limits
#' @return formatted string of fill gaps parameters for report display
formatCorrectionsParamFillGaps <- function(resamplePeriod, gapLimit) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(resamplePeriod) && !isEmptyOrBlank(gapLimit)) {
    formattedParameters <- paste0("Resample Period ", resamplePeriod,";"," Gap Limit ", gapLimit)
  }
  return(formattedParameters)
}

#' formats the deviation correction parameters
#' @description formats the deviation correction parameters
#' @param deviationValue the value for the deviation
#' @param deviationType a description of the type of deviation for this correction
#' @param windowSizeInMinutes the window for the deviation in minutes
#' @return formatted string of deviation parameters for report display
formatCorrectionsParamDeviation <- function(deviationValue, deviationType, windowSizeInMinutes) {
  formattedParameters <- ""
  if (!isEmptyOrBlank(deviationValue) && !isEmptyOrBlank(deviationType) && !isEmptyOrBlank(windowSizeInMinutes)) {
    formattedParameters <- paste0("Deviation type ", deviationType, "; value: ", deviationValue, ", window size ", windowSizeInMinutes, " minutes")
  }
  return(formattedParameters)
}
