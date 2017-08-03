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
  addNaNote <- any(do.call("rbind", gapsTable[['gaps']])[['startTime']] == "n/a**") ||  any(do.call("rbind", gapsTable[['gaps']])[['endTime']] == "n/a**")
  
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
  
  tsDetailsTable <- list()
  tsDetails <- constructTSDetails(reportData, timezone)
  tsDetailsTable[['tsAttrs']] <- formatDataTable(tsDetails[['tsAttrs']])
  tsDetailsTable[['tsExtAttrs']] <- formatDataTable(tsDetails[['tsExtAttrs']])
  addChangeNote <- tsDetails[['changeNote']]
  
  advOptions <- formatAdvReportOptions(fetchReportMetadataField(reportData,'excludeCorrections'))
  
  return(list(
      tsDetails = list(hasData=TRUE, data=tsDetailsTable, addChangeNote=addChangeNote),
      relatedSeries = list(hasData=!isEmptyOrBlank(relatedSeriesTable), data=relatedSeriesTable),
      gaps = list(hasData=(!isEmptyOrBlank(gapsTable[['gaps']]) || !isEmptyOrBlank(gapsTable[['tolerances']])), data=gapsTable, addNaNote=addNaNote),
      corrections = list(hasData=(!isEmptyOrBlank(correctionsTable[['pre']]) || !isEmptyOrBlank(correctionsTable[['normal']]) || !isEmptyOrBlank(correctionsTable[['post']])), data=correctionsTable, corrUrl=corrUrl),
      thresholds = list(hasData=!isEmptyOrBlank(thresholdsTable), data=thresholdsTable),
      ratings = list(hasData=(!isEmptyOrBlank(ratingsTable[['curves']]) || !isEmptyOrBlank(ratingsTable[['shifts']])), data=ratingsTable),
      metadata = list(hasData=!isEmptyOrBlank(metadataTable), data=metadataTable),
      approvals = list(hasData=!isEmptyOrBlank(approvalsTable), data=approvalsTable),
      advOptions = advOptions
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
  
  if(!isEmptyOrBlank(inputData) || !isEmptyVar(inputData)){
    returnData <- unname(rowSplit(inputData))
  }
  
  return(returnData)
}

constructTSDetails <- function(reportData, timezone){
  tsAttrs <- data.frame(stringsAsFactors = FALSE)
  tsExtAttrs <- data.frame(stringsAsFactors = FALSE)
  
  
  metadata <- parseTSSPrimaryTsMetadata(reportData)
  methodData <- parseTSSMethods(reportData, timezone)
  itData <- parseTSSInterpolationTypes(reportData, timezone)
  processorData <- parseTSSProcessors(reportData, timezone)
  changeNote <- FALSE
  
  if(!isEmptyOrBlank(metadata)){
    #Time Series Attributes - Note: The order that these are added matters in order to have proper ordering on the report
    tsAttrs <- rbind(tsAttrs, data.frame(label="Label", value=metadata[['identifier']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Parameter", value=metadata[['parameter']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Units", value=metadata[['unit']], indent=8, stringsAsFactors = FALSE))
    
    #Interpolation Types
    itValue <- ""
    if(!isEmptyOrBlank(itData) && !isEmptyVar(itData)){
      itValue <- itData[1,][['type']]
      
      #Add an asterisk if there is more than one interpolation type and only list the first
      if(nrow(itData) > 1){
        changeNote <- TRUE
        itValue <- paste(itValue, "*")
      }
    }
    tsAttrs <- rbind(tsAttrs, data.frame(label="Interpolation", value=itValue, indent=8, stringsAsFactors = FALSE))
    
    #Time Series Attributes (continued from above)
    tsAttrs <- rbind(tsAttrs, data.frame(label="Sub-Location", value=metadata[['sublocation']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="UTC Offset", value=metadata[['utcOffset']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Computation", value=metadata[['computation']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Period", value=metadata[['period']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Publish", value=metadata[['publish']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Description", value=metadata[['description']], indent=8, stringsAsFactors = FALSE))
    tsAttrs <- rbind(tsAttrs, data.frame(label="Comments", value=metadata[['comment']], indent=8, stringsAsFactors = FALSE))
    
    #Measurement Methods
    methodValue <- ""
    methodStartTime <- NULL #Null so that it is hidden if there is no method
    if(!isEmptyOrBlank(methodData) && !isEmptyVar(methodData)){
      methodValue <- methodData[1,][['methodCode']]
      methodStartTime <- methodData[1,][['startTime']]
      
      #Add an asterisk if there is more than one measurement method and only list the last
      if(nrow(methodData) > 1){
        changeNote <- TRUE
        methodValue <- paste(methodValue, "*")
      }
    }
    tsAttrs <- rbind(tsAttrs, data.frame(label="Measurement Method", value=methodValue, indent=8, stringsAsFactors = FALSE))
    
    if(!is.null(methodStartTime)){
      tsAttrs <- rbind(tsAttrs, data.frame(label="Method Start Time", value=methodStartTime, indent=26, stringsAsFactors = FALSE))
    }
    
    
    #Processors
    processorValue <- ""
    processorStartTime <- NULL #Null so that it is hidden if there is no processor
    processorEndTime <- NULL #Null so that it is hidden if there is no processor
    
    if(!isEmptyOrBlank(processorData) && !isEmptyVar(processorData)){
      processorValue <- processorData[1,][['processorType']]
      processorStartTime <- processorData[1,][['startTime']]
      processorEndTime <- processorData[1,][['endTime']]
      
      #Add an asterisk if there is more than one processor and only list the first
      if(nrow(processorData) > 1){
        changeNote <- TRUE
        processorValue <- paste(processorValue, "*")
      }
    }
    tsAttrs <- rbind(tsAttrs, data.frame(label="Processing Type", value=processorValue, indent=8, stringsAsFactors = FALSE))
    
    if(!is.null(processorStartTime)){
      tsAttrs <- rbind(tsAttrs, data.frame(label="Period Start Time", value=processorStartTime, indent=26, stringsAsFactors = FALSE))
    }
    
    if(!is.null(processorEndTime)){
      tsAttrs <- rbind(tsAttrs, data.frame(label="Period End Time", value=processorEndTime, indent=26, stringsAsFactors = FALSE))
    }
    
    
    #Time Series Extended Attributes
    extAttrs <- metadata[['extendedAttributes']]
    
    accessValue <- ifelse(isEmptyOrBlank(extAttrs[['ACCESS_LEVEL']]), " ", extAttrs[['ACCESS_LEVEL']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="NWISWeb Access Level", value=accessValue, stringsAsFactors = FALSE))
    
    plotMeasValue <- ifelse(isEmptyOrBlank(extAttrs[['PLOT_MEAS']]), " ", extAttrs[['PLOT_MEAS']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="NWISWeb Plot Field Data", value=plotMeasValue, stringsAsFactors = FALSE))
    
    dataGapValue <- ifelse(isEmptyOrBlank(extAttrs[['DATA_GAP']]), " ", extAttrs[['DATA_GAP']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="NWISWeb Gap Tolerance (Minutes)", value=dataGapValue, stringsAsFactors = FALSE))
    
    activeValue <- ifelse(isEmptyOrBlank(extAttrs[['ACTIVE_FLAG']]), " ", extAttrs[['ACTIVE_FLAG']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="Include in NWISWeb Current Table", value=activeValue, stringsAsFactors = FALSE))
    
    webValue <- ifelse(isEmptyOrBlank(extAttrs[['WEB_DESCRIPTION']]), " ", extAttrs[['WEB_DESCRIPTION']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="NWISWeb Description", value=webValue, stringsAsFactors = FALSE))
    
    statBeginValue <- ifelse(isEmptyOrBlank(extAttrs[['STAT_BEGIN_YEAR']]), " ", extAttrs[['STAT_BEGIN_YEAR']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="NWISWeb Stat Begin Date", value=statBeginValue, stringsAsFactors = FALSE))
    
    adapsValue <- ifelse(isEmptyOrBlank(extAttrs[['ADAPS_DD']]), " ", extAttrs[['ADAPS_DD']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="ADAPS DD", value=adapsValue, stringsAsFactors = FALSE))
    
    primaryValue <- ifelse(isEmptyOrBlank(extAttrs[['PRIMARY_FLAG']]), " ", extAttrs[['PRIMARY_FLAG']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="Primary", value=primaryValue, stringsAsFactors = FALSE))
    
    transportValue <- ifelse(isEmptyOrBlank(extAttrs[['TRANSPORT_CODE']]), " ", extAttrs[['TRANSPORT_CODE']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="Transport Code", value=transportValue, stringsAsFactors = FALSE))
    
    specialValue <- ifelse(isEmptyOrBlank(extAttrs[['SPECIAL_DATA_TYPE']]), " ", extAttrs[['SPECIAL_DATA_TYPE']])
    tsExtAttrs <- rbind(tsExtAttrs, data.frame(label="Special Data Type", value=specialValue, stringsAsFactors = FALSE))
  }
  
  return(list(
    tsAttrs = tsAttrs,
    tsExtAttrs = tsExtAttrs,
    changeNote = changeNote
  ))
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
    warning(paste("Returning list() for TSS thresholds. Error:", e))
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
    qualifiers[['value']] <- paste(qualifiers[['code']],qualifiers[['displayName']], sep=" - ")
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
    notes <- notes[order(notes[['startDate']]),]
    notes[['startDate']] <- formatOpenDateLabel(notes[['startDate']])
    notes[['endDate']] <- formatOpenDateLabel(notes[['endDate']])
    notes[['identifier']] <- ""
    notes[['code']] <- ""
    notes[['displayName']] <- ""
    notes[['appliedBy']] <- ""
    notes[['dateApplied']] <- ""
    notes[['metaType']] <- 'Note'
    notes <- notes[c("startDate", "endDate", "identifier", "code", "displayName", "appliedBy", "dateApplied", "value", "metaType")]
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
    grades <- grades[order(grades[['startDate']]),]
    grades[['startDate']] <- formatOpenDateLabel(grades[['startDate']])
    grades[['endDate']] <- formatOpenDateLabel(grades[['endDate']])
    grades[['identifier']] <- ""
    grades[['code']] <- ""
    grades[['displayName']] <- ""
    grades[['appliedBy']] <- ""
    grades[['dateApplied']] <- ""
    grades[['metaType']] <- 'Grade'
    grades <- grades[c("startDate", "endDate", "identifier", "code", "displayName", "appliedBy", "dateApplied", "value", "metaType")]
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
      gaps[which(gaps[['gapExtent']] == "OVER_START"),][['startTime']] <- "n/a**"
      gaps[which(gaps[['gapExtent']] == "OVER_START"),][['durationInHours']] <- ""
    }
    
    if(nrow(gaps[which(gaps[['gapExtent']] == "OVER_END"),]) > 0){
      gaps[which(gaps[['gapExtent']] == "OVER_END"),][['endTime']] <- "n/a**"
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
  Offset <- ".dplyr"
  DriftPoints <- ".dplyr"
  Value <- ".dplyr"
  StartShiftPoints <- ".dplyr"
  EndShiftPoints <- ".dplyr"
  UsgsType <- ".dplyr"
  UpperThresholdPoints <- ".dplyr"
  LowerThresholdPoints <- ".dplyr"
  ResamplePeriod <- ".dplyr"
  GapLimit <- ".dplyr"
  DeviationValue <- ".dplyr"
  DeviationType <- ".dplyr"
  WindowSizeInMinutes <- ".dplyr"
  ungroup <- ".dplyr"
  
  params <- corrections$parameters
  corrections$parameters <- NULL
  
  if (!isEmptyVar(params)) {
    corrections <- cbind(corrections, params)
  }
  
  corrections_formatted <- corrections %>%
    rowwise() %>%
    mutate(timezone=timezone) %>%
    mutate(
      formattedParameters = switch(type,
                                   "Offset" = formatCorrectionsParamOffset(Offset),
                                   "Drift" = formatCorrectionsParamDrift(DriftPoints, timezone),
                                   "SinglePoint" = formatCorrectionsParamSinglePoint(Value),
                                   "USGSMultiPoint" = formatCorrectionsParamUSGSMultiPoint(StartShiftPoints, EndShiftPoints, UsgsType),
                                   "AdjustableTrim" = formatCorrectionsParamAdjustableTrim(UpperThresholdPoints, LowerThresholdPoints, timezone),
                                   "FillGaps" = formatCorrectionsParamFillGaps(ResamplePeriod, GapLimit),
                                   "Deviation" = formatCorrectionsParamDeviation(DeviationValue, DeviationType, WindowSizeInMinutes),
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
    formattedParameters <- paste0(offset)
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
  driftPoints <- as.data.frame(driftPoints)
  if (!isEmptyOrBlank(driftPoints) && !isEmptyOrBlank(timezone) && (all(c("Time","Offset") %in% (names(driftPoints))))) {
    for (i in 1:nrow(driftPoints)) {
      formattedParameters <- paste0(formattedParameters, "Correction of (date/time, diff): ", flexibleTimeParse(driftPoints[['Time']][[i]], timezone, FALSE), ", ", driftPoints[['Offset']][[i]], "ft. ")
    }
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
    formattedParameters <- paste0(value)
  }
  return(formattedParameters)
}

#' formats the USGSMultiPoint correction parameters
#' @description formats the USGSMultiPoint correction parameters
#' @param startShiftPoints startShift points values and offset as a list
#' @param endShiftPoints endShift points values and offset as a list
#' @param usgsType the USGSMultiPoint type
#' @return formatted string of USGSMultiPoint parameters for report display
formatCorrectionsParamUSGSMultiPoint <- function(startShiftPoints, endShiftPoints, usgsType) {
  formattedParameters <- ""
  startShiftPoints <- as.data.frame(startShiftPoints)
  endShiftPoints <- as.data.frame(endShiftPoints)
  if (!isEmptyOrBlank(startShiftPoints) && !isEmptyOrBlank(endShiftPoints) && !isEmptyOrBlank(usgsType)) {
    if (all(c("Value","Offset") %in% names(startShiftPoints))) {
      for (i in 1:nrow(startShiftPoints)) {
        formattedParameters <- paste0(formattedParameters, "Start shift points value ", startShiftPoints[['Value']][[i]], ", offset ", 
                                      round(as.numeric(startShiftPoints[['Offset']][[i]]), 3), ". ")
      }
    }
    if (all(c("Value","Offset") %in% names(endShiftPoints))) {
      for (i in 1:nrow(endShiftPoints)) {
        formattedParameters <- paste0(formattedParameters, "End shift points value ", endShiftPoints[['Value']][[i]], ", offset ", 
                                      round(as.numeric(endShiftPoints[['Offset']][[i]]), 3), ". ")
      }
    }
    formattedParameters <- paste0(formattedParameters, usgsType)
  }
  return(formattedParameters)
}

#' formats the adjustable trim correction parameters
#' @description formats the adjustable trim correction parameters
#' @param upperThresholdPoints upper threshold points date/times and values as a list
#' @param lowerThresholdPoints lower threshold points date/times and values as a list
#' @param timezone the timezone for the drift parameters
#' #' @return formatted string of adjustable trim parameters for report display
formatCorrectionsParamAdjustableTrim <- function(upperThresholdPoints, lowerThresholdPoints, timezone) {
  formattedParameters <- ""
  upperThresholdPoints <- as.data.frame(upperThresholdPoints)
  lowerThresholdPoints <- as.data.frame(lowerThresholdPoints)
  if (!isEmptyOrBlank(upperThresholdPoints) || !isEmptyOrBlank(lowerThresholdPoints) && !isEmptyOrBlank(timezone)) {
    if (all(c("Value","Time") %in% names(upperThresholdPoints))) {
      for (i in 1:nrow(upperThresholdPoints)) { 
        formattedParameters <- paste0(formattedParameters, "Upper threshold: ", flexibleTimeParse(upperThresholdPoints[['Time']][[i]], timezone, FALSE), ", ", 
                                      round(as.numeric(upperThresholdPoints[['Value']][[i]]), 3), "ft. ")
      } 
    }
    if (all(c("Value","Time") %in% names(lowerThresholdPoints))) {
      for (i in 1:nrow(lowerThresholdPoints)) { 
        formattedParameters <- paste0(formattedParameters, "Lower threshold: ", flexibleTimeParse(lowerThresholdPoints[['Time']][[i]], timezone, FALSE), ", ", 
                                      round(as.numeric(lowerThresholdPoints[['Value']][[i]]), 3), "ft. ")
      } 
    }
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

#' Parse TSS Primary TS Metadata
#' 
#' @description TSS wrapper for the readPrimaryTSMetadata function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
parseTSSPrimaryTsMetadata <- function(reportData){
  metadata <- tryCatch({
    readPrimaryTSMetadata(reportData)
  }, error=function(e){
    warning(paste("Returning NULL for primary TS metadata. Error:", e))
    return(NULL)
  })
  
  return(metadata)
}

#' Parse TSS Methods
#' 
#' @description TSS wrapper for the readMethods function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSMethods <- function(reportData, timezone){
  methods <- tryCatch({
    readMethods(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for methods. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(methods)){
    methods <- methods[order(methods[['endTime']], decreasing=TRUE),]
    methods[['startTime']] <- formatOpenDateLabel(methods[['startTime']])
    methods[['endTime']] <- formatOpenDateLabel(methods[['endTime']])
  }
  
  return(methods)
}

#' Parse TSS Interpolation Types
#' 
#' @description TSS wrapper for the readInterpolationTypes function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSInterpolationTypes <- function(reportData, timezone){
  interpolationTypes <- tryCatch({
    readInterpolationTypes(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for interpolation types. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(interpolationTypes)){
    interpolationTypes <- interpolationTypes[order(interpolationTypes[['endTime']], decreasing=TRUE),]
    interpolationTypes[['startTime']] <- formatOpenDateLabel(interpolationTypes[['startTime']])
    interpolationTypes[['endTime']] <- formatOpenDateLabel(interpolationTypes[['endTime']])
  }
  
  return(interpolationTypes)
}

#' Parse TSS Processors
#' 
#' @description TSS wrapper for the readProcessors function
#' that handles errors thrown and returns the proper data
#' @param reportData The full report JSON object
#' @param timezone The timezone to parse data into
parseTSSProcessors <- function(reportData, timezone){
  processors <- tryCatch({
    readProcessors(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for processors. Error:", e))
    return(NULL)
  })
  
  if(!isEmptyOrBlank(processors)){
    processors <- processors[order(processors[['endTime']], decreasing=TRUE),]
    processors[['startTime']] <- formatOpenDateLabel(processors[['startTime']])
    processors[['endTime']] <- formatOpenDateLabel(processors[['endTime']])
  }
  
  return(processors)
}

#' Format Advanced Report Options
#' 
#' @description Format user applied advanced options to print on the report
#' @param advancedReportOptions The param to format
#' @return advOptions List of applied options to print on the report
formatAdvReportOptions <- function(advancedReportOptions) {
  advOptions <- list()
  if(!isEmptyOrBlank(advancedReportOptions)) {
    advOptions <- paste0("Delete region corrections excluded")
  } 
  
  return(advOptions)
}
