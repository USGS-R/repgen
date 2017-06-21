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
    return(list())
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
    methods <- methods[order(methods[['startTime']]),]
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
    interpolationTypes <- interpolationTypes[order(interpolationTypes[['startTime']]),]
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
    processors <- processors[order(processors[['startTime']]),]
    processors[['startTime']] <- formatOpenDateLabel(processors[['startTime']])
    processors[['endTime']] <- formatOpenDateLabel(processors[['endTime']])
  }
  
  return(processors)
}