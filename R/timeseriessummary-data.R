#'@aliases parseCustomDataElementsForTemplate
#'@rdname parseCustomDataElementsForTemplate
setMethod("parseCustomDataElementsForTemplate", signature(reportData = "timeseriessummary"), 
    definition = function(reportData) {
      return(parseCustomDataElementsForTemplateForTimeSeriesSummary(reportData))
    }
)

#' parseCustomDataElementsForTemplateForTimeSeriesSummary
#' @description Will return the derivations array as a json fragment
#' @param reportData full report data structure 
#' @return list of data elements for template
#' @importFrom jsonlite toJSON
parseCustomDataElementsForTemplateForTimeSeriesSummary <- function(reportData) {
  timezone <- fetchReportMetadataField(reportData, 'timezone')
  
  relatedSeriesTable <- formatDataRow(parseTSSRelatedSeries(reportData))
  gapsTable <- formatDataRow(parseGaps(reportData, timezone))
  thresholdsTable <- formatDataRow(parseTSSThresholds(reportData, timezone))
  
  correctionsTable <- list()
  correctionsTable[['pre']] <- formatDataRow(parseProcessingCorrections(reportData, "pre", timezone))
  correctionsTable[['normal']] <- formatDataRow(parseProcessingCorrections(reportData, "normal", timezone))
  correctionsTable[['post']] <- formatDataRow(parseProcessingCorrections(reportData, "post", timezone))
  
  ratingsTable <- list()
  ratingsTable[['curves']] <- formatDataRow(parseTSSRatingCurves(reportData, timezone))
  ratingsTable[['shifts']] <- formatDataRow(parseTSSRatingShifts(reportData, timezone))
  
  metadataTable <- list()
  metadataTable <- mergeLists(parseTSSQualifiers(reportData, timezone),parseTSSNotes(reportData, timezone))
  metadataTable <- mergeLists(metadataTable, parseTSSGrades(reportData, timezone))
  metadataTable <- data.frame(metadataTable)
  metadataTable <- formatDataRow(metadataTable)
  
  approvalsTable <- formatDataRow(reportData[['approvals']])
  
  return(list(
      relatedSeries = relatedSeriesTable,
      gaps = gapsTable,
      corrections = correctionsTable,
      thresholds = thresholdsTable,
      ratings = ratingsTable,
      metadata = metadataTable,
      approvals = approvalsTable
  ))
}

formatDataRow <- function(inputData){
  returnData <- data.frame()
  inputData <- as.data.frame(inputData)
  
  if(!isEmptyOrBlank(inputData)){
    returnData <- unname(rowSplit(inputData))
  }
  
  return(returnData)
}

parseTSSThresholds <- function(reportData, timezone){
  thresholds <- tryCatch({
    readThresholds(reportData)
  }, error=function(e){
    warning(paste("Returning NULL for TSS thresholds. Error:", e))
    return(NULL)
  })
  
  thresholds[['periods']] <- lapply(thresholds[['periods']], function(p){
    p[['startTime']] <- flexibleTimeParse(p[['startTime']], timezone)
    p[['endTime']] <- flexibleTimeParse(p[['endTime']], timezone)
    return(p)
  })
  
  return(thresholds)
}

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
  downchainIds = downchain[['identifier']]
  
  relatedSeriesRows <- seq(max(length(upchainIds), length(downchainIds)))
  relatedSeriesList <- data.frame(upchainIds[relatedSeriesRows], downchainIds[relatedSeriesRows], stringsAsFactors = FALSE)
  relatedSeriesList[is.na(relatedSeriesList)] <- ""
  colnames(relatedSeriesList) <- c("upchain", "downchain")
  
  return(relatedSeriesList)
}

parseTSSRatingCurves <- function(reportData, timezone){
  curves <- tryCatch({
    readRatingCurves(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Rating Curves. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(curves)){
    colnames(curves)[which(colnames(curves) == 'remarks')] <- "curveRemarks"
    
    curves[['applicablePeriods']] <- lapply(curves[['applicablePeriods']], function(p){
      p[['startTime']] <- flexibleTimeParse(p[['startTime']], timezone)
      p[['endTime']] <- flexibleTimeParse(p[['endTime']], timezone)
      return(p)
    })
  }
  
  return(curves)
}

parseTSSRatingShifts <- function(reportData, timezone){
  shifts <- tryCatch({
    readRatingShifts(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Rating Shifts. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(shifts)){
    shifts[['variablePoints']] <- apply(shifts, 1, function(x) {paste(paste(x[['stagePoints']], x[['shiftPoints']], sep=", "), collapse="; ")})
  }
  
  return(shifts)
}

parseTSSQualifiers <- function(reportData, timezone){
  qualifiers <- tryCatch({
    readQualifiers(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Qualifiers. Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(qualifiers)){
    qualifiers <- as.data.frame(qualifiers)
    colnames(qualifiers)[which(colnames(qualifiers) == 'identifier')] <- "value"
    qualifiers[['metaType']] <- 'Qualifier'
  }
  
  return(qualifiers)
}

parseTSSNotes <- function(reportData, timezone){
  notes <- tryCatch({
    readNotes(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Notes Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(notes)){
    notes <- as.data.frame(notes)
    colnames(notes)[which(colnames(notes) == 'note')] <- "value"
    notes[['metaType']] <- 'Note'
  }
  
  return(notes)
}

parseTSSGrades <- function(reportData, timezone){
  grades <- tryCatch({
    readGrades(reportData, timezone)
  }, error=function(e){
    warning(paste("Returning list() for TSS Grades Error:", e))
    return(list())
  })
  
  if(!isEmptyOrBlank(grades)){
    grades <- as.data.frame(grades)
    colnames(grades)[which(colnames(grades) == 'code')] <- "value"
    grades[['metaType']] <- 'Grade'
  }
  
  return(grades)
}