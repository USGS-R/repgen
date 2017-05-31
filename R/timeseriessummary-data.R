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
  
  relatedSeriesList <- list()
  relatedSeriesList[['upchain']] <- reportData[['upchainTs']][['identifier']]
  relatedSeriesList[['downchain']] <- reportData[['downchainTs']][['identifier']]
  releatedSeriesRows <- seq(max(length(relatedSeriesList[['upchain']]), length(relatedSeriesList[['downchain']])))
  relatedSeriesList <- data.frame(relatedSeriesList[['upchain']][releatedSeriesRows], relatedSeriesList[['downchain']][releatedSeriesRows], stringsAsFactors = FALSE)
  relatedSeriesList[is.na(relatedSeriesList)] <- ""
  colnames(relatedSeriesList) <- c("upchain", "downchain")
  relatedSeriesTable <- formatDataRow(relatedSeriesList)
  
  gapsTable <- formatDataRow(reportData[['gaps']])
  
  correctionsTable <- list()
  correctionsTable[['pre']] <- formatDataRow(parseProcessingCorrections(reportData, "pre", timezone))
  correctionsTable[['normal']] <- formatDataRow(parseProcessingCorrections(reportData, "normal", timezone))
  correctionsTable[['post']] <- formatDataRow(parseProcessingCorrections(reportData, "post", timezone))
  
  thresholdsTable <- formatDataRow(parseTSSThresholds(reportData, timezone))
  
  ratingsList <- list()
  ratingsList[['curves']] <- reportData[['ratingCurves']]
  
  if(!isEmptyOrBlank(ratingsList[['curves']])){
    colnames(ratingsList[['curves']])[which(colnames(ratingsList[['curves']]) == 'remarks')] <- "curveRemarks"
  }
  
  ratingsList[['shifts']] <- reportData[['ratingShifts']]
  
  if(!isEmptyOrBlank(ratingsList[['shifts']])){
    ratingsList[['shifts']][['variablePoints']] <- apply(ratingsList[['shifts']], 1, function(x) {paste(paste(x[['stagePoints']], x[['shiftPoints']], sep=", "), collapse="; ")})
  }
  ratingsTable <- list()
  ratingsTable[['curves']] <- formatDataRow(ratingsList[['curves']])
  ratingsTable[['shifts']] <- formatDataRow(ratingsList[['shifts']])
  
  metadataList <- list()
  metadataList[['qualifiers']] <- reportData[['primaryTsMetadata']][['qualifiers']]
  
  if(length(metadataList[['qualifiers']] > 0)){
    metadataList[['qualifiers']] <- metadataList[['qualifiers']][,c("startDate", "endDate", "identifier")]
    metadataList[['qualifiers']][['metaType']] <- 'Qualifier'
  }
  metadataList[['notes']] <- reportData[['primaryTsMetadata']][['notes']]
  
  if(length(metadataList[['notes']]) > 0){
    colnames(metadataList[['notes']])[which(colnames(metadataList[['notes']]) == 'note')] <- "identifier"
    metadataList[['notes']][['metaType']] <- 'Note'
  }
  metadataList[['grades']] <- reportData[['primaryTsMetadata']][['grades']]
  
  if(length(metadataList[['grades']]) > 0){
    colnames(metadataList[['grades']])[which(colnames(metadataList[['grades']]) == 'code')] <- "identifier"
    metadataList[['grades']][['metadataType']] <- 'Grade'
  }
  
  metadataTable <- mergeLists(metadataList[['qualifiers']], metadataList[['notes']])
  metadataTable <- mergeLists(metadataTable, metadataList[['grades']])
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