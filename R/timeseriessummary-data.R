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
  relatedSeriesList <- list()
  relatedSeriesList[['upchain']] <- reportData[['upchainTs']][['identifier']]
  relatedSeriesList[['downchain']] <- reportData[['downchainTs']][['identifier']]
  releatedSeriesRows <- seq(max(length(relatedSeriesList[['upchain']]), length(relatedSeriesList[['downchain']])))
  relatedSeriesList <- data.frame(relatedSeriesList[['upchain']][releatedSeriesRows], relatedSeriesList[['downchain']][releatedSeriesRows], stringsAsFactors = FALSE)
  relatedSeriesList[is.na(relatedSeriesList)] <- ""
  colnames(relatedSeriesList) <- c("upchain", "downchain")
  relatedSeriesTable <- formatDataRow(relatedSeriesList)
  
  gapsList <- list()
  gapsList <- reportData[['gaps']]
  gapsTable <- formatDataRow(gapsList)
  
  correctionsList <- list()
  correctionsList[['pre']] <- reportData[['corrections']][['preProcessing']]
  correctionsList[['normal']] <- reportData[['corrections']][['normal']]
  correctionsList[['post']] <- reportData[['corrections']][['postProcessing']]
  correctionsTable <- list()
  correctionsTable[['pre']] <- formatDataRow(correctionsList[['pre']])
  correctionsTable[['normal']] <- formatDataRow(correctionsList[['normal']])
  correctionsTable[['post']] <- formatDataRow(correctionsList[['post']])
  
  thresholdsList <- list()
  thresholdsList <- reportData[['thresholds']]
  thresholdsTable <- formatDataRow(thresholdsList)
  
  ratingsList <- list()
  ratingsList[['curves']] <- reportData[['ratingCurves']]
  colnames(ratingsList[['curves']])[which(colnames(ratingsList[['curves']]) == 'remarks')] <- "curveRemarks"
  ratingsList[['shifts']] <- reportData[['ratingShifts']]
  ratingsTable <- list()
  ratingsTable[['curves']] <- formatDataRow(ratingsList[['curves']])
  ratingsTable[['shifts']] <- formatDataRow(ratingsList[['shifts']])
  
  metadataList <- list()
  metadataList[['qualifiers']] <- reportData[['qualifiers']]
  metadataList[['qualifiers']][['metaType']] <- 'Qualifier'
  metadataList[['notes']] <- reportData[['notes']]
  metadataList[['notes']][['metaType']] <- 'Note'
  metadataList[['grades']] <- reportData[['grades']]
  metadataList[['grades']][['metadataType']] <- 'Grade'
  metadataList <- data.frame(unlist(metadataList))
  metadataTable <- formatDataRow(metadataList)
  
  
  approvalsTable <- list()
  approvalsTable <- reportData[['approvals']]
  
  return(list(
      relatedSeries = relatedSeriesTable,
      gaps = gapsTable,
      corrections = correctionsTable,
      thresholds = thresholdsTable,
      ratings = ratingsTable
  ))
}

formatDataRow <- function(inputData){
  returnData <- data.frame()
  
  if(!isEmptyOrBlank(inputData)){
    returnData <- unname(rowSplit(inputData))
  }
  
  return(returnData)
}