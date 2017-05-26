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
  
  thresholdsTable <- list()
  thresholdsTable <- reportData[['thresholds']]
  
  ratingsTable <- list()
  ratingsTable[['curves']] <- reportData[['ratingCurves']]
  ratingsTable[['shifts']] <- reportData[['ratingShifts']]
  
  metadataTable <- list()
  metadataTable[['qualifiers']] <- reportData[['qualifiers']]
  metadataTable[['notes']] <- reportData[['notes']]
  metadataTable[['grades']] <- reportData[['grades']]
  
  approvalsTable <- list()
  approvalsTable <- reportData[['approvals']]
  
  return(list(
      relatedSeries = relatedSeriesTable,
      gaps = gapsTable,
      corrections = correctionsTable
  ))
}

formatDataRow <- function(inputData){
  returnData <- data.frame()
  
  if(!isEmptyOrBlank(inputData)){
    returnData <- unname(rowSplit(inputData))
  }
  
  return(returnData)
}