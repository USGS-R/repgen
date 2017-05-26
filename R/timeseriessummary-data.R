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
  relatedSeriesTable <- list()
  relatedSeriesTable[['upchain']] <- reportData[['upchainTs']][['identifier']]
  relatedSeriesTable[['downchain']] <- reportData[['downchainTs']][['identifier']]
  releatedSeriesRows <- seq(max(length(relatedSeriesTable[['upchain']]), length(relatedSeriesTable[['downchain']])))
  relatedSeriesTable <- data.frame(relatedSeriesTable[['upchain']][releatedSeriesRows], relatedSeriesTable[['downchain']][releatedSeriesRows], stringsAsFactors = FALSE)
  relatedSeriesTable[is.na(relatedSeriesTable)] <- ""
  colnames(relatedSeriesTable) <- c("upchain", "downchain")
  
  gapsTable <- list()
  gapsTable <- reportData[['gaps']]
  
  correctionsTable <- list()
  correctionsTable[['pre']] <- reportData[['corrections']][['preProcessing']]
  correctionsTable[['normal']] <- reportData[['corrections']][['normal']]
  correctionsTable[['post']] <- reportData[['corrections']][['postProcessing']]
  
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
      relatedSeries = unname(rowSplit(relatedSeriesTable)),
      gaps = unname(rowSplit(gapsTable))
  ))
}