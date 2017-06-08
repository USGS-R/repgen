# This R file's purpose is for extracting values from json 
# The functions shouldn't modify the data, and should handle missing json or empty json parameters

#' Fetch Report Metadata
#'
#' @description Given a full report object this will extract the metadata
#' @param reportObject The full report data loaded from the report JSON
fetchReportMetadata <- function(reportObject){
  val <- reportObject[['reportMetadata']]
  return(val)
}

#' Fetch Report Metadata Field
#'
#' @description Given a full report object this will extract the data
#' associated with the specified field.
#' @param reportObject The full report data loaded from the report JSON
#' @param field The specific field to select from the metadata
fetchReportMetadataField <- function(reportObject, field){
  val <- reportObject[['reportMetadata']][[field]]
  return(val)
}

#' Fetch Approvals for a given Time Series
#'
#' @description Given a full report object this will extract the
#' approvals for the supplied series name.
#' @param reportObject The full report data loaded from the report JSON
#' @param seriesName The specific field to select from the metadata
fetchApprovalsForSeries <- function(reportObject, seriesName){
  val <- reportObject[[seriesName]][['approvals']]
  return(val)
}

#' Fetch Rating Shifts
#'
#' @description Given a report object, will attempt to pull the rating shifts list.
#' @param reportObject the full report data 
#' @return The list of ratingShifts attached to the report. If none, will be NULL.
fetchRatingShifts <- function(reportObject){
  val <- reportObject[['ratingShifts']]
  return(val)
}

#' Fetch Discharge measurements
#'
#' @description Given a report object, will attempt to pull the measurements list.
#' @param reportObject the full report data 
#' @return The list of measurements attached to the report. If none, will be NULL.
fetchMeasurements <- function(reportObject){
  val <- reportObject[['measurements']]
  return(val)
}

#' Fetch maximum stage height
#'
#' @description Given a report object will pull the max stage value.
#' @param reportObject a report object
#' @return numeric value for max stage
fetchMaxStage <- function(reportObject){
  val <- as.numeric(reportObject[['maximumStageHeight']][['value']])
  return(val)
}

#' Fetch minimum stage height
#'
#' @description Given a report object will pull the min stage value.
#' @param reportObject a report object
#' @return numeric value for min stage
fetchMinStage <- function(reportObject){
  val <- as.numeric(reportObject[['minimumStageHeight']][['value']])
  return(val)
}

#' Fetch time series
#'
#' @description Given a report object, will pull time series with given name
#' @param reportObject the full report data
#' @param seriesName the time series name to fetch
fetchTimeSeries <- function(reportObject, seriesName){
  val <- reportObject[[seriesName]]
  return(val)
}

#' Fetch ground water levels
#'
#' @description Given a report object, will pull the ground water levels
#' @param reportObject the full report data
fetchGroundWaterLevels <- function(reportObject){
  val <- reportObject[['gwlevel']]
  return(val)
}

#' Fetch water quality measurements
#'
#' @description Given a report object, will pull the water quality measurements
#' @param reportObject the full report data
fetchWaterQualityMeasurements <- function(reportObject){
  val <- reportObject[['waterQuality']]
  return(val)
}

#' Fetch field visit measurements
#'
#' @description Given a report object, will pull the field visit measurements
#' @param reportObject the full report data
fetchFieldVisitMeasurements <- function(reportObject){
  val <- reportObject[['fieldVisitMeasurements']]
  return(val)
}

#' Fetch corrections for a time series
#' 
#' @description Given a report object and series name, will pull the corrections
#' list for the specified time series
#' @param reportObject the full report data
#' @param seriesCorrName the time series name to pull corrections for
fetchCorrections <- function(reportObject, seriesCorrName){
  val <- reportObject[[seriesCorrName]]
  return(val)
}

#' Fetch Min/Max IV for DV Hydro Report
#'
#' @description Given a report object, will pull the min or max IV points data
#' @param reportObject the full report data
#' @param stat 'MIN' or 'MAX' data to select
fetchMinMaxIVs <- function(reportObject, stat){
  stat <- toupper(stat)
  stat_val <- reportObject[['maxMinData']][['seriesTimeSeriesPoints']][[1]][['theseTimeSeriesPoints']][[stat]]
  return(stat_val)
}

#' Fetch field visit readings
#'
#' @description Given a report object, will pull the field visit readings
#' @param reportObject the full report data
fetchFieldVisitReadings <- function(reportObject){
  val <- reportObject$readings
  return(val)
}

#' Fetch Rating Shifts Field
#'
#' @description Given a full report object this will extract the data
#' associated with the specified field.
#' @param reportObject The full report data loaded from the report JSON
#' @param field The specific field to select from the rating shifts
fetchRatingShiftsField <- function(reportObject, field){
  val <- reportObject[['ratingShifts']][[field]]
  return(val)
}

#' Fetch Measurements Field
#'
#' @description Given a full report object this will extract the data
#' associated with the specified field.
#' @param reportObject The full report data loaded from the report JSON
#' @param field The specific field to select from the measurements
fetchMeasurementsField <- function(reportObject, field){
  val <- reportObject[['measurements']][[field]]
  return(val)
}

#'Fetch Primary Series Approvals (DV and Five YR)
#'
#' @description Given a full report object this will extract the data 
#' for primary series approvals.
#' @param reportObject The full report data loaded from the report JSON
fetchPrimarySeriesApprovals <- function(reportObject){
  val <- reportObject[['primarySeriesApprovals']]
  return(val)
}

#'Fetch Primary Series Qualifiers (DV and Five YR)
#'
#' @description Given a full report object this will extract the data 
#' for primary series qualifiers.
#' @param reportObject The full report data loaded from the report JSON
fetchPrimarySeriesQualifiers <- function(reportObject){
  val <- reportObject[['primarySeriesQualifiers']]
  return(val)
}

#'Fetch Field Visits (CORR)
#'
#' @description Given a full report object this will extract the field
#' vists data.
#' @param reportObject The full report data loaded from the report JSON
fetchFieldVists <- function(reportObject){
  val <- reportObject[['fieldVisits']]
  return(val)
}

#'Fetch Processing Corrections (CORR)
#'
#' @description Given a full report object this will extract the corrections
#' data for the specified processing order.
#' @param reportObject The full report data loaded from the report JSON
#' @param processOrder The processing order to get corrections for. Valid
#' choices: "pre", "post", and "normal"
fetchProcessingCorrections <- function(reportObject, processOrder){
  processOrder <- switch(processOrder,
    "pre"="preProcessing",
    "post"="postProcessing",
    "normal"="normal",
    ""
  )
  
  val <- reportObject[['corrections']][[processOrder]]
  
  return(val)
}

#'Fetch Threshold Data (CORR)
#'
#' @description Given a full report object this will extract the thresholds
#' @param reportObject The full report JSON object
fetchThresholds <- function(reportObject){
  val <- reportObject[['thresholds']]
  return(val)
}

#'Fetch Excluded Control Conditions (V-Diagram)
#'
#'@description Given a full report object this will extract the excluded control conditions
#'@param reportObject The full report JSON object
fetchExcludedControlConditions <- function(reportObject){
  val <- reportObject[['excludedControlConditions']]
  return(val)
}

#'Fetch Gaps (TSS)
#'
#'@description Given a full report object this will extract the gaps
#'@param reportObject The full report JSON object
fetchGaps <- function(reportObject){
  val <- reportObject[['gaps']]
  return(val)
}

#'Fetch Upchain Series (TSS)
#'
#'@description Given a full report object this will extract the upchain related series
#'@param reportObject The full report JSON object
fetchUpchainSeries <- function(reportObject){
  val <- reportObject[['upchainTs']]
  return(val)
}

#'Fetch Downchain Series (TSS)
#'
#'@description Given a full report object this will extract the downchain related series
#'@param reportObject The full report JSON object
fetchDownchainSeries <- function(reportObject){
  val <- reportObject[['downchainTs']]
  return(val)
}

#'Fetch Qualifiers (TSS)
#'
#'@description Given a full report object this will extract the qualifiers
#'@param reportObject The full report JSON object
fetchQualifiers <- function(reportObject){
  val <- reportObject[['primaryTsMetadata']][['qualifiers']]
  return(val)
}

#'Fetch Notes (TSS)
#'
#'@description Given a full report object this will extract the notes
#'@param reportObject The full report JSON object
fetchNotes <- function(reportObject){
  val <- reportObject[['primaryTsMetadata']][['notes']]
  return(val)
}

#'Fetch Grades (TSS)
#'
#'@description Given a full report object this will extract the grades
#'@param reportObject The full report JSON object
fetchGrades <- function(reportObject){
  val <- reportObject[['primaryTsMetadata']][['grades']]
  return(val)
}

#'Fetch Rating Curves (TSS)
#'
#'@description Given a full report object this will extract the rating curves
#'@param reportObject The full report JSON object
fetchRatingCurves <- function(reportObject){
  val <- reportObject[['ratingCurves']]
  return(val)
}

#'Fetch Approvals (TSS)
#'
#'@description Given a full report object this will extract the approvals
#'@param reportObject The full report JSON object
fetchApprovals <- function(reportObject){
  val <- reportObject[['approvals']]
  return(val)
}

#'Fetch Gap Tolerances (TSS)
#'
#'@description Given a full report object this will extract the gap tolerances
#'@param reportObject The full report JSON object
fetchGapTolerances <- function(reportObject){
  val <- reportObject[['gapTolerances']]
  return(val)
}