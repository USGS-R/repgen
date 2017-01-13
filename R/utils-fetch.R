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
#' @param field The specific field to select from the metadata
fetchApprovalsForSeries <- function(reportObject, seriesName){
  val <- reportObject[[seriesName]][['approvals']]
  return(val)
}

# used in dvhydrograph and fiveyrgwsum
parseEstimatedStatDerived <- function(data, points, date_index, legend_nm, chain_nm, estimated){
  if(estimated){
    formatted_data <- list(time = points[['time']][date_index],
                           value = points[['value']][date_index],
                           legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]),
                           estimated=estimated)
  } else if(!estimated && length(date_index) != 0) {
    formatted_data <- list(time = points[['time']][-date_index],
                           value = points[['value']][-date_index],
                           legend.name = data[['reportMetadata']][[legend_nm]],
                           estimated=estimated)
  } else {
    formatted_data <- list(time = points[['time']],
                           value = points[['value']],
                           legend.name = data[['reportMetadata']][[legend_nm]],
                           estimated=estimated)
  }
  
  formatted_data$field <- chain_nm
  return(formatted_data)
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
  val <- as.numeric(reportObject[['maximumStageHeight']])
  return(val)
}

#' Fetch minimum stage height
#'
#' @description Given a report object will pull the min stage value.
#' @param reportObject a report object
#' @return numeric value for min stage
fetchMinStage <- function(reportObject){
  val <- as.numeric(reportObject[['minimumStageHeight']])
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
  val <- reportObject$gwlevel
  return(val)
}

#' Fetch water quality measurements
#'
#' @description Given a report object, will pull the water quality measurements
#' @param reportObject the full report data
fetchWaterQualityMeasurements <- function(reportObject){
  val <- reportObject$waterQuality
  return(val)
}

#' Fetch field visit measurements
#'
#' @description Given a report object, will pull the field visit measurements
#' @param reportObject the full report data
fetchFieldVisitMeasurements <- function(reportObject){
  val <- reportObject$fieldVisitMeasurements
  return(val)
}