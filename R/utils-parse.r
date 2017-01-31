#' Parse report metadata field
#' 
#' @description Given a full report object and field name, returns the
#' metadata value for the provided field or returns the default value
#' if the provided field does not exist in the metadata.
#' @param reportObject the object representing the full report JSON
#' @param field the field name to read from the metadata
#' @param defaultValue the optional default value to return if the
#' provided field is not found in the report JSON
parseReportMetadataField <- function(reportObject, field, defaultValue=NULL){
  metaField <- tryCatch({
      readReportMetadataField(reportObject, field)
  }, error=function(e) {
      warning(paste0("Returning default value or NULL for metadata field ", field, " value. Error: ", e))
      return(defaultValue)
  })

  return(metaField)
}

#' Parse the Min or Max IV Data
#'
#' @description Reads the min or max IV Data from the reportObject then takes the
#' first entry and formats it properly for plotting.
#' @param reportObject the full report data
#' @param stat the stat to look up (MAX or MIN)
#' @param tsType the type of the TS (to use for the legend name)
#' @param timezone the timezone to parse the times into
#' @param inverted whether or not the TS is inverted
#' @return the first min or max IV data point from the list of min max IVs
parseMinMaxIV <- function(reportObject, stat, timezone, tsType, inverted){
  IVData <- tryCatch({
    readMinMaxIVs(reportObject, stat, timezone, inverted)
  }, error=function(e) {
    warning(paste("Returning NULL for ", stat, " IV value. Error:", e))
    return(NULL)
  })

  if(is.null(IVData) | isEmptyOrBlank(IVData)){
    returnList <- NULL
  } else {
    legend_nm <- paste(IVData[['label']], tsType, ":", IVData[['value']][1])
    returnList <- list(time=IVData[['time']][1], value=IVData[['value']][1], legend.name=legend_nm)
  }

  return(returnList)
}

#' Parse Min and Max IVs
#'
#' @description Given the full report JSON object, reads the
#' min and max IVs and formats them properly for plotting
#' @param reportObject the full report JSON object
#' @param timezone the time zone to parse points into
#' @param type the type of TS that these points belong to
#' @param invertedFlag whether or not the axis for the TS is inverted
#' @param excludeMinMaxFlag wheter or not min / max IVs should be plotted or labeled
#' @param excludeZeroNegativeFlag whether or not zero/negative values are included
#' @return a list containing the min and max IV values named as 'max_iv' and 'min_iv'
#' as well as a boolean 'canLog' that represents whether or not the IVs allow for a
#' logged Y-Axis.
parseMinMaxIVs <- function(reportObject, timezone, type, invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag){
  #Get max and min IV points
  max_iv <- parseMinMaxIV(reportObject, "MAX", timezone, type, invertedFlag)
  min_iv <- parseMinMaxIV(reportObject, "MIN", timezone, type, invertedFlag)
  returnList <- NULL

  #Make sure at least one value is valid
  if(!anyDataExist(max_iv) && !anyDataExist(min_iv)){
    return(NULL)
  }

  #If we are excluding min/max points or if we are excluding zero / negative
  #points and the max/min vlaues are zero / negative, then replace them
  #with labels that go on the top of the chart.

  #Max Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) || 
      (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(max_iv[['value']]) && as.numeric(max_iv[['value']]) <= 0)){
    returnList <- list(max_iv_label=max_iv)
  }  else if(anyDataExist(max_iv[['value']])){
    returnList <- list(max_iv=max_iv)
  }

  #Min Checking
  if( (!isEmptyOrBlank(excludeMinMaxFlag) && excludeMinMaxFlag) 
      || (!isEmptyOrBlank(excludeZeroNegativeFlag) && excludeZeroNegativeFlag && !isEmptyOrBlank(min_iv[['value']]) && as.numeric(min_iv[['value']]) <= 0) ){
    returnList <- append(returnList, list(min_iv_label=min_iv))
  } else if(anyDataExist(min_iv[['value']])) {
    returnList <- append(returnList, list(min_iv=min_iv))
  }

  #Check if the IVs allow for a log axis or not
  returnList[['canLog']] <- TRUE
  
  if((!isEmptyOrBlank(returnList[['max_iv']][['value']]) && returnList[['max_iv']][['value']] <= 0) || (!isEmptyOrBlank(returnList[['min_iv']][['value']]) && returnList[['min_iv']][['value']] <= 0)){
    returnList[['canLog']] <- FALSE
  }

  return(returnList)
}

#' Parse Ground Water Levels
#'
#' @description Given the full report JSON object reads the ground
#' water levels and handles read errors.
#' @param reportObject the full report JSON object
parseGroundWaterLevels <- function(reportObject){
  gw_level <- tryCatch({
    readGroundWaterLevels(reportObject)
  }, error = function(e) {
    warning(paste("Returning NULL for ground water levels. Error:", e))
    return(NULL)
  })

  if(!anyDataExist(gw_level) || nrow(gw_level) == 0){
    gw_level <- NULL
    warning("Data was retrieved for ground water levels but it was empty. Returning NULL.")
  }
  return(gw_level)
}

#' Parse Field Visit Measurements
#'
#' @description Given the full report JSON object, reads the field
#' visit measurements and handles read errors.
#' @param reportObject the full report JSON object
parseFieldVisitMeasurements <- function(reportObject){
  meas_Q <- tryCatch({
    readFieldVisitMeasurementsQPoints(reportObject)
  }, error = function(e) {
    warning(paste("Returning NULL for field visit measurements. Error:", e))
    return(NULL)
  })

  if(!anyDataExist(meas_Q) || nrow(meas_Q) == 0){
    meas_Q <- NULL
    warning("Data was retrieved for field visit measurements but it was empty. Returning NULL.")
  }
  return(meas_Q)
}

#' Parse Time Series
#'
#' @description Default wrapper for the readTimeSeries functions that handles
#' errors thrown by those functions if the specified time series is
#' not found and throws a warning message. Also handles time series that
#' are returned without any point data and treats them as NULL.
#' @param reportObject the full report JSON object
#' @param seriesField the JSON field name for the TS data 
#' @param descriptionField the JSON field name for the TS legend name
#' @param timezone The timezone to parse the TS points into
#' @param estimated whether or not the retrieved time series should be estimated or non-estimated
#' @return The requested time series or NULL if the request time series was not found.
parseTimeSeries <- function(reportObject, seriesField, descriptionField, timezone, estimated=FALSE, isDV=FALSE, requiredFields=NULL){
  timeSeries <- tryCatch({
    if(estimated){
      readEstimatedTimeSeries(reportObject, seriesField, timezone=timezone, descriptionField=descriptionField, isDV=isDV, requiredFields=requiredFields)
    } else {
      readNonEstimatedTimeSeries(reportObject, seriesField, timezone=timezone, descriptionField=descriptionField, isDV=isDV, requiredFields=requiredFields)
    }
  }, error=function(e) {
    warning(paste("Returning NULL for Time Series: {", seriesField, "}. Error:", e))
    return(NULL)
  })

  if(isEmptyOrBlank(timeSeries) || !anyDataExist(timeSeries[['points']])){
    return(NULL)
  }

  return(timeSeries)
}