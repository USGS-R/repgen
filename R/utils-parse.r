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

#' Parse the Min or Max IV Data DVHydro
#'
#' @description Reads the min or max IV Data from the reportObject then takes the
#' first entry and formats it properly for plotting.
#' @param reportObject the full report data
#' @param stat the stat to look up (MAX or MIN)
#' @param tsType the type of the TS (to use for the legend name)
#' @param timezone the timezone to parse the times into
#' @param inverted whether or not the TS is inverted
#' @return the first min or max IV data point from the list of min max IVs
parseMinMaxIVDV <- function(reportObject, stat, timezone, tsType, inverted){
  IVData <- tryCatch({
    readMinMaxIVsDV(reportObject, stat, timezone, inverted)
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

#' Parse Min and Max IVs DVHydro
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
parseMinMaxIVsDV <- function(reportObject, timezone, type, invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag){
  #Get max and min IV points
  max_iv <- parseMinMaxIVDV(reportObject, "max", timezone, type, invertedFlag)
  min_iv <- parseMinMaxIVDV(reportObject, "min", timezone, type, invertedFlag)
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

#' Parse Water Quality Measurements
#'
#' @description Given the full report JSON object reads the water
#' quality measurements and handles read errors.
#' @param reportObject the full report JSON object
parseWaterQualityMeasurements <- function(reportObject){
  wqdata <- tryCatch({
    readWaterQualityMeasurements(reportObject)
  }, error = function(e) {
    warning(paste("Returning NULL for water quality measurements. Error:", e))
    return(NULL)
  })

  if(!anyDataExist(wqdata) || nrow(wqdata) == 0){
    wqdata <- NULL
    warning("Data was retrieved for water quality measurements but it was empty. Returning NULL.")
  }
  return(wqdata)
}

#' Parse Field Visit Measurements
#'
#' @description Given the full report JSON object, reads the field
#' visit measurements and handles read errors.
#' @param reportObject the full report JSON object
#' @param excludeZeroNegativeFlag whether or not zero/negative values are included
parseFieldVisitMeasurements <- function(reportObject, excludeZeroNegativeFlag){
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
  
  #Check if the field visit measurements (if they exist) allow for a log axis or not, and remove zeros/negative values if indicated
  if (!isEmptyOrBlank(meas_Q)) {
    meas_Q[['canLog']] <- isLogged(meas_Q, isVolFlow=TRUE, excludeZeroNegativeFlag)
  }
  return(meas_Q)
}

#' Parse Field Visit Readings
#'
#' @description Given the full report JSON object, reads the field
#' visit readings and handles read errors.
#' @param reportObject the full report JSON object
parseFieldVisitReadings <- function(reportObject){
  readings <- tryCatch({
    readFieldVisitReadings(reportObject)
  }, error = function(e) {
    warning(paste("Returning NULL for field visit readings. Error:", e))
    return(NULL)
  })
  
  if(!anyDataExist(readings) || nrow(readings) == 0){
    readings <- NULL
    warning("Data was retrieved for field visit readings but it was empty. Returning NULL.")
  }
  
  return(readings)
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
#' @param isDV true to treat the series as a DV series (and parse dates accordingly), defaults to FALSE
#' @param requiredFields An optional list of names of required JSON fields to overwrite the default
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

#' Parse Time Series DV
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
#' @param isDV true to treat the series as a DV series (and parse dates accordingly), defaults to FALSE
#' @param requiredFields An optional list of names of required JSON fields to overwrite the default
#' @return The requested time series or NULL if the request time series was not found.
parseTimeSeriesDV <- function(reportObject, seriesField, descriptionField, timezone, estimated=FALSE, isDV=FALSE, requiredFields=NULL){
  timeSeries <- tryCatch({
    if(estimated){
      readEstimatedTimeSeriesDV(reportObject, seriesField, timezone=timezone, descriptionField=descriptionField, isDV=isDV, requiredFields=requiredFields)
    } else {
      readNonEstimatedTimeSeriesDV(reportObject, seriesField, timezone=timezone, descriptionField=descriptionField, isDV=isDV, requiredFields=requiredFields)
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

#' Parse Primary Series Approvals (Five YR)
#'
#' @description Default wrapper for the readPrimarySeriesApprovals function
#' that handles errors thrown and returns the proper data.
#' @param reportObject the full report JSON object
#' @param startDate the start date of the report
#' @param endDate the end date of the report
parsePrimarySeriesApprovals <- function(reportObject, startDate, endDate){
  approvals <- tryCatch({
    readPrimarySeriesApprovals(reportObject, startDate, endDate)
  }, error=function(e) {
    warning(paste("Returning NULL for Primary Series Approvals. Error:", e))
    return(NULL)
  })

  return(approvals)
}

#' Parse Primary Series Approvals (DV)
#'
#' @description Default wrapper for the readPrimarySeriesApprovals function
#' that handles errors thrown and returns the proper data.
#' @param reportObject the full report JSON object
#' @param startDate the start date of the report
#' @param endDate the end date of the report
parsePrimarySeriesApprovalsDV <- function(reportObject, startDate, endDate){
  approvals <- tryCatch({
    readPrimarySeriesApprovalsDV(reportObject, startDate, endDate)
  }, error=function(e) {
    warning(paste("Returning NULL for Primary Series Approvals. Error:", e))
    return(NULL)
  })
  
  return(approvals)
}

#' Parse Primary Series Qualifiers (Five YR)
#'
#' @description Default wrapper for the readPrimarySeriesQualifiers function
#' that handles errors thrown and returns the proper data.
#' @param reportObject the full report JSON object
#' @param filterCode The code to filter read qualifiers to
parsePrimarySeriesQualifiers <- function(reportObject, filterCode=NULL){
  qualifiers <- tryCatch({
    readPrimarySeriesQualifiers(reportObject, filterCode=filterCode)
  }, error=function(e) {
    warning(paste("Returning NULL for Primary Series Qualifiers Error:", e))
    return(NULL)
  })
  
  return(qualifiers)
}

#' Parse Primary Series Qualifiers (DV)
#'
#' @description Default wrapper for the readPrimarySeriesQualifiers function
#' that handles errors thrown and returns the proper data.
#' @param reportObject the full report JSON object
#' @param filterCode The code to filter read qualifiers to
parsePrimarySeriesQualifiersDV <- function(reportObject, filterCode=NULL){
  qualifiers <- tryCatch({
    readPrimarySeriesQualifiersDV(reportObject, filterCode=filterCode)
  }, error=function(e) {
    warning(paste("Returning NULL for Primary Series Qualifiers Error:", e))
    return(NULL)
  })
  
  return(qualifiers)
}

#' Parse Excluded Control Conditions (VDI)
#' 
#' @description Default wrapper for the readExcludedControlConditions function
#' that handles errors thrown and returns the proper data.
#' @param reportObject The full report JSON object
parseExcludedControlConditions <- function(reportObject){
  conditions <- tryCatch({
    readExcludedControlConditions(reportObject)
  }, error=function(e){
    warning(paste("Returning empty list for Excluded Control Conditions. Error:", e))
    return(NULL)
  })
  
  return(conditions)
}

#' Parse Rating Shifts Data
#' @description Takes in a report object and returns the rating shift information
#' @param reportObject An R object with the raw data required for rating shifts
#' @return A list containing rating shift information
#'
parseRatingShiftsData <- function(reportObject){
  shiftPoints <- fetchRatingShiftsField(reportObject, "shiftPoints")
  validParam(shiftPoints, "shiftPoints")
  
  stagePoints <- fetchRatingShiftsField(reportObject, "stagePoints")
  validParam(stagePoints, "stagePoints")
  
  shiftId <- fetchRatingShiftsField(reportObject, "shiftNumber")
  validParam(shiftId, "shiftNumber")
  
  startTime <- fetchRatingShiftsField(reportObject, "applicableStartDateTime")
  validParam(startTime, "applicableStartDateTime")
  
  endTime <- fetchRatingShiftsField(reportObject, "applicableEndDateTime")
  validParam(endTime, "applicableEndDateTime")
  
  rating <- fetchRatingShiftsField(reportObject, "curveNumber")
  validParam(rating, "curveNumber")
  
  comments <- fetchRatingShiftsField(reportObject, "shiftRemarks")
  validParam(comments, "shiftRemarks")
  
  ratingShifts <- fetchRatingShifts(reportObject)
  
  numOfShifts <- ifelse(!isEmptyOrBlank(ratingShifts), sizeOf(ratingShifts), 0)
  
  return(list(
    shiftPoints=shiftPoints, 
    stagePoints=stagePoints, 
    shiftId=shiftId, 
    startTime=startTime,
    endTime=endTime,
    numOfShifts=numOfShifts,
    rating=rating,
    comments=comments))
}

#' Parse Processing Corrections
#'
#' @description Default wrapper for the readProcessingCorrections function
#' that handles errors thrown and returns the proper data
#' @param reportObject The full report JSON object 
#' @param processOrder The processing order to fetch data for
#' @param timezone The timezone to parse data into
parseProcessingCorrections <- function(reportObject, processOrder, timezone){
  corrections <- tryCatch({
    readProcessingCorrections(reportObject, processOrder, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for", processOrder, "corrections. Error:", e))
    return(NULL)
  })
  
  return(corrections)
}

#' Parse Thresholds
#'
#' @description Default wrapper for the readThresholds function
#' that handles errors thrown and returns the proper data
#' @param reportObject The full report JSON object
#' @param timezone The timezone to parse data into
parseThresholds <- function(reportObject, timezone){
  thresholds <- tryCatch({
    readThresholds(reportObject)
  }, error=function(e){
    warning(paste("Returning NULL for thresholds. Error:", e))
    return(NULL)
  })
  
  return(thresholds)
}

#' Parse Gaps
#'
#' @description Default wrapper for the readGaps function
#' that handles errors thrown and returns the proper data
#' @param reportObject The full report JSON object
#' @param timezone The timezone to parse data into
parseGaps <- function(reportObject, timezone){
  gaps <- tryCatch({
    readGaps(reportObject, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for gaps. Error:", e))
    return(NULL)
  })
  
  return(gaps)
}

#' Parse Approvals
#'
#' @description Default wrapper for the readApprovals function
#' that handles errors thrown and returns the proper data
#' @param reportObject The full report JSON object
#' @param timezone The timezone to parse data into
parseApprovals <- function(reportObject, timezone){
  approvals <- tryCatch({
    readApprovals(reportObject, timezone)
  }, error=function(e){
    warning(paste("Returning NULL for approvals. Error:", e))
    return(NULL)
  })
  
  return(approvals)
}