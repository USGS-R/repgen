#' Get the size of a dataframe.
#' 
#' @description Will throw an error if data frame is NULL or NA.
#' @param df the data frame to get the size of
sizeOf <- function(df){
  if (is.null(df)) {
    stop('data frame is null, cannot determine size')
  }
  return(nrow(df))
}

############ used in dvhydrograph-data, fiveyeargwsum-data, uvhydrograph-data ############ 

#' Extract and Restructure Groundwater Levels
#' 
#' @param ts A list, containing a time series data structure.
#' @param ... Unknown. Possibly obsolete.
#' @export
getGroundWaterLevels <- function(ts, ...){
  y <- as.numeric(ts$gwlevel[['groundWaterLevel']])
  x <- ts$gwlevel[['recordDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, field=rep("gwlevel", length(time)), stringsAsFactors = FALSE))
}

#' Extract and Restructure Water Quality Measurements
#' 
#' @param ts A list, containing a time series data structure.
#' @param ... Unknown. Possibly obsolete.
#' @export
getWaterQualityMeasurements<- function(ts, ...){
  if(is.null(ts$waterQuality)) {
    df <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))
    df <- na.omit(df)
    return(df)
  }
  y <- ts$waterQuality$value[['value']]
  x <- ts$waterQuality[['sampleStartDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, field=rep("waterQuality", length(time)), stringsAsFactors = FALSE))
}

#' Extract and Restructure Field Visit Discharge Measurements Points
#' 
#' @param ts A list, containing a time series data structure.
#' @export
getFieldVisitMeasurementsQPoints <- function(ts){
  y <- ts$fieldVisitMeasurements[['discharge']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  minQ <- ts$fieldVisitMeasurements[['errorMinDischarge']]
  maxQ <- ts$fieldVisitMeasurements[['errorMaxDischarge']]
  n <- ts$fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minQ=minQ, maxQ=maxQ, n=n, month=month, 
                    field=rep("fieldVisitMeasurements", length(time)), stringsAsFactors = FALSE))
}

#' Extract and Restructure Field Visit Measurement Shifts
#' 
#' @param ts A list, containing a time series data structure.
#' @export
getFieldVisitMeasurementsShifts <- function(ts){
  if(is.null(ts$fieldVisitMeasurements[['shiftInFeet']])) {
    df <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))
    df <- na.omit(df)
    return(df)
  }
  
  shiftInFeet <- ts$fieldVisitMeasurements[['shiftInFeet']]
  measurementStartDate <- ts$fieldVisitMeasurements[['measurementStartDate']]
  
  errorMinShiftInFeet <- ts$fieldVisitMeasurements[['errorMinShiftInFeet']]
  errorMaxShiftInFeet <- ts$fieldVisitMeasurements[['errorMaxShiftInFeet']]
  
  y <- c()
  x <- c()
  minShift <- c()
  maxShift <- c()
  
  # We index by length(shiftInFeet) here, while admitting it is fairly
  # arbitrary, because it seems like if all these vectors are not the same
  # length, something is likely gravely wrong.
  for (i in 1:length(shiftInFeet)) {
    # if both min. & max. shift values are not the NA indicator
    if (!isEmptyOrBlank(errorMinShiftInFeet[i]) &&
        !isEmptyOrBlank(errorMaxShiftInFeet[i])) {
      # use them
      y <- c(y, shiftInFeet[i])
      x <- c(x, measurementStartDate[i])
      minShift <- c(minShift, errorMinShiftInFeet[i])
      maxShift <- c(maxShift, errorMaxShiftInFeet[i])
    }
  }
  
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minShift=minShift, maxShift=maxShift, month=month, 
                    field=rep("fieldVisitMeasurements", length(time)), stringsAsFactors = FALSE))
}

#' Extract and Restructure Corrections
#' 
#' @param ts A list, containing a time series data structure.
#' @param field A field name.
#' @export
getCorrections <- function(ts, field){
  if(length(ts[[field]]) == 0){
    return()
  }
  
  x <- ts[[field]][['startTime']]
  comment <- ts[[field]][['comment']]
  if(!is.null(comment)) {
    comment <- paste("Start", comment, sep=" : ")
  }
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  
  x2 <- ts[[field]][['endTime']]
  comment2 <- ts[[field]][['comment']]
  if(!is.null(comment2)) {
    comment2 <- paste("End", comment2, sep=" : ")
  }
  time2 = as.POSIXct(strptime(x2, "%FT%T"))
  month2 <- format(time2, format = "%y%m") #for subsetting later by month
  
  #labeled as NA in table:
  if(is.null(comment)){ comment <- "N/A" }
  if(is.null(comment2)){ comment2 <- "N/A" }
  
  #value needs to be NA in order for series corrections to make it through checks in parseUVData
  return(data.frame(time=c(time, time2), value = NA, month=c(month, month2),
                    comment=c(comment, comment2), field=rep(field, length(c(time, time2))), stringsAsFactors = FALSE))
}

getEstimatedDates <- function(data, chain_nm, time_data, isDV=FALSE){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  
  date_index <- c()
  
  startTime <- flexibleTimeParse(data[[chain_nm]]$estimatedPeriods$startDate[i], data$reportMetadata$timezone)
  endTime <- flexibleTimeParse(data[[chain_nm]]$estimatedPeriods$endDate[i], data$reportMetadata$timezone)
  
  est_dates <- data.frame(start = startTime, end = endTime)
  
  for(n in seq(nrow(est_dates))){
    date_index_n <- which(time_data >= est_dates$start[n] & time_data < est_dates$end[n])
    #Could enable later as a fix for dates that start and end at the same time due to precision issues
    #date_index_n <- c(date_index_n,  which(time_data == est_dates$start[n] & time_data == est_dates$end[n]))
    date_index <- append(date_index, date_index_n)
  }
  
  return(date_index)
}

getYvals_approvals <- function(object, num_vals){
  ylim <- ylim(object)$side.2[1]
  yvals <- rep(ylim, num_vals)
  return(yvals)
}

getApprovalDates <- function(data, chain_nm, approval){
  i <- which(data[[chain_nm]]$approvals$description == approval)
  startTime <- flexibleTimeParse(data[[chain_nm]]$approvals$startTime[i], data$reportMetadata$timezone)
  endTime <- flexibleTimeParse(data[[chain_nm]]$approvals$endTime[i], data$reportMetadata$timezone)
  return(data.frame(startTime=startTime, endTime=endTime))
}

#' Extract and Restructure a Time Series
#' 
#' @description This function is deprecated. Please switch over to using 
#'   \code{readTimeSeries} and \code{readEstimatedTimeSeries}. Note that
#'   \code{readTimeSeries} and \code{readEstimatedTimeSeries} now return a list
#'   of the full time series object instead of the data frame created and
#'   returned by this function. This means that downstream calls using this time
#'   series will need to be updated to pass in the correct parameters. See
#'   \code{inst/extdata/testsnippets/test-timeSeries.JSON} in the repgen source
#'   directory for example JSON outlining how a time series returned from 
#'   \code{readTimeSeries}/\code{readEstimatedTimeSeries} will look.
#'
#' @param ts A list, containing a time series data structure.
#' @param field A field name.
#' @param estimatedOnly Extract only estimated values when \code{TRUE}.
#' @param shiftTimeToNoon Reference time to 12:00 p.m. when \code{TRUE}.
#' @export
getTimeSeries <- function(ts, field, estimatedOnly = FALSE, shiftTimeToNoon=TRUE){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  
  if(!is.null(y) & !is.null(x)){
    time <- flexibleTimeParse(x, ts$reportMetadata$timezone, shiftTimeToNoon)
    
    month <- format(time, format = "%y%m") # for subsetting later by month
    # the old data frame format
    uv_series <- data.frame(time=time, value=y, month=month, stringsAsFactors = FALSE)
    
    if(estimatedOnly) {
      s <- ts[[field]]$estimatedPeriods[['startTime']]
      estimatedStartTimes <- as.POSIXct(strptime(s, "%FT%T"))
      e <- ts[[field]]$estimatedPeriods[['endTime']]
      estimatedEndTimes <- as.POSIXct(strptime(e, "%FT%T"))
      estimatedPeriods <- data.frame(start=estimatedStartTimes, end=estimatedEndTimes)
      
      estimatedSubset <- data.frame(time=as.POSIXct(NA), value=as.character(NA), month=as.character(NA))
      estimatedSubset <- na.omit(estimatedSubset)
      for(i in 1:nrow(estimatedPeriods)) {
        p <- estimatedPeriods[i,]
        startTime <- p$start
        endTime <- p$end
        estimatedSubset <- rbind(estimatedSubset, uv_series[uv_series$time > startTime & uv_series$time < endTime,])
      }
      uv_series <- estimatedSubset
    }
    #keep data points in order by date/time
    uv_series <- uv_series[order(uv_series$time),]
    
    #add field for splitDataGaps function
    uv_series$field <- rep(field, nrow(uv_series))
    
    #if this data is on a logged axis, remove negatives and zeros
    loggedData <- isLogged(ts[[field]]$points, ts[[field]][['isVolumetricFlow']], fetchReportMetadataField(ts, 'excludeZeroNegative'))
    flagZeroNeg <- fetchReportMetadataField(ts, 'excludeZeroNegative')
    if(loggedData && !isEmptyOrBlank(flagZeroNeg) && flagZeroNeg){
      uv_series <- removeZeroNegative(uv_series)
    }
    
  } else {
    uv_series <- NULL
  }
  
  return(uv_series)
}

#' Read time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param timezone the timezone to parse times to
#' @param seriesName the name of the time series to extract
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
readTimeSeries <- function(reportObject, seriesName, timezone, shiftTimeToNoon=FALSE) {
  seriesData <- fetchTimeSeries(reportObject, seriesName)

  requiredFields <- c(
    "points",
    "approvals",
    "qualifiers",
    "startTime",
    "endTime",
    "notes",
    "isVolumetricFlow",
    "description",
    "units",
    "grades",
    "type",
    "gaps",
    "estimatedPeriods",
    "gapTolerances",
    "name"
  )

  #Format Point data
  seriesData[['points']][['time']] <- flexibleTimeParse(seriesData[['points']][['time']], timezone, shiftTimeToNoon)
  seriesData[['points']][['value']] <- as.numeric(seriesData[['points']][['value']])
  seriesData[['points']][['month']] <- format(seriesData[['points']][['time']], format = "%y%m")
  seriesData[['points']] <- data.frame(seriesData[['points']])

  #Format Report Metadata
  seriesData[['startTime']] <- flexibleTimeParse(seriesData[['startTime']], timezone)
  seriesData[['endTime']] <- flexibleTimeParse(seriesData[['endTime']], timezone)
  seriesData[['estimated']] <- FALSE

  return(seriesData)
}

#' Read an estaimted time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param timezone the timezone to parse times to
#' @param seriesName the name of the time series to extract
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
readEstimatedTimeSeries <- function(reportObject, seriesName, timezone, shiftTimeToNoon=FALSE) {
  #Read and format all time series data
  seriesData <- readTimeSeries(reportObject, seriesName, timezone, shiftTimeToNoon)
  seriesData[['estimated']] <- TRUE 
  startEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['startTime']], timezone)
  endEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['endTime']], timezone)
  
  #Extract and build estimated periods
  estimatedPeriods <- data.frame(start=startEst, end=endEst)
  estimatedSubset <- data.frame(time=as.POSIXct(NA), value=as.character(NA), month=as.character(NA))
  estimatedSubset <- na.omit(estimatedSubset)

  #Extract only data in estimated periods
  for(i in 1:nrow(estimatedPeriods)) {
    p <- estimatedPeriods[i,]
    startTime <- p$start
    endTime <- p$end
    estimatedSubset <- rbind(estimatedSubset, seriesData[['points']][seriesData[['points']][['time']] > startTime & seriesData[['points']][['time']] < endTime,])
  }

  #Replace data with onyl estimated data
  seriesName[['points']] <- estimatedSubset

  return(seriesData)
}



getTimeSeriesLabel<- function(ts, field){
  param <- ts[[field]]$type
  units <- ts[[field]]$units
  
  if(!is.null(units)) {
    return(paste(param, " (", units, ")"))
  } else {
    return(param)
  }
}
