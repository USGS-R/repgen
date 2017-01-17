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
#' Read ground water levels
#' 
#' @description Given a full report object, returns the ground water levels
#' measurements formatted as a time series point set.
#' @param reportObject the object representing the full report JSON
readGroundWaterLevels <- function(reportObject){
  #Fetch and Validate Data
  gwData <- fetchGroundWaterLevels(reportObject)
  requiredFields <- c('groundWaterLevel', 'recordDateTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- na.omit(returnDf)

  #Transform data
  if(validateFetchedData(gwData, 'Ground Water Levels', requiredFields)){
    value <- as.numeric(gwData[['groundWaterLevel']])
    time <- as.POSIXct(strptime(gwData[['recordDateTime']], "%FT%T"))
    month <- format(time, format = "%y%m")
    returnDf <- data.frame(time=time, value=value, month=month, stringsAsFactors=FALSE)
  }
  return(returnDf)
}

#' Read water quality measurements
#'
#' @description Given a full report object, returns the water quality
#' measurements formatted as a time series point set.
#' @param reportObject the object representing the full report JSON
readWaterQualityMeasurements <- function(reportObject){
  #Fetch and Validate Data
  wqData <- fetchWaterQualityMeasurements(reportObject)
  requiredFields <- c('value', 'sampleStartDateTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- na.omit(returnDf)

  #Transform data
  if(validateFetchedData(wqData, 'Water Quality measurements', requiredFields)){
    value <- wqData[['value']][['value']]
    time <- as.POSIXct(strptime(wqData[['sampleStartDateTime']], "%FT%T"))
    month <- format(time, format = "%y%m")
    returnDf <- data.frame(time=time, value=value, month=month, stringsAsFactors=FALSE)
  }

  return(returnDf)
}

#' Read field visit measurements
#'
#' @description Given a full report object, returns the field visit 
#' measurement discharge points formatted as a time series point set
#' @param reportObject the object representing the full report JSON
readFieldVisitMeasurementsQPoints <- function(reportObject){
  visitData <- fetchFieldVisitMeasurements(reportObject)
  requiredFields <- c('discharge', 'measurementStartDate', 'errorMinDischarge', 'errorMaxDischarge', 'measurementNumber')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- na.omit(returnDf)

  if(validateFetchedData(visitData, "Field Visit Measurements", requiredFields)){
    value <- visitData[['discharge']]
    time <- as.POSIXct(strptime(visitData[['measurementStartDate']], "%FT%T"))
    minQ <- visitData[['errorMinDischarge']]
    maxQ <- visitData[['errorMaxDischarge']]
    n <- visitData[['measurementNumber']]
    month <- format(time, format = "%y%m")
    returnDf <- data.frame(time=time, value=value, minQ=minQ, maxQ=maxQ, n=n, month=month, stringsAsFactors=FALSE)
  }

  return(returnDf)
}

#' Read field visit readings
#'
#' @description Given a full report object, returns the field visit 
#' readings formatted as a data frame
#' @param reportObject the object representing the full report JSON
readFieldVisitReadings <- function(reportObject,commentFlag){
  visitReadings <- fetchFieldVisitReadings(reportObject)
  requiredFields <- c('time')
  returnDf <- data.frame(time=as.character(NA), party=as.character(NA), sublocation=as.character(NA), monitoringMethod=as.character(NA), value=as.character(NA), uncertainty=as.character(NA), estimatedTime=as.character(NA), comments=as.list(NA), associatedIvValue=as.character(NA), qualifiers=as.list(NA), associatedIvTime=as.character(NA), diffPeak=as.character(NA),stringsAsFactors=FALSE)

  if(validateFetchedData(visitReadings, "Readings", requiredFields)){
    time <- as.POSIXct(visitReadings[['time']])
    party <- visitReadings[['party']]
    sublocation <- visitReadings[['sublocation']]
    monitoringMethod <- visitReadings[['monitoringMethod']]
    value <- visitReadings[['value']]
    uncertainty <- visitReadings[['uncertainty']]
    estimatedTime <- visitReadings[['estimatedTime']] 
    comments <- visitReadings[['comments']]
    associatedIvValue <- visitReadings[['uncertainty']]
    qualifiers <- sapply(readQualifiers(visitReadings[['associatedIvTime']], visitReadings[['associatedIvQualifiers']]), paste0)
    associatedIvTime <- visitReadings[['associatedIvTime']]
    diffPeak <- readIvDifference(visitReadings[['value']], visitReadings[['associatedIvValue']])
    returnDf <- data.frame(time=time, party=party, sublocation=sublocation, monitoringMethod=monitoringMethod, value=value, uncertainty=uncertainty, estimatedTime=estimatedTime, comments=comments, associatedIvValue=associatedIvValue, qualifiers=qualifiers, associatedIvTime=associatedIvTime, diffPeak=diffPeak)
  }
  
  return(returnDf)
}

#' Read field visit readings qualifiers
#'
#' @description Given an associated Instantaneous Value date and time and qualifiers, 
#' returns the qualifiers formatted as a data frame
#' @param time associated Instantaneous Value date and time
#' @param inQualifiers list of associated Instantaneous Value qualifiers
readQualifiers <- function(time, inQualifiers) {
  
  returnDf <- data.frame(code=as.character(NA), identifier=as.character(NA), description=as.character(NA), stringsAsFactors=FALSE)
  
  if(length(inQualifiers) < 1) return(NA);
  q <- inQualifiers[[1]]
  
  if(is.null(q) || length(q) < 1) return(NA);
  
  qualifiers <- q[time>q$startDate & q$endDate>time,]
  code <- qualifiers[['code']]
  identifier <- qualifiers[['identifier']]
  description <- qualifiers[['displayName']]
  returnDf <- data.frame(code=code, indentifier=identifier, description=description)
  return(returnDf)
}

readIvDifference <- function(readingVal, ivVal) {
  result <- "NA"
  v1 <- as.numeric(readingVal)
  v2 <- as.numeric(ivVal)
  if(is.numeric(v1) & is.numeric(v2)) {
    val <- v2-v1
    if(!is.na(val) && all(c(length(v1),length(v2)) != 0)) {
      result <- as.character(round(val, digits = nchar(ivVal)))
      
      if(abs(val) > 0.05) {
        result <- paste(result, "**")
      }
    }
  }
  return(result)
}

#' Read field visit measurements shifts
#'
#' @description Given a full report object, returns the field visit
#' measurement shifts data formatted as a time series point set
#' @param reportObject the object representing the full report JSON
readFieldVisitMeasurementsShifts <- function(reportObject){
  visitData <- fetchFieldVisitMeasurements(reportObject)
  requiredFields <- c('shiftInFeet', 'measurementStartDate', 'errorMinShiftInFeet', 'errorMaxShiftInFeet')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- na.omit(returnDf)

  if(validateFetchedData(visitData, "Field Visit Measurements", requiredFields)){
    shiftInFeet <- visitData[['shiftInFeet']]
    measurementStartDate <- visitData[['measurementStartDate']]
    errorMinShiftInFeet <- visitData[['errorMinShiftInFeet']]
    errorMaxShiftInFeet <- visitData[['errorMaxShiftInFeet']]

    value <- c()
    time <- c()
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
        value <- c(value, shiftInFeet[i])
        time <- c(time, measurementStartDate[i])
        minShift <- c(minShift, errorMinShiftInFeet[i])
        maxShift <- c(maxShift, errorMaxShiftInFeet[i])
      }
    }
    time <- as.POSIXct(strptime(time, "%FT%T")) 
    month <- format(time, format = "%y%m")

    returnDf <- data.frame(time=time, value=value, minShift=minShift, maxShift=maxShift, stringsAsFactors=FALSE)
  }

  return(returnDf)
}

#' Read corrections
#' 
#' @description Given a full report object and the name of a time series,
#' returns the corrections list for that time series
#' @param reportObject the object representing the full report JSON
readCorrections <- function(reportObject, seriesCorrName){
  corrData <- fetchCorrections(reportObject, seriesCorrName)
  requiredFields <- c('startTime', 'endTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- na.omit(returnDf)

  if(validateFetchedData(corrData, seriesName, requiredFields)){
    timeStart <- as.POSIXct(strptime(corrData[['startTime']], "%FT%T"))
    monthStart <- format(timeStart, format = "%y%m")
    commentStart <- corrData[['comment']]

    timeEnd <- as.POSIXct(strptime(corrData[['endTime']], "%FT%T"))
    monthEnd <- format(timeEnd, format = "%y%m")
    commentEnd <- corrData[['comment']]

    if(!is.null(commentStart)){
      commentStart <- paste("Start", commentStart, sep=" : ")
    }

    if(!is.null(commentEnd)){
      commentEnd <- paste("End", commentEnd, sep=" : ")
    }

    returnDf <- data.frame(time=c(timeStart, timeEnd), value=NA, month=c(monthStart, monthEnd), comment=c(commentStart, commentEnd), stringsAsFactors=FALSE)
  }

  return(returnDf)
}

#' Read Approval Points
#' @description given a list of approvals and points, will return the points divided up into separate lists for the different approval levels
#' @param approvals list of approvals
#' @param points list of points to apply approvals to
#' @param timezone the timezone to convert everything to
#' @param legend_nm the name of the series to put in label (suffix)
#' @param appr_var_all the ordered variable names to map the approval levels (Approved, In Review, Working) to (Eg: c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv") )
#' @param point_type the symbol to attach to each point
#' @return list of point lists, for each approval level. The name of each list will be taken from appr_var_all
readApprovalPoints <- function(approvals, points, timezone, legend_nm, appr_var_all, point_type=NULL){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  
  working_index <- readApprovalIndex(points, approvals, "Working", timezone);
  review_index <- readApprovalIndex(points, approvals, "In Review", timezone);
  approved_index <- readApprovalIndex(points, approvals, "Approved", timezone);
  
  review_index <- setdiff(review_index, working_index)
  approved_index <- setdiff(approved_index, working_index)
  approved_index <- setdiff(approved_index, review_index)
  
  date_index_list <- list(list(type="Approved",approved_index), list(type="In Review",review_index), list(type="Working",working_index))
  
  for(sub_list in date_index_list){
    approval_info <- list()
    for(list in sub_list){
      appr_var <- appr_var_all[which(appr_type == sub_list["type"])]
      for(i in seq_along(list)){
        d <- list[[i]]
        
        applicable_dates <- points[['time']][d]
        applicable_values <- points[['value']][d]
        
        approval_info[[i]] <- list(time = applicable_dates,
            value = applicable_values,
            legend.name = paste(sub_list["type"], legend_nm),
            point_type = point_type)
      }
      
      if(length(approval_info) > 0){
        names(approval_info) <- rep(appr_var, length(list))
      }
    }
    approvals_all <- append(approvals_all, approval_info)
  }
  
  return(approvals_all)
}

#' Read Approval Bars
#' @description for a timeseries, will return a list of approval bars to be plotted
#' @param ts the timeseries to get approval bars for
#' @param timezone the timezone to convert all times to
#' @param legend_nm the name to be assigned to the legend entries (as a suffix)
#' @param snapToDayBoundaries true to shift all bar edges to the closest end/beginning of the days
#' @param returns a list of approval bar ranges, lists should contain the possible named items appr_working_uv, appr_inreview_uv, appr_approved_uv
readApprovalBar <- function(ts, timezone, legend_nm, snapToDayBoundaries=FALSE){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  approval_info <- list()
  appr_dates <- NULL
  
  if (!isEmptyOrBlank(ts$approvals$startTime) && !isEmptyOrBlank(ts$startTime)) {
    startTime <-
        flexibleTimeParse(ts$approvals$startTime, timezone = timezone)
    chain.startTime <-
        flexibleTimeParse(ts$startTime, timezone = timezone)
    
    # clip start points to chart window
    for (i in 1:length(startTime)) {
      if (startTime[i] < chain.startTime) {
        startTime[i] <- chain.startTime
      }
    }
    
    endTime <-
        flexibleTimeParse(ts$approvals$endTime, timezone = timezone)
    chain.endTime <-
        flexibleTimeParse(ts$endTime, timezone = timezone)
    
    # clip end points to chart window
    for (i in 1:length(endTime)) {
      if (chain.endTime < endTime[i]) {
        endTime[i] <- chain.endTime
      }
    }
    
    type <- ts$approvals$description
    type <- unlist(lapply(type, function(desc) {
              switch(
                  desc,
                  "Working" = "appr_working_uv",
                  "In Review" = "appr_inreview_uv",
                  "Approved" = "appr_approved_uv"
              )
            }))
    legendnm <- ts$approvals$description
    appr_dates <-
        data.frame(
            startTime = startTime, endTime = endTime,
            type = type, legendnm = legendnm,
            stringsAsFactors = FALSE
        )
  }
  
  if (!isEmpty(appr_dates) && nrow(appr_dates)>0) {
    for(i in 1:nrow(appr_dates)){
      start <- appr_dates[i, 1];
      end <- appr_dates[i, 2];
      t <- appr_dates[i, 3];
      
      if(snapToDayBoundaries) {
        if(t == 'appr_working_uv') { #working always extends outward
          start <- toStartOfDay(start)
          end <- toEndOfDay(end)
        } else if(t =='appr_approved_uv') { #working always extends inward
          start <- toEndOfDay(start)
          end <- toStartOfDay(end)
        } else { #appr_inreview_uv case, have to determine which way to extend based on bracketing approvals (if any)
          #start side
          if(i == 1) { #no approval to the left so expand
            start <- toStartOfDay(start)
          } else if(appr_dates[(i-1), 3] == "appr_approved_uv"){
            start <- toStartOfDay(start)
          } else if(appr_dates[(i-1), 3] == "appr_working_uv"){
            start <- toEndOfDay(start)
          }
          
          #end side
          if(i == nrow(appr_dates)) { #no approval to the right so expand
            end <- toEndOfDay(end)
          } else if(appr_dates[(i+1), 3] == "appr_approved_uv"){
            end <- toEndOfDay(end)
          } else if(appr_dates[(i+1), 3] == "appr_working_uv"){
            end <- toStartOfDay(end)
          }
        }
      }
      
      approval_info[[i]] <- list(
          x0 = start, x1 = end,
          legend.name = paste(appr_dates[i, 4], legend_nm),
          time = appr_dates[1, 1]
      ) ##added a fake time var to get through a future check
      
      names(approval_info)[[i]] <- appr_dates[i, 3]
    }
    approvals_all <- append(approvals_all, approval_info)
    
  }
  
  return(approvals_all)
}

#' Read Approval index
#' @description Given a list of points, a set of approvals, and the approvalLevel to apply, will return the indexes of all points to be assigned the approval level
#' @param points the points to apply approvals against
#' @param approvals list of approvals to read from
#' @param approvalLevel the approval level to read and apply to points
#' @param timezone the timezone to convert all datetimes to (in approvals)
readApprovalIndex <- function(points, approvals, approvalLevel, timezone) {
  points$time <- as.POSIXct(strptime(points$time, "%F"))
  dates <- readApprovalRanges(approvals, approvalLevel, timezone)
  dates$startTime <- as.POSIXct(strptime(dates$startTime, "%F"))
  dates$endTime <- as.POSIXct(strptime(dates$endTime, "%F"))
  
  dates_index <- apply(dates, 1, function(d, points){
        which(points$time >= d[1] & points$time <= d[2])}, 
      points=points)
  
  if(class(dates_index) == "list"){
    dates_index <- unique(unlist(dates_index, recursive=FALSE))
  }
  
  return(dates_index)
}

#' Read Approval Ranges
#' @param approvals the approvals list object to read from
#' @param approvalLevel the approval level to read, typically "Working", "In Review", or "Approved"
#' @param timezone the timezone to parse times to
#' @return data frame of start and end times for each approval range
readApprovalRanges <- function(approvals, approvalLevel, timezone){
  i <- which(approvals$description == approvalLevel)
  startTime <- flexibleTimeParse(approvals$startTime[i], timezone)
  endTime <- flexibleTimeParse(approvals$endTime[i], timezone)
  return(data.frame(startTime=startTime, endTime=endTime))
}

# This function is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries. 
# Note that readTimeSeries and readEstimatedTimeSeries now return a list of the full timeseries object
# instead of the dataframe created and returned by this function. This means that downstream calls
# using this time series will need to be updated to pass in the correct parameters.
# See line 169 below for the old data frame format and see inst/extdata/testsnippets/test-timeSeries.JSON
# for example JSON outlining how a time series returned from readTimeSeries/readEstimatedTimeSeries will look

#' @export
getTimeSeries <- function(ts, field, estimatedOnly = FALSE, shiftTimeToNoon=TRUE){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  
  if(!is.null(y) & !is.null(x)){
    time <- flexibleTimeParse(x, ts$reportMetadata$timezone, shiftTimeToNoon)
    
    month <- format(time, format = "%y%m") #for subsetting later by month
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
  
  warning("'getTimeSeries' is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries.")
  return(uv_series)
}

# used in dvhydrograph and fiveyrgwsum
# This function is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries. 
# Note that readTimeSeries and readEstimatedTimeSeries now return a list of the full timeseries object
# instead of the dataframe created and returned by this function. This means that downstream calls
# using this time series will need to be updated to pass in the correct parameters.
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
  warning("'parseEstimatedStatDerived' is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries.")
  return(formatted_data)
}

# used in dvhydrograph and fiveyrgwsum
# This function is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries. 
# Note that readTimeSeries and readEstimatedTimeSeries now return a list of the full timeseries object
# instead of the dataframe created and returned by this function. This means that downstream calls
# using this time series will need to be updated to pass in the correct parameters.
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
  
  warning("'getEstimatedDates' called by 'parseEstimatedStatDerived' which is deprecated. Please switch over to using readTimeSeries and readEstimatedTimeSeries.")
  return(date_index)
}

#' Read time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param timezone the timezone to parse times to
#' @param seriesName the name of the time series to extract
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
#' @param isDV whether or not the specified time series is a daily value time series
readTimeSeries <- function(reportObject, seriesName, timezone, descriptionField=NULL, shiftTimeToNoon=FALSE, isDV=FALSE) {
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

  if(validateFetchedData(seriesData, seriesName, requiredFields)){
    #Format Point data
    seriesData[['points']][['time']] <- flexibleTimeParse(seriesData[['points']][['time']], timezone, shiftTimeToNoon)
    seriesData[['points']][['value']] <- as.numeric(seriesData[['points']][['value']])
    seriesData[['points']][['month']] <- format(seriesData[['points']][['time']], format = "%y%m")
    seriesData[['points']] <- data.frame(seriesData[['points']])

    #Format Report Metadata
    seriesData[['startTime']] <- flexibleTimeParse(seriesData[['startTime']], timezone, shiftTimeToNoon)
    seriesData[['endTime']] <- flexibleTimeParse(seriesData[['endTime']], timezone, shiftTimeToNoon)
  } else {
    stop(paste("Retrieved Time Series: ", seriesName, " is empty."))
  }

  seriesData[['estimated']] <- FALSE
  
  #Handle DV Series
  if(isDV){
    seriesData[['isDV']] <- TRUE
    
    #--used in dvhydrograph and fiveyrgwsum--
    if(!isEmptyOrBlank(descriptionField)){
      seriesData[['legend.name']] <- paste(ifelse(estiamted, "Estimated", ""), fetchReportMetadataField(descriptionField))
    }
  } else {
    seriesData[['isDV']] <- FALSE
  }

  return(seriesData)
}

#' Read an estaimted time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param timezone the timezone to parse times to
#' @param seriesName the name of the time series to extract
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
readEstimatedTimeSeries <- function(reportObject, seriesName, timezone, descriptionField=NULL, shiftTimeToNoon=FALSE, isDV=FALSE) {
  #Read and format all time series data
  seriesData <- readTimeSeries(reportObject, seriesName, timezone, descriptionField, shiftTimeToNoon, isDV)
  seriesData[['estimated']] <- TRUE 

  estimatedSubset <- data.frame(time=as.POSIXct(NA), value=as.character(NA), month=as.character(NA))
  estimatedSubset <- na.omit(estimatedSubset)

  if(!isEmptyOrBlank(seriesData[['estimatedPeriods']])){
    #Extract and build estimated periods
    startEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['startDate']], timezone)
    endEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['endDate']], timezone)
    estimatedPeriods <- data.frame(start=startEst, end=endEst)
    
    #Extract only data in estimated periods
    if(nrow(estimatedPeriods) > 0){
      for(i in 1:nrow(estimatedPeriods)) {
        p <- estimatedPeriods[i,]
        startTime <- p$start
        endTime <- p$end
        estimatedSubset <- rbind(estimatedSubset, seriesData[['points']][seriesData[['points']][['time']] >= startTime & seriesData[['points']][['time']] < endTime,])
      }
    }
  }

  #Replace data with only estimated data
  seriesData[['points']] <- estimatedSubset

  return(seriesData)
}
