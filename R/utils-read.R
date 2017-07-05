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

#' Read report metadata field
#' 
#' @description Given a full report object and field name, returns the
#' metadata value for the provided field.
#' @param reportObject the object representing the full report JSON
#' @param field the field name to read from the metadata
readReportMetadataField <- function(reportObject, field){
  metaField <- fetchReportMetadataField(reportObject, field)
  
  if(is.null(metaField)){
    stop(paste("Report metadata could not be found for field: {", field, "}"))
  } else {
    return(metaField)
  }
}

############ used in dvhydrograph-data, fiveyeargwsum-data, uvhydrograph-data ############ 
#' Read ground water levels
#' 
#' @description Given a full report object, returns the ground water levels
#' measurements formatted as a time series point set.
#' @param reportObject the object representing the full report JSON
#' @return data frame
#' @importFrom stats na.omit
readGroundWaterLevels <- function(reportObject){
  #Fetch and Validate Data
  gwData <- fetchGroundWaterLevels(reportObject)
  requiredFields <- c('groundWaterLevel', 'recordDateTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)

  #Transform data
  if(validateFetchedData(gwData, 'Ground Water Levels', requiredFields, stopEmpty=FALSE)){
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
#' @return data frame
#' @importFrom stats na.omit
readWaterQualityMeasurements <- function(reportObject){
  #Fetch and Validate Data
  wqData <- fetchWaterQualityMeasurements(reportObject)
  requiredFields <- c('value', 'sampleStartDateTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)

  #Transform data
  if(validateFetchedData(wqData, 'Water Quality measurements', requiredFields, stopEmpty=FALSE)){
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
#' @return data frame 
#' @importFrom stats na.omit
readFieldVisitMeasurementsQPoints <- function(reportObject){
  visitData <- fetchFieldVisitMeasurements(reportObject)
  requiredFields <- c('discharge', 'measurementStartDate', 'errorMinDischarge', 'errorMaxDischarge', 'measurementNumber')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)

  if(validateFetchedData(visitData, "Field Visit Measurements", requiredFields, stopEmpty=FALSE)){
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
readFieldVisitReadings <- function(reportObject){
  visitReadings <- fetchFieldVisitReadings(reportObject)
  requiredFields <- c('visitTime')
  returnDf <- data.frame(stringsAsFactors=FALSE)

  # declare objects to get rid of dplyr warning in Check
  # these are column names and will be used appropriately when it gets to that line
  associatedIvValue <- visitTime <- associatedIvTime <-associatedIvQualifiers <- value <- '.dplyr.var'
  
  if(validateFetchedData(visitReadings, "Readings", requiredFields, stopEmpty=TRUE)){
    #Move associated IV information to the highest valued reading
    if(!all(sapply(visitReadings$associatedIvValue, function(e){isEmptyOrBlank(e)}))){
      orderedIVs <- visitReadings %>% dplyr::mutate(sort = is.na(associatedIvValue)) %>% dplyr::group_by(visitTime) %>% dplyr::arrange(visitTime, sort) %>% dplyr::ungroup() %>% dplyr::select(associatedIvValue, associatedIvTime, associatedIvQualifiers)
      orderedValues <- visitReadings %>% dplyr::mutate(sort = as.numeric(value)) %>% dplyr::group_by(visitTime) %>% dplyr::arrange(visitTime, desc(sort)) %>% dplyr::select(-sort, -associatedIvValue, -associatedIvTime, -associatedIvQualifiers)
      visitReadings <- dplyr::bind_cols(orderedValues, orderedIVs) %>% dplyr::arrange(visitTime) %>% as.data.frame()
    }
    
    #Format the data frame to a table
    for(listRows in row.names(visitReadings)){
      listElements <- visitReadings[listRows,]
      visitTime <- listElements[['visitTime']]
      time <- listElements[['time']]
      party <- listElements[['party']]
      sublocation <- listElements[['sublocation']]
      monitoringMethod <- listElements[['monitoringMethod']]
      value <- listElements[['value']]
      uncertainty <- listElements[['uncertainty']]
      comments <- listElements[['comments']]
      associatedIvValue <- listElements[['associatedIvValue']]
      qualifiers <- readFetchedQualifiers(listElements[['associatedIvQualifiers']], listElements[['associatedIvTime']])
      associatedIvTime <- listElements[['associatedIvTime']]
      diffPeak <- readIvDifference(listElements[['value']], listElements[['associatedIvValue']])
      readings <- data.frame(visitTime=nullMask(visitTime), party=nullMask(party), sublocation=nullMask(sublocation), monitoringMethod=nullMask(monitoringMethod), value=nullMask(value), uncertainty=nullMask(uncertainty), time=nullMask(time), comments=I(list(comments)), associatedIvValue=nullMask(associatedIvValue), qualifiers=I(list(qualifiers)), associatedIvTime=nullMask(associatedIvTime), diffPeak=nullMask(diffPeak),stringsAsFactors=FALSE)
      returnDf <- rbind(returnDf, readings) 
    }
  }
  
  return(returnDf)
}

#' Read all qualifiers from field visit readings
#'
#' @description Given a full report object of parsed field visit readings, 
#' returns all deduplicated qualifiers from the field visit readings formatted 
#' as a data frame
#' @param visitReadings the object representing the parsed field visit readings
readAllFieldVisitQualifiers <- function(visitReadings){

  returnDf <- data.frame(stringsAsFactors=FALSE)

    for(listRows in row.names(visitReadings)){
      listElements <- visitReadings[listRows,]
      qualifiers <- listElements[['qualifiers']][[1]]
      if(!is.null(qualifiers) && length(qualifiers) > 0){
      allQualifiers <- data.frame(qualifiers=qualifiers, stringsAsFactors=FALSE)
      returnDf <- rbind(returnDf, allQualifiers) 
      }
    }

  return(returnDf)
}


#' Read field visit reading qualifiers
#'
#' @description Given an associated Instantaneous Value date and time and qualifiers, 
#' returns the qualifiers formatted as a data frame
#' @param inQualifiers list of associated Instantaneous Value qualifiers
#' @param time associated Instantaneous Value date and time (optional, defaults to NULL)
readFetchedQualifiers <- function(inQualifiers, time=NULL) {
  returnDf <- data.frame(stringsAsFactors=FALSE)

  if(length(inQualifiers) < 1) return(NULL);
  
  q <- inQualifiers[[1]]
  if(is.null(q) || length(q) < 1) return(NULL);
  
  if (!is.null(time)){
    qualifiers <- q[time>q$startDate & q$endDate>time,]
  } else {
    qualifiers <- q
  }
  
  if(nrow(qualifiers) > 0) {
    code <- qualifiers[['code']]
    identifier <- qualifiers[['identifier']]
    description <- qualifiers[['displayName']]
    quals <- data.frame(code=nullMask(code),identifier=nullMask(identifier),description=nullMask(description),stringsAsFactors=FALSE)
    returnDf <- rbind(returnDf, quals)
  };
  return(returnDf)
}

#' Calculate the difference between the field visit measurement value and the associated
#' Instantaneous Value
#'
#' @description Given a field visit measurement value and an associated Instantaneous Value date,
#' returns the difference.
#' @param readingVal field visit measurement value
#' @param ivVal associated Instantaneous Value
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
#' @return data frame
#' @importFrom stats na.omit
readFieldVisitMeasurementsShifts <- function(reportObject){
  visitData <- fetchFieldVisitMeasurements(reportObject)
  requiredFields <- c('shiftInFeet', 'measurementStartDate', 'errorMinShiftInFeet', 'errorMaxShiftInFeet')
  returnDf <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)

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

    returnDf <- data.frame(time=time, month=month, value=value, minShift=minShift, maxShift=maxShift, stringsAsFactors=FALSE)
  }

  return(returnDf)
}

#' Read corrections
#' 
#' @description Given a full report object and the name of a time series,
#' returns the corrections list for that time series
#' @param reportObject the object representing the full report JSON
#' @param seriesCorrName the object representing the correction data
#' @return data frame of correction information
#' @importFrom stats na.omit
readCorrections <- function(reportObject, seriesCorrName){
  corrData <- fetchCorrections(reportObject, seriesCorrName)
  requiredFields <- c('startTime', 'endTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)

  if(validateFetchedData(corrData, seriesCorrName, requiredFields)){
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

#' Read Rating Shifts (UV Hydro)
#' @description given a list of rating shifts returns shift data
#' @param reportObject the object representing the full report JSON
#' @return data frame of rating shift information
readRatingShiftsUvHydro <- function(reportObject) {
  ratingShiftData <- fetchRatingShifts(reportObject)
  requiredFields <- c('applicableStartDateTime', 'applicableEndDateTime')
  returnDf <- data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)
  returnDf <- stats::na.omit(returnDf)
  
  if(validateFetchedData(ratingShiftData, "ratingShiftDataUVHydro", requiredFields)) {
    timeStart <- as.POSIXct(strptime(ratingShiftData[['applicableStartDateTime']], "%FT%T"))
    monthStart <- format(timeStart, format = "%y%m")
    commentStart <- ratingShiftData[['shiftRemarks']]
    
    timeEnd <- as.POSIXct(strptime(ratingShiftData[['applicableEndDateTime']], "%FT%T"))
    monthEnd <- format(timeEnd, format = "%y%m")
    commentEnd <- ratingShiftData[['shiftRemarks']]
    
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
#' @return named list of data frames. Each frame wil be named according to appr_var_all and data frame will have respective points
readApprovalPoints <- function(approvals, points, timezone, legend_nm, appr_var_all, point_type=NULL){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  
  working_index <- readApprovalIndex(points, approvals, "Working", timezone)
  review_index <- readApprovalIndex(points, approvals, "In Review", timezone)
  approved_index <- readApprovalIndex(points, approvals, "Approved", timezone)
  
  review_index <- setdiff(review_index, working_index)
  approved_index <- setdiff(approved_index, working_index)
  approved_index <- setdiff(approved_index, review_index)
  
  date_index_list <- list(list(type="Approved",index=approved_index), 
      list(type="Working",index=working_index), 
      list(type="In Review",index=review_index))
  
  approvals_all <- lapply(date_index_list, function(level, points, legend_nm, point_type){
        
        d <- level[['index']]
        
        applicable_dates <- points[['time']][d]
        applicable_values <- points[['value']][d]
        
        if(any(!is.na(applicable_dates))) {
          approval_info_level <- data.frame(
              time=applicable_dates,
              value=applicable_values,
              legend.name=paste(level[["type"]], legend_nm),
              point_type=point_type,
              stringsAsFactors=FALSE)
        } else {
          approval_info_level <- data.frame(time=.POSIXct(character()),
              value=numeric(),
              legend.name=character(),
              point_type=numeric(),
              stringsAsFactors=FALSE)
        }
        
        return(approval_info_level)
      }, points, legend_nm, point_type)
  
  appr_type_ordered <- sapply(date_index_list, function(level){ level[['type']]})
  names(approvals_all) <- appr_var_all[match(appr_type, appr_type_ordered)]
  
  return(approvals_all)
}

#' Read Approval Bars
#' @description for a timeseries, will return a list of approval bars to be plotted
#' @param ts the timeseries to get approval bars for, *ts must be parsed by readTimeseries*
#' @param timezone the timezone to convert all times to
#' @param legend_nm the name to be assigned to the legend entries (as a suffix)
#' @param snapToDayBoundaries true to shift all bar edges to the closest end/beginning of the days
#' @return list of approval bar ranges, lists should contain the possible named items appr_working_uv, appr_inreview_uv, appr_approved_uv
readApprovalBar <- function(ts, timezone, legend_nm, snapToDayBoundaries=FALSE){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  approval_info <- list()
  appr_dates <- NULL
  
  if (!isEmptyOrBlank(ts$approvals$startTime) && !isEmptyOrBlank(ts$startTime)) {
    startTime <-
        flexibleTimeParse(ts$approvals$startTime, timezone = timezone)
    chain.startTime <- ts$startTime #start time must be preparsed, relies on readTimeSeries
    
    # clip start points to chart window
    for (i in 1:length(startTime)) {
      if (startTime[i] < chain.startTime) {
        startTime[i] <- chain.startTime
      }
    }
    
    endTime <-
        flexibleTimeParse(ts$approvals$endTime, timezone = timezone)
    chain.endTime <- ts$endTime  #end time must be preparsed, relies on readTimeSeries
    
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

#' Read time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param seriesName the name of the time series to extract
#' @param timezone the timezone to parse times to
#' @param descriptionField The JSON field name to fetch description inofmration from
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
#' @param isDV whether or not the specified time series is a daily value time series
#' @param estimated whether or not the time series should be marked as estimated
#' @param requiredFields optional overriding of required fields for a time series
#' @param onlyMonth 4 character month code to limit points to (EG: "1608" only includes August 2016 points)
readTimeSeries <- function(reportObject, seriesName, timezone, descriptionField=NULL, shiftTimeToNoon=FALSE, 
    isDV=FALSE, estimated=FALSE, requiredFields=NULL, onlyMonth=NULL) {
  seriesData <- fetchTimeSeries(reportObject, seriesName)
  if(is.null(requiredFields)){
    requiredFields <- c(
      "points",
      "approvals",
      "qualifiers",
      "isVolumetricFlow",
      "units",
      "grades",
      "type",
      "gaps",
      "gapTolerances",
      "name"
    )
  }

  if(validateFetchedData(seriesData, seriesName, requiredFields)){
    #Format Point data
    seriesData[['points']][['time']] <- flexibleTimeParse(seriesData[['points']][['time']], timezone, shiftTimeToNoon)
    seriesData[['points']][['value']] <- as.numeric(seriesData[['points']][['value']])
    seriesData[['points']][['month']] <- format(seriesData[['points']][['time']], format = "%y%m")
    
    if(!isEmptyOrBlank(onlyMonth)) {
      seriesData[['points']] <- subsetByMonth(data.frame(seriesData[['points']]), onlyMonth) 
    } else {
      seriesData[['points']] <- data.frame(seriesData[['points']])
    }

    #Format Report Metadata
    seriesData[['startTime']] <- flexibleTimeParse(seriesData[['startTime']], timezone, shiftTimeToNoon)
    seriesData[['endTime']] <- flexibleTimeParse(seriesData[['endTime']], timezone, shiftTimeToNoon)
  }
  
  seriesData[['estimated']] <- estimated 

  #Handle DV Series
  if(isDV){
    seriesData[['isDV']] <- TRUE
    
    #--used in dvhydrograph and fiveyrgwsum--
    if(!isEmptyOrBlank(descriptionField)){
      if(!isEmptyOrBlank(fetchReportMetadataField(reportObject, descriptionField))){
        seriesData[['legend.name']] <- paste(ifelse(estimated, "Estimated", ""), fetchReportMetadataField(reportObject, descriptionField))
      } else {
        stop(paste("Data retrieved for: '", seriesName, "' is missing provided description field: ", descriptionField))
      }
    }
  } else {
    seriesData[['isDV']] <- FALSE
  }
  
  time <- NULL #only here to remove check warnings
  
  #Sort points by time
  seriesData[['points']] <- seriesData[['points']] %>% arrange(time)

  return(seriesData)
}

#' Read an estaimted time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param seriesName the name of the time series to extract
#' @param timezone the timezone to parse times to
#' @param descriptionField The JSON field name to fetch description inofmration from
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
#' @param isDV whether or not the specified time series is a daily value time series
#' @param requiredFields optional overriding of required fields for a time series
#' @param inverted whether or not the time series is inverted
#' @param onlyMonth 4 character month code to limit points to (EG: "1608" only includes August 2016 points)
#' @return a timeseries object with only points in the estimated ranges
#' @importFrom stats na.omit
readEstimatedTimeSeries <- function(reportObject, seriesName, timezone, descriptionField=NULL, shiftTimeToNoon=FALSE, isDV=FALSE, requiredFields=NULL, inverted=FALSE, onlyMonth=NULL) {
  #Read and format all time series data
  seriesData <- readTimeSeries(reportObject, seriesName, timezone, descriptionField, shiftTimeToNoon, isDV, estimated=!inverted, requiredFields=requiredFields, onlyMonth=onlyMonth)

  if(!isEmptyOrBlank(seriesData[['estimatedPeriods']])){
    #Extract and build estimated periods
    estimatedSubset <- data.frame(time=as.POSIXct(NA), value=as.character(NA), month=as.character(NA))
    estimatedSubset <- stats::na.omit(estimatedSubset)
    startEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['startDate']], timezone)
    endEst <- flexibleTimeParse(seriesData[['estimatedPeriods']][['endDate']], timezone)
    estimatedPeriods <- data.frame(start=startEst, end=endEst)
    
    time <- NULL #only here to remove check warnings
    start <- NULL #only here to remove check warnings
    
    #Sort estimated periods
    estimatedPeriods <- estimatedPeriods %>% arrange(start)
    
    #Extract only data in estimated periods
    if(nrow(estimatedPeriods) > 0){
      for(i in 1:nrow(estimatedPeriods)) {
        p <- estimatedPeriods[i,]
        startTime <- p$start
        endTime <- p$end
        estimatedSubset <- rbind(estimatedSubset, subset(seriesData[['points']], (time >= startTime) & (time < endTime)))
      }
    }

    #Replace data with only saved data
    if(inverted){
      nonEstimatedSubset <- subset(seriesData[['points']], !(time %in% estimatedSubset[['time']]))
      seriesData[['points']] <- nonEstimatedSubset
    } else{
      seriesData[['points']] <- estimatedSubset
    }
  } else {
    #If we're only keeping estimated data then keep an empty list of points
    if(!inverted){
      seriesData[['points']] <- stats::na.omit(data.frame(time=as.POSIXct(NA), value=as.character(NA), month=as.character(NA)))
    }
  }
  
  #Sort points by time
  seriesData[['points']] <- seriesData[['points']] %>% arrange(time)

  return(seriesData)
}

#' Read a non-estaimted time series
#'
#' @description Reads and formats a time series from the provided full report object
#' @param reportObject the full JSON report object
#' @param seriesName the name of the time series to extract
#' @param timezone the timezone to parse times to
#' @param descriptionField The JSON field name to fetch description inofmration from
#' @param shiftTimeToNoon [DEFAULT: FALSE] whether or not to shift DV times to noon
#' @param isDV whether or not the specified time series is a daily value time series
#' @param requiredFields optional overriding of required fields for a time series
#' @param onlyMonth 4 character month code to limit points to (EG: "1608" only includes August 2016 points)
#' @return ts with only points which are not in the estimated range
readNonEstimatedTimeSeries <- function(reportObject, seriesName, timezone, descriptionField=NULL, shiftTimeToNoon=FALSE, isDV=FALSE, requiredFields=NULL, onlyMonth=NULL) {
  return(readEstimatedTimeSeries(reportObject, seriesName, timezone, descriptionField, shiftTimeToNoon, isDV, requiredFields, inverted=TRUE, onlyMonth=onlyMonth))
}

#' Read Mean Gage Heights
#' @description get the list of gage heights attached to a report. Will include a year+month field as a month identifier for each record.
#' @param reportObject the full JSON report object
#' @return data frame of mean gage heights
#' @importFrom stats na.omit
readMeanGageHeights<- function(reportObject){
  fieldVisitMeasurements <- fetchFieldVisitMeasurements(reportObject)
  if(is.null(fieldVisitMeasurements[['meanGageHeight']])) {
    df <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))
    df <- stats::na.omit(df)
    return(df)
  }
  y <- fieldVisitMeasurements[['meanGageHeight']]
  x <- fieldVisitMeasurements[['measurementStartDate']]
  n <- fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, n=n, month=month, stringsAsFactors = FALSE))
}

#' Read Readings
#' @description get the list of readings attached to a report. Will include a year+month field as a month identifier for each record.
#' @param reportObject the full JSON report object
#' @param readingsFieldName field name containing readings
#' @param filter optional filter to restrict to reading types (reference, crestStage, or waterMark)
#' @return data frame of reading records
readReadings <- function(reportObject, readingsFieldName, filter="") {
  time <- as.POSIXct(strptime(reportObject[[readingsFieldName]][['time']], "%FT%T"))
  value <- as.numeric(reportObject[[readingsFieldName]][['value']])
  type <- reportObject[[readingsFieldName]][['type']]
  uncertainty <- as.numeric(reportObject[[readingsFieldName]][['uncertainty']])
  month <- format(time, format = "%y%m") #for subsetting later by month
  
  if (filter == "reference") {
    index <- which(type == "ReferencePrimary")
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  } else if (filter == "crestStage") {
    typeIndex <- which(type == "ExtremeMax")
    index <- typeIndex
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  } else if (filter == "waterMark") {
    index <- which(type == "") ### What is the condition for high water mark?
    x <- time[index]
    y <- value[index]
    uncertainty <- uncertainty[index]
    month <- month[index]
  } else {
    x <- time
    y <- value
  }
  
  #Covers the case when no uncertainty is provided. This seems to work since the reference
  #is plotted correctly with no error bars
  uncertainty[is.na(uncertainty)] <- 0
  
  returnFrame <- data.frame(time=x, value=y, uncertainty=uncertainty, month=month, stringsAsFactors = FALSE)
  
  #Only keep rows that have a time (time of the reading)
  returnFrame <- returnFrame[which(!is.na(returnFrame["time"])),]
  
  return(returnFrame)
}

#' Read Min/Max IV Data
#'
#' @description Reads and formats Min/Max IV Data from the provided full report object
#' @param reportObject the full JSON report object
#' @param stat the stat to pull (MAX or MIN)
#' @param timezone the timezone to parse times into
#' @param inverted whether or not the time series is inverted
readMinMaxIVs <- function(reportObject, stat, timezone, inverted){
  stat <- toupper(stat)
  statData <- fetchMinMaxIVs(reportObject, stat)
  returnList <- list()
  requiredFields <- c('time', 'value')

  if(validateFetchedData(statData, paste(stat, "IV Data"), requiredFields)){
    time <- flexibleTimeParse(statData[['time']], timezone=timezone)
    value <- statData[['value']]
    statLabel <- ifelse(inverted, ifelse(stat == "MAX", "MIN", "MAX"), stat)
    label <- paste(paste0(substring(statLabel, 1, 1), substring(tolower(statLabel), 2)), 
                 "Instantaneous", sep='. ')
    returnList <- list(time=time, value=value, label=label)
  }

  return(returnList)
}

#' Read Primary Series Approvals (DV and Five YR)
#'
#' @description Reads and formats the primarySeriesApprovals as a time series
#' with no points and only approvals. Used to have DV Hydro and Five YR GW
#' base their approval bars off of the primary (upchain) series approvals instead
#' of the stat derived approvals.
#' @param reportObject the full report JSON object
#' @param startTime the start time of the report
#' @param endTime the end time of the report
readPrimarySeriesApprovals <- function(reportObject, startTime, endTime){
  requiredFields <- c('level', 'description', 'startTime', 'endTime')
  returnList <- list()
  approvalData <- fetchPrimarySeriesApprovals(reportObject)

  if(validateFetchedData(approvalData, "Primary (Upchain) Series Approvals", requiredFields)){
    returnList[['approvals']] <- approvalData
    returnList[['startTime']] <- startTime
    returnList[['endTime']] <- endTime
  }

  return(returnList)
}

#' Read Primary Series Qualifiers (DV and Five YR)
#'
#' @description Reads and formats the primarySeriesQualifiers. Used to
#' allow DV Hydro and 5 Year GW to format their max/min UV colors.
#' @param reportObject the full report JSON object
#' @param filterCode The qualifier code to filter read qualifiers to
readPrimarySeriesQualifiers <- function(reportObject, filterCode=NULL){
  requiredFields <- c('code', 'startDate', 'endDate')
  returnList <- list()
  qualifierData <- fetchPrimarySeriesQualifiers(reportObject)
  
  if(validateFetchedData(qualifierData, "Primary (Upchain) Series Qualifiers", requiredFields)){
    if(!isEmptyOrBlank(filterCode)){
      returnList <- qualifierData[which(qualifierData[['code']] == filterCode),]
    } else {
      returnList <- qualifierData
    }
    
  }
  
  return(returnList)
}

#' Read Field Vists (CORR)
#'
#' @description Reads and formats the field Vists
#' @param reportObject the full report JSON object
#' @param timezone the timezone
readFieldVists <- function(reportObject, timezone){
  requiredFields <- c('startTime', 'endTime')
  fieldVists <- fetchFieldVists(reportObject)
  returnList <- list()

  if(validateFetchedData(fieldVists, 'Field Vists', requiredFields, stopEmpty=FALSE)){
    returnList <- fieldVists
    returnList[['startTime']] <- flexibleTimeParse(returnList[['startTime']], timezone)
    returnList[['endTime']] <- flexibleTimeParse(returnList[['endTime']], timezone)
  }

  return(returnList)
}

#' Read Processing Corrections (CORR)
#'
#' @description Reads and formats the corrections data for
#' the specified processing order.
#' @param reportObject the full report JSON object
#' @param processOrder The processing order to get corrections for. Valid choices: "pre", "post", and "normal"
#' @param timezone target timezone to parse data into
readProcessingCorrections <- function(reportObject, processOrder, timezone){
  requiredFields <- c('startTime', 'endTime')
  corrections <- fetchProcessingCorrections(reportObject, processOrder)
  returnList <- list()

  if(validateFetchedData(corrections, paste(processOrder, 'processing corrections'), requiredFields, stopEmpty=FALSE)){
    returnList <- corrections
    returnList[['startTime']] <- flexibleTimeParse(returnList[['startTime']], timezone)
    returnList[['endTime']] <- flexibleTimeParse(returnList[['endTime']], timezone)
    returnList[['appliedTimeUtc']] <- flexibleTimeParse(returnList[['appliedTimeUtc']], timezone)
  }

  return(returnList)
}

#' Read Thresholds (CORR)
#'
#' @description Reads and formats the Thresholds data
#' @param reportObject the full report JSON object
readThresholds <- function(reportObject){
  requiredFields <- c('periods')
  thresholds <- fetchThresholds(reportObject)
  returnList <- list()

  if(validateFetchedData(thresholds, 'Thresholds', requiredFields, stopEmpty=FALSE)){
    returnList <- thresholds
  }

  return(returnList)
}

#' Read Excluded Control Conditions (V-Diagram)
#' 
#' @description  Reads and formats the excluded control condition data
#' @param reportObject The full report JSON object
readExcludedControlConditions <- function(reportObject){
  requiredFields <- c('name')
  conditions <- fetchExcludedControlConditions(reportObject)
  returnList <- list()
  
  if(validateFetchedData(conditions, 'Excluded Control Conditions', requiredFields, stopEmpty=FALSE)){
    returnList <- conditions
  }
  
  return(returnList)
}

#' Read Gaps (TSS)
#' 
#' @description  Reads and formats the gaps
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readGaps <- function(reportObject, timezone){
  requiredFields <- c('startTime', 'endTime')
  gaps <- fetchGaps(reportObject)
  returnList <- list()
  
  if(validateFetchedData(gaps, 'Gaps', requiredFields, stopEmpty=FALSE)){
    returnList <- gaps
    returnList[['startTime']] <- flexibleTimeParse(returnList[['startTime']], timezone)
    returnList[['endTime']] <- flexibleTimeParse(returnList[['endTime']], timezone)
  }
  
  return(returnList)
}

#' Read Upchain Series (TSS)
#' 
#' @description  Reads and formats the related upchain series
#' @param reportObject The full report JSON object
readUpchainSeries <- function(reportObject){
  requiredFields <- c('identifier')
  upchain <- fetchUpchainSeries(reportObject)
  returnList <- list()
  
  if(validateFetchedData(upchain, 'Related Upchain Series', requiredFields, stopEmpty=FALSE)){
    returnList <- upchain
  }
  
  return(returnList)
}

#' Read Downchain Series (TSS)
#' 
#' @description  Reads and formats the related downchain series
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readDownchainSeries <- function(reportObject){
  requiredFields <- c('identifier')
  downchain <- fetchDownchainSeries(reportObject)
  returnList <- list()
  
  if(validateFetchedData(downchain, 'Related Downchain Series', requiredFields, stopEmpty=FALSE)){
    returnList <- downchain
  }
  
  return(returnList)
}

#' Read Qualifiers (TSS)
#' 
#' @description  Reads and formats the qualifiers
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readQualifiers <- function(reportObject, timezone){
  requiredFields <- c('startDate', 'endDate', 'identifier')
  qualifiers <- fetchQualifiers(reportObject)
  returnList <- list()
  
  if(validateFetchedData(qualifiers, 'Qualifiers', requiredFields, stopEmpty=FALSE)){
    returnList <- qualifiers
    returnList[['startDate']] <- flexibleTimeParse(returnList[['startDate']], timezone)
    returnList[['endDate']] <- flexibleTimeParse(returnList[['endDate']], timezone)
  }
  
  return(returnList)
}

#' Read Notes (TSS)
#' 
#' @description  Reads and formats the notes
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readNotes <- function(reportObject, timezone){
  requiredFields <- c('startDate', 'endDate', 'note')
  notes <- fetchNotes(reportObject)
  returnList <- list()
  
  if(validateFetchedData(notes, 'Notes', requiredFields, stopEmpty=FALSE)){
    returnList[['startDate']] <- flexibleTimeParse(notes[['startDate']], timezone)
    returnList[['endDate']] <- flexibleTimeParse(notes[['endDate']], timezone)
    returnList[['note']] <- notes[['note']]
  }
  
  return(returnList)
}

#' Read Grades (TSS)
#' 
#' @description  Reads and formats the grades
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readGrades <- function(reportObject, timezone){
  requiredFields <- c('startDate', 'endDate', 'code')
  grades <- fetchGrades(reportObject)
  returnList <- list()
  
  if(validateFetchedData(grades, 'Grades', requiredFields, stopEmpty=FALSE)){
    returnList[['startDate']] <- flexibleTimeParse(grades[['startDate']], timezone)
    returnList[['endDate']] <- flexibleTimeParse(grades[['endDate']], timezone)
    returnList[['code']] <- grades[['code']]
  }
  
  return(returnList)
}

#' Read Rating Curves (TSS)
#' 
#' @description  Reads and formats the rating curves
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readRatingCurves <- function(reportObject, timezone){
  requiredFields <- c('curveNumber', 'applicablePeriods', 'ratingType', 'remarks')
  curves <- fetchRatingCurves(reportObject)
  returnList <- list()
  
  if(validateFetchedData(curves, 'Rating Curves', requiredFields, stopEmpty=FALSE)){
    returnList <- data.frame(curves, stringsAsFactors = FALSE)
  }
  
  return(returnList)
}

#' Read Rating Shifts (TSS)
#' 
#' @description  Reads and formats the rating shifts
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readRatingShifts <- function(reportObject, timezone){
  requiredFields <- c('curveNumber', 'shiftPoints', 'stagePoints', 'applicableStartDateTime', 'applicableEndDateTime')
  returnList <- list()
  shifts <- fetchRatingShifts(reportObject)
  
  if(validateFetchedData(shifts, 'Rating Shifts', requiredFields, stopEmpty=FALSE)){
    returnList <- shifts
    returnList[['applicableStartDateTime']] <- flexibleTimeParse(returnList[['applicableStartDateTime']], timezone)
    returnList[['applicableEndDateTime']] <- flexibleTimeParse(returnList[['applicableEndDateTime']], timezone)
  }
  
  return(returnList)
}

#' Read Approvals (TSS)
#' 
#' @description  Reads and formats the approvals
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readApprovals <- function(reportObject, timezone){
  requiredFields <- c('startTime', 'endTime', 'level', 'comment')
  approvals <- fetchApprovals(reportObject)
  returnList <- list()
  
  if(validateFetchedData(approvals, 'Approvals', requiredFields, stopEmpty=FALSE)){
    returnList <- approvals
    returnList[['startTime']] <- flexibleTimeParse(returnList[['startTime']], timezone)
    returnList[['endTime']] <- flexibleTimeParse(returnList[['endTime']], timezone)
  }
  
  return(returnList)
}

#' Read Gap Tolerances (TSS)
#' 
#' @description Reads and formats the gaps tolerances
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
readGapTolerances <- function(reportObject, timezone){
  requiredFields <- c('startTime', 'endTime', 'toleranceInMinutes')
  gapTolerances <- fetchGapTolerances(reportObject)
  returnList <- list()
  
  if(validateFetchedData(gapTolerances, 'Gap Tolerances', requiredFields, stopEmpty=FALSE)){
    returnList <- gapTolerances
    returnList[['startTime']] <- flexibleTimeParse(returnList[['startTime']], timezone)
    returnList[['endTime']] <- flexibleTimeParse(returnList[['endTime']], timezone)
  }
  
  return(returnList)
}