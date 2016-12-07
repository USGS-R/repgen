
#'@title get value from extremes json list
#'@description convienence function for accessing from the "values" block in 
#'extremes json
#'@param data a list, can be the output of \code{\link[jsonlite]{fromJSON}}.
#'@param param the field name (e.g., 'locationNumber')
#'@param ... additional arguments passed to \code{repgen:::validParam}, 
#'such as \code{required}, or \code{as.numeric}
#'@return a value or array corresponding to the field specified by \code{param}
#'@export
getReportMetadata <- function(data, param, ...){
  val <- data$reportMetadata[[param]]
  return(validParam(val, param, ...))
}

numShifts <- function(ts){
  if (is.null(ts$ratingShifts)) {
    stop('required field ratingShifts is missing.')
  }
  return(nrow(ts$ratingShifts))
}

# as.numeric forces NULL to be NA
validParam <- function(val, param, required = FALSE, as.numeric = FALSE){
  if (is.null(val)){
    if (required){
      stop('required value ', param, ' missing.')
    }
    ifelse(as.numeric, return(as.numeric(NA)), return(""))
  } else {
    return(val)
  }
}

#'@export 
getRatingShifts <- function(ts, param, ...){
  val <- ts$ratingShifts[[param]]
  return(validParam(val, param, ...))
}

#'@export
getMeasurements <- function(ts, param, ...){
  val <- ts$measurements[[param]]
  return(validParam(val, param, ...))
}

#'@export
getMaxStage <- function(ts, ...){
  val <- as.numeric(ts$maximumStageHeight)
  return(validParam(val, param = 'maximumStageHeight', ...))
}

#'@export
getMinStage <- function(ts, ...){
  val <- as.numeric(ts$minimumStageHeight)
  return(validParam(val, param = 'minimumStageHeight', ...))
}

#' @export
#finds if the plot data has any zero values
zeroValues <- function(data, val_nm){ 
  if(class(data) == "list"){
    zeroList <- lapply(data, function(x) {any(na.omit(x[[val_nm]]) == 0)})
    zeroData <- any(unlist(unname(zeroList)))
  } else {
    zeroData <- any(na.omit(data[[val_nm]]) == 0)
  }
  return(zeroData)
}

#' @export
#finds if the plot data has any zero values
negValues <- function(data, val_nm){    
  if(class(data) == "list"){
    negList <- lapply(data, function(x) {any(na.omit(x[[val_nm]]) < 0)})
    negData <- any(unlist(unname(negList)))
  } else {
    negData <- any(na.omit(data[[val_nm]]) < 0)
  }
  return(negData)
}

#' @export
# adds periods of zero or negative data to the gaps field of the specified ts
findZeroNegativeGaps <- function(field, data, isDV){
  #Ensure we are supposed to remove zeros and negatives before doing so
  loggedData <- isLogged(data, data[[field]]$points, field)
  flagZeroNeg <- getReportMetadata(data, 'excludeZeroNegative')
  if(!loggedData || isEmptyOrBlank(flagZeroNeg) || !flagZeroNeg){
    return(NULL)
  }
  
  uv_series <- data[[field]]$points
  if(!is.null(uv_series) & nrow(uv_series) != 0){
    uv_series <- uv_series %>% 
    rename(rawTime = time) %>% 
    mutate(time = flexibleTimeParse(rawTime, data$reportMetadata$timezone, isDV)) %>% 
    select(time, rawTime, value)

    #Select times from each point that will be excluded
    potentialNewGaps <- uv_series %>% filter(value > 0) %>% select(time, rawTime)

    #Determine start / end times for gaps created by these points
    gapTolerance <- ifelse(isDV, 1, 15)
    gapUnits <- ifelse(isDV, "days", "mins")
    potentialNewGaps <- potentialNewGaps %>% mutate(diff = c(difftime(tail(strptime(time, "%Y-%m-%d %H:%M:%S"), -1),
                                                                      head(strptime(time, "%Y-%m-%d %H:%M:%S"), -1), 
                                                                      units=gapUnits),0), 
                                                    prev = lag(diff))
    startGaps <- potentialNewGaps %>% filter(diff > gapTolerance) %>% select(rawTime)
    endGaps <- potentialNewGaps %>% filter(prev > gapTolerance) %>% select(rawTime)
    
    appGaps <- data.frame(startTime = startGaps$rawTime, endTime = endGaps$rawTime)
  }

  return(appGaps)
}

#' @export
# user specified option to treat negative/zero values as NA in order to have the plot logged
removeZeroNegative <- function(df){
  df <- df %>% 
    filter(value > 0)
  return(df)
}

#if absolutely no data comes back after parsing - skip to render with a message
anyDataExist <- function(data){
  emptyData <- any(c(length(data) == 0, nrow(data) == 0, is.null(data)))
  notEmptyData <- !emptyData
  return(notEmptyData)
}

############ used in dvhydrograph-data, fiveyeargwsum-data, uvhydrograph-data ############ 
#'@export
getGroundWaterLevels<- function(ts, ...){
  y <- as.numeric(ts$gwlevel[['groundWaterLevel']])
  x <- ts$gwlevel[['recordDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, field=rep("gwlevel", length(time)), stringsAsFactors = FALSE))
}

#'@export
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

#'@export
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

#'@export
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
    if (!is.na(errorMinShiftInFeet[i]) && !is.na(errorMaxShiftInFeet[i])) {
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

#'@export
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

getEstimatedDates <- function(data, chain_nm, time_data){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- flexibleTimeParse(data[[chain_nm]]$qualifiers$startDate[i], data$reportMetadata$timezone)
  endTime <- flexibleTimeParse(data[[chain_nm]]$qualifiers$endDate[i], data$reportMetadata$timezone)
  est_dates <- data.frame(start = startTime, end = endTime)
  
  date_index <- c()
  for(n in seq(nrow(est_dates))){
    date_index_n <- which(time_data > est_dates$start[n] & time_data < est_dates$end[n])
    date_index <- append(date_index, date_index_n)
  }
  
  return(date_index)
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

getApprovalIndex <- function(data, points, chain_nm, approval, subsetByMonth=FALSE) {
  points$time <- as.POSIXct(strptime(points$time, "%F"))
  dates <- getApprovalDates(data, chain_nm, approval)
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

getApprovals <- function(data, chain_nm, legend_nm, appr_var_all, month=NULL, point_type=NULL, subsetByMonth=FALSE, approvalsAtBottom=TRUE, applyFakeTime=FALSE, extendToWholeDays=FALSE, shiftTimeToNoon=TRUE){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  
  if(approvalsAtBottom==FALSE) {     
    if(subsetByMonth){
      points <- subsetByMonth(getTimeSeries(data, chain_nm), month)
    } else {
      points <- data[[chain_nm]][['points']]
    }
      
    working_index <- getApprovalIndex(data, points, chain_nm, "Working");
    review_index <- getApprovalIndex(data, points, chain_nm, "In Review");
    approved_index <- getApprovalIndex(data, points, chain_nm, "Approved");
    
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
          
          if (applyFakeTime) {
            applicable_dates <- points[['time']][d] + hours(23) + minutes(59)
          } else {
            applicable_dates <- points[['time']][d]
          }
          
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
  } else { # approvals at bottom 
    approval_info <- list()
    appr_dates <- NULL
    chain <- data[[chain_nm]]
    
    if (!isEmptyOrBlank(chain$approvals$startTime) && !isEmptyOrBlank(chain$startTime)) {
      
      timezone <- data$reportMetadata$timezone
      
      startTime <-
        flexibleTimeParse(chain$approvals$startTime, timezone = timezone)
      chain.startTime <-
        flexibleTimeParse(chain$startTime, timezone = timezone)
      
      # clip start points to chart window
      for (i in 1:length(startTime)) {
        if (startTime[i] < chain.startTime) {
          startTime[i] <- chain.startTime
        }
      }
      
      endTime <-
        flexibleTimeParse(chain$approvals$endTime, timezone = timezone)
      chain.endTime <-
        flexibleTimeParse(chain$endTime, timezone = timezone)
      
      # clip end points to chart window
      for (i in 1:length(endTime)) {
        if (chain.endTime < endTime[i]) {
          endTime[i] <- chain.endTime
        }
      }
      
      type <- data[[chain_nm]]$approvals$description
      type <- unlist(lapply(type, function(desc) {
        switch(
          desc,
          "Working" = "appr_working_uv",
          "In Review" = "appr_inreview_uv",
          "Approved" = "appr_approved_uv"
        )
      }))
      legendnm <- data[[chain_nm]]$approvals$description
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
        
        if(extendToWholeDays) {
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
  }
  
  return(approvals_all)
}

subsetByMonth <- function(pts, onlyMonth) {
  if(!is.null(pts) && nrow(pts) > 0) {
    return(subset(pts, month == onlyMonth))
  }
  return(pts)
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
    loggedData <- isLogged(ts, ts[[field]]$points, field)
    flagZeroNeg <- getReportMetadata(ts, 'excludeZeroNegative')
    if(loggedData && !isEmptyOrBlank(flagZeroNeg) && flagZeroNeg){
      uv_series <- removeZeroNegative(uv_series)
    }
    
  } else {
    uv_series <- NULL
  }
  
  return(uv_series)
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

#'Import a JSON file to use for report
#'@importFrom jsonlite fromJSON
#'@param file incoming json file
#'@rdname json 
#'@export
json <- function(file){
  if (!file.exists(file)){
    stop(file, ' not found')
  }
  json = fromJSON(file)
  return(json)
}
