
#'@title get value from extremes json list
#'@description convienence function for accessing from the "values" block in 
#'extremes json
#'@param ts a list, can be the output of \code{\link[jsonlite]{fromJSON}}.
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
    ifelse(as.numeric, return(as.numeric(NA)), return(" "))
  } else {
    return(val)
  }
}

getRatingShifts <- function(ts, param, ...){
  val <- ts$ratingShifts[[param]]
  return(validParam(val, param, ...))
}

getMeasurements <- function(ts, param, ...){
  val <- ts$measurements[[param]]
  return(validParam(val, param, ...))
}

getMaxStage <- function(ts, ...){
  val <- as.numeric(ts$maximumStageHeight)
  return(validParam(val, param = 'maximumStageHeight', ...))
}

getMinStage <- function(ts, ...){
  val <- as.numeric(ts$minimumStageHeight)
  return(validParam(val, param = 'minimumStageHeight', ...))
}

#finds if the plot data has any zero values
zeroValues <- function(dataList, val_nm){    
  logList <- lapply(dataList, function(x) {any(na.omit(x[[val_nm]]) == 0)})
  logVector <- any(unlist(unname(logList)))
}

#finds if the plot data has any zero values
negValues <- function(dataList, val_nm){    
  logList <- lapply(dataList, function(x) {any(na.omit(x[[val_nm]]) < 0)})
  logVector <- any(unlist(unname(logList)))
}

#if absolutely no data comes back after parsing - skip to render with a message
anyDataExist <- function(data){
  emptyData <- any(c(length(data) == 0, nrow(data) == 0, is.null(data)))
  notEmptyData <- !emptyData
  return(notEmptyData)
}

############ used in dvhydrograph-data, fiveyeargwsum-data, uvhydrograph-data ############ 

getGroundWaterLevels<- function(ts, ...){
  y <- as.numeric(ts$gwlevel[['groundWaterLevel']])
  x <- ts$gwlevel[['recordDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, stringsAsFactors = FALSE))
}


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
  return(data.frame(time=time, value=y, month=month, stringsAsFactors = FALSE))
}


getFieldVisitMeasurementsQPoints <- function(ts){
  y <- ts$fieldVisitMeasurements[['discharge']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  minQ <- ts$fieldVisitMeasurements[['errorMinDischarge']]
  maxQ <- ts$fieldVisitMeasurements[['errorMaxDischarge']]
  n <- ts$fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minQ=minQ, maxQ=maxQ, n=n, month=month, stringsAsFactors = FALSE))
}


getFieldVisitMeasurementsShifts <- function(ts){
  y <- ts$fieldVisitMeasurements[['shiftInFeet']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  minShift <- ts$fieldVisitMeasurements[['errorMinShiftInFeet']]
  maxShift <- ts$fieldVisitMeasurements[['errorMaxShiftInFeet']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minShift=minShift, maxShift=maxShift, month=month, stringsAsFactors = FALSE))
}


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
          comment=c(comment, comment2), stringsAsFactors = FALSE))
  # }
}
getEstimatedDates <- function(data, chain_nm, time_data){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- formatDates(data[[chain_nm]]$qualifiers$startDate[i])
  endTime <- formatDates(data[[chain_nm]]$qualifiers$endDate[i])
  est_dates <- data.frame(start = startTime, end = endTime)
  
  date_index <- c()
  for(n in seq(nrow(est_dates))){
    date_index_n <- which(time_data > est_dates$start[n] & time_data < est_dates$end[n])
    date_index <- append(date_index, date_index_n)
  }
  
  return(date_index)
}

getApprovals <- function(data, chain_nm, legend_nm, appr_var_all, month=NULL, point_type=NULL, subsetByMonth=FALSE, approvalsAtBottom=TRUE, applyFakeTime=FALSE){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  
  if(approvalsAtBottom==FALSE) {
    approved_dates <- getApprovalDates(data, chain_nm, "Approved")
    review_dates <-getApprovalDates(data, chain_nm, "In Review")
    working_dates <- getApprovalDates(data, chain_nm, "Working")

    approved_points <- list()
    review_points <- list()
    working_points <- list()
      
    if(subsetByMonth){
      points <- subsetByMonth(getTimeSeries(data, chain_nm), month)
      points_no_times <- points
      points_no_times$time <- as.POSIXct(strptime(points_no_times$time, "%F"))
    } else {
      points <- data[[chain_nm]][['points']]
      points_no_times <- points
      points_no_times$time <- as.POSIXct(strptime(points_no_times[['time']], "%F"))
    }

    working_index<- apply(working_dates, 1, function(d, points){
      which(points$time >= d[1] & points$time <= d[2])}, 
      points=points_no_times)

    review_index<- apply(review_dates, 1, function(d, points){
      which(points$time >= d[1] & points$time <= d[2])}, 
      points=points_no_times)

    approved_index<- apply(approved_dates, 1, function(d, points){
      which(points$time >= d[1] & points$time <= d[2])}, 
      points=points_no_times)

    if(is.list(working_index)){
      working_index <- unique(unlist(working_index, recursive=FALSE))
    }

    if(is.list(review_index)){
      review_index <- unique(unlist(review_index, recursive=FALSE))
    }

    if(is.list(approved_index)){
      approved_index <- unique(unlist(approved_index, recursive=FALSE))
    }
    
    review_index <- setdiff(review_index, working_index)
    approved_index <- setdiff(approved_index, review_index)

    date_index_list <- list(list(type="Working",working_index), list(type="In Review",review_index), list(type="Approved",approved_index))

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
  } else { #approvals at bottom 
    approval_info <- list()
    appr_dates <- NULL
    
    if (!isEmpty(data[[chain_nm]]$approvals$startTime)) {
      startTime <- flexibleTimeParse(data[[chain_nm]]$approvals$startTime, timezone = data$reportMetadata$timezone)
      endTime <- flexibleTimeParse(data[[chain_nm]]$approvals$endTime, timezone = data$reportMetadata$timezone)
      #hacky fix for 9999 year issue which prevents the rectangles from displaying on the graphs 
      #apologies to the people of 2100 who have to revisit this
      for (i in 1:length(endTime)) {
        if (as.Date(endTime)[i] > "2100-12-31") { 
          endT <- endTime
          md <- strftime(endT, format="%m-%d")
          time <- strftime(endT, format="%H:%M:%S")
          reformatted <- paste0("2100-", md," ", time)
          endTime[i] <- reformatted
        }
      }
      type <- data[[chain_nm]]$approvals$description
      type <- unlist(lapply(type, function(desc) {
        switch(desc,
               "Working" = "appr_working_uv",
               "In Review" = "appr_inreview_uv",
               "Approved" = "appr_approved_uv")
      }))
      legendnm <- data[[chain_nm]]$approvals$description
      appr_dates <- data.frame(startTime=startTime, endTime=endTime, type=type, legendnm=legendnm, stringsAsFactors = FALSE)
    }
    
    if (!isEmpty(appr_dates) && nrow(appr_dates)>0) {  
      for(i in 1:nrow(appr_dates)){
        approval_info[[i]] <- list(x0 = appr_dates[i, 1],
                                   x1 = appr_dates[i, 2],
                                   y0 = substitute(getYvals_approvals(plot_object, 1)), 
                                   y1 = substitute(getYvals_approvals(plot_object, 1) + addHeight(plot_object)),             
                                   legend.name = paste(appr_dates[i, 4], legend_nm), time=appr_dates[1,1]) ##added a fake time var to get through a future check
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
  startTime <- formatDates(data[[chain_nm]]$approvals$startTime[i], type=NA)
  endTime <- formatDates(data[[chain_nm]]$approvals$endTime[i], type=NA)
  return(data.frame(startTime=startTime, endTime=endTime))
}

#'@importFrom lubridate parse_date_time
getTimeSeries <- function(ts, field, estimatedOnly = FALSE){
  y <- ts[[field]]$points[['value']]
  x <- ts[[field]]$points[['time']]
  
  if(!is.null(y) & !is.null(x)){
    time <- flexibleTimeParse(x, ts$reportMetadata$timezone)
    
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
    
  } else {
    uv_series <- NULL
  }
  
  return(uv_series)
}

#'@title will attempt to parse a DV, UTC time, or offset time
#'@description convienence that will attempt to parse a DV, UTC time, or offset time
#'extremes json
#'@param x the date/time
#'@param timezone a timezone code
#'@return time vector
#'@export
#'@importFrom lubridate parse_date_time
flexibleTimeParse <- function(x, timezone) {
  
  #first attempt utc
  format <- "Ymd HMOS z"
  time <- parse_date_time(x,format, tz=timezone,quiet = TRUE)
  
  #then attempt an offset time
  if(isEmpty(time)) {
    format <- "Ymd T* z*"
    time <- parse_date_time(x,format, tz=timezone, quiet = TRUE)
  }
  
  #then attempt a DV
  if(isEmpty(time)) {
    format <- "Ymd"
    time <- parse_date_time(x,format, tz=timezone,quiet = TRUE)
    time <- time + hours(12)
  }
  
  return(time)
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
