# This R file's purpose is for extracting values from json 
# The functions shouldn't modify the data, and should handle missing json or empty json parameters

#' Fetch Report Metadata
#'
#' @description Given a full report object this will extract the metadata
#' @param reportObject The full report data loaded from the report JSON
fetchReportMetadata <- function(reportObject){
  return(reportObject[['reportMetadata']])
}

#' Fetch Report Metadata Field
#'
#' @description Given a full report object this will extract the data
#' associated with the specified field.
#' @param reportObject The full report data loaded from the report JSON
#' @param field The specific field to select from the metadata
fetchReportMetadataField <- function(reportObject, field){
  return(reportObject[['reportMetadata']][[field]])
}

#' Fetch Approvals for a given Time Series
#'
#' @description Given a full report object this will extract the
#' approvals for the supplied series name.
#' @param reportObject The full report data loaded from the report JSON
#' @param field The specific field to select from the metadata
fetchApprovalsForSeries <- function(reportObject, seriesName){
  return(reportObject[[seriesName]][['approvals']])
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

#' Fetch Rating Shifts
#' @description Given a report object, will attempt to pull the rating shifts list.
#' @param reportObject the full report data 
#' @return The list of ratingShifts attached to the report. If none, will be NULL.
fetchRatingShifts <- function(reportObject){
  val <- reportObject$ratingShifts
  return(val)
}

#' Fetch Discharge measurements
#' @description Given a report object, will attempt to pull the measurements list.
#' @param reportObject the full report data 
#' @return The list of measurements attached to the report. If none, will be NULL.
fetchMeasurements <- function(reportObject){
  val <- reportObject$measurements
  return(val)
}