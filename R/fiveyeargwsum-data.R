

parseFiveYrData <- function(data){
  
  stat_info <- getPriorityStat(data)
  stat <- getStatDerived_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, estimated = FALSE)
  
  est_stat <- getStatDerived_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, estimated = TRUE)
  
  max_iv <- getMaxMinIv_fiveyr(data, 'MAX')
  min_iv <- getMaxMinIv_fiveyr(data, 'MIN')
  
  appr_approved <- getApprovals_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, "Approved")
  appr_inreview <- getApprovals_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, "In-Review")
  appr_working <- getApprovals_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, "Working")

  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {all(unlist(lapply(list(x$time, x$value), function(y) {length(y) != 0}))) } )))]
  allVars <- allVars[which(!names(allVars) %in% c("data"))]
  
  plotData <- rev(allVars)
  
  return(plotData)
  
}

parseFiveYrSupplemental <- function(data, parsedData, zero_logic){
  
  isVolFlow <- data[['primaryTimeSeries']][['isVolumetricFlow']]
  if(is.null(isVolFlow) || !isVolFlow || zero_logic){
    logAxis <- FALSE
  } else if(isVolFlow && !zero_logic){  
    logAxis <- TRUE
  }
  
  if(logAxis){
    seq_horizGrid <- unique(floor(log(parsedData$min_iv$value))):unique(ceiling(log(parsedData$max_iv$value)))
  } else {
    seq_horizGrid <- unique(floor(parsedData$min_iv$value)):unique(ceiling(parsedData$max_iv$value))
  }
  
  horizontalGrid <- signif(seq(from=seq_horizGrid[1], to=seq_horizGrid[2], along.with=seq_horizGrid), 1)
  
  startDate <- formatDates_fiveyr(data$reportMetadata$startDate, "start")
  endDate <- formatDates_fiveyr(data$reportMetadata$endDate, "end")
  
  date_seq_mo <- seq(from=startDate, to=endDate, by="month")
  first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
  date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
  month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
  month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
  month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid",
                   "first_yr", "month_label_split")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv_fiveyr <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  list(time = formatDates_fiveyr(stat_vals[['time']][1], type=NA),
       value = stat_vals[['value']][1])
}

getPriorityStat <- function(data){
  descriptions <- c(data$reportMetadata$downChainDescriptions1, 
                    data$reportMetadata$downChainDescriptions2, 
                    data$reportMetadata$downChainDescriptions3)
  
  match_index <- grep(x = descriptions, pattern = "Mean")
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Max")} 
  if(length(match_index) == 0){match_index <- grep(x = descriptions, pattern = "Min")}
  
  match_names <- c("firstDownChain", "secondDownChain", "thirdDownChain")
  match_description_nm <- c("downChainDescriptions1", "downChainDescriptions2", "downChainDescriptions3")
  
  return(list(data_nm = match_names[match_index], descr_nm = match_description_nm[match_index]))
}

getStatDerived_fiveyr <- function(data, chain_nm, legend_nm, estimated){
  
  points <- data[[chain_nm]][['points']]
  points$time <- formatDates_fiveyr(points[['time']], type=NA)
  
  est_dates <- getEstimatedDates(data, chain_nm)
  date_index <- which(points$time >= est_dates[1] & points$time <= est_dates[2])
  
  if(estimated){
    list(time = points[['time']][-date_index],
         value = points[['value']][-date_index],
         legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]))
  } else if(!estimated && length(date_index) != 0) {
    list(time = points[['time']][date_index],
         value = points[['value']][date_index],
         legend.name = data[['reportMetadata']][[legend_nm]])
  } else {
    list(time = points[['time']],
         value = points[['value']],
         legend.name = data[['reportMetadata']][[legend_nm]])
  }
}

getApprovals_fiveyr <- function(data, chain_nm, legend_nm, appr_type){
  
  points <- data[[chain_nm]][['points']]
  points$time <- formatDates_fiveyr(points[['time']], type=NA)
  
  appr_dates <- getApprovalDates(data, chain_nm, appr_type)
  date_index <- which(points$time >= appr_dates[1] & points$time <= appr_dates[2])
  applicable_dates <- points[['time']][-date_index]

  return(list(time = applicable_dates,
              value = substitute(getYvals_approvals(dvhplot, length(applicable_dates))),
              legend.name = paste(appr_type, data[['reportMetadata']][[legend_nm]])))

}

getYvals_approvals <- function(object, num_vals){
  ylim <- ylim(object)$side.2[1]
  yvals <- rep(ylim, num_vals)
}

getEstimatedDates <- function(data, chain_nm){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- formatDates_fiveyr(data[[chain_nm]]$qualifiers$startDate[i], type=NA)
  endTime <- formatDates_fiveyr(data[[chain_nm]]$qualifiers$endDate[i], type=NA)
  return(c(startTime, endTime))
}

getApprovalDates <- function(data, chain_nm, appr_type){
  i <- which(data[[chain_nm]]$approvals$description == appr_type)
  startTime <- formatDates_fiveyr(data[[chain_nm]]$approvals$startTime[i], type=NA)
  endTime <- formatDates_fiveyr(data[[chain_nm]]$approvals$endTime[i], type=NA)
  return(c(startTime, endTime))
}

formatDates_fiveyr <- function(char_date, type){
  date_formatted <- as.POSIXct(strptime(char_date, "%FT%T"))
  if(!is.na(type) && type=="start"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-01"))
  } else if(!is.na(type) && type=="end"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-30"))
  }
  return(date_formatted)
}

reorder_approvals <- function(object){
  approvals_match <- lapply(object$view.1.2, function(x) {match(c("Approved", "In-Review", "Working"), x$legend.name)})
  approvals_logic <- lapply(approvals_match, function(x) {any(!is.na(x))})
  approvals_index <- which(unlist(approvals_logic))
  notApprovals_index <- which(!unlist(approvals_logic))
  object$view.1.2 <- object$view.1.2[c(approvals_index, notApprovals_index)]
  return(object)
}
