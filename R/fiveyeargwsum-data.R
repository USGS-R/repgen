

parseFiveYrData <- function(data){
  
  stat_info <- getPriorityStat(data)
  stat <- getStatDerived_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, estimated = FALSE)
  
  est_stat <- getStatDerived_fiveyr(data, stat_info$data_nm, stat_info$descr_nm, estimated = TRUE)
  
  max_iv <- getMaxMinIv_fiveyr(data, 'MAX')
  min_iv <- getMaxMinIv_fiveyr(data, 'MIN')
  
  approvals <- getApprovals(data, chain_nm=stat_info$data_nm, legend_nm=data[['reportMetadata']][[stat_info$descr_nm]], 
                                   appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv", "appr_admin_uv"), point_type=73, extendToWholeDays=TRUE)
  
  gw_level <- getGroundWaterLevels(data)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "stat_info", "approvals"))]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)

  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

parseFiveYrSupplemental <- function(data, parsedData){
  
  logAxis <- isLogged(data, parsedData, "firstDownChain")
  
  if(logAxis){
    seq_horizGrid <- unique(floor(log(parsedData$min_iv$value))):unique(ceiling(log(parsedData$max_iv$value)))
  } else {
    seq_horizGrid <- unique(floor(parsedData$min_iv$value)):unique(ceiling(parsedData$max_iv$value))
  }
  
  horizontalGrid <- signif(seq(from=seq_horizGrid[1], to=seq_horizGrid[2], along.with=seq_horizGrid), 1)
  
  startDate <- toStartOfMonth(flexibleTimeParse(data$reportMetadata$startDate, data$reportMetadata$timezone))
  endDate <- toEndOfMonth(flexibleTimeParse(data$reportMetadata$endDate, data$reportMetadata$timezone))
  
  date_seq_mo <- seq(from=startDate, to=endDate, by="month")
  first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
  date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
  month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
  month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
  month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  
  type <- data[['firstDownChain']][['type']]
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid",
                   "first_yr", "month_label_split")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv_fiveyr <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  list(time = flexibleTimeParse(stat_vals[['time']][1], timezone=data$reportMetadata$timezone),
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
  points$time <- flexibleTimeParse(points[['time']], timezone=data$reportMetadata$timezone)
  
  date_index <- getEstimatedDates(data, chain_nm, points$time)
  formatted_data <- parseEstimatedStatDerived(data, points, date_index, legend_nm, chain_nm, estimated)
  
  return(formatted_data)
}
