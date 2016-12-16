parseDVData <- function(data){
  
  rmZeroNeg <- getReportMetadata(data, 'excludeZeroNegative')
  not_include <- c("not_include", "data", "approvals", 'rmZeroNeg', 'excludeMinMax')
  
  stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE, rmZeroNeg)
  stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = FALSE, rmZeroNeg)
  stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = FALSE, rmZeroNeg)
  comp <- getStatDerived(data, "comparisonSeries", "comparisonSeriesDescriptions", estimated = FALSE, rmZeroNeg)
  
  est_stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = TRUE, rmZeroNeg)
  est_stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = TRUE, rmZeroNeg)
  est_stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = TRUE, rmZeroNeg)
  est_comp <- getStatDerived(data, "comparisonSeries", "comparisonSeriesDescriptions", estimated = TRUE, rmZeroNeg)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  excludeMinMax <- data[['reportMetadata']][['excludeMinMax']]
  if( (!isEmptyOrBlank(excludeMinMax) && excludeMinMax) || 
      (!isEmptyOrBlank(rmZeroNeg) && rmZeroNeg && !isEmptyOrBlank(max_iv$value) && max_iv$value <= 0) ){
    max_iv_label <- getMaxMinIv(data, 'MAX')
    not_include <- c(not_include, 'max_iv')
  } 
  if( (!isEmptyOrBlank(excludeMinMax) && excludeMinMax) 
     || (!isEmptyOrBlank(rmZeroNeg) && rmZeroNeg && !isEmptyOrBlank(min_iv$value) && min_iv$value <= 0) ){
    min_iv_label <- getMaxMinIv(data, 'MIN')
    not_include <- c(not_include, 'min_iv')
  }
  
  approvals <- getApprovals(data, chain_nm="firstDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions1"]],
                            appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), applyFakeTime=TRUE, point_type=73, extendToWholeDays=TRUE)
  
  if ("fieldVisitMeasurements" %in% names(data)) {
    meas_Q <- getFieldVisitMeasurementsQPoints(data) 
  }
  
  gw_level <- getGroundWaterLevels(data)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)

  allVars <- parseEstimatedEdges(data, allVars)
  
  plotData <- rev(allVars)
  
  return(plotData)
}

parseRefData <- function(data, series) {
  
  legend_name <- switch(series,
                        secondary = "inputDataDescriptions2",
                        tertiary = "inputDataDescriptions3",
                        quaternary = "inputDataDescriptions4")
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  
  time <- flexibleTimeParse(data[[ref_name]]$points$time, timezone=data$reportMetadata$timezone)
  ref_points <- data.frame(time = time, 
                           value = data[[ref_name]]$points$value)
  
  #if this data is on a logged axis, remove negatives and zeros
  if(!isEmptyVar(ref_points)){
    loggedData <- isLogged(data, ref_points, ref_name)
    rmZeroNeg <- getReportMetadata(data, 'excludeZeroNegative')
    if(loggedData && !isEmptyOrBlank(rmZeroNeg) && rmZeroNeg){
      ref_points <- removeZeroNegative(ref_points)
    }
  }
  
  ref_data <- list(time = ref_points$time, 
                   value = ref_points$value,
                   legend.name = data$reportMetadata[[legend_name]],
                   field = rep(ref_name, length(ref_points$time)))
  
  
  # need to name data so that "Switch" in dvhydrograph-styles.R will be able to match
  if(series == "secondary"){
    secondary_ref <- ref_data
  } else if(series == "tertiary"){
    tertiary_ref <- ref_data
  } else if(series == "quaternary"){
    quaternary_ref <- ref_data
  }
  
  # add in approval lines from primary plot
  approvals <- getApprovals(data, chain_nm=ref_name, legend_nm=data[['reportMetadata']][[legend_name]],
                            appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), point_type=73, extendToWholeDays=TRUE)

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  not_include <- c("data", "series", "legend_name", "ref_name", "time", "ref_data", 
                   "approvals", "loggedData", "ref_points", "rmZeroNeg")
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars, isDV=TRUE)
  
  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

parseEstimatedEdges <- function(data, allVars){
  datasets <- unlist(allVars, recursive = FALSE)[which(grepl("stat\\d", names(unlist(allVars, recursive = FALSE))))]
  times <- datasets[which(grepl("time", names(datasets)))]
  values <- datasets[which(grepl("value", names(datasets)))]
  estData <- data.frame(time=unname(unlist(times[which(grepl("est", names(times)))])), value=unname(unlist(values[which(grepl("est", names(values)))])), set="est")
  statData <- data.frame(time=unname(unlist(times[which(!grepl("est", names(times)))])), value=unname(unlist(values[which(!grepl("est", names(values)))])), set="stat")
  
  data <- rbind(estData, statData) %>% arrange(time) %>%
    mutate(y0 = ifelse(set != lag(set), lag(value), NA)) %>%
    filter(set != lag(set)) %>% select(x = time, y0, y1 = value, newSet=set) %>% as.list
  
  allVars <- c(allVars, estEdges = list(data))
  return(allVars)
}

parseDVSupplemental <- function(data, parsedData){
  logAxis <- isLogged(data, parsedData, "firstDownChain")
  type <- data[['firstDownChain']][['type']]
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "seq_horizGrid")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  time_val <- flexibleTimeParse(stat_vals[['time']][1], timezone=data$reportMetadata$timezone)
  val <- stat_vals[['value']][1]
  # semantics for min/max are swapped on inverted plots
  if(getReportMetadata(data, 'isInverted')){
    stat <- ifelse(stat == "MAX", "MIN", "MAX") 
  }
  label <- paste(paste0(substring(toupper(stat), 1, 1), substring(tolower(stat), 2)), 
                 "Instantaneous", sep='. ')
  maxmin <- list(time = time_val, value = val, label = label,
                 legend.name = paste(label, data[['firstDownChain']][['type']], ":", val))
  return(maxmin)
}

#' @export
getStatDerived <- function(data, chain_nm, legend_nm, estimated, rmZeroNeg){
  
  points <- data[[chain_nm]][['points']]
  points$time <- flexibleTimeParse(points[['time']], timezone=data$reportMetadata$timezone, shiftTimeToNoon=FALSE)
  
  #if this data is on a logged axis, remove negatives and zeros
  if(!isEmptyVar(points)){
    loggedData <- isLogged(data, points, chain_nm)
    if(loggedData && !isEmptyOrBlank(rmZeroNeg) && rmZeroNeg){
      points <- removeZeroNegative(points)
    }
  }
  
  date_index <- getEstimatedDates(data, chain_nm, points$time, isDV=TRUE)
  formatted_data <- parseEstimatedStatDerived(data, points, date_index, legend_nm, chain_nm, estimated)
  
  time_order <- order(formatted_data$time)
  formatted_data$time <- formatted_data$time[time_order]
  formatted_data$value <- formatted_data$value[time_order]
  
  return(formatted_data)
}

#' Use the last point plus 2400 to extend step
#' the points do not have times, but the x limit is extended with a time to show the whole day
#' the step needs to be extended to meet this time
#' @param toPlot list of items that will be called in the do.call 
extendStep <- function(toPlot){
  #check first whether it's a feature added to the plot as a step
  isStep <- 'type' %in% names(toPlot) && toPlot[['type']] == "s"
  
  if(isStep){
    toPlot$x <- c(toPlot$x,  tail(toPlot$x, 1) + 87000) #this is 2400, if changing to POSTLT, need to use lubridate
    toPlot$y <- c(toPlot$y,  tail(toPlot$y,1))
  }
  
  return(toPlot)
}

