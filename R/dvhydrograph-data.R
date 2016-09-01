parseDVData <- function(data){
  
  stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = FALSE)
  stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = FALSE)
  comp <- getStatDerived(data, "comparisonSeries", "comparisonSeriesDescriptions", estimated = FALSE)
  
  est_stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = TRUE)
  est_stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = TRUE)
  est_stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = TRUE)
  est_comp <- getStatDerived(data, "comparisonSeries", "comparisonSeriesDescriptions", estimated = TRUE)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  
  approvals <- getApprovals(data, chain_nm="firstDownChain", legend_nm=data[['reportMetadata']][["downChainDescriptions1"]],
                            appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), applyFakeTime=TRUE, point_type=73)
  
  if ("fieldVisitMeasurements" %in% names(data)) {
    meas_Q <- getFieldVisitMeasurementsQPoints(data) 
  }
  
  gw_level <- getGroundWaterLevels(data)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "approvals"))]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars)
  
  plotData <- rev(allVars)
  
  return(plotData)
}

parseRefData <- function(data, series) {
  
  legend_name <- switch(series,
                        secondary = "inputDataDescriptions2",
                        tertiary = "inputDataDescriptions3",
                        quaternary = "inputDataDescriptions4")
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  
  time <- formatDates(data[[ref_name]]$points$time)
  ref_data <- list(time = time, 
                   value = data[[ref_name]]$points$value,
                   legend.name = data$reportMetadata[[legend_name]],
                   field = rep(ref_name, length(time)))
  
  
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
                            appr_var_all=c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv"), point_type=73)

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  not_include <- c("data", "series", "legend_name", "ref_name", "time", "ref_data", "approvals")
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars)
  
  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

parseDVSupplemental <- function(data, parsedData){
  
  logAxis <- isLogged(data, parsedData, "firstDownChain")
  
  if(logAxis){
    seq_horizGrid <- unique(floor(log(parsedData$min_iv$value))):unique(ceiling(log(parsedData$max_iv$value)))
  } else {
    seq_horizGrid <- unique(floor(parsedData$min_iv$value)):unique(ceiling(parsedData$max_iv$value))
  }
  
  horizontalGrid <- signif(seq(from=seq_horizGrid[1], to=seq_horizGrid[2], along.with=seq_horizGrid), 1)
  type <- data[['firstDownChain']][['type']]
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getMaxMinIv <- function(data, stat){
  stat_vals <- data[['maxMinData']][[1]][[1]][['theseTimeSeriesPoints']][[stat]]
  list(time = formatDates(stat_vals[['time']][1]),
       value = stat_vals[['value']][1])
}

getStatDerived <- function(data, chain_nm, legend_nm, estimated){
  
  points <- data[[chain_nm]][['points']]
  points$time <- formatDates(points[['time']])
  
  date_index <- getEstimatedDates(data, chain_nm, points$time)
  formatted_data <- parseEstimatedStatDerived(data, points, date_index, legend_nm, chain_nm, estimated)
  
  time_order <- order(formatted_data$time)
  formatted_data$time <- formatted_data$time[time_order]
  formatted_data$value <- formatted_data$value[time_order]
  
  return(formatted_data)
}

