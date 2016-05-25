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
                            appr_var_all=c("appr_approved", "appr_inreview", "appr_working"), plot_type="dvhydro")
  
  meas_Q <- getFieldVisitMeasurementsQPoints(data) 
  
  gw_level <- getGroundWaterLevels(data)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- allVars[which(!names(allVars) %in% c("data", "approvals"))]
  
  plotData <- splitDataGaps(rev(allVars), 'time', c("max_iv", "min_iv", "gw_level", 
                                                    "appr_approved", "appr_inreview", "appr_working", "meas_Q"))
  
  return(plotData)
}

parseRefData <- function(data, series) {
  
  legend_name <- switch(series,
                        secondary = "inputDataDescriptions2",
                        tertiary = "inputDataDescriptions3",
                        quaternary = "inputDataDescriptions4")
  
  ref_name <- paste0(series, "ReferenceTimeSeries")
  
  ref_data <- list(time = formatDates(data[[ref_name]]$points$time), 
                   value = data[[ref_name]]$points$value,
                   legend.name = data$reportMetadata[[legend_name]])
  
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
                            appr_var_all=c("appr_approved", "appr_inreview", "appr_working"), plot_type="dvhydro")

  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  not_include <- c("data", "series", "legend_name", "ref_name", "ref_data", "approvals")
  allVars <- allVars[which(!names(allVars) %in% not_include)]
  
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {all(unlist(lapply(list(x$time, x$value), function(y) {length(y) != 0}))) } )))]
  
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
  
  if(estimated){
    formatted_data <- list(time = points[['time']][date_index],
                           value = points[['value']][date_index],
                           legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]))
  } else if(!estimated && length(date_index) != 0) {
    formatted_data <- list(time = points[['time']][-date_index],
                           value = points[['value']][-date_index],
                           legend.name = data[['reportMetadata']][[legend_nm]])
  } else {
    formatted_data <- list(time = points[['time']],
                           value = points[['value']],
                           legend.name = data[['reportMetadata']][[legend_nm]])
  }
  time_order <- order(formatted_data$time)
  formatted_data$time <- formatted_data$time[time_order]
  formatted_data$value <- formatted_data$value[time_order]
  return(formatted_data)
}

splitDataGaps <- function(data, gapData_nm, ignore_nm){
  notIgnore <- which(!names(data) %in% ignore_nm)
  ignore <- which(names(data) %in% ignore_nm)
  
  if(length(notIgnore) != 0){
  
    data_split <- list()
    
    for(i in notIgnore){
      t <- data[[i]][[gapData_nm]]
      t_dates <- as.Date(format(t, "%Y-%m-%d"))
      diff_days <- diff(t_dates)
      split_index <- which(abs(diff_days) > 1) + 1
      
      if(length(split_index) != 0){
        
        # checking if there is a split right before the last data point
        # if there is, do not add it as a break
        last_break <- tail(split_index, 1)
        if(last_break != length(t)){
          t.breaks <- c(head(t, 1), t[split_index], tail(t, 1))
          t.categ <- cut(t, breaks = t.breaks, 
                         labels = as.character(1:(length(t.breaks)-1)),
                         include.lowest = TRUE, right = FALSE)
        } else {
          # make last entry it's own category
          t.breaks <- c(head(t, 1), t[split_index])
          t.categ <- cut(t, breaks = t.breaks, 
                         labels = as.character(1:(length(t.breaks)-1)),
                         include.lowest = TRUE, right = FALSE)
          add_categ <- as.character(length(split_index)+1)
          t.categ <- factor(t.categ, levels = c(levels(t.categ), add_categ))
          t.categ[last_break] <- add_categ
        }
        
        dataDf <- as.data.frame(data[[i]]) %>% 
          mutate(timeCategory = t.categ) %>% 
          split(., t.categ)
        
        dataList <- lapply(dataDf, as.list)
        names(dataList) <- rep(names(data)[i], length(dataList))
        
        dup <- which(duplicated(names(dataList)))
        dataList[[1]][['legend.name']] <- as.character(unique(dataList[[1]][['legend.name']]))
        dataList[dup] <- lapply(dataList[dup], function(l) {
          l[['legend.name']] <- NULL
          return(l)
        })
        
      } else {
        dataList <- data[i]
        
      }
      
      data_split <- append(data_split, dataList)
    }
    
    data_split <- connectTS(data_split)
    data_split <- append(data_split, data[ignore])
    
    # reorder so that estimated series are plotted first
    # that way non-estimated points are on top of estimated points
    which_est <- grep("est", names(data_split))
    which_not_est <- grep("est", names(data_split), invert=TRUE)
    data_reordered <- data_split[c(which_est, which_not_est)]
    
  } else {
    return(data)
  }
}

connectTS <- function(data_split){
  
  len_data <- length(data_split)
  if(len_data <= 1){  # you can't connect data that is only of length one
    return(data_split)
  } else {
  
    first_dates <- lapply(data_split, function(d) {d$time[1]})
    ranks <- order(unlist(first_dates))
    
    #reorder the data
    data_split <- data_split[ranks]
    first_dates <- lapply(data_split, function(d) {d$time[1]})
    last_dates <- lapply(data_split, function(d) {tail(d$time, 1)})
    first_vals <- lapply(data_split, function(d) {d$value[1]})
    last_vals <- lapply(data_split, function(d) {tail(d$value, 1)})
  
    for(t_now in seq(len_data-1)){
      t_next <- t_now + 1
      f <- diff(c(last_dates[[t_now]], first_dates[[t_next]]))
      is_est <- length(grep("est", names(data_split)[t_now])) != 0
      
      #estimated time period is set, extend others to connect ts
      if(f >= 0 && f <= 1 && is_est){
        data_split[[t_now]][['time']] <- append(data_split[[t_now]][['time']], first_dates[[t_next]])
        data_split[[t_now]][['value']] <- append(data_split[[t_now]][['value']], first_vals[[t_next]])
      } else if(f >= 0 && f <= 1 && !is_est){
        data_split[[t_next]][['time']] <- append(last_dates[[t_now]], data_split[[t_next]][['time']])
        data_split[[t_next]][['value']] <- append(last_vals[[t_now]], data_split[[t_next]][['value']])
      } 
    }
    
  }
  
  return(data_split)
}
