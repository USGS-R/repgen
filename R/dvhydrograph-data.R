parseDVData <- function(data){
  
  stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = FALSE)
  stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = FALSE)
  
  est_stat1 <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = TRUE)
  est_stat2 <- getStatDerived(data, "secondDownChain", "downChainDescriptions2", estimated = TRUE)
  est_stat3 <- getStatDerived(data, "thirdDownChain", "downChainDescriptions3", estimated = TRUE)
  
  max_iv <- getMaxMinIv(data, 'MAX')
  min_iv <- getMaxMinIv(data, 'MIN')
  
  gw_level <- getDiscreteGWData(data)
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {any(unlist(lapply(c(x$time, x$value), function(y) {length(y) != 0}))) } )))]
  allVars <- allVars[which(!names(allVars) %in% c("data"))]
  
  plotData <- splitDataGaps(rev(allVars), 'time', c("max_iv", "min_iv", "gw_level"))
  
  return(plotData)
  
}

parseSecRefData <- function(data, parsedData, zero_logic, isVolFlow, seq_horizGrid) {
  
  secondary_ref <- list(time = formatDates(data$secondaryReferenceTimeSeries$points$time), value = data$secondaryReferenceTimeSeries$points$value)
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}

parseTerRefData <- function(data, parsedData, parseSecRefData, zero_logic, isVolFlow, seq_horizGrid) {
  
  tertiary_ref <- list(time = formatDates(data$tertiaryReferenceTimeSeries$points$time), value = data$tertiaryReferenceTimeSeries$points$value)
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid","parseSecRefData")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}

parseQuaRefData <- function(data, parsedData, parseSecRefData, zero_logic, isVolFlow, seq_horizGrid, parseTerRefData) {
  
  quaternary_ref <- list(time = formatDates(data$quaternaryReferenceTimeSeries$points$time), value = data$quaternaryReferenceTimeSeries$points$value)  
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid","parseSecRefData","parseTerRefData")
  refData <- allVars[which(!names(allVars) %in% not_include)]
}


parseDVSupplemental <- function(data, parsedData, zero_logic){
  
  isVolFlow <- data[['firstDownChain']][['isVolumetricFlow']]
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
  type <- data[['firstDownChain']][['type']]
  
  allVars <- as.list(environment())
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {!is.null(x)} )))]
  allVars <- allVars[unname(unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} )))]
  not_include <- c("data", "parsedData", "zero_logic", "isVolFlow", "seq_horizGrid")
  supplemental <- allVars[which(!names(allVars) %in% not_include)]
  
}

getDiscreteGWData <- function(data){
  vals <- as.numeric(data[['groundWater']][['groundWaterLevel']])
  date_formatted <- as.POSIXct(strptime(data[['groundWater']][['dateString']], "%Y%m%d"))
  list(time = date_formatted,
       value = vals)
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
    list(time = points[['time']][date_index],
         value = points[['value']][date_index],
         legend.name = paste("Estimated", data[['reportMetadata']][[legend_nm]]))
  } else if(!estimated && length(date_index) != 0) {
    list(time = points[['time']][-date_index],
         value = points[['value']][-date_index],
         legend.name = data[['reportMetadata']][[legend_nm]])
  } else {
    list(time = points[['time']],
         value = points[['value']],
         legend.name = data[['reportMetadata']][[legend_nm]])
  }
}

splitDataGaps <- function(data, gapData_nm, ignore_nm){
  notIgnore <- which(!names(data) %in% ignore_nm)
  ignore <- which(names(data) %in% ignore_nm)
  
  data_split <- list()
  
  for(i in notIgnore){
    t <- data[[i]][[gapData_nm]]
    t_dates <- as.Date(format(t, "%Y-%m-%d"))
    diff_days <- diff(t_dates)
    split_index <- which(diff_days > 2) + 1
    
    if(length(split_index) != 0){
      t.breaks <- c(head(t, 1), t[split_index], tail(t, 1))
      t.categ <- cut(t, breaks = t.breaks, 
                     labels = as.character(1:(length(t.breaks)-1)),
                     include.lowest = TRUE, right = FALSE)
      
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
}

connectTS <- function(data_split){
  
  len_data <- length(data_split)
  if(len_data == 1){  # you can't connect data that is only of length one
    return(data_split)
  } else {
  
    first_dates <- lapply(data_split, function(d) {d$time[1]})
    ranks <- rank(unlist(first_dates))
    
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
      if(f <= 1 && !is_est){
        data_split[[t_now]][['time']] <- append(data_split[[t_now]][['time']], first_dates[[t_next]])
        data_split[[t_now]][['value']] <- append(data_split[[t_now]][['value']], first_vals[[t_next]])
      } else if(f <= 1 && is_est){
        data_split[[t_next]][['time']] <- append(last_dates[[t_now]], data_split[[t_next]][['time']])
        data_split[[t_next]][['value']] <- append(last_vals[[t_now]], data_split[[t_next]][['value']])
      }
    }
    
  }
  
  return(data_split)
}
