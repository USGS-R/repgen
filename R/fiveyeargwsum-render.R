createfiveyeargwsumPlot <- function(reportObject){
  #Rendering Options
  options(scipen=8)
  
  #Validation Options
  requiredTimeSeriesFields <- c(
    "points",
    "approvals",
    "qualifiers",
    "startTime",
    "endTime",
    "isVolumetricFlow",
    "units",
    "grades",
    "type",
    "gaps",
    "gapTolerances",
    "name"
  )
  requiredMetadataFields <- c(
    'startDate',
    'endDate',
    'isInverted',
    'timezone',
    'stationId',
    'stationName',
    'title'
  )

  #Validate Report Metadata
  metaData <- fetchReportMetadata(reportObject)

  #Get Necessary Report Metadata
  if(validateFetchedData(metaData, "metadata", requiredMetadataFields)){
    timezone <- fetchReportMetadataField(reportObject, 'timezone')
    excludeMinMaxFlag <- parseReportMetadataField(reportObject, 'excludeMinMax', FALSE)
    invertedFlag <- parseReportMetadataField(reportObject, 'isInverted', FALSE)
    startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone=timezone)
    endDate <- toEndOfDay(flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone=timezone))
    date_seq_mo <- seq(from=startDate, to=endDate, by="month")
    first_yr <- date_seq_mo[which(month(date_seq_mo) == 1)[1]]
    date_seq_yr <- seq(from=first_yr, to=endDate, by="year")
    month_label_location <- date_seq_mo + (60*60*24*14) #make at 15th of month
    month_label_split <- strsplit(as.character(month(date_seq_mo, label=TRUE)), "")
    month_label <- unlist(lapply(month_label_split, function(x) {x[1]}))
  }

  #Get Priority Stat Time Series
  priorityStat <- getPriorityStat(reportObject)

  #Get Basic Plot data
  statTimeSeries <- parseTimeSeries(reportObject, priorityStat[['data_nm']], priorityStat[['desc_nm']], timezone, isDV=TRUE)
  statTimeSeriesEst <- parseTimeSeries(reportObject, priorityStat[['data_nm']], priorityStat[['desc_nm']], timezone, estimated=TRUE, isDV=TRUE)

  #Validate Basic Plot Data
  if(is.null(c(statTimeSeries, statTimeSeriesEst))){
    return(NULL)
  }

  #Get Additional Plot Data
  groundWaterLevels <- parseGroundWaterLevels(reportObject)
  minMaxIVs <- parseMinMaxIVs(reportObject, timezone, stat1TimeSeries[['type']], invertedFlag, excludeMinMaxFlag, excludeZeroNegativeFlag)
  minMaxCanLog <- TRUE

  if(!isEmptyOrBlank(minMaxIVs)){
    minMaxLabels <- minMaxIVs[grepl("label", names(minMaxIVs))]
    minMaxPoints <- minMaxIVs[!grepl("label", names(minMaxIVs))]
    minMaxCanLog <- minMaxIVs[['canLog']]
  }

  logAxis <- isLogged(stat1TimeSeries[['points']], stat1TimeSeries[['isVolumetricFlow']], excludeZeroNegativeFlag) && minMaxCanLog

  #Create the Base Plot Object
  plot_object <- gsplot(yaxs = 'i', ylog=logAxis, xaxt = "n", mar = c(8, 4, 4, 2) + 0.1) %>%
      axis(side = 1, at = date_seq_mo, labels = FALSE) %>%
      view(xlim = c(startDate, endDate)) %>%
      legend(location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = isInverted) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(ylab = "Water Level, Below LSD (feet)")

  plot_object <- 
    XAxisLabels(plot_object, month_label, month_label_location, date_seq_yr + months(6))

  #Plot Time Series
  plot_object <- plotTimeSeries(plot_object, statTimeSeries, 'statTimeSeries', timezone, FALSE, getFiveyearPlotConfig, isDV=TRUE)
  plot_object <- plotTimeSeries(plot_object, statTimeSeriesEst, 'statTimeSeriesEst', timezone, FALSE, getFiveyearPlotConfig, isDV=TRUE)

  # add vertical lines to delineate calendar year boundaries
  plot_object <- DelineateYearBoundaries(plot_object, fiveyrInfo$date_seq_yr)
  
  plot_object <- ApplyApprovalBarStyles(plot_object, fiveyrData)
  
  plot_object <- rmDuplicateLegendItems(plot_object)
  
  # patch up top extent of y-axis
  plot_object <- RescaleYTop(plot_object)

  #Add Min/Max labels if we aren't plotting min and max
  if(!is.null(fiveyrData$max_iv_label) && !is.null(fiveyrData$min_iv_label)){
    #Extract Timezone
    tzf <- format(as.POSIXct(fiveyrData$max_iv_label$time), "%z")
    #Insert ":" before 2nd to last character
    tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf) 
    plot_object <- plot_object %>% 
        mtext(paste0(maxLabel, " ", fiveyrInfo$type, ": ", fiveyrData$max_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$max_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                            side = 3, axes=FALSE, cex=0.625, line = 0.85, adj = 0) %>%
        mtext(paste0(minLabel, " ", fiveyrInfo$type, ": ", fiveyrData$min_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$min_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                            side = 3, axes=FALSE, cex=0.625, line = 0.1, adj = 0)
  }

  return(plot_object)
}


createfiveyeargwsumPlot_old <- function(data){
  options(scipen=8)
  isInverted <- data$reportMetadata$isInverted
  fiveyrData <- parseFiveYrData(data)
  
  #semantics for min/max are swapped on inverted plots
  maxLabel <- "Max. Instantaneous"
  minLabel <- "Min. Instantaneous"
  if(isInverted) {
    maxLabel <- "Min. Instantaneous"
    minLabel <- "Max. Instantaneous"
  }
  
  if(anyDataExist(fiveyrData)){
    
    fiveyrInfo <- parseFiveYrSupplemental(data, fiveyrData)
    
    plot_object <- gsplot(yaxs = 'i', xaxt = "n", mar = c(8, 4, 4, 2) + 0.1) %>%
      axis(side = 1, at = fiveyrInfo$date_seq_mo, labels = FALSE) %>%
      view(xlim = c(fiveyrInfo$startDate, fiveyrInfo$endDate)) %>%
      legend(location = "below", cex = 0.8, ncol = 2, y.intersp = 1.5) %>%
      axis(side = 2, reverse = isInverted) %>%
      grid(col = "lightgrey", lty = 1) %>%
      title(ylab = "Water Level, Below LSD (feet)")

    plot_object <-
      XAxisLabels(plot_object,
                  fiveyrInfo$month_label,
                  fiveyrInfo$month_label_location,
                  fiveyrInfo$date_seq_yr + months(6))
    
    for (i in grep("^appr_.*_uv$", names(fiveyrData), invert = TRUE)) {
      fiveyrPlotConfig <-
        getFiveyearPlotConfig(fiveyrData[i], fiveyrInfo, maxLabel = maxLabel, minLabel = minLabel)
      for (j in seq_len(length(fiveyrPlotConfig))) {
        plot_object <-
          do.call(names(fiveyrPlotConfig[j]), append(list(object = plot_object), fiveyrPlotConfig[[j]]))
      }
    }
    
    # add vertical lines to delineate calendar year boundaries
    plot_object <- DelineateYearBoundaries(plot_object, fiveyrInfo$date_seq_yr)
    
    plot_object <- ApplyApprovalBarStyles(plot_object, fiveyrData)
    
    plot_object <- rmDuplicateLegendItems(plot_object)
    
    # patch up top extent of y-axis
    plot_object <- RescaleYTop(plot_object)

    #Add Min/Max labels if we aren't plotting min and max
    if(!is.null(fiveyrData$max_iv_label) && !is.null(fiveyrData$min_iv_label)){
      #Extract Timezone
      tzf <- format(as.POSIXct(fiveyrData$max_iv_label$time), "%z")
      #Insert ":" before 2nd to last character
      tzf <- sub("([[:digit:]]{2,2})$", ":\\1", tzf) 
      plot_object <- plot_object %>% 
          mtext(paste0(maxLabel, " ", fiveyrInfo$type, ": ", fiveyrData$max_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$max_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                              side = 3, axes=FALSE, cex=0.625, line = 0.85, adj = 0) %>%
          mtext(paste0(minLabel, " ", fiveyrInfo$type, ": ", fiveyrData$min_iv_label$value, " ", data$firstDownChain$units, format(as.POSIXct(fiveyrData$min_iv_label$time), " %b %d, %Y %H:%M:%S"), " (UTC ", tzf, ")"), 
                              side = 3, axes=FALSE, cex=0.625, line = 0.1, adj = 0)
    }
  }
  else {
    plot_object <- NULL
  }

  
  
}

getFiveyearPlotConfig <- function(reportObject, info=NULL, ...) {
  styles <- getFiveyearStyle()
  
  x <- reportObject[[1]]$time
  y <- reportObject[[1]]$value
  legend.name <- reportObject[[1]]$legend.name
  args <- list(...)
  
  styles <- switch(names(reportObject), 
      stat = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$stat_lines)
          ),
      est_stat = list(
          lines = append(list(x=x, y=y, legend.name=legend.name), styles$est_stat_lines)
          ),
      max_iv = list(
          points = append(list(x=x, y=y, legend.name=paste(args$maxLabel, info$type, ":", y)), styles$max_iv_points)
          ),
      min_iv = list(
          points = append(list(x=x, y=y, legend.name=paste(args$minLabel, info$type, ":", y)), styles$min_iv_points)
          ),
      gw_level = list(
          points = append(list(x=x,y=y), styles$gw_level_points)
      )
  )
  
  return(styles)
}
