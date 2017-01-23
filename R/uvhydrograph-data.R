#'@importFrom lubridate parse_date_time

getMonths <- function(reportObject){
  corr <- getTimeSeries(reportObject, "primarySeries")
  uncorr <- getTimeSeries(reportObject, "primarySeriesRaw")
  months <- unique(c(corr$month, uncorr$month))
  return(sort(months))
}

parsePrimaryUVData <- function(data, month) {
  timezone <- fetchReportMetadataField(data, "timezone")
  
  corr_UV <- subsetByMonth(getTimeSeries(data, "primarySeries" ), month)
  est_UV <- subsetByMonth(getTimeSeries(data, "primarySeries", estimatedOnly=TRUE), month)
  uncorr_UV <- subsetByMonth(getTimeSeries(data, "primarySeriesRaw" ), month)
  comp_UV <- subsetByMonth(getTimeSeries(data, "comparisonSeries" ), month)
  water_qual <- tryCatch({
    subsetByMonth(readWaterQualityMeasurements(data), month)
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
  })
  
  series_corr2 <- tryCatch({
      subsetByMonth(readCorrections(data, "primarySeriesCorrections"), month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
    })
  meas_Q <- tryCatch({
    subsetByMonth(readFieldVisitMeasurementsQPoints(data), month) 
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
  })
  
  ref_readings <- subsetByMonth(readReadings(data, "reference"), month)
  csg_readings <- subsetByMonth(readReadings(data, "crestStage"), month)
  hwm_readings <- subsetByMonth(readReadings(data, "waterMark"), month)
  
  #Add reference data to the plot if it is available and this is a Q plot type
  if(any(grepl("Discharge", fetchReportMetadataField(data,'primaryParameter'))))
  {
    #Reference Time Series Data
    corr_UV_Qref <- subsetByMonth(getTimeSeries(data, "referenceSeries"), month)
    est_UV_Qref <- subsetByMonth(getTimeSeries(data, "referenceSeries", estimatedOnly=TRUE), month)
  }
  
  approvals_uv <- readApprovalBar(data[['primarySeries']], timezone, 
                                    legend_nm=paste("UV", getTimeSeriesLabel(data, "primarySeries")))
                                
  approvals_first_stat <- readApprovalPoints(fetchApprovalsForSeries(data, "firstDownChain"), subsetByMonth(getTimeSeries(data, "firstDownChain"), month), 
                                              timezone, legend_nm=fetchReportMetadataField(data, "downChainDescriptions1"),
                                              appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), point_type=21)
                                          
  approvals_second_stat <- readApprovalPoints(fetchApprovalsForSeries(data, "secondDownChain"), subsetByMonth(getTimeSeries(data, "secondDownChain"), month), 
                                              timezone, legend_nm=fetchReportMetadataField(data, "downChainDescriptions2"),
                                              appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), point_type=24)
                                    
  approvals_third_stat <- readApprovalPoints(fetchApprovalsForSeries(data, "thirdDownChain"), subsetByMonth(getTimeSeries(data, "thirdDownChain"), month), 
                                              timezone, legend_nm=fetchReportMetadataField(data, "downChainDescriptions3"),
                                              appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), point_type=25)
                                   
  approvals_fourth_stat <- readApprovalPoints(fetchApprovalsForSeries(data, "fourthDownChain"), subsetByMonth(getTimeSeries(data, "fourthDownChain"), month), 
                                              timezone, legend_nm=fetchReportMetadataField(data, "downChainDescriptions4"),
                                              appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), point_type=22)
  
  
  approvals <- append(approvals_uv, approvals_first_stat)
  approvals <- append(approvals, approvals_second_stat)
  approvals <- append(approvals, approvals_third_stat)
  approvals <- append(approvals, approvals_fourth_stat)
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "month", "approvals", "approvals_uv", 
                                                  "approvals_first_stat", "approvals_second_stat", "approvals_third_stat",
                                                  "approvals_fourth_stat", "timezone"
  ))]
  
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars)
  
  # optionally exclude negative/zero values here
  
  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

parseSecondaryUVData <- function(data, month) {
  timezone <- fetchReportMetadataField(data, "timezone") 
  
  if(any(grepl("referenceSeries", names(data))) && !any(grepl("Discharge", fetchReportMetadataField(data,'primaryParameter')))) {
    #Reference Time Series Data
    corr_UV2 <- subsetByMonth(getTimeSeries(data, "referenceSeries"), month)
    est_UV2 <- subsetByMonth(getTimeSeries(data, "referenceSeries", estimatedOnly=TRUE), month)
    series_corr2 <- tryCatch({
      subsetByMonth(readCorrections(data, "referenceSeriesCorrections"), month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
    })
    approvals <- readApprovalBar(data[["referenceSeries"]], timezone, 
        legend_nm=getTimeSeriesLabel(data, "referenceSeries"))
  } else {
    #Upchain Time Series Data
    corr_UV2 <- subsetByMonth(getTimeSeries(data, "upchainSeries"), month)
    est_U2 <- subsetByMonth(getTimeSeries(data, "upchainSeries", estimatedOnly=TRUE), month)
    uncorr_UV2 <- subsetByMonth(getTimeSeries(data, "upchainSeriesRaw"), month)
    series_corr2 <- tryCatch({
      subsetByMonth(readCorrections(data, "upchainSeriesCorrections"), month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
    })
    approvals <- readApprovalBar(data[['upchainSeries']], timezone, 
        legend_nm=getTimeSeriesLabel(data, "upchainSeries"))
  }
  
  effect_shift <- subsetByMonth(getTimeSeries(data, "effectiveShifts"), month)
  gage_height <- subsetByMonth(readMeanGageHeights(data), month)

  gw_level <- tryCatch({
    subsetByMonth(readGroundWaterLevels(data), month)
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
  })

  meas_shift <- tryCatch({
    subsetByMonth(readFieldVisitMeasurementsShifts(data), month)
  }, error = function(e) {
    na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
  })
  
  allVars <- as.list(environment())
  allVars <- append(approvals, allVars)
  allVars <- allVars[which(!names(allVars) %in% c("data", "month", "approvals", "approvals_uv", 
                                                  "approvals_first_stat", "approvals_second_stat", "approvals_third_stat",
                                                  "approvals_fourth_stat", "timezone"
                                                  ))]
  
  allVars <- allVars[!unlist(lapply(allVars, isEmptyVar),FALSE,FALSE)]
  allVars <- applyDataGaps(data, allVars)
  
  # optionally exclude negative/zero values here
  
  plotData <- rev(allVars) #makes sure approvals are last to plot (need correct ylims)
  return(plotData)
}

#'@importFrom lubridate days_in_month
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@importFrom lubridate ymd
parseSupplementalPrimaryInfo <- function(data, pts) {
  if(!is.null(pts$corr_UV)){
    lims_UV <- getUvhLims(pts$corr_UV)
  } else {
    lims_UV <- getUvhLims(pts$uncorr_UV)
  }

  if(any(grepl("Discharge", fetchReportMetadataField(data,'primaryParameter'))))
  {
    if(!is.null(pts$corr_UV_Qref)){
      lims_UV <- append(lims_UV, getUvhLims(pts$corr_UV_Qref))
    }

    reference_lbl <- getTimeSeriesLabel(data, "referenceSeries")
    ref_units <- data$referenceSeries$units
  }
  
  primary_lbl <- getTimeSeriesLabel(data, "primarySeries")
  primary_type <- data[['primarySeries']]$type
  reference_type <- data[['referenceSeries']]$type
  date_lbl <- paste(lims_UV$xlim[1], "through", lims_UV$xlim[2])
  comp_UV_lbl <- data$reportMetadata$comparisonStationId
  comp_UV_type <- data[['comparisonSeries']]$type
  comp_UV_TS_lbl <- getTimeSeriesLabel(data, "comparisonSeries");
  dates <- seq(lims_UV$xlim[1], lims_UV$xlim[2], by="days")
  
  logAxis <- isLogged(pts, data[["firstDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(data, 'excludeZeroNegative'))
  
  days <- seq(days_in_month(dates[1]))
  year <- year(dates[1])
  month <- month(dates[1])
  plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=data$reportMetadata$timezone)), length=tail(days,1), by="days")
  
  
  primaryInverted <- function(renderName) {
    dataName <- switch(renderName,
      corr_UV = "primarySeries",
      corr_UV_Qref = "referenceSeries",
      est_UV = "primarySeries",
      est_UV_Qref = "referenceSeries",
      uncorr_UV = "primarySeriesRaw",
      comp_UV = "comparisonSeries",  
      water_qual = "primarySeries",  #if primary is flipping, this will flip
      stat_1 = "firstDownChain",
      stat_2 = "secondDownChain",
      stat_3 = "thirdDownChain",
      stat_4 = "fourthDownChain")
  
  
    isInverted <- ifelse(!is.null(dataName), isTimeSeriesInverted(data[[dataName]]), NA)
    return(isInverted)
  }

  #for any one plot, all data must be either inverted or not
  isInverted <- all(na.omit(unlist(lapply(names(pts), primaryInverted))))
  
  allVars <- as.list(environment())
  allVars <- allVars[unlist(lapply(allVars, function(x) {!is.null(x)} ),FALSE,FALSE)]
  allVars <- allVars[unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} ),FALSE,FALSE)]
  supplemental <- allVars[which(!names(allVars) %in% c("data", "plotName", "pts"))]
  
  return(supplemental)
}

#'@importFrom lubridate days_in_month
#'@importFrom lubridate year
#'@importFrom lubridate month
#'@importFrom lubridate ymd
parseSecondarySupplementalInfo <- function(reportObject, pts) {
  lims_UV2 <- getUvhLims(pts$corr_UV2)
  
  if(any(grepl("referenceSeries", names(reportObject))) && !any(grepl("Discharge", fetchReportMetadataField(reportObject,'primaryParameter')))) {
    secondary_lbl <- getTimeSeriesLabel(reportObject, "referenceSeries")
    sec_units <- reportObject$referenceSeries$units
    
  }
  else if(any(grepl("upchainSeries", names(reportObject)))) {
    secondary_lbl <- getTimeSeriesLabel(reportObject, "upchainSeries")
    sec_units <- reportObject$upchainSeries$units
  }
  
  sec_dates <- seq(lims_UV2$xlim[1], lims_UV2$xlim[2], by="days")
  date_lbl2 <- paste(lims_UV2$xlim[1], "through", lims_UV2$xlim[2])
  days <- seq(days_in_month(sec_dates[1]))
  year <- year(sec_dates[1])
  month <- month(sec_dates[1])
  plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=reportObject$reportMetadata$timezone)), length=tail(days,1), by="days")
  tertiary_lbl <- getTimeSeriesLabel(reportObject, "effectiveShifts")
  
  sec_logAxis <- isLogged(pts, reportObject[["secondDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  tertiary_logAxis <- isLogged(pts, reportObject[["thirdDownChain"]][['isVolumetricFlow']], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  
  secondaryInverted <- function(renderName) {
    if(any(grepl("referenceSeries", names(data))) && !any(grepl("Discharge", fetchReportMetadataField(reportObject,'primaryParameter')))) {
      dataName <- switch(renderName,
          corr_UV2 = "referenceSeries",
          est_UV2 = "referenceSeries"
      )
    } else { 
      dataName <- switch(renderName,
          corr_UV2 = "upchainSeries",
          est_UV2 = "upchainSeries",
          uncorr_UV2 = "upchainSeriesRaw"
      )
    }
    isInverted <- ifelse(!is.null(dataName), isTimeSeriesInverted(reportObject[[dataName]]), NA)
    return(isInverted)
  }
  
  #for any one plot, all data must be either inverted or not
  isInverted <- all(na.omit(unlist(lapply(names(pts), secondaryInverted))))
  
  allVars <- as.list(environment())
  allVars <- allVars[unlist(lapply(allVars, function(x) {!is.null(x)} ),FALSE,FALSE)]
  allVars <- allVars[unlist(lapply(allVars, function(x) {nrow(x) != 0 || is.null(nrow(x))} ),FALSE,FALSE)]
  supplemental <- allVars[which(!names(allVars) %in% c("data", "plotName", "pts"))]
  
  return(supplemental)
}

correctionsTable <- function(data) {
  if (any(names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up", "series_corr2"))) {
    corrections <- data[[grep("series_corr", names(data))]]
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), as.character(corrections$time), corrections$comment))
    colnames(corrections_table) <- c("", "Time", "Comments")
    return(corrections_table)
  } else (return(corrections_table <- NULL))
}

addGroupCol <- function(data, newColumnName, isNewCol, newGroupValue=NULL, groupChildValue=NULL, vars=NULL){
  build_vec <- c()
  prev <- NULL
  for(r in 1:nrow(data)){
    if(r == 1 || isNewCol(data, r, vars)){
      if(typeof(newGroupValue) != "closure"){
        newVal <- newGroupValue
      } else {
        newData <- newGroupValue(data, prev, r, build_vec, vars)
        newVal <- newData$value
        vars <- c(vars, newData$vars)
      }
      
      build_vec <- c(build_vec, newVal)
      prev <- newVal
    } else {
      if(typeof(groupChildValue) != "closure"){
        childVal <- groupChildValue
      } else {
        childVal <- groupChildValue(data, build_vec, r, vars)
      }

      build_vec <- c(build_vec, childVal)
    }
  }
  
  data[newColumnName] <- build_vec
  
  return(data)
}

xposGroupValue <- function(data, prev, r, build_vec, vars) {
  colData <- data[which(data['colNum'] == data[r, 'colNum']),]
  # work around warnings from devtools::check()
  time <- ""
  label <- ""
  colData <- colData %>% arrange(desc(time), desc(label))
  shift <- head(colData,1)['time'] + vars$secondOffset + data[r, 'boxWidth'] / 2 > vars$limits$xlim[[2]]

  if(shift){
    colData <- colData %>% arrange(time, desc(label))
  }

  return(c(value=ifelse(shift, head(colData,1)['time'] - vars$secondOffset - data[r, 'boxWidth'] / 2, head(colData,1)['time'] + vars$secondOffset + data[r, 'boxWidth'] / 2),
           vars=list()))
}

yposGroupValue <- function(data, prev, r, build_vec, vars) {
  if(data[r,'xpos'] > data[r,'time']){
    value <- vars$limits$ylim[[2]]
  } else {
    if(r > 1 && abs(data[r,'xpos'] - data[r-1, 'xpos']) < vars$secondOffset + data[r,'boxWidth']){
      value <- build_vec[r-1] - vars$subtractor
    } else {
      value <- vars$limits$ylim[[2]]
    }
  }

  return(c(value=value, vars=list()))
}

#' @importFrom dplyr row_number
#' @importFrom dplyr desc
parseLabelSpacing <- function(data, limits) {
  
  if (names(data) %in% c("series_corr", "series_corr_ref", "series_corr_up", "series_corr2")){
    #Number of seconds to offset labels by for display
    secondOffset <- 4 * 60 * 60

    #Width of one digit in hours
    digitSeconds <- 4 * 60 * 60

    #Total width of both bounding box left and right margins 
    baseBoxSize <- 4 * 60 * 60

    #Minimum space between the right side of a label box and the next correction line to not merge columns
    minSpacerSize <- 2 * 60 * 60

    #The percentage of the y-range to subtract each time we add a new label to a column
    subtractor <- (limits$ylim[[2]] - limits$ylim[[1]]) * 0.065

    # work around warnings from devtools::check()
    time <- ""
    label <- ""
    
    #Save original order as label and re-order by time and then by label (descending)
    corrs <- data[[1]] %>% select(time) %>% mutate(label = row_number()) %>% arrange(time, desc(label))
    
    #Calculate the largest width label for the current time
    corrs <- addGroupCol(corrs, 'boxWidth',  isNewCol = function(data, r, vars){data[r-1, 'time'] != data[r, 'time']}, 
                                             newGroupValue=function(data, prev, r, build_vec, vars){c(value=vars$baseBoxSize + vars$digitSeconds * nchar(as.character(data[r, 'label'])), vars=list())},
                                             vars = list(baseBoxSize=baseBoxSize,digitSeconds=digitSeconds),
                                             groupChildValue=function(data,build_vec,r,vars){build_vec[r-1]})

    #Calculate the column number of each row by looking for column breaks
    corrs <- addGroupCol(corrs, 'colNum', isNewCol = function(data, r, vars){difftime(data[r, 'time'], data[r-1, 'time'], units="secs") >= vars$secondOffset + data[r-1, 'boxWidth'] + vars$minSpacerSize}, 
                                          newGroupValue = function(data, prev, r, build_vec, vars){c(value=ifelse(isEmptyOrBlank(prev), 1, prev + 1), vars=list())},
                                          vars = list(secondOffset=secondOffset, minSpacerSize=minSpacerSize),
                                          groupChildValue=function(data,build_vec,r,vars){build_vec[r-1]})
        
    #Calculate the x-position of new columns
    corrs <- addGroupCol(corrs, 'xpos', isNewCol = function(data, r, vars){data[r-1, 'colNum'] != data[r, 'colNum']}, 
                                        newGroupValue=xposGroupValue,
                                        vars=list(secondOffset=secondOffset, limits=limits),
                                        groupChildValue=function(data,build_vec,r,vars){build_vec[r-1]})

    #Calculate the y-position of each label in each column
    corrs <- addGroupCol(corrs, 'ypos', isNewCol = function(data, r, vars){data[r-1, 'colNum'] != data[r, 'colNum']}, 
                                        newGroupValue=yposGroupValue,
                                        groupChildValue=function(data,build_vec,r,vars){build_vec[r-1] - vars$subtractor},
                                        vars=list(subtractor=subtractor, limits=limits, secondOffset=secondOffset))

    ##The scaling factor for the bounding shape of this label in inches. Scaling factor is fairly arbitrary but is relative the cex value used for the text for these labels in the styles and the colWidth
    corrs <- corrs %>% mutate(r = 1+0.525*nchar(as.character(label)))
      
    spacingInfo <- list(x=corrs$xpos, xorigin=corrs$time, y=corrs$ypos, r=corrs$r, label=corrs$label)
  } else {
    spacingInfo <- list()
  }
  
  return(spacingInfo)
}

getUvhLims <- function(pts = NULL, xMinField = 'time', xMaxField = 'time', yMinField = 'value', yMaxField = 'value'){
  x_mx <- max(pts[[xMaxField]], na.rm = TRUE)
  x_mn <- min(pts[[xMinField]], na.rm = TRUE)
  y_mx <- max(pts[[yMaxField]], na.rm = TRUE)
  y_mn <- min(pts[[yMinField]], na.rm = TRUE)
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in points. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))
}



#' TODO
extendYaxisLimits <- function(gsplot, error_bar_args){
  side <- ifelse(!is.null(error_bar_args$side), error_bar_args$side, 2)
  side_nm <- paste0('side.', side)
  
  lowest_error_bar <- min(error_bar_args$y - error_bar_args$y.low)
  lowest_y <- min(ylim(gsplot, side=side)[1], lowest_error_bar)
  
  highest_error_bar <- max(error_bar_args$y + error_bar_args$y.high)
  highest_y <- max(ylim(gsplot, side=side)[2], highest_error_bar)
  
  gsplot[[side_nm]][['lim']] <- c(lowest_y, highest_y)
  return(gsplot)
}
