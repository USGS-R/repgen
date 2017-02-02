#' Get months
#' @description For a report, return a list of all months with data (sorted) in format 'YYMM'.
#' @param reportObject a UV Hydrograph report
#' @param timezone the timezone to parse read time series into
#' @return list of months, sorted, as 'YYMM'
#'@importFrom lubridate parse_date_time
getMonths <- function(reportObject, timezone){
  corr <- readTimeSeries(reportObject, "primarySeries", timezone)[['points']]
  uncorr <- readTimeSeries(reportObject, "primarySeriesRaw", timezone)[['points']]
  months <- unique(c(corr[['month']], uncorr[['month']]))
  return(sort(months))
}

readCorrectionsByMonth <- function(reportObject, fieldName, month) {
  corrections <- tryCatch({
       subsetByMonth(readCorrections(reportObject, fieldName), month)
     }, error = function(e) {
       na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
     })
  return(corrections)
}

readUvComparisonSeriesByMonth <- function(reportObject, month) {
  comparison <- tryCatch({
        subsetByMonth(readTimeSeries(reportObject, "comparisonSeries", timezone)[['points']], month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), stringsAsFactors=FALSE))
      })
  return(comparison)
}

#' TODO
readPrimaryUvHydroApprovalBars <- function(reportObject, timezone, month) {
  approvals <- readApprovalBar(readTimeSeries(reportObject, "primarySeries", timezone), timezone, 
      legend_nm=paste("UV", getTimeSeriesLabel(reportObject, "primarySeries")))
  return(approvals)
}

readUvNonEstimatedSeries <- function(reportObject, month, timezone) {
  series <- tryCatch({
        subsetByMonth(readNonEstimatedTimeSeries(reportObject, "comparisonSeries", timezone)[['points']], month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), stringsAsFactors=FALSE))
      })
  return(series)
}

readUvEstimatedSeries <- function(reportObject, month, timezone) {
  series <- tryCatch({
        subsetByMonth(readEstimatedTimeSeries(reportObject, "comparisonSeries", timezone)[['points']], month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), stringsAsFactors=FALSE))
      })
  return(series)
}

getPrimarySeriesList <- function(reportObject, month, timezone) {
  correctedSeries <- readNonEstimatedTimeSeries(reportObject, "primarySeries", timezone)
  estimatedSeries <- readEstimatedTimeSeries(reportObject, "primarySeries", timezone)
  uncorrectedSeries <- readTimeSeries(reportObject, "primarySeriesRaw", timezone)
  
  inverted <- isTimeSeriesInverted(correctedSeries)
  loggedAxis <- isLogged(correctedSeries[['points']], reportObject[["[primarySeries"]][['isVolumetricFlow']], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  
  corrected <- subsetByMonth(correctedSeries[['points']], month)
  estimated <- subsetByMonth(estimatedSeries[['points']], month)
  uncorrected <- subsetByMonth(uncorrectedSeries[['points']], month)
  
  #Add reference data to the plot if it is available and this is a Q plot type
  corrected_reference <- NULL
  estimated_reference <- NULL
  if(isPrimaryDischarge(reportObject))
  {
    #Reference Time Series Data
    corrected_reference <- readUvNonEstimatedSeries(reportObject, "referenceSeries", month)
    estimated_reference <- readUvEstimatedSeries(reportObject, "referenceSeries", month)
  }
  
  comparison <- readUvComparisonSeriesByMonth(reportObject, month)
  
  return(list(
          corrected=corrected, 
          estimated=estimated, 
          uncorrected=uncorrected, 
          corrected_reference=corrected_reference,
          estimated_reference=estimated_reference,
          comparison=comparison,
          inverted=inverted,
          loggedAxis=loggedAxis))
}

getPrimaryDvList <- function(reportObject, month, timezone) {
  paramPrefixes <- c("approved_dv", "inreview_dv", "working_dv")
  
  if(!isEmptyOrBlank(reportObject[["firstDownChain"]])) {
    first_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "firstDownChain"), 
        subsetByMonth(readTimeSeries(reportObject, "firstDownChain", timezone, shiftTimeToNoon=TRUE)[['points']], month), 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions1"),
          appr_var_all=paramPrefixes, point_type=21)
  } else {
    first_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["secondDownChain"]])) {
    second_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "secondDownChain"), 
        subsetByMonth(readTimeSeries(reportObject, "secondDownChain", timezone, shiftTimeToNoon=TRUE)[['points']], month), 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions2"),
          appr_var_all=paramPrefixes, point_type=24)
  } else {
    second_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["thirdDownChain"]])) {
    third_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "thirdDownChain"), 
        subsetByMonth(readTimeSeries(reportObject, "thirdDownChain", timezone, shiftTimeToNoon=TRUE)[['points']], month), 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions3"),
          appr_var_all=paramPrefixes, point_type=25)
  } else {
    third_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["fourthDownChain"]])) {
    fourth_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "fourthDownChain"), 
        subsetByMonth(readTimeSeries(reportObject, "fourthDownChain", timezone, shiftTimeToNoon=TRUE)[['points']], month), 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions4"),
          appr_var_all=paramPrefixes, point_type=22)
  } else {
    fourth_stat <- list()
  }
  
  all <- append(first_stat, second_stat)
  all <- append(all, third_stat)
  all <- append(all, fourth_stat)
  
  return(all)
}

readAllUvReadings <- function(reportObject, month) {
  ref_readings <- subsetByMonth(readReadings(reportObject, "reference"), month)
  csg_readings <- subsetByMonth(readReadings(reportObject, "crestStage"), month)
  hwm_readings <- subsetByMonth(readReadings(reportObject, "waterMark"), month)
  
  return(list(reference=ref_readings, crest_stage_gage=csg_readings, high_water_mark=hwm_readings))
}

readUvWq <- function(reportObject, month) {
  water_qual <- tryCatch({
        subsetByMonth(readWaterQualityMeasurements(reportObject), month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
      })
  return(water_qual)
}

readUvQMeasurements <- function(reportObject, month) {
  meas_Q <- tryCatch({
        subsetByMonth(readFieldVisitMeasurementsQPoints(reportObject), month) 
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
      })
  return(meas_Q)
}

#' Depending on conditions, might be ref series or upchain series
getSecondarySeriesList <- function(reportObject, month, timezone) {
  if(hasReferenceSeries(reportObject) && !isPrimaryDischarge(reportObject)) {
    #Reference Time Series Data
    correctedSeries <- readNonEstimatedTimeSeries(reportObject, "referenceSeries", timezone)
    estimatedSeries <- readEstimatedTimeSeries(reportObject, "referenceSeries", timezone)
    uncorrectedSeries <- NULL
  } else {
    #Upchain Time Series Data
    correctedSeries <- readNonEstimatedTimeSeries(reportObject, "upchainSeries", timezone)
    estimatedSeries <- readEstimatedTimeSeries(reportObject, "upchainSeries", timezone)
    uncorrectedSeries <- readTimeSeries(reportObject, "upchainSeriesRaw", timezone)
  }
  
  inverted = isTimeSeriesInverted(correctedSeries)
  corrected <- subsetByMonth(correctedSeries[['points']], month)
  estimated <- subsetByMonth(estimatedSeries[['points']], month)
  if(!is.null(uncorrectedSeries)) {
    uncorrected <- subsetByMonth(uncorrectedSeries[['points']], month)
  } else {
    uncorrected <- NULL
  }
  
  return(list(corrected=corrected, estimated=estimated, uncorrected=uncorrected, inverted=inverted))
}

readEffectiveShifts <- function(reportObject, timezone, month) {
  effect_shift <- tryCatch({
      subsetByMonth(
        readTimeSeries(reportObject, "effectiveShifts", timezone,requiredFields=c("points"))[['points']], 
        month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
    })
  return(effect_shift)
}

readUvGwLevel <- function(reportObject, month) {
  gw_level <- tryCatch({
        subsetByMonth(readGroundWaterLevels(reportObject), month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
      })
  return(gw_level)
}

readUvMeasurementShifts <- function(reportObject, month) {
  meas_shift <- tryCatch({
      subsetByMonth(readFieldVisitMeasurementsShifts(reportObject), month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
    })
  return(meas_shift)
}

readUvGageHeight <- function(reportObject, month) {
  gage_height <- subsetByMonth(readMeanGageHeights(reportObject), month)
  return(gage_height)
}

#' TODO
readSecondaryUvHydroApprovalBars <- function(reportObject, timezone, month) {
  if(hasReferenceSeries(reportObject) && !isPrimaryDischarge(reportObject)) {
    #Reference Time Series Data
    approvals <- readApprovalBar(readTimeSeries(reportObject, "referenceSeries", timezone), timezone, 
        legend_nm=getTimeSeriesLabel(reportObject, "referenceSeries"))
  } else if(hasUpchainSeries(reportObject)) {
    #Upchain Time Series Data
    approvals <- readApprovalBar(readTimeSeries(reportObject, "upchainSeries", timezone), timezone, 
        legend_nm=getTimeSeriesLabel(reportObject, "upchainSeries"))
  } else {
    approvals <- list()
  }
  return(approvals)
}

#' Is Primary Discharge
#' Determines if the primary series in the report is Discharge parameter
#' @param reportObject UV Hydro report object
#' @return true/false
isPrimaryDischarge <- function(reportObject) {
  return(any(grepl("Discharge", fetchReportMetadataField(reportObject,'primaryParameter'))))
}

#' Has Refernce Series
#' Determines if the report has a reference series attached
#' @param reportObject UV Hydro report object
#' @return true/false
hasReferenceSeries <- function(reportObject) {
  return(any(grepl("referenceSeries", names(reportObject))))
}

#' Has Upchain Series
#' Determines if the report has a upchain series attached
#' @param reportObject UV Hydro report object
#' @return true/false
hasUpchainSeries <- function(reportObject) {
  return(any(grepl("upchainSeries", names(reportObject))))
}

#' Calculate Primary Lims
#' @description Will provide the lims of the primary plot given relevant data
#' @param primarySeriesList list of UV series that may appear on plot
#' @param isDischarge whether or not the primary series is discharge
#' @return the lims object that to be applied to the primary plot
calculatePrimaryLims <- function(primarySeriesList, isDischarge) {
  if(!is.null(primarySeriesList[['corrected']])){
    lims <- calculateLims(primarySeriesList[['corrected']])
  } else {
    lims <- calculateLims(primarySeriesList[['uncorrected']])
  }
  
  if(isDischarge) {
    if(!is.null(primarySeriesList[['corrected_reference']])){
      lims <- append(lims, calculateLims(primarySeriesList[['corrected_reference']]))
    }
  }
  
  return(lims)
}

getUvTimeInformationFromLims <- function(lims, timezone) {
  sec_dates <- seq(lims[['xlim']][1], lims[['xlim']][2], by="days")
  days <- seq(days_in_month(sec_dates[1]))
  year <- year(sec_dates[1])
  month <- month(sec_dates[1])
  plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=timezone)), length=tail(days,1), by="days")
  
  start <- plotDates[1]
  end <- tail(plotDates,1) + hours(23) + minutes(45)
  
  return(list(dates=plotDates, days=days, start=start, end=end))
}

getTimeSeriesUvInfo <- function(reportObject, seriesName) {
  label <- getTimeSeriesLabel(reportObject, seriesName)
  units <- reportObject[[seriesName]][['units']]
  type <- reportObject[[seriesName]][['type']]
  return(list(label=label, units=units))
}

getSecondaryTimeSeriesUvInfo <- function(reportObject, timezone, month) {
  if(hasReferenceSeries(reportObject) && !isPrimaryDischarge(reportObject)) {
    infos <- getTimeSeriesUvInfo(reportObject, "referenceSeries")
  } else if(hasUpchainSeries(reportObject)) {
    infos <- getTimeSeriesUvInfo(reportObject, "upchainSeries")
  }
  return(infos)
}

#' Corrections as table
#' @description Given a list of corrections, will create a table structure with unique entries for all corrections that appear in the list.
#' @param corrections list of corrections
#' @return table structure with unique row entries for each correction type found
correctionsAsTable <- function(corrections) {
  if(!is.null(corrections) && nrow(corrections) > 0) {
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), as.character(corrections[['time']]), corrections[['comment']]))
    colnames(corrections_table) <- c("", "Time", "Comments")
    return(corrections_table)
  } else {
    return(corrections_table <- NULL)
  }
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
        newVal <- newData[['value']]
        vars <- c(vars, newData[['vars']])
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
  shift <- head(colData,1)['time'] + vars[['secondOffset']] + data[r, 'boxWidth'] / 2 > vars[['limits']][['xlim']][[2]]

  if(shift){
    colData <- colData %>% arrange(time, desc(label))
  }

  value <- ifelse(shift, head(colData,1)['time'] - vars[['secondOffset']] - data[r, 'boxWidth'] / 2, head(colData,1)['time'] + vars[['secondOffset']] + data[r, 'boxWidth'] / 2)
  
  value <- unname(unlist(value))
  
  return(c(value=value, vars=list()))
}

#' TODO
yposGroupValue <- function(data, prev, r, build_vec, vars) {
  if(data[r,'xpos'] > data[r,'time']){
    value <- vars[['limits']][['ylim']][[2]]
  } else {
    if(r > 1 && abs(data[r,'xpos'] - data[r-1, 'xpos']) < vars[['secondOffset']] + data[r,'boxWidth']){
      value <- build_vec[r-1] - vars[['subtractor']]
    } else {
      value <- vars[['limits']][['ylim']][[2]]
    }
  }

  return(c(value=value, vars=list()))
}

#' TODO
#' @importFrom dplyr row_number
#' @importFrom dplyr desc
parseCorrectionsLabelSpacing <- function(corrections, limits) {
  #Number of seconds to offset labels by for display
  secondOffset <- 4 * 60 * 60

  #Width of one digit in hours
  digitSeconds <- 4 * 60 * 60

  #Total width of both bounding box left and right margins 
  baseBoxSize <- 4 * 60 * 60

  #Minimum space between the right side of a label box and the next correction line to not merge columns
  minSpacerSize <- 2 * 60 * 60

  #The percentage of the y-range to subtract each time we add a new label to a column
  subtractor <- (limits[['ylim']][[2]] - limits[['ylim']][[1]]) * 0.065

  # work around warnings from devtools::check()
  time <- ""
  label <- ""
  
  #Save original order as label and re-order by time and then by label (descending)
  corrs <- corrections %>% select(time) %>% mutate(label = row_number()) %>% arrange(time, desc(label))
  
  #Calculate the largest width label for the current time
  corrs <- addGroupCol(corrs, 'boxWidth',  isNewCol = function(data, r, vars){data[r-1, 'time'] != data[r, 'time']}, 
                                           newGroupValue=function(data, prev, r, build_vec, vars){c(value=vars[['baseBoxSize']] + vars[['digitSeconds']] * nchar(as.character(data[r, 'label'])), vars=list())},
                                           vars = list(baseBoxSize=baseBoxSize,digitSeconds=digitSeconds),
                                           groupChildValue=function(data,build_vec,r,vars){build_vec[r-1]})

  #Calculate the column number of each row by looking for column breaks
  corrs <- addGroupCol(corrs, 'colNum', isNewCol = function(data, r, vars){difftime(data[r, 'time'], data[r-1, 'time'], units="secs") >= vars[['secondOffset']] + data[r-1, 'boxWidth'] + vars[['minSpacerSize']]}, 
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
                                      groupChildValue=function(data,build_vec,r,vars){build_vec[r-1] - vars[['subtractor']]},
                                      vars=list(subtractor=subtractor, limits=limits, secondOffset=secondOffset))

  ##The scaling factor for the bounding shape of this label in inches. Scaling factor is fairly arbitrary but is relative the cex value used for the text for these labels in the styles and the colWidth
  corrs <- corrs %>% mutate(r = 1+0.525*nchar(as.character(label)))
    
  spacingInfo <- list(x=corrs[['xpos']], xorigin=corrs[['time']], y=corrs[['ypos']], r=corrs[['r']], label=corrs[['label']])
  
  return(spacingInfo)
}
