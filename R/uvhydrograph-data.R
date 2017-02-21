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

#' Parse Corrections
#' @description Read corrections for a given series
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return corrections subset by month
parseCorrectionsByMonth <- function(reportObject, seriesName, month) {
  corrections <- tryCatch({
       subsetByMonth(readCorrections(reportObject, seriesName), month)
     }, error = function(e) {
       na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
     })
  return(corrections)
}

#' Parse Secondary Corrections
#' @description depending on the report configuration, corrections might come from a reference or upchain series
#' @param reportObject the report to render
#' @return corrections list from the correct series
parseSecondaryCorrectionsByMonth <- function(reportObject, month) {
  hasReferenceSeries <- hasReferenceSeries(reportObject)
  hasUpchainSeries <- hasUpchainSeries(reportObject)
  
  if(hasReferenceSeries) {
    corrections <- parseCorrectionsByMonth(reportObject, "referenceSeriesCorrections", month)
  } else {
    corrections <- parseCorrectionsByMonth(reportObject, "upchainSeriesCorrections", month)
  }
  return(corrections)
}

#' Parse UV Comparison Series
#' @description Read entire comparison series
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return comparison series points subset by month
parseUvComparisonSeriesByMonth <- function(reportObject, month, timezone) {
  comparison <- tryCatch({
        readTimeSeries(reportObject, "comparisonSeries", timezone, onlyMonth=month)
      }, error = function(e) {
        NULL
      })
  return(comparison)
}

#' Parse UV Non-Estimated Series
#' @description Read non-estimated portion of a series
#' @param reportObject entire UV Hydro report object
#' @param seriesName series to read
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return series points subset by month
parseUvNonEstimatedSeries <- function(reportObject, seriesName, month, timezone) {
  series <- tryCatch({
        readNonEstimatedTimeSeries(reportObject, seriesName, timezone, onlyMonth=month)
      }, error = function(e) {
        NULL
      })
  return(series)
}

#' Parse UV Estimated Series
#' @description Read estimated portion of a series
#' @param reportObject entire UV Hydro report object
#' @param seriesName series to read
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return series points subste by month
parseUvEstimatedSeries <- function(reportObject, seriesName, month, timezone) {
  series <- tryCatch({
        readEstimatedTimeSeries(reportObject, seriesName, timezone, onlyMonth=month)
      }, error = function(e) {
        NULL
      })
  return(series)
}

#' Parse Primary Series List
#' @description Read all series including reference or comparison series
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return named list of timeseries objects (NULL if not in report object) as well as inverted and loggedAxis flags. loggedAxis is set so that all series are supported on the same axis.
parsePrimarySeriesList <- function(reportObject, month, timezone) {
  correctedSeries <- readNonEstimatedTimeSeries(reportObject, "primarySeries", timezone, onlyMonth=month)
  estimatedSeries <- readEstimatedTimeSeries(reportObject, "primarySeries", timezone, onlyMonth=month)
  uncorrectedSeries <- readTimeSeries(reportObject, "primarySeriesRaw", timezone, onlyMonth=month)
  
  inverted <- isTimeSeriesInverted(correctedSeries)
  
  loggedAxis <- isLogged(correctedSeries[['points']], correctedSeries[["isVolumetricFlow"]], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  
  #Add reference data to the plot if it is available and this is a Q plot type
  corrected_reference <- NULL
  estimated_reference <- NULL
  if(isPrimaryDischarge(reportObject))
  {
    #Reference Time Series Data
    corrected_reference <- parseUvNonEstimatedSeries(reportObject, "referenceSeries", month, timezone)
    if(!isEmptyOrBlank(estimated_reference)) {
      loggedAxis <- loggedAxis && isLogged(corrected_reference[['points']], corrected_reference[["isVolumetricFlow"]], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
    }
    
    estimated_reference <- parseUvEstimatedSeries(reportObject, "referenceSeries", month, timezone)
    if(!isEmptyOrBlank(estimated_reference)) {
      loggedAxis <- loggedAxis && isLogged(estimated_reference[['points']], estimated_reference[["isVolumetricFlow"]], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
    }
  }
  
  comparison <- parseUvComparisonSeriesByMonth(reportObject, month, timezone)
  if(!isEmptyOrBlank(comparison)) {
    loggedAxis <- loggedAxis && isLogged(comparison[['points']], comparison[["isVolumetricFlow"]], fetchReportMetadataField(reportObject, 'excludeZeroNegative'))
  }
  
  return(list(
          corrected=correctedSeries, 
          estimated=estimatedSeries, 
          uncorrected=uncorrectedSeries, 
          corrected_reference=corrected_reference,
          estimated_reference=estimated_reference,
          comparison=comparison,
          inverted=inverted,
          loggedAxis=loggedAxis))
}

#' Parse Secondary Series List
#' @description Depending on conditions, might be ref series or upchain series
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return named list of series to be included on secondary plot
parseSecondarySeriesList <- function(reportObject, month, timezone) {
  if(hasReferenceSeries(reportObject) && !isPrimaryDischarge(reportObject)) {
    #Reference Time Series Data
    correctedSeries <- readNonEstimatedTimeSeries(reportObject, "referenceSeries", timezone, onlyMonth=month)
    estimatedSeries <- readEstimatedTimeSeries(reportObject, "referenceSeries", timezone, onlyMonth=month)
    uncorrectedSeries <- NULL
  } else {
    #Upchain Time Series Data
    correctedSeries <- readNonEstimatedTimeSeries(reportObject, "upchainSeries", timezone, onlyMonth=month)
    estimatedSeries <- readEstimatedTimeSeries(reportObject, "upchainSeries", timezone, onlyMonth=month)
    uncorrectedSeries <- readTimeSeries(reportObject, "upchainSeriesRaw", timezone, onlyMonth=month)
  }
  
  inverted = isTimeSeriesInverted(correctedSeries)
  
  return(list(corrected=correctedSeries, estimated=estimatedSeries, uncorrected=uncorrectedSeries, inverted=inverted))
}

#' Parse primary dv lists
#' @description Read daily values subsetted by month and named by approval level for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @param timezone timezone to parse all data into
#' @return subset list of DV points. Each point is named with or "approved_dv", "inreview_dv", "working_dv"
parsePrimaryDvList <- function(reportObject, month, timezone) {
  paramPrefixes <- c("approved_dv", "inreview_dv", "working_dv")
  all <- list()
  
  if(!isEmptyOrBlank(reportObject[["firstDownChain"]])) {
    first_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "firstDownChain"), 
        readTimeSeries(reportObject, "firstDownChain", timezone, shiftTimeToNoon=TRUE, onlyMonth=month)[['points']], 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions1"),
          appr_var_all=paramPrefixes, point_type=21)
  } else {
    first_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["secondDownChain"]])) {
    second_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "secondDownChain"), 
        readTimeSeries(reportObject, "secondDownChain", timezone, shiftTimeToNoon=TRUE, onlyMonth=month)[['points']], 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions2"),
          appr_var_all=paramPrefixes, point_type=24)
  } else {
    second_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["thirdDownChain"]])) {
    third_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "thirdDownChain"), 
        readTimeSeries(reportObject, "thirdDownChain", timezone, shiftTimeToNoon=TRUE, onlyMonth=month)[['points']], 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions3"),
          appr_var_all=paramPrefixes, point_type=25)
  } else {
    third_stat <- list()
  }
  
  if(!isEmptyOrBlank(reportObject[["fourthDownChain"]])) {
    fourth_stat <- readApprovalPoints(
        fetchApprovalsForSeries(reportObject, "fourthDownChain"), 
        readTimeSeries(reportObject, "fourthDownChain", timezone, shiftTimeToNoon=TRUE, onlyMonth=month)[['points']], 
          timezone, legend_nm=fetchReportMetadataField(reportObject, "downChainDescriptions4"),
          appr_var_all=paramPrefixes, point_type=22)
  } else {
    fourth_stat <- list()
  }
  
  statList <- list(first_stat, second_stat, third_stat, fourth_stat)
  for(level in paramPrefixes) {
    for(stat in statList) {
      if(!isEmptyOrBlank(stat[[level]]) && nrow(stat[[level]]) > 0) {
        if(isEmptyOrBlank(all[[level]])) {
          all[[level]] <- stat[[level]]
        } else {
          all[[level]] <- rbind(all[[level]], stat[[level]])
        }
      }
    }
  }
  
  return(all)
}

#' Read Primary Uv Hydro Approval Bars
#' @description will read the relevant approval bar from primary series
#' @param reportObject entire UV Hydro report object
#' @param timezone timezone to parse all data into
#' @return approval bar plotting info for primary series
readPrimaryUvHydroApprovalBars <- function(reportObject, timezone, month) {
  approvals <- readApprovalBar(readTimeSeries(reportObject, "primarySeries", timezone, onlyMonth=month), timezone, 
      legend_nm=paste("UV", getTimeSeriesLabel(reportObject, "primarySeries")))
  return(approvals)
}

#' Read Secondary Uv Hydro Approval Bars
#' @description will read the relevant approval bar to display depending on report configuration
#' @param reportObject entire UV Hydro report object
#' @param timezone timezone to parse all data into
#' @return approval bar plotting info from either reference or upchain series
readSecondaryUvHydroApprovalBars <- function(reportObject, timezone) {
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

#' Read Readings
#' @description Read readings subsetted by month and separated by type for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return named list of readings by type and subsetted by month
readAllUvReadings <- function(reportObject, month) {
  ref_readings <- subsetByMonth(readReadings(reportObject, "reference"), month)
  csg_readings <- subsetByMonth(readReadings(reportObject, "crestStage"), month)
  hwm_readings <- subsetByMonth(readReadings(reportObject, "waterMark"), month)
  
  return(list(reference=ref_readings, crest_stage_gage=csg_readings, high_water_mark=hwm_readings))
}

#' Read Water Quality Measurements
#' @description Read WQ measurements subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of WQ data, default to empty frame if none found
readUvWq <- function(reportObject, month) {
  water_qual <- tryCatch({
        subsetByMonth(readWaterQualityMeasurements(reportObject), month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
      })
  return(water_qual)
}

#' Read Discharge Measurements
#' @description Read Q measurements subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of Q data, default to empty frame if none found
readUvQMeasurements <- function(reportObject, month) {
  meas_Q <- tryCatch({
        subsetByMonth(readFieldVisitMeasurementsQPoints(reportObject), month) 
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
      })
  return(meas_Q)
}

#' Read Ground Water level
#' @description Read gw level subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of gw level data, default to empty frame if none found
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

#' Read Ground Water level
#' @description Read gw level subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of gw level data, default to empty frame if none found
readUvGwLevel <- function(reportObject, month) {
  gw_level <- tryCatch({
        subsetByMonth(readGroundWaterLevels(reportObject), month)
      }, error = function(e) {
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA)))
      })
  return(gw_level)
}

#' Read Measured Shifts
#' @description Read measured shifts subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of measured shifts data
readUvMeasurementShifts <- function(reportObject, month) {
  meas_shift <- tryCatch({
      subsetByMonth(readFieldVisitMeasurementsShifts(reportObject), month)
    }, error = function(e) {
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
    })
  return(meas_shift)
}

#' Read Gage Height
#' @description Read gage height subsetted by month for UV Hydrograph
#' @param reportObject entire UV Hydro report object
#' @param month subset only into this month
#' @return subset of gage height data
readUvGageHeight <- function(reportObject, month) {
  gage_height <- subsetByMonth(readMeanGageHeights(reportObject), month)
  return(gage_height)
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
#' @description Will provide the lims of the primary plot given relevant data, should cover all series
#' @param primarySeriesList list of UV series that may appear on plot
#' @param isDischarge whether or not the primary series is discharge
#' @return the lims object that to be applied to the primary plot
calculatePrimaryLims <- function(primarySeriesList, isDischarge) {
  if(!is.null(primarySeriesList[['corrected']])){
    lims <- calculateLims(primarySeriesList[['corrected']][['points']])
  } else {
    lims <- calculateLims(primarySeriesList[['uncorrected']][['points']])
  }
  
  if(isDischarge) {
    if(!is.null(primarySeriesList[['corrected_reference']])){
      lims <- append(lims, calculateLims(primarySeriesList[['corrected_reference']][['points']]))
    }
  }
  
  return(lims)
}

#' Get UV time information from lims
#' Returns time information for the lims
#' @param lims the lims to describe
#' @param timezone the timezone to convert all data to
#' @return named list of metadata about the lims (dates, days, start, end)
#' @importFrom lubridate hours
#' @importFrom lubridate minutes
parseUvTimeInformationFromLims <- function(lims, timezone) {
  sec_dates <- seq(lims[['xlim']][1], lims[['xlim']][2], by="days")
  days <- seq(days_in_month(sec_dates[1]))
  year <- year(sec_dates[1])
  month <- month(sec_dates[1])
  plotDates <- seq(as.POSIXct(ymd(paste(year, month, days[1], sep="-"),tz=timezone)), length=tail(days,1), by="days")
  
  start <- plotDates[1]
  end <- tail(plotDates,1) + hours(23) + minutes(45)
  
  return(list(dates=plotDates, days=days, start=start, end=end))
}

#' Read Time Series UV Info
#' @description Returns metadata for a TS.
#' @param reportObject full UV Hydro object
#' @param seriesName name of ts field to get info from
#' @return named list of metadata (label, units, type)
readTimeSeriesUvInfo <- function(reportObject, seriesName) {
  label <- getTimeSeriesLabel(reportObject, seriesName)
  units <- reportObject[[seriesName]][['units']]
  type <- reportObject[[seriesName]][['type']]
  return(list(label=label, units=units, type=type))
}

#' Read Secondary Time Series UV INFO
#' Decides where or not the report wants reference or upchain series as it's main secondary series and returns metadata for that.
#' @param reportObject full UV Hydro object
#' @return timeseries metadata see readTimeSeriesUvInfo
readSecondaryTimeSeriesUvInfo <- function(reportObject) {
  if(hasReferenceSeries(reportObject) && !isPrimaryDischarge(reportObject)) {
    infos <- readTimeSeriesUvInfo(reportObject, "referenceSeries")
  } else if(hasUpchainSeries(reportObject)) {
    infos <- readTimeSeriesUvInfo(reportObject, "upchainSeries")
  }
  return(infos)
}

#' Corrections as table
#' @description Given a list of corrections, will create a table structure with unique entries for all corrections that appear in the list.
#' @param corrections list of corrections
#' @return table structure with unique row entries for each correction type found
parseCorrectionsAsTable <- function(corrections) {
  if(!is.null(corrections) && nrow(corrections) > 0) {
    corrections_table <- as.data.frame(cbind(seq(nrow(corrections)), as.character(corrections[['time']]), corrections[['comment']]), stringsAsFactors=FALSE)
    colnames(corrections_table) <- c("", "Time", "Comments")
    return(corrections_table)
  } else {
    return(corrections_table <- NULL)
  }
}


#' Add Group Col for Corrections
#' @description helper function to create table of information for placing corrections labels
#' @param data existing table data (for corrections)
#' @param newColumnName name of column to place new data derived
#' @param isNewCol function to determine if new column should be created for derived data
#' @param newGroupValue function for deriving new data from existing data
#' @param groupChildValue function to ??? TODO
#' @param vars variables to pass into newGroupValue and groupChildValue
#' @return mutated data table (with added columns/rows)
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

#' X Position of Group Value
#' @description helper function to figure out the x value (time) of where to place group value ???
#' TODO 
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

#' Y position of Group Value
#' @description helper function to figure out the y value of where to place group value ???
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

#' Parse corrections label spacing
#' @description each correction is a time/comment pair. This will deterimin how to place labels so they do not overlap each other.
#' @param correction list of corrections (time/comment pairs)
#' @param limits the lims that the correction labels should not leave
#' @return list of named items (x, xorigin, y, r, label) which desribes where/how to to place each label
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

#' Get Correction Arrows
#' For a set of correction labels, will return a list of arrows to connect label to correction line.
#' @param correcionLabels list of labels with positioning information already calculated/attached
#' @return list of data describing how to draw lines to corresponding labels
getCorrectionArrows <- function(correctionLabels) {
  corrArrows <- list()
  
  #Make the correction label lines connect to the outside of the bounding box and not to the center of the label
  if(!isEmptyOrBlank(correctionLabels)){
    lengthAdjustments <- 60 * 60 * 2.85 * correctionLabels[['r']]
    
    corrArrows <- correctionLabels %>% as.data.frame() %>% select(x, xorigin, y) %>%
        mutate(x = ifelse(x > xorigin, x - lengthAdjustments, x + lengthAdjustments)) %>% 
        as.list()
  }
  
  return(corrArrows)
}

#' Get positions for Corrections
#' Given a list of corrections, will return a list of times (with duplicates removed) which are the x position of vertical lines
#' @param corrections a list of corrections
#' @return list of time/x for each correction 
getCorrectionPositions <- function(corrections) {
  corrAblinePositions <- list()
  
  #Remove overlapping correction ablinesmy assum
  if(!isEmptyOrBlank(corrections) && !isEmptyVar(corrections)){
    corrAblinePositions <- corrections[which(!duplicated(corrections[['time']])),][['time']]
  }
  
  return(corrAblinePositions)
}
