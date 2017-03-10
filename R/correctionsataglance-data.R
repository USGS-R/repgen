#' Get a list of start dates
#' @description Given a start date and end date and whether 
#' or not the start date is the first of the month, provides a list of 
#' YYYY-MM-DD used in other functions
#' @param startD the start date of the report
#' @param endD the end date of the report
#' @return a list of start dates for the corr report sections
calcStartSeq <- function(startD, endD) {
  firstOfMonth <- isFirstDayOfMonth(startD)
  firstOfMonth_end <- isFirstDayOfMonth(endD)
  
  if(firstOfMonth){
    startSeq <- seq(startD, endD, by="1 month")
  } else {
    nextMonth <- toStartOfMonth(startD) + months(1) + hours(hour(startD)) + minutes(minute(startD)) + seconds(second(startD))
    
    if(nextMonth >= endD){
      startSeq <- seq(startD, endD, by="1 month")
    } else {
      startSeq <- seq(nextMonth, endD, by="1 month")
      startSeq <- append(startD, startSeq)
    }
  }
  
  if(firstOfMonth_end){
    startSeq <- head(startSeq, -1)
  }
  
  return(startSeq)
}

#' Get a list of end dates
#' @description Given a startSeq date list and end date and whether 
#' or not the end date is the first of the month, provides a list of 
#' YYYY-MM-DD used in other functions
#' @param startSeq the start date
#' @param endD the end date
#' @return a list of end dates for the corr report sections
calcEndSeq <- function(startSeq, endD) {
  endSeq <- c(startSeq[-1], endD)
  return(endSeq)
}

#' Provides the label for the corr report sections
#' @description Given the start and end dates for the sections and the
#' total number of days, provides the list of month labels to aply to the report
#' removing those labels where there isn't enough room
#' @param startSeq a list of start dates for months
#' @param endSeq a list of end dates for months
#' @param numdays the full number of days requested in the report
#' @return the list of months to label in the corr report
labelDateSeq <- function(startSeq, endSeq, numdays) {
  #don't print Month Year in plot if there isn't enough room inside the rectangle
  dateSeq <- startSeq
  #realSeq <- dateSeq #so we're not passing NA into other places of the report
  labelSeq <- format(dateSeq, " %m/%Y ")
  for (i in 1:length(dateSeq)) { 
    if (isTextLong(labelText=labelSeq[i],dateRange=NULL,startD=startSeq[i],endD=endSeq[i],totalDays=numdays))
      dateSeq[i] <- NA
  }
  return(dateSeq)
}

#' Checks for overlapping positions in order to make new lines when needed
#' @description The function checks for overlapping date ranges and if there
#' is overlap, adds a line to the chart and puts the overlapping data there
#' @param dataList The data to check for overlapping dates
#' @return The number of lines to add and a list of the data that need to be
#' shifted
findOverlap <- function(dataList){
  fixLines <- lapply(dataList, function(dataIn) {
    addLines <- NULL
    dataLength <- length(dataIn[['startDates']])
    
    if(!isEmptyOrBlank(dataIn)){
      if(dataLength > 1){
        
        dataIn[["position"]] <- seq(dataLength)
        dataIn[["line_num"]] <- 1
        
        #ordered by applied date for process order data
        if (isEmptyOrBlank(dataIn[["applyDates"]])) {
          dataIn[["applyDates"]] <- NA
        }
        dataIn_df <- as.data.frame(dataIn, stringsAsFactors = FALSE)
        if(any(names(dataIn) %in% 'corrLabel')){
          dates_df_ordered <- dataIn_df[order(dataIn_df[["applyDates"]]),]
          dataIn_df <- dates_df_ordered
        } 
          
        for(n in 2:dataLength){
          before_n <- seq((n-1))
          is_overlap <- dataIn_df[["startDates"]][n] < dataIn_df[["endDates"]][before_n] & 
            dataIn_df[["endDates"]][n] > dataIn_df[["startDates"]][before_n]
          
          if(all(is_overlap)){ #dates overlap with all previous dates
            dataIn_df[["line_num"]][n] <- max(dataIn_df[["line_num"]]) + 1
          } else if(any(is_overlap)){ # dates overlap with some of the previous dates
            lines <- dataIn_df[["line_num"]][before_n]
            overlap_lines <- lines[is_overlap]
            no_overlap_lines <- lines[!is_overlap]
            new_line <- no_overlap_lines[which(no_overlap_lines != overlap_lines)]
            
            if(length(new_line) != 0){ # overlap occurs somewhere in all existing lanes 
              dataIn_df[["line_num"]][n] <- min(new_line)
            } else {
              dataIn_df[["line_num"]][n] <- max(dataIn_df[["line_num"]]) + 1
            }
          }
        }
        
        line_num <- 0 # work around irrelevant warnings from devtools::check()
        new_line_df <- dataIn_df %>% filter(line_num != 1)
        if(nrow(new_line_df) != 0){
          #new lines = max line number (not including first, bc it's not new)
          addLines <- list(rectToShift = new_line_df[["position"]], 
                           lineNum = new_line_df[["line_num"]],
                           numNewLines = max(new_line_df[["line_num"]]) - 1) 
        }
      }
    } 
    return(addLines)
  })
  
  lineUnlist <- unlist(fixLines)
  linesToAdd <- grep('numNewLines', names(lineUnlist))
  totalNewLines <- unname(lineUnlist[linesToAdd])
  
  return(list(totalNewLines = sum(totalNewLines), dataShiftInfo = fixLines))
}

#' Parse CORR Approvals
#'
#' @description Reads and formats the approvals for use on the CORR report
#' @param timeSeries The time series to get approvals from
#' @param timezone The timezone to parse data into
#' @param dateSeq The sequence of dates from the report start date to the report end date
parseCorrApprovals <- function(timeSeries, timezone, dateSeq){
  approvals <- timeSeries[['approvals']]
  returnData <- list()
  
  if(!isEmptyOrBlank(approvals)){
    approvals[['startTime']] <- flexibleTimeParse(approvals[['startTime']], timezone)
    approvals[['endTime']] <- flexibleTimeParse(approvals[['endTime']], timezone)
    approvals[['description']] <- paste("Approval:", approvals[['description']])
    colors <- c()
    
    labels <- format(dateSeq, "%m/%Y")
    
    for (i in 1:length(approvals[['level']])) {
      #Assign proper approval colors
      colors[[i]] <- switch(as.character(approvals[['description']][[i]]),
                            "Approval: Working"="#DC143C",
                            "Approval: In Review"="#FFD700",
                            "Approval: Approved"="#228B22",
                            "grey" #Defaults to 4 which corresponds to grey for an unrecognized type
      )
    }
    
    returnData <- list(
      startDates = approvals[['startTime']],
      endDates = approvals[['endTime']],
      type = approvals[['description']],
      colors = colors,
      approvalLabel = labels
    )
  }
  
  return(returnData)
}

#' Parse CORR Thresholds
#'
#' @description Retrieves and formats thresholds for the CORR report
#' @param reportObject The full report JSON object
#' @param timezone The timezone to parse data into
parseCorrThresholds <- function(reportObject, timezone){
  thresholdData <- tryCatch({
    readThresholds(reportObject)
  }, error=function(e) {
    warning(paste("Returning NULL for threhsolds. Error:", e))
    return(NULL)
  })
  
  formattedData <- list()
  
  #default to one empty row if there are no threshold data set to the entire time
  #range of the report with an empty label
  returnData <- list(
    startDates = flexibleTimeParse(reportObject[["reportMetadata"]][["startDate"]], timezone),
    endDates = flexibleTimeParse(reportObject[["reportMetadata"]][["endDate"]], timezone),
    metaLabel = ""    
  )
  
  if(!isEmptyOrBlank(thresholdData) && nrow(thresholdData) > 0){
    for(i in seq(nrow(thresholdData))){
      threshold <- thresholdData[i,]
      
      periods <- threshold[['periods']][[1]]
      type <- rep(threshold[['type']], times=nrow(periods))
      startDates <- periods[['startTime']]
      endDates <- periods[['endTime']]
      value <- periods[['referenceValue']]
      suppressData <- periods[['suppressData']]
      
      formattedData <- rbind(formattedData, data.frame(type, startDates, endDates, value, suppressData))
    }
    
    formattedData <- formattedData[which(formattedData[['suppressData']]),]
    
    returnData <- list(
      startDates = flexibleTimeParse(formattedData[['startDates']], timezone),
      endDates = flexibleTimeParse(formattedData[['endDates']], timezone),
      metaLabel = paste(formattedData[['type']], formattedData[['value']])
    )
  }
  
  return(returnData)
}

#' Parse CORR Qualifiers
#'
#' @description Retrieves and formats qualifiers for the CORR report
#' @param timeSeries The time series to get qualifiers for
#' @param timezone The timezone to parse data into
parseCorrQualifiers <- function(timeSeries, timezone){
  qualifiers <- timeSeries[['qualifiers']]
  returnData <- list()

  if(!isEmptyOrBlank(qualifiers)){
    returnData <- list(
      startDates = flexibleTimeParse(qualifiers[['startDate']], timezone),
      endDates = flexibleTimeParse(qualifiers[['endDate']], timezone),
      metaLabel = qualifiers[['identifier']]
    )
  }

  return(returnData)
}

#' Parse CORR Grades
#'
#' @description Retrieves and formats grades for the CORR report
#' @param timeSeries The time series to get grades for
#' @param timezone The timezone to parse data into
parseCorrGrades <- function(timeSeries, timezone){
  grades <- timeSeries[['grades']]
  returnData <- list()

  if(!isEmptyOrBlank(grades)){
    returnData <- list(
      startDates = flexibleTimeParse(grades[['startDate']], timezone),
      endDates = flexibleTimeParse(grades[['endDate']], timezone),
      metaLabel = paste("Grade", grades[['code']])
    )
  }
  
  return(returnData)
}

#' Parse CORR Notes
#'
#' @description Retrieves and formats notes for the CORR report
#' @param timeSeries The time series to get notes for
#' @param timezone The timezone to parse data into
parseCorrNotes <- function(timeSeries, timezone){
  notes <- timeSeries[['notes']]
  returnData <- list()

  if(!isEmptyOrBlank(notes)){
    returnData <- list(
      startDates = flexibleTimeParse(notes[['startDate']], timezone),
      endDates = flexibleTimeParse(notes[['endDate']], timezone),
      metaLabel = notes[['note']]
    )
  }
    
  return(returnData)
}

#' Parse CORR Processing getCorrectionsLabels
#'
#' @description Returns the processing order corrections for the specified processing order.
#' These lanes should still show up even if they are empty, so this will always return a list
#' even if it has no data, unlike the other parse functions for lane data.
#' @param reportObject The full report JSON object 
#' @param processOrder The processing order to fetch data for
#' @param timezone The timezone to parse data into
parseCorrProcessingCorrections <- function(reportObject, processOrder, timezone){
  corrections <- tryCatch({
   readProcessingCorrections(reportObject, processOrder, timezone)
  }, error=function(e){
    warning(paste("Returning empty list for", processOrder, "corrections. Error:", e))
    return(list())
  })

  returnData <- list(
    startDates = corrections[['startTime']],
    endDates = corrections[['endTime']],
    applyDates = corrections[['appliedTimeUtc']],
    corrLabel = corrections[['type']]
  )

  return(returnData)
}

#' Parse Corr Field Visits
#' 
#' @description Returns the field visit data and formats it properly for plotting
#' @param reportObject The full report JSON object
#' @param timezone The timezone of the report
parseCorrFieldVisits <- function(reportObject, timezone){
  fieldVisits <- tryCatch({
    readFieldVists(reportObject, timezone)
  }, error=function(e){
    warning(paste("Returning empty list for field visits. Error:", e))
    return(list())
  })

  returnData <- list(
    startDates = fieldVisits[['startTime']]
  )

  return(returnData)
}

#' Get Lane Y Data
#' 
#' @description Get the Y position data for rectangles for the specified lane
#' @param data The lane data to get Y positional data for
#' @param height The height to use for lane rectangles
#' @param intialHeight The height to start calculating Y positions from
#' @param overlapInfo [DEFAULT: NULL] Calculated rectangle overlap data to use
getLaneYData <- function(data, height, initialHeight, overlapInfo=NULL){
  dataLength <- ifelse(isEmptyOrBlank(data[['startDates']]), 2, length(data[['startDates']]))
  laneYTop <- vector(mode = "numeric", length = dataLength)
  laneYBottom <- laneYTop
  laneYTop[] <- initialHeight
  laneYBottom[] <- initialHeight - height
  if(!isEmptyOrBlank(overlapInfo)){
      laneYTop[overlapInfo[["rectToShift"]]] <- initialHeight - height*(overlapInfo[["lineNum"]] - 1) 
      laneYBottom[overlapInfo[["rectToShift"]]] <- laneYTop[overlapInfo[["rectToShift"]]] - height
  }
  laneNameYPos <- mean(c(laneYBottom, laneYTop))

  return(list(laneYTop=laneYTop, laneYBottom=laneYBottom, laneNameYPos=laneNameYPos))
}

#' Get Lane Label Data
#'
#' @description Get the label positions and text for the specified lane
#' @param data The lane data to get label positions and text for
#' @param laneYTop The upper Y-bound of the lane
#' @param laneYBottom The lower Y-bound of the lane
#' @param dateRange The date range of the report
#' @param isDateData [DEFAULT: FALSE] Whether or not the data is just dates
getLaneLabelData <- function(data, laneYTop, laneYBottom, dateRange, isDateData=FALSE){
  returnData <- na.omit(data.frame(text=as.character(NA), x=as.numeric(NA), y=as.numeric(NA), shift=as.logical(NA), stringsAsFactors = FALSE))
  pos <- c()
  
  if(!isEmptyOrBlank(grep('Label', names(data)))){
    labelText <- data[[grep('Label', names(data))]]
    labelPositions <- NULL
    shiftText <- NULL
    
    if(length(labelText) > 0){
      labelPositions <- findTextLocations(data, laneYTop, laneYBottom, isDateData=isDateData)
      shiftText <- isTextLong(labelText, dateRange, data[['startDates']], data[['endDates']])
      shiftText <- sapply(shiftText, function(shift){ifelse(is.na(shift), FALSE, shift)})
      pos <- rep(4, length(labelPositions[['x']]))
      pos[which(labelPositions[['x']] >= dateRange[2] - days(1))] <- 2
      pos <- unname(unlist(as.list(pos)))
    }
    
    returnData <- data.frame(text=labelText, x=labelPositions[['x']], y=labelPositions[['y']], shift=shiftText, pos=pos, stringsAsFactors = FALSE)
  }
  
  return(returnData)
}

#' Create Lane
#'
#' @description Get the lane data for the provided data set
#' @param data The dataset to create a lane for
#' @param height The height to use for lane rectangles
#' @param initialHeight The height to start building this lane from
#' @param dateRange The date range of the report
#' @param bgColor The background color to use for this lane
#' @param laneName [DEFAULT: NULL] The display name to use for this lane
#' @param overlapInfo [DEFAULT: NULL] Calculated rectangle overlap data to use
createLane <- function(data, height, initialHeight, dateRange, bgColor, laneName=NULL, overlapInfo=NULL){
  #Bound Dates
  fixedDates <- boundLaneDates(data[['startDates']], data[['endDates']], dateRange)
  data[['startDates']] <- fixedDates[['startDates']]
  data[['endDates']] <- fixedDates[['endDates']]
  
  #Get Y Position Data
  yPosData <- getLaneYData(data, height, initialHeight, overlapInfo=overlapInfo)
  
  #Get Label Data
  labelData <- getLaneLabelData(data, yPosData[['laneYTop']], yPosData[['laneYBottom']], dateRange)

  laneData <- c(data, list(
    laneYTop = yPosData[['laneYTop']],
    laneYBottom = yPosData[['laneYBottom']],
    laneNameYPos = yPosData[['laneNameYPos']],
    laneName = laneName,
    bgColor = bgColor,
    labels = labelData
  ))

  return(laneData)
}

#' Create Approval Lane
#'
#' @description Get the lane data for the approval lane. This logic
#' differs slightly from other lanes because the approval lane has
#' some unique behaviors.
#' @param approvalData The approvals to create thr lane using
#' @param height The height to use for lane rectangles
#' @param initialHeight The height to start building this lane from
#' @param dateRange The date range of the report
#' @param startSeq The sequence of all month start dates on the report
#' @param endSeq The sequence of all month end dates on the report
createApprovalLane <- function(approvalData, height, initialHeight, dateRange, startSeq, endSeq){
  #Bound Dates
  fixedDates <- boundLaneDates(approvalData[['startDates']], approvalData[['endDates']], dateRange)
  approvalData[['startDates']] <- fixedDates[['startDates']]
  approvalData[['endDates']] <- fixedDates[['endDates']]
  
  #Get Y Data
  approvalYData <- getLaneYData(approvalData, height, initialHeight)
  
  #Get Label Data
  approvalLabelData <- c(approvalData, list(startSeq=startSeq, endSeq=endSeq))
  approvalLabels <- getLaneLabelData(approvalLabelData, approvalYData[['laneYTop']], approvalYData[['laneYBottom']], dateRange, isDateData=TRUE)
  
  approvalLane <- c(approvalData, list(
    laneYTop = approvalYData[['laneYTop']],
    laneYBottom = approvalYData[['laneYBottom']],
    labels = approvalLabels
  ))
  
  return(approvalLane)
}

#' Create Plot lanes
#'
#' @description Given lists of required and optional data, creates the
#' data lanes for the plot and returns them as a list.
#' @param approvalData The approval data to create the approval lane from
#' @param requiredData The vector of required data sets (processing order corrections)
#' @param requiredNames The vector of display names for the required data sets
#' @param optionalData The vector of optional data sets (thesholds, qualifiers, notes, and grades)
#' @param optionalNames The vector of display names for the optional data sets
#' @param dateRange The date range of the report
#' @param startSeq The sequence of all month start dates on the report
#' @param endSeq The sequence of all month end dates on the report
#' @return A list holding the created data lanes, the approval lane, the calculated lane rectangle
#' height, and the list of labels to be put into the label table.
createPlotLanes <- function(approvalData, requiredData, requiredNames, optionalData, optionalNames, dateRange, startSeq, endSeq){
  returnLanes <- list()
  optionalData <- optionalData[!unlist(lapply(optionalData, function(o){isEmptyOrBlank(o[['startDates']])}))]
  optionalLaneCount <- length(optionalData)
  allLaneData <- c(requiredData, optionalData)
  allNameData <- c(requiredNames, optionalNames)
  overlapInfo <- findOverlap(allLaneData)
  rectHeight <- 100/(8 + 2*optionalLaneCount + overlapInfo[["totalNewLines"]])
  currentHeight <- 100
  bgColors <- list("white", "#CCCCCC")

  #Generate the approval lane seperately because it behaves differently
  approvalLane <- createApprovalLane(approvalData, rectHeight, currentHeight, dateRange, startSeq, endSeq)
  
  #Get the starting y position after the approval lane
  currentHeight <- min(approvalLane[['laneYBottom']]) - rectHeight

  #Generate Data Lanes
  lastLabelIndex <- 0
  tableLabels <- c()
  
  for(i in seq(length(allLaneData))){
    #Lane Properties
    laneName <- names(allLaneData[i])
    bgColor <- bgColors[[((i-1) %% length(bgColors)) + 1]]
    laneDisplayName <- allNameData[[laneName]]

    #Generate the lane
    returnLanes[[laneName]] <- createLane(allLaneData[[i]], rectHeight, currentHeight, dateRange, bgColor, laneDisplayName, overlapInfo[['dataShiftInfo']][[laneName]])

    #Split shifted labels from the lane plot to the lable table
    splitLabelData <- splitShiftedLabels(returnLanes[[laneName]][['labels']], lastLabelIndex)
    returnLanes[[laneName]][['labels']] <- splitLabelData[['labels']]
    lastLabelIndex <- splitLabelData[['endLabelIndex']]
    tableLabels <- c(tableLabels, splitLabelData[['tableLabels']])

    #Shift next lane down
    currentHeight <- min(returnLanes[[laneName]][['laneYBottom']]) - rectHeight
  }
  
  names(returnLanes) <- names(allLaneData)

  return(list(dataLanes=returnLanes, approvalLane=approvalLane, rectHeight=rectHeight, tableLabels=tableLabels))
}

#' Split Shifted Labels
#' 
#' @description Given the full list labels for a data lane splits out the
#' labels that are marked as being shifted and stores them in a list to be
#' put into the label table. The labels on the plot are replaced by numbers
#' representing the table row that is holding the relevant label.
#' @param inputLabels The full list of labels from the lane
#' @param startLabelIndex The row number to start labeling removed labels from
#' @return A list containing the labels for the table, the update plot labels,
#' and the last row number that was labeled (to start the next lane labels from)
splitShiftedLabels <- function(inputLabels, startLabelIndex){
  endLabelIndex <- startLabelIndex
  tableLabels <- c()
  removedLabels <- list()
  labels <- inputLabels
  shifts <- labels[['shift']]  
  
  if(nrow(labels) > 0 && !isEmptyOrBlank(shifts) && nrow(labels[shifts,]) > 0){
    tableLabels <- labels[shifts,][['text']]
    labels[shifts,][['text']] <- sapply(seq(nrow(labels[shifts,])), function(i){as.character(i+startLabelIndex)})
    endLabelIndex <- max(as.numeric(labels[shifts,][['text']]))
  }
  
  return(list(tableLabels=tableLabels, labels=labels, endLabelIndex=endLabelIndex))
}

#' Create Label Table
#' 
#' @description Creates the table that holds the labels that were removed
#' from the plot because they didn't fit in their respective rectangles.
#' @param labels The list of labels to store in the table
createLabelTable <- function(labels){
  if(!isEmptyOrBlank(labels)){
    tableData <- data.frame(seq(labels), labels, stringsAsFactors=FALSE)
    colnames(tableData) <- c("", "Label")
  } else {
    tableData <- NULL
  }

  return(tableData)
}

#' Find location for labels 
#'
#' @description For each corr report section, determine where to place the label
#' @param dataIn The dataset to identify text label locations for
#' @param yTop The upper Y-bound of the lane
#' @param yBottom The lower Y-bound of the lane
#' @param isDateData A flag indicating if the incoming label data are dates or not
findTextLocations <- function(dataIn, yTop, yBottom, isDateData = FALSE){
  #put text in the center of the rectangles
  if(isDateData){
    xl <- dataIn[["startSeq"]]
    xr <- dataIn[["endSeq"]]
    yb <- rep(yBottom[1], length(xl))
    yt <- rep(yTop[1], length(xl))
    dataSeq <- seq(length(xl))
  } else {
    xl <- dataIn[["startDates"]]
    xr <- dataIn[["endDates"]]
    yb <- yBottom
    yt <- yTop
    dataSeq <- seq(length(xl))
  }
  
  x <- as.POSIXct(character()) 
  y <- as.numeric()
  for(n in dataSeq){
    x <- c(x, mean(c(xl[n], xr[n])))
    y <- c(y, mean(c(yb[n], yt[n])))
  }

  return(list(x = x, y = y))
}

#' Check label length to make sure it will fit
#' @description Checks to see if the label to apply to the section block on the CORR report 
#' is too large for the space alotted 
#' @param labelText the label to apply to the CORR block
#' @param dateRange the start and end dates for the block section
#' @param startD a list of the start dates for the month sequence
#' @param endD a list of the end dates for the month sequence
#' @param totalDays the number of days for the section of the block 
#' @return TRUE or FALSE if the label is too long for the section available
isTextLong <- function(labelText, dateRange = NULL, startD, endD, totalDays = NULL){
  if(is.null(totalDays)){
    early <- which(startD < dateRange[1])
    late <- which(endD > dateRange[2])
    startD[early] <- dateRange[1]
    endD[late] <- dateRange[2]
    totalDays <- difftime(dateRange[2], dateRange[1], units="days")
  } 
  
  widthOfChar <- (1/365)*totalDays*1.5 #each character will be 1/365 * num days in the range 
  widthOfLabel <- nchar(labelText)*widthOfChar
  widthOfRect <- as.numeric(difftime(strptime(endD, format="%Y-%m-%d"), strptime(startD,format="%Y-%m-%d"), units="days"))
  moveText <- widthOfLabel >= widthOfRect
  return(moveText)
}

#' Bound Lane Dates
#' @description Given start and end dates, bound them to the provided date range
#' @param startDates The start dates to bound
#' @param endDates The end dates to bound
#' @param dateRange The date range to bound the dates to
#' @param padDays [DEFAULT: 1] The number of days to pad dates with
boundLaneDates <- function(startDates, endDates, dateRange, padDays=1){
  startDates <- do.call('c', lapply(startDates, function(d){boundDate(d, dateRange, padDays)}))
  endDates <-  do.call('c', lapply(endDates, function(d){boundDate(d, dateRange, padDays)}))
  return(list(startDates=startDates, endDates=endDates))
}