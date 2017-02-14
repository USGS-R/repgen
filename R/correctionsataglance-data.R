#' Get a list of start dates
#' @description Given a start date and end date and whether 
#' or not the start date is the first of the month, provides a list of 
#' YYYY-MM-DD used in other functions
#' @param startD the start date
#' @param endD the end date
#' @param firstOfMonth TRUE or FALSE whether the startD is the first of the month
#' @return a list of start dates for the corr report sections
calcStartSeq <- function(startD, endD, firstOfMonth) {
 if(firstOfMonth){
    startSeq <- seq(startD, endD, by="1 month")
  } else {
    fromDate <- as.POSIXct(format(seq(startD, length=2, by="month")[2], "%Y-%m-01"))
    if (fromDate >= endD) {
      startSeq <- seq(fromDate, endD, by="-1 month")
    } else {
      startSeq <- seq(fromDate, endD, by="1 month")
    }
    startSeq <- c(startD, startSeq)
  }
  return(startSeq)
}

#' Get a list of end dates
#' @description Given a startSeq date list and end date and whether 
#' or not the end date is the first of the month, provides a list of 
#' YYYY-MM-DD used in other functions
#' @param startSeq the start date
#' @param endD the end date
#' @param firstOfMonth_end TRUE or FALSE whether the end date is the first of the month
#' @return a list of end dates for the corr report sections
calcEndSeq <- function(startSeq, endD, firstOfMonth_end) {
  if(firstOfMonth_end){
    endSeq <- startSeq[-1]
    startSeq <- head(startSeq, -1)
  } else {
    endSeq <- c(startSeq[-1], endD)
  }
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
#   #don't print Month Year in plot if there isn't enough room inside the rectangle
  dateSeq <- startSeq
  #realSeq <- dateSeq #so we're not passing NA into other places of the report
  labelSeq <- format(dateSeq, " %m/%Y ")
  for (i in 1:length(dateSeq)) { 
    if (isTextLong(labelText=labelSeq[i],dateRange=NULL,startD=startSeq[i],endD=endSeq[i],totalDays=numdays))
      dateSeq[i] <- NA
  }
  return(dateSeq)
}

#' Formats the threshold correction type 
#' @description Takes threshold data and if not suppressed
#' they will then be plotted on the chart
#' @param thresholds The threshold data from the JSON
#' @return The formatted threshold data for the chart
formatThresholdsData <- function(thresholds, timezone){
  if(length(thresholds) == 0){
    return()
  }

  th_data <- lapply(thresholds[["periods"]], function(d) {
    isSuppressed <- d[["suppressData"]]
    add_data <- list(isSuppressed=isSuppressed,
                    startTime = flexibleTimeParse(d[["startTime"]][isSuppressed],timezone),
                    endTime = flexibleTimeParse(d[["endTime"]][isSuppressed],timezone),
                    value = d[["referenceValue"]][isSuppressed])
    return(add_data)
  })
  
  threshold_data <- th_data[[1]]
  if(length(th_data) > 1){n
    for(i in 2:length(th_data)){
      threshold_data <- Map(c, threshold_data, th_data[[i]])
    }
  }

  sentence <- paste(thresholds[["type"]][threshold_data$isSuppressed], 
                    threshold_data[["value"]])
  threshold_data <- append(threshold_data, list(sentence = sentence, metaLabel="THRESHOLD"))
   
  return(threshold_data)  
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
parseCorrApprovals <- function(timeSeries, timezone, dateSeq){
  approvals <- timeSeries[['approvals']]
  approvals[['startTime']] <- flexibleTimeParse(approvals[['startTime']], timezone)
  approvals[['endTime']] <- flexibleTimeParse(approvals[['endTime']], timezone)
  colors <- c()
  
  labels <- format(dateSeq, "%m/%Y")

  #Change any end times that are in the year 9999 to something that SVGs can handle
  #Slight hack, possibly look for a better solution in the future?
  for (i in 1:length(approvals[['endTime']])) {
    endT <- approvals[['endTime']][i]
    if (endT > as.Date("2100-12-31")) {
      approvals[['endTime']][i] <- toEndOfTime(endT)
    }

    colors[[i]] <- switch(as.character(approvals[['level']][[i]]),
      "2" = "#228B22",
      "1" = "#FFD700",
      "0" = "#DC143C"
    )
  }

  returnData <- list(
    startDates = approvals[['startTime']],
    endDates = approvals[['endTime']],
    type = approvals[['description']],
    colors = colors,
    approvalLabel = labels
  )

  return(returnData)
}

#' Parse CORR Qualifiers
#'
#' @description Reads and formats the qualifiers for use on the CORR report
parseCorrQualifiers <- function(timeSeries, timezone){
  qualifiers <- timeSeries[['qualifiers']]

  returnData <- list(
    startDates = flexibleTimeParse(qualifiers[['startDate']], timezone),
    endDates = flexibleTimeParse(qualifiers[['endDate']], timezone),
    metaLabel = "TEST"
  )
}

#' Parse CORR Grades
#'
#'
parseCorrGrades <- function(timeSeries, timezone){
  grades <- timeSeries[['grades']]

  returnData <- list(
    startDates = flexibleTimeParse(grades[['startDate']], timezone),
    endDates = flexibleTimeParse(grades[['endDate']], timezone),
    metaLabel = "TEST"
  )
}

#' Parse CORR Notes
#'
#'
parseCorrNotes <- function(timeSeries, timezone){
  notes <- timeSeries[['notes']]

  returnData <- list(
    startDates = flexibleTimeParse(notes[['startDate']], timezone),
    endDates = flexibleTimeParse(notes[['endDate']], timezone),
    metaLabel = "TEST"
  )
}

#' Parse CORR Processing getCorrectionsLabels
#'
#'
parseCorrProcessingCorrections <- function(reportObject, processOrder, timezone){
  corrections <- readProcessingCorrections(reportObject, processOrder, timezone)

  returnData <- list(
    startDates = corrections[['startTime']],
    endDates = corrections[['endTime']],
    corrLabel = corrections[['type']]
  )
}

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

getLaneLabelData <- function(data, laneYTop, laneYBottom, dateRange, isDateData=FALSE){
  labelText <- data[[grep('Label', names(data))]]
  labelPositions <- NULL
  shiftText <- NULL

  if(length(labelText) > 0){
    labelPositions <- findTextLocations(data, laneYTop, laneYBottom, dateRange=dateRange, isDateData=isDateData)
    shiftText <- isTextLong(labelText, dateRange, data[['startDates']], data[['endDates']])
  }

  return(list(labelText=labelText, labelPositions=labelPositions, shiftText=shiftText))
}

getLaneData <- function(data, height, initialHeight, dateRange, bgColor, laneName=NULL, overlapInfo=NULL){
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
    labelText = labelData[['labelText']],
    labelTextPositions = labelData[['labelPositions']],
    shiftText = labelData[['shiftText']]
  ))

  return(laneData)
}

createApprovalLane <- function(approvalData, height, initialHeight, dateRange, startSeq, endSeq){
  approvalYData <- getLaneYData(approvalData, height, initialHeight)
  approvalLabelData <- c(approvalData, list(startSeq=startSeq, endSeq=endSeq))
  approvalLabels <- getLaneLabelData(approvalLabelData, approvalYData[['laneYTop']], approvalYData[['laneYBottom']], dateRange, isDateData=TRUE)
  
  approvalLane <- c(approvalData, list(
    laneYTop = approvalYData[['laneYTop']],
    laneYBottom = approvalYData[['laneYBottom']],
    labelText = approvalLabels[['labelText']],
    labelTextPositions = approvalLabels[['labelPositions']]
  ))
  
  return(approvalLane)
}

#' Create Plot lanes
#'
#' @description Given lists of required and optional data, creates the
#' data lanes for the plot and returns them as a list.
#' 
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
  for(i in seq(length(allLaneData))){
    laneName <- names(allLaneData[i])
    bgColor <- bgColors[[((i-1) %% length(bgColors)) + 1]]
    laneDisplayName <- allNameData[[laneName]]
    returnLanes[[laneName]] <- getLaneData(allLaneData[[i]], rectHeight, currentHeight, dateRange, bgColor, laneDisplayName, overlapInfo[['dataShiftInfo']][[laneName]])
    currentHeight <- min(returnLanes[[laneName]][['laneYBottom']]) - rectHeight
  }
  
  names(returnLanes) <- names(allLaneData)

  return(list(dataLanes=returnLanes, approvalLane=approvalLane, rectHeight=rectHeight))
}

#' Find location for labels 
#' @description For each corr report section, determine where to place the label
#' @param dataIn The dataset to identify text label locations for
#' @param isDateData A flag indicating if the incoming label data are dates or not
#' @param ... Additional arguments passed in to the function
findTextLocations <- function(dataIn, yTop, yBottom, isDateData = FALSE, ...){
  #put text in the center of the rectangles
  args <- list(...)
  
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
    
    #if date range for data is outside of the plot date range,
    #use plot date range to center the text
    earlier <- xl < args[["dateRange"]][1]
    later <- xr > args[["dateRange"]][2]
    if(any(earlier)){xl[which(earlier)] <- args[["dateRange"]][1]}
    if(any(later)){xr[which(later)] <- args[["dateRange"]][2]}
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

#' Creates table to display removed labels
#' @description If labels are marked as need to be move due to not enough space, they're 
#' placed into a table below the CORR chart and given a number label in the chart to 
#' reference in the table below
#' @param allData All the rows for the CORR report
#' @param empty_nms A list of the unnamed CORR report sections 
#' @return allData with new numText parameter identifying the label number 
#' for cross referencing and the table of labels/numbers to print below 
#' the CORR report chart
createLabelTable <- function(allData, labelData, empty_nms){
  num <- 1
  lastNum <- 0
  tableLabels <- c()
  for(d in which(!names(allData) %in% c('approvalData', empty_nms))){
    label_i <- grep('Label', names(allData[[d]]))
    toMove <- which(labelData[[d]][['moveText']])
    
    if(length(toMove) > 0){
      num <- lastNum + 1
    
      lastNum <- (length(toMove)-1) + num
      labNum <- seq(from = num, to = lastNum)
      
      addToTable <- allData[[d]][[label_i]][toMove]
      allData[[d]][[label_i]][toMove] <- NA
      allData[[d]][['numText']][toMove] <- as.character(labNum)
    
      tableLabels <- c(tableLabels, addToTable)
    }  
  }

  if(tail(seq(lastNum), 1) != 0){
    labelTable <- data.frame(seq(lastNum), tableLabels)
    colnames(labelTable) <- c("", "Label")
  } else {
    labelTable <- NULL
  }
  
  return(list(allData = allData, labelTable = labelTable))
}