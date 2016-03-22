# library(timeline)

parseCorrectionsData <- function(data){

  dateData <- formatDateRange(data$primarySeries$requestedStartTime, data$primarySeries$requestedEndTime)
  apprData <- formatDataList(data$primarySeries$approvals, 'APPROVALS', datesRange = dateData$dateSeq) #top bar = primary series approvals
  fieldVisitData <- list(startDates = formatDates(data$fieldVisits$startTime),
                         dataNum = length(data$fieldVisits$startTime)) #points on top bar = field visits
  PREData <- formatDataList(data$corrections, "PRE_PROCESSING") #lane one = pre-processing
  NORMALData <- formatDataList(data$corrections, "NORMAL") #lane two = normal
  POSTData <- formatDataList(data$corrections, "POST_PROCESSING") #lane three = post-processing

  #lines between three and four = ?
  ThresholdsData <- formatDataList(formatThresholdsData(data$thresholds), 'META', annotation = 'sentence')

  QualifiersData <- formatDataList(data$primarySeries$qualifiers, 'META', annotation = 'identifier')
  NotesData <- formatDataList(data$primarySeries$notes, 'META', annotation = 'note') 
  GradesData <- formatDataList(data$primarySeries$grades, 'META', annotation = 'code')
  
  parsedDataList <- list(apprData = apprData,
                         PREData = PREData,
                         NORMALData = NORMALData,
                         POSTData = POSTData)
  optionalLanes <- list(ThresholdsData = ThresholdsData,
                        QualifiersData = QualifiersData,
                        NotesData = NotesData,
                        GradesData = GradesData)
  #remove NULL data if it is optional for the timeline
  optionalLanes <- optionalLanes[!unlist(lapply(optionalLanes, is.null))]
  
  #number of horizontal lines always = date bar/field visits + pre-processing + normal + post-processing
  #plus one line inbetween each data lane (except data bar & field visits) = 2 * 4
  #then add in threshold lines and metadata lines
  numPlotLines <- 8 + 2*length(optionalLanes)
  
  parsedDataList <- append(parsedDataList, optionalLanes)
  findOverlap <- findOverlap(parsedDataList)
  
  numPlotLines <- numPlotLines + findOverlap$numToAdd
  rectHeight <- 100/numPlotLines
  
  parsedData <- addYData(parsedDataList, rectHeight, findOverlap$dataShiftInfo, dateData$dateRange)
  parsedDataList <- parsedData$plotData
  tableData <- parsedData$tableData
  
  dateData[['xyText']] <- findTextLocations(dateData, isDateData = TRUE,
                                            ybottom = parsedDataList$apprData$ybottom,
                                            ytop = parsedDataList$apprData$ytop)
  
  apprData_parsed <- parsedDataList$apprData
  parsedDataList$apprData <- NULL

  bgColors <- rep(c("white", "#CCCCCC"), len = length(parsedDataList))
  processOrderLabel <- mean(c(parsedDataList$PREData$ylaneName,
                               parsedDataList$NORMALData$ylaneName,
                               parsedDataList$POSTData$ylaneName))
  
  additionalPlotData <- list(dateData = dateData,
                             numPlotLines = numPlotLines,
                             rectHeight = rectHeight,
                             bgColors = bgColors,
                             processOrderLabel = processOrderLabel)
  
  plotData <- list(apprData = apprData_parsed,
                   fieldVisitData = fieldVisitData, 
                   laneData = parsedDataList,
                   additionalPlotData = additionalPlotData, 
                   tableData = tableData)
                          
  return(plotData)

}

formatDateRange <- function(startD, endD){
  startD <- formatDates(startD)
  endD <- formatDates(endD)
  firstOfMonth <- day(startD) == 1
  firstOfMonth_end <- day(endD) == 1
  
  if(firstOfMonth){
    startSeq <- seq(startD, endD, by="1 month")
  } else {
    fromDate <- as.POSIXct(format(seq(startD, length=2, by="month")[2], "%Y-%m-01"))
    startSeq <- seq(fromDate, endD, by="1 month")
    startSeq <- c(startD, startSeq)
  }

  if(firstOfMonth_end){
    endSeq <- startSeq[-1]
    startSeq <- head(startSeq, -1)
  } else {
    endSeq <- c(startSeq[-1], endD)
  }
  
  #don't print Month Year in plot if there isn't enough room inside the rectangle
  dateSeq <- startSeq
  startSpan <- as.numeric(endSeq[1] - startSeq[1])
  endSpan <- as.numeric(tail(endSeq, 1) - tail(startSeq, 1))
  if(startSpan <= 16){dateSeq[1] <- NA}
  if(endSpan <= 16){dateSeq[length(dateSeq)] <- NA}
  
  return(list(dateRange = c(startD, endD),
              dateSeq = dateSeq,
              startMonths = startSeq,
              endMonths = endSeq,
              middleDate = median(startSeq)))
}

formatDataList <- function(dataIn, type, ...){
  args <- list(...)
  
  if(length(dataIn) == 0){
    return()
  }
  
  start <- which(names(dataIn) %in% c('startTime', 'startDate'))
  end <- which(names(dataIn) %in% c('endTime', 'endDate'))  
  
  if(!type %in% c('APPROVALS', 'META')){
    type_i <- which(dataIn$processingOrder == type)
    i <- type_i[order(dataIn[[start]][type_i])] #order by start date
  } else {
    i <- order(dataIn[[start]]) #order by start date
  }  
  
  typeData <- list(startDates = formatDates(dataIn[[start]][i]),
                   endDates = formatDates(dataIn[[end]][i]),
                   dataNum = length(dataIn[[start]][i]))
  
  if(type == 'APPROVALS'){
    approvalInfo <- getApprovalColors(dataIn$description)
    extraData <- list(apprCol = approvalInfo$color,
                      apprType = paste("Approval:", approvalInfo$type),
                      apprDates = args$datesRange)
  } else if(type == 'META') {
    extraData <- list(metaLabel = dataIn[[args$annotation]][i])
  } else {
    extraData <- list(corrLabel = dataIn$type[i],
                      applyDates = dataIn$appliedTimeUtc[i])
  }
  
  typeData <- append(typeData, extraData)
  
  if(typeData$dataNum == 0){
    typeData <- NULL
  }

  return(typeData)
}

formatThresholdsData <- function(thresholds){
  if(length(thresholds) == 0){
    return()
  }

  th_data <- lapply(thresholds$periods, function(d) {
    isSuppressed <- d$suppressData
    add_data <- list(isSuppressed=isSuppressed,
                    startTime = d$startTime[isSuppressed],
                    endTime = d$endTime[isSuppressed],
                    value = d$referenceValue[isSuppressed])
    return(add_data)
  })
  
  threshold_data <- th_data[[1]]
  if(length(th_data) > 1){
    for(i in 2:length(th_data)){
      threshold_data <- Map(c, threshold_data, th_data[[i]])
    }
  }

  sentence <- paste(thresholds$type[threshold_data$isSuppressed], 
                    threshold_data$value)
  threshold_data <- append(threshold_data, list(sentence = sentence))
   
  return(threshold_data)  
}

getApprovalColors <- function(approvals){
  approvalType <- c("Working", "In-review", "Approved")
  approvalColors <- c("red", "yellow", "green")
  matchApproval <- match(approvals, approvalType)
  rect_type <- approvalType[matchApproval]
  rect_colors <- approvalColors[matchApproval]
  return(list(color = rect_colors, type = rect_type))
}

findOverlap <- function(dataList){
  
  fixLines <- lapply(dataList, function(dataIn) {
    if(!is.null(dataIn)){
      new_line <- FALSE
      dataIn$position <- seq(dataIn$dataNum)
      
      if(dataIn$dataNum > 1){ 
        
        #ordered by applied date for process order data
        if(any(names(dataIn) %in% 'corrLabel')){
          dates_df <- as.data.frame(dataIn[-dataIn$dataNum], stringsAsFactors = FALSE)
          dates_df_ordered <- dates_df[order(dataIn$applyDates),]
          dates_list <- as.list(dates_df_ordered)
          dataIn <- append(dates_list, list(dataNum = dataIn$dataNum))
        }
          
        for(n in 2:dataIn$dataNum){
          before_n <- seq((n-1))
          is_overlap <- dataIn$startDate[n] < dataIn$endDate[before_n]
          new_line[n] <- all(is_overlap)
        }
      }

      rectToShift <- dataIn$position[new_line]
      
      addLines <- list(rectToShift = rectToShift, 
                       numNewLines = length(rectToShift))
      
      if(addLines$numNewLines == 0){
        addLines <- NULL
      } 
      
    } else {
        addLines <- NULL
    }

    return(addLines)
  })
  
  lineUnlist <- unlist(fixLines)
  linesToAdd <- grep('numNewLines', names(lineUnlist))
  numToAdd <- unname(lineUnlist[linesToAdd])
  
  return(list(numToAdd = sum(numToAdd), dataShiftInfo = fixLines))
}

addYData <- function(allData, height, overlapInfo, dateLim){
  empty_i <- which(unlist(lapply(allData, is.null)))
  emptyDataNames <- names(allData)[empty_i]
  
  startH <- 100
  for(d in seq(length(allData))){
    len_data <- allData[[d]][['dataNum']]
    len_data <- ifelse(is.null(len_data), 2, len_data)
    
    ytop <- vector(mode = "numeric", length = len_data)
    ybottom <- ytop
    
    ytop[] <- startH
    ybottom[] <- startH - height
    
    if(!is.null(overlapInfo[[d]])){

      newLine_i <- overlapInfo[[d]]$rectToShift
      
      ########## ~~~~~~ NOT FINISHED ~~~~~~ ##########
      
      ### HAVEN'T FIGURED OUT WHAT HAPPENS WHEN THERE IS MORE THAN ONE NEW LINE
      ### HOW DO YOU KNOW WHICH LINE HAS WHICH POINT??
      ### PROBABLY ADD A "line_num" ARG TO THE overlapInfo FUNCTION
      for(line in seq(overlapInfo[[d]]$numNewLines)){
        startH <- startH - height*line
        ytop[newLine_i] <- startH
        ybottom[newLine_i] <- startH - height
      }

    }
    
    allData[[d]][['ytop']] <- ytop
    allData[[d]][['ybottom']] <- ybottom
    
    ylaneName <- mean(c(min(ybottom), max(ytop)))
    allData[[d]][['ylaneName']] <- ylaneName
    
    if(!names(allData[d]) %in% c('apprData', emptyDataNames)) {
      allData[[d]][['xyText']] <- findTextLocations(allData[[d]], dateLim = dateLim)
      label_i <- grep('Label', names(allData[[d]]))
      allData[[d]][['moveText']] <- isTextLong(allData[[d]][[label_i]], dateLim, 
                                                allData[[d]][['startDates']], 
                                                allData[[d]][['endDates']])
    }
    
    startH <- startH - 2*height #shift down below rect + add space between data lanes
  }
  
  correctLabels <- createLabelTable(allData, emptyDataNames)
  labelTable <- correctLabels$labelTable
  allData <- correctLabels$allData
  
  return(list(plotData = allData, tableData = labelTable))
}

findTextLocations <- function(dataIn, isDateData = FALSE, ...){
  #put text in the center of the rectangles
  args <- list(...)
  
  if(isDateData){
    xl <- dataIn$startMonths
    xr <- dataIn$endMonths
    yb <- rep(args$ybottom[1], length(xl))
    yt <- rep(args$ytop[1], length(xl))
    dataSeq <- seq(length(xl))
  } else {
    xl <- dataIn$startDates
    xr <- dataIn$endDates
    yb <- dataIn$ybottom
    yt <- dataIn$ytop
    dataSeq <- seq(dataIn$dataNum)
    
    #if date range for data is outside of the plot date range,
    #use plot date range to center the text
    earlier <- xl < args$dateLim[1]
    later <- xr > args$dateLim[2]
    if(any(earlier)){xl[which(earlier)] <- args$dateLim[1]}
    if(any(later)){xr[which(later)] <- args$dateLim[2]}
  }
  
  x <- as.POSIXct(character()) 
  y <- as.numeric()
  for(n in dataSeq){
    x <- c(x, mean(c(xl[n], xr[n])))
    y <- c(y, mean(c(yb[n], yt[n])))
  }

  return(list(x = x, y = y))
}

isTextLong <- function(labelText, dateLim, startD, endD){
  early <- which(startD < dateLim[1])
  late <- which(endD > dateLim[2])
  startD[early] <- dateLim[1]
  endD[late] <- dateLim[2]
  
  totalDays <- difftime(dateLim[2], dateLim[1], units="days")
  widthOfChar <- (1/365)*totalDays*2.25 #each character will be 1/365 * num days in the range * buffer
  widthOfLabel <- nchar(labelText)*widthOfChar
  widthOfRect <- difftime(endD, startD, units="days")
  moveText <- widthOfLabel >= widthOfRect
  return(moveText)
}

createLabelTable <- function(allData, empty_nms){
  num <- 1
  lastNum <- 0
  tableLabels <- c()
  for(d in which(!names(allData) %in% c('apprData', empty_nms))){
    label_i <- grep('Label', names(allData[[d]]))
    toMove <- which(allData[[d]][['moveText']])
    
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
