#' Reads and formats data for the CORR Report
#' 
#' @description Given the entire JSON data object, reads and formats the data 
#' into the format needed to create the CORR report
#' 
#' @param reportObject A corrections at a glance report JSON string
#' @return Returns all the data, formatted correctly for the CORR report
#' 
parseCorrectionsData <- function(reportObject){
  startD <- flexibleTimeParse(reportObject[["primarySeries"]][["requestedStartTime"]], reportObject[["reportMetadata"]][["timezone"]])
  endD <- flexibleTimeParse(reportObject[["primarySeries"]][["requestedEndTime"]], reportObject[["reportMetadata"]][["timezone"]])
  firstOfMonth <- isFirstDayOfMonth(startD)
  firstOfMonth_end <- isFirstDayOfMonth(endD)
  numdays <- calculateTotalDays(startD, endD)
  startSeq <- calcStartSeq(startD, endD, firstOfMonth)
  endSeq <- calcEndSeq(startSeq, endD, firstOfMonth_end)
  realSeq <- startSeq
  dateRange <- c(startD, endD)
  dateSeq <- calcDateSeq(startSeq, endSeq, numdays)
  startMonths <- startSeq
  endMonths <- endSeq
  middleDate <- median(startSeq)
  dateData <- list(dateRange = dateRange, dateSeq = dateSeq, realSeq = realSeq, startMonths = startMonths, endMonths = endMonths, middleDate = middleDate)
              
  apprData <- formatDataList(reportObject[["primarySeries"]][["approvals"]], 'APPROVALS', datesRange = dateData[["realSeq"]], timezone=reportObject[["reportMetadata"]][["timezone"]]) #top bar = primary series approvals
  fieldVisitData <- list(startDates = flexibleTimeParse(reportObject[["fieldVisits"]][["startTime"]], timezone=reportObject[["reportMetadata"]][["timezone"]]),
                         dataNum = length(reportObject[["fieldVisits"]][["startTime"]])) #points on top bar = field visits
  
  PreData <- reportObject[["corrections"]][["preProcessing"]] #lane one = pre-processing
  PreData <- formatDataList(PreData, PreData[["processingOrder"]], timezone=reportObject[["reportMetadata"]][["timezone"]])
  NormalData <- reportObject[["corrections"]][["normal"]] #lane two = normal
  NormalData <- formatDataList(NormalData, NormalData[["processingOrder"]], timezone=reportObject[["reportMetadata"]][["timezone"]])
  PostData <- reportObject[["corrections"]][["postProcessing"]] #lane three = post-processing
  PostData <- formatDataList(PostData, PostData[["processingOrder"]], timezone=reportObject[["reportMetadata"]][["timezone"]])
  
  #lines between three and four = ?
  ThresholdsData <- formatDataList(formatThresholdsData(reportObject[["thresholds"]]), 'META', timezone=reportObject[["reportMetadata"]][["timezone"]], annotation = 'sentence')

  QualifiersData <- formatDataList(reportObject[["primarySeries"]][["qualifiers"]], 'META', timezone=reportObject[["reportMetadata"]][["timezone"]], annotation = 'identifier')
  NotesData <- formatDataList(reportObject[["primarySeries"]][["notes"]], 'META', timezone=reportObject[["reportMetadata"]][["timezone"]], annotation = 'note') 
  GradesData <- formatDataList(reportObject[["primarySeries"]][["grades"]], 'META', timezone=reportObject[["reportMetadata"]][["timezone"]], annotation = 'code')
  
  parsedDataList <- list(apprData = apprData,
                         PreData = PreData,
                         NormalData = NormalData,
                         PostData = PostData)
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
  
  numPlotLines <- numPlotLines + findOverlap[["numToAdd"]]
  rectHeight <- 100/numPlotLines
  
  parsedData <- addYData(parsedDataList, rectHeight, findOverlap[["dataShiftInfo"]], dateData[["dateRange"]])
  parsedDataList <- parsedData[["plotData"]]
  tableData <- parsedData[["tableData"]]
  
  dateData[['xyText']] <- findTextLocations(dateData, isDateData = TRUE,
                                            ybottom = parsedDataList[["apprData"]][["ybottom"]],
                                            ytop = parsedDataList[["apprData"]][["ytop"]])
  
  apprData_parsed <- parsedDataList$apprData
  parsedDataList$apprData <- NULL

  bgColors <- rep(c("white", "#CCCCCC"), len = length(parsedDataList))
  processOrderLabel <- mean(c(parsedDataList[["PreData"]][["ylaneName"]],
                               parsedDataList[["NormalData"]][["ylaneName"]],
                               parsedDataList[["PostData"]][["ylaneName"]]))
  
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

calcEndSeq <- function(startSeq, endD, firstOfMonth_end) {
  if(firstOfMonth_end){
    endSeq <- startSeq[-1]
    startSeq <- head(startSeq, -1)
  } else {
    endSeq <- c(startSeq[-1], endD)
  }
  return(endSeq)
}
  
calcDateSeq <- function(startSeq, endSeq, numdays) {
#   #don't print Month Year in plot if there isn't enough room inside the rectangle
  dateSeq <- startSeq
  #realSeq <- dateSeq #so we're not passing NA into other places of the report
  labelSeq <- format(dateSeq, " %m/%Y ")
  for (i in 1:length(dateSeq)) { 
    if (isTextLong(labelText=labelSeq[i],dateLim=NULL,startD=startSeq[i],endD=endSeq[i],totalDays=numdays))
      dateSeq[i] <- NA
  }
  return(dateSeq)
}
  
  
  

formatDataList <- function(dataIn, type, timezone, ...){
  
  args <- list(...)
  
  if(length(dataIn) == 0){
    return()
  }
  
  start_i <- which(names(dataIn) %in% c('startTime', 'startDate'))
  end_i <- which(names(dataIn) %in% c('endTime', 'endDate'))
  dataIn[[start_i]] <- flexibleTimeParse(dataIn[[start_i]], timezone)
  dataIn[[end_i]] <- flexibleTimeParse(dataIn[[end_i]], timezone)
  
  if (!isEmptyOrBlank(type)) {
    if(type == 'APPROVALS' && length(dataIn)>0){
      for (i in 1:length(dataIn[[end_i]])) {
        endT <- dataIn[[end_i]][i]
        if (endT > as.Date("2100-12-31")) {
          dataIn[[end_i]][i] <- toEndOfTime(endT)
        }
      }
    }
  }

  if(!type %in% c('APPROVALS', 'META')){
    type_i <- which(dataIn[["processingOrder"]] == type)
    i <- type_i[order(dataIn[[start_i]][type_i])] #order by start date
  } else {
    i <- order(dataIn[[start_i]]) #order by start date
  }

  typeData <- list(startDates = dataIn[[start_i]][i],
                   endDates = dataIn[[end_i]][i],
                   dataNum = length(dataIn[[start_i]][i]))
  
  if(type == 'APPROVALS'){
    approvalColors <- getApprovalColors(dataIn[["level"]])
    extraData <- list(apprCol = approvalColors,
                      apprType = paste("Approval:", dataIn[["description"]]),
                      apprDates = args[["datesRange"]])
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

  th_data <- lapply(thresholds[["periods"]], function(d) {
    isSuppressed <- d[["suppressData"]]
    add_data <- list(isSuppressed=isSuppressed,
                    startTime = d[["startTime"]][isSuppressed],
                    endTime = d[["endTime"]][isSuppressed],
                    value = d[["referenceValue"]][isSuppressed])
    return(add_data)
  })
  
  threshold_data <- th_data[[1]]
  if(length(th_data) > 1){
    for(i in 2:length(th_data)){
      threshold_data <- Map(c, threshold_data, th_data[[i]])
    }
  }

  sentence <- paste(thresholds[["type"]][threshold_data$isSuppressed], 
                    threshold_data[["value"]])
  threshold_data <- append(threshold_data, list(sentence = sentence))
   
  return(threshold_data)  
}

# Returns just the colors, in order, for a list of approval levels
# Known Approval Levels:
# 0=Working, 1=In Review, 2=Approved  Anything else is colored as 'grey'
getApprovalColors <- function(approvalLevels){
  knownApprovalLevels <- c(0, 1, 2)
  approvalColors <- c("#DC143C", "#FFD700", "#228B22", "grey") #Hex values are colors for known approvals.  Grey only used for possible unknown values.
  matchApproval <- match(approvalLevels, knownApprovalLevels, 4) #Defaults to 4 which corresponds to grey for an unrecognized type
  colors <- approvalColors[matchApproval]
  return(colors)
}

findOverlap <- function(dataList){
  
  fixLines <- lapply(dataList, function(dataIn) {
    addLines <- NULL
    
    if(!is.null(dataIn)){
      
      if(dataIn$dataNum > 1 & !any(names(dataIn) %in% 'apprType')){
        
        dataIn[["position"]] <- seq(dataIn[["dataNum"]])
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
          
        for(n in 2:dataIn$dataNum){
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

      newLine_i <- overlapInfo[[d]][["rectToShift"]]
      line <- overlapInfo[[d]][["lineNum"]]
      
      ytop[newLine_i] <- startH - height*(line - 1) #num lines away from first line
      ybottom[newLine_i] <- ytop[newLine_i] - height
      
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
    
    startH <- min(ybottom) - height #shift down below rect + add space between data lanes
  }
  
  correctLabels <- createLabelTable(allData, emptyDataNames)
  labelTable <- correctLabels[["labelTable"]]
  allData <- correctLabels[["allData"]]
  
  return(list(plotData = allData, tableData = labelTable))
}

findTextLocations <- function(dataIn, isDateData = FALSE, ...){
  #put text in the center of the rectangles
  args <- list(...)
  
  if(isDateData){
    xl <- dataIn[["startMonths"]]
    xr <- dataIn[["endMonths"]]
    yb <- rep(args[["ybottom"]][1], length(xl))
    yt <- rep(args[["ytop"]][1], length(xl))
    dataSeq <- seq(length(xl))
  } else {
    xl <- dataIn[["startDates"]]
    xr <- dataIn[["endDates"]]
    yb <- dataIn[["ybottom"]]
    yt <- dataIn[["ytop"]]
    dataSeq <- seq(dataIn[["dataNum"]])
    
    #if date range for data is outside of the plot date range,
    #use plot date range to center the text
    earlier <- xl < args[["dateLim"]][1]
    later <- xr > args[["dateLim"]][2]
    if(any(earlier)){xl[which(earlier)] <- args[["dateLim"]][1]}
    if(any(later)){xr[which(later)] <- args[["dateLim"]][2]}
  }
  
  x <- as.POSIXct(character()) 
  y <- as.numeric()
  for(n in dataSeq){
    x <- c(x, mean(c(xl[n], xr[n])))
    y <- c(y, mean(c(yb[n], yt[n])))
  }

  return(list(x = x, y = y))
}

isTextLong <- function(labelText, dateLim = NULL, startD, endD, totalDays = NULL){
  if(is.null(totalDays)){
    early <- which(startD < dateLim[1])
    late <- which(endD > dateLim[2])
    startD[early] <- dateLim[1]
    endD[late] <- dateLim[2]
    totalDays <- difftime(dateLim[2], dateLim[1], units="days")
  } 
  
  widthOfChar <- (1/365)*totalDays*1.5 #each character will be 1/365 * num days in the range 
  widthOfLabel <- nchar(labelText)*widthOfChar
  widthOfRect <- as.numeric(difftime(strptime(endD, format="%Y-%m-%d"), strptime(startD,format="%Y-%m-%d"), units="days"))
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
