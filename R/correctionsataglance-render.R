correctionsataglanceReport <- function(reportObject) {
  #Date & Time Data
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone)
  endDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone)
  startSeq <- calcStartSeq(startDate, endDate, isFirstDayOfMonth(startDate))
  endSeq <- calcEndSeq(startSeq, endDate, isFirstDayOfMonth(endDate))
  dateSeq <- labelDateSeq(startSeq, endSeq, calculateTotalDays(startDate, endDate))

  #Parse Basic Plot Data
  primarySeries <- readTimeSeries(reportObject, 'primarySeries', 'primaryParameter', timezone)
  apprData <- formatDataList(primarySeries[['approvals']], 'APPROVALS', datesRange = startSeq, timezone=timezone)
  fieldVisits <- readFieldVists(reportObject, timezone)
  fieldVisitData <- list(startDates = fieldVisits[["startTime"]], dataNum = length(fieldVisits[["startTime"]])) 

  #Pre Data (lane one)
  PreData <- readProcessingCorrections(reportObject, "pre")
  PreData <- formatDataList(PreData, PreData[['processingOrder']], timezone=timezone)

  #Normal Data (lane two)
  NormalData <- readProcessingCorrections(reportObject, "normal")
  NormalData <- formatDataList(PreData, PreData[['processingOrder']], timezone=timezone)

  #Post Data (lane three)
  PostData <- readProcessingCorrections(reportObject, "post")
  PostData <- formatDataList(PreData, PreData[['processingOrder']], timezone=timezone)

  #Lines between three and four and additional data
  ThresholdsData <- formatDataList(formatThresholdsData(readThresholds(reportObject)), 'META', timezone=timezone, annotation = 'sentence')
  QualifiersData <- formatDataList(primarySeries[['qualifiers']], 'META', timezone=timezone, annotation = 'identifier')
  NotesData <- formatDataList(primarySeries[['notes']], 'META', timezone=timezone, annotation = 'note')
  GradesData <- formatDataList(primarySeries[['grades']], 'META', timezone=timezone, annotation = 'code')

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

  #Additional Necessary Data
  parsedDataList <- append(parsedDataList, optionalLanes)
  findOverlap <- findOverlap(parsedDataList)
  rectHeight <- 100/(8 + 2*length(optionalLanes) + findOverlap[["numToAdd"]])
  
  parsedData <- addYData(parsedDataList, rectHeight, findOverlap[["dataShiftInfo"]], c(startDate, endDate))
  
  parsedDataList <- parsedData[["plotData"]]
  tableData <- parsedData[["tableData"]]
  
  xyText <- findTextLocations(list(startMonths=startSeq, endMonths=endSeq), isDateData = TRUE,
                                            ybottom = parsedDataList[['apprData']][["ybottom"]],
                                            ytop = parsedDataList[['apprData']][["ytop"]])
  
  apprData_parsed <- parsedDataList[['apprData']]
  parsedDataList[['apprData']] <- NULL

  bgColors <- rep(c("white", "#CCCCCC"), len = length(parsedDataList))
  processOrderLabel <- mean(c(parsedDataList[['PreData']][["ylaneName"]],
                               parsedDataList[['NormalData']][["ylaneName"]],
                               parsedDataList[['PostData']][["ylaneName"]]))
    
  timeline <- gsplot() %>% 
    
  #initial setup for plot
  axis(side=1, labels=FALSE, tick=FALSE) %>%
  axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
  axis(side=3, labels=FALSE, tick=FALSE) %>% 
  points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=c(startDate, endDate)) %>% 
  mtext(text = "Processing Order", side=2, cex=1.1, line=1.5,
        at=processOrderLabel) %>% 
  legend(x = as.numeric(median(startSeq)), 
          y = 115, bty = 'n')
  
  #approvals at top bar
  if(!is.null(apprData_parsed)){
    timeline <- timeline %>%
      rect(xleft = apprData_parsed[['startDates']],
           xright = apprData_parsed[['endDates']],
           ybottom = apprData_parsed[['ybottom']],
           ytop = apprData_parsed[['ytop']], 
           col = apprData_parsed[['apprCol']],
           border=NA, legend.name = apprData_parsed[['apprType']]) %>% 
    
      rect(xleft = startSeq,
           xright = endSeq,
           ybottom = apprData_parsed[['ybottom']],
           ytop = apprData_parsed[['ytop']]) %>% 
      text(x = xyText[['x']], 
           y = xyText[['y']], 
           labels = format(dateSeq, "%m/%Y"))
  }

  timeline <- rmDuplicateLegendItems(timeline)
  
  #field visit points
  if(!is.null(fieldVisitData)){
    timeline <- timeline %>%
      points(x = fieldVisitData[['startDates']], 
             y=rep(unique(parsedDataList[['apprData']][['ybottom']]), 
                   fieldVisitData[['dataNum']]), 
             pch=24, col="black", bg="grey", legend.name = "Field Visits")
  }
  
  allLaneNames <- unlist(strsplit(names(parsedDataList), split="Data"))

  for(lane in seq(length(parsedDataList))){
    thisLane <- parsedDataList[[lane]] 
    labelName <- names(thisLane)[grep('Label', names(thisLane))]
    
    timeline <- plotLanes(gsplotObject = timeline,
                          laneData = thisLane, 
                          bgColor = bgColors[[lane]],
                          labelName = labelName,
                          laneName = allLaneNames[lane], 
                          dateRange = c(startDate, endDate),
                          rectHeight = rectHeight)
  }

  return(list(timeline = timeline, tableOfLabels = tableData))
  
}

doAddToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('ytop', 'ybottom', 'ylaneName'))
  return(addLogic)
}

plotLanes <- function(gsplotObject, laneData, bgColor, labelName, laneName, dateRange, rectHeight){  
  notOptionalLanes <- c('Pre', 'Normal', 'Post')
  
  #add rect background for processing order + any other existing lanes
  if(laneName %in% notOptionalLanes || doAddToPlot(laneData)){
    ytop_rect <- max(laneData$ytop)
    ybottom_rect <- min(laneData$ybottom)
    
    gsplotObject <- gsplotObject %>%
      
      #background
      rect(xleft = dateRange[1],
           xright = dateRange[2],
           ybottom = ybottom_rect-(rectHeight/2),
           ytop = ytop_rect+(rectHeight/2), 
           border = NA, col=bgColor) %>% 
      
      mtext(text = laneName, 
            at=laneData$ylaneName,
            side=2, cex=0.9)
    
    if(laneName != "Pre"){
      gsplotObject <- abline(gsplotObject, h = ytop_rect+(rectHeight/2), lwd = 4, col="black")
    }
    
  }
  
  #add data to lanes
  if(doAddToPlot(laneData)){

    #Fix for Year 9999 not rendering properly
    if(laneData$endDates > dateRange[2]){
      laneData$endDates <- toEndOfTime(laneData$endDates)
    }

    gsplotObject <- gsplotObject %>%
      
      rect(xleft = laneData$startDates,
           xright = laneData$endDates,
           ybottom = laneData$ybottom,
           ytop = laneData$ytop) 
    
    if(!all(is.na(laneData[[labelName]]))){
      gsplotObject <- gsplotObject %>%
        text(x = laneData$xyText$x, 
             y = laneData$xyText$y, 
             labels = laneData[[labelName]], cex=1) 
    }
    
    if(any(names(laneData) %in% 'numText')){   
      i <- which(!is.na(laneData$numText))
      if (any(laneData$moveText)) { 
        pos <- NA
        #get the full range of dates for the lane
        dateRange <- format(dateRange, "%m/%d/%Y")
        #get the dates where we have labels/footnotes
        labelDate <- format(laneData$xyText$x, "%m/%d/%Y")
          #move the label to the right if the label position (labelDate) is the same as the first date on the left side
          if (any(labelDate <= dateRange[1])) {
            pos<-4
          }
          #move the label to the left if the label position (labelDate) is the same as the last date on the right side
          if (any(labelDate >= dateRange[2])) {
            pos<-2
          }
      }
      #if none of the labels need to move around, default their position to the right
      if (is.na(pos)) {
        pos<-4
      }
      gsplotObject <- gsplotObject %>%
        points(x = laneData$xyText$x[i],
               y = laneData$xyText$y[i], pch = 8, col = 'dodgerblue') %>% 
        text(x = laneData$xyText$x[i],
             y = laneData$xyText$y[i], 
             labels = laneData$numText[i], cex = 1, pos = pos)
    }
  }
  
  
  return(gsplotObject)    
}
