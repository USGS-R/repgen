correctionsataglanceReport <- function(reportObject) {
  #Date & Time Data
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone)
  endDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone)
  startSeq <- calcStartSeq(startDate, endDate, isFirstDayOfMonth(startDate))
  endSeq <- calcEndSeq(startSeq, endDate, isFirstDayOfMonth(endDate))
  dateSeq <- labelDateSeq(startSeq, endSeq, calculateTotalDays(startDate, endDate))
  dateRange <- c(startDate, endDate)

  #Parse Basic Plot Data
  primarySeries <- readTimeSeries(reportObject, 'primarySeries', 'primaryParameter', timezone)
  preData <- parseCorrProcessingCorrections(reportObject, "pre", timezone)
  normalData <- parseCorrProcessingCorrections(reportObject, "normal", timezone)
  postData <- parseCorrProcessingCorrections(reportObject, "post", timezone)

  #Parse Optional Plot Data
  fieldVisitData <- readFieldVists(reportObject, timezone)
  thresholdsData <- formatThresholdsData(readThresholds(reportObject), timezone)
  approvalData <- parseCorrApprovals(primarySeries, timezone)
  qualifiersData <- parseCorrQualifiers(primarySeries, timezone)
  notesData <- parseCorrNotes(primarySeries, timezone)
  gradesData <- parseCorrGrades(primarySeries, timezone)

  #Map required and optional data and lane display names
  requiredData <- list(preData=preData, normalData=normalData, postData=postData)
  optionalData <- list(thresholdsData=thresholdsData, qualifiersData=qualifiersData, notesData=notesData, gradesData=gradesData)
  requiredNames <- list(preData="Pre", normalData="Normal", postData="Post")
  optionalNames <- list(thresholdsData="Thresholds", qualifiersData="Qualifiers", notesData="Notes", gradesData="Grades")
  
  #Generate Plot Lanes for Parsed Data
  allLaneData <- createPlotLanes(approvalData, requiredData, requiredNames, optionalData, optionalNames, dateRange)
  rectHeight <- allLaneData[['rectHeight']]
  approvalLane <- allLaneData[['approvalLane']]
  dataLanes <- allLaneData[['dataLanes']]
  processOrderLabelYPos <- mean(c(dataLanes[['preData']][["laneNameYPos"]],
                                  dataLanes[['normalData']][["laneNameYPos"]],
                                  dataLanes[['postData']][["laneNameYPos"]]))
  
  #Create Base Plot
  timeline <- gsplot() %>% 
    axis(side=1, labels=FALSE, tick=FALSE) %>%
    axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
    axis(side=3, labels=FALSE, tick=FALSE) %>% 
    points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=dateRange) %>% 
    mtext(text = "Processing Order", side=2, cex=1.1, line=1.5,
          at=processOrderLabelYPos) %>% 
    legend(x = as.numeric(median(startSeq)), 
            y = 115, bty = 'n')

  #approvals at top bar
  if(!is.null(approvalLane[['startDates']])){
    timeline <- timeline %>%
      rect(xleft = approvalLane[['startDates']],
           xright = approvalLane[['endDates']],
           ybottom = approvalLane[['laneYBottom']],
           ytop = approvalLane[['laneYTop']], 
           col = "red",
           border=NA, legend.name = approvalLane[['type']]) %>% 
    
      rect(xleft = startSeq,
           xright = endSeq,
           ybottom = approvalLane[['laneYBottom']],
           ytop = approvalLane[['laneYTop']]) %>%
      text(x = approvalLane[['labelTextPositions']][['x']], 
           y = approvalLane[['labelTextPositions']][['y']], 
           labels = format(dateSeq, "%m/%Y"))
  }

  timeline <- rmDuplicateLegendItems(timeline)
  
  #field visit points
  #if(!is.null(fieldVisitData)){
  #  timeline <- timeline %>%
  #    points(x = fieldVisitData[['startDates']], 
  #           y=rep(unique(yData[['approvalData']][['ybottom']]), 
  #                 length(fieldVisitData[['startDates']])), 
  #           pch=24, col="black", bg="grey", legend.name = "Field Visits")
  #}
  
  for(lane in seq(length(dataLanes))){
    thisLane <- dataLanes[[lane]]
    
    laneName <- names(dataLanes[lane])
    labelName <- names(thisLane)[grep('Label', names(thisLane))]
    
    timeline <- plotLanes(gsplotObject = timeline,
                          laneData = thisLane,
                          labelName = labelName,
                          laneName = laneName,
                          dateRange = dateRange,
                          rectHeight = rectHeight)
  }

  return(list(timeline = timeline, tableOfLabels = NULL))
  
}

addToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('laneYTop', 'laneYBottom', 'laneNameYPos'))
  return(addLogic)
}

doAddToPlot <- function(data){
  return(!isEmptyOrBlank(data[['startDates']]))
}

plotLanes <- function(gsplotObject, laneData, labelName, laneName, dateRange, rectHeight){  
  notOptionalLanes <- c('preData', 'normalData', 'postData')
  
  #add rect background for processing order + any other existing lanes
  if(laneName %in% notOptionalLanes || doAddToPlot(laneData)){
    ytop_rect <- max(laneData[['laneYTop']])
    ybottom_rect <- min(laneData[['laneYBottom']])
    
    gsplotObject <- gsplotObject %>%
      
      #background
      rect(xleft = dateRange[1],
           xright = dateRange[2],
           ybottom = ybottom_rect-(rectHeight/2),
           ytop = ytop_rect+(rectHeight/2), 
           border = NA, col=laneData[['bgColor']]) %>% 
      
      mtext(text = laneData[['laneName']], 
            at=laneData[['laneNameYPos']],
            side=2, cex=0.9)
    
    if(laneName != "preData"){
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
           ybottom = laneData[['laneYBottom']],
           ytop = laneData[['laneYTop']]) 
    
    if(!isEmptyOrBlank(labelName) && !all(is.na(laneData[[labelName]]))){
      gsplotObject <- gsplotObject %>%
        text(x = laneData$labelTextPositions$x, 
             y = laneData$labelTextPositions$y, 
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
