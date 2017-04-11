#' Corrections at a glance report
#'
#' @description Given the full report JSON object, creates a CORR report
#' @param reportObject The full report JSON object
correctionsataglanceReport <- function(reportObject) {
  #Date & Time Data
  timezone <- fetchReportMetadataField(reportObject, 'timezone')
  startDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'startDate'), timezone)
  endDate <- flexibleTimeParse(fetchReportMetadataField(reportObject, 'endDate'), timezone)
  startSeq <- calcStartSeq(startDate, endDate)
  endSeq <- calcEndSeq(startSeq, endDate)
  dateSeq <- labelDateSeq(startSeq, endSeq, calculateTotalDays(startDate, endDate))
  dateRange <- c(startDate, endDate)

  #Parse Basic Plot Data
  primarySeriesRequiredFields <- c('approvals','qualifiers','grades','notes')
  primarySeries <- readTimeSeries(reportObject, 'primarySeries', timezone, descriptionField = 'primaryParameter', requiredFields=primarySeriesRequiredFields)
  preData <- parseCorrProcessingCorrections(reportObject, "pre", timezone)
  normalData <- parseCorrProcessingCorrections(reportObject, "normal", timezone)
  postData <- parseCorrProcessingCorrections(reportObject, "post", timezone)

  #Parse Optional Plot Data
  fieldVisitData <- parseCorrFieldVisits(reportObject, timezone)
  thresholdData <- parseCorrThresholds(reportObject, timezone)
  approvalData <- parseCorrApprovals(primarySeries, timezone, dateSeq)
  qualifiersData <- parseCorrQualifiers(primarySeries, timezone)
  notesData <- parseCorrNotes(primarySeries, timezone)
  gradesData <- parseCorrGrades(primarySeries, timezone)

  #Map required and optional data and lane display names
  requiredData <- list(preData=preData, normalData=normalData, postData=postData, thresholdData=thresholdData, qualifiersData=qualifiersData, notesData=notesData, gradesData=gradesData)
  optionalData <- list()
  requiredNames <- list(preData="Pre", normalData="Normal", postData="Post", thresholdData="Thresholds", qualifiersData="Qualifiers", notesData="Notes", gradesData="Grades")
  optionalNames <- list()
  
  #Generate Plot Lanes for Parsed Data
  allLaneData <- createPlotLanes(approvalData, requiredData, requiredNames, optionalData, optionalNames, dateRange, startSeq, endSeq)
  rectHeight <- allLaneData[['rectHeight']]
  approvalLane <- allLaneData[['approvalLane']]
  tableLabels <- allLaneData[['tableLabels']]
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
  
  if(!hasValidDataToPlot(dataLanes)){
    return('The requested dataest is empty or blank.')
  }

  #approvals at top bar
  if(!isEmptyOrBlank(approvalLane[['startDates']])){
    timeline <- timeline %>%
      rect(xleft = approvalLane[['startDates']],
           xright = approvalLane[['endDates']],
           ybottom = approvalLane[['laneYBottom']],
           ytop = approvalLane[['laneYTop']], 
           col = approvalLane[['colors']],
           border=NA, legend.name = approvalLane[['type']]) %>%
      rect(xleft = startSeq,
           xright = endSeq,
           ybottom = approvalLane[['laneYBottom']],
           ytop = approvalLane[['laneYTop']]) %>%
      text(x = approvalLane[['labels']][['x']], 
           y = approvalLane[['labels']][['y']], 
           labels = approvalLane[['labels']][['text']])
  }

  timeline <- rmDuplicateLegendItems(timeline)
  
  #field visit points
  if(!isEmptyOrBlank(fieldVisitData[['startDates']])){
    timeline <- timeline %>%
      points(x = fieldVisitData[['startDates']], 
             y=rep(unique(approvalLane[['laneYBottom']]), 
                   length(fieldVisitData[['startDates']])), 
             pch=24, col="black", bg="grey", legend.name = "Field Visits")
  }
  
  #Plot Lanes
  for(lane in seq(length(dataLanes))){
    thisLane <- dataLanes[[lane]]
    
    laneName <- names(dataLanes[lane])
    
    timeline <- plotLanes(gsplotObject = timeline,
                          laneData = thisLane,
                          laneName = laneName,
                          dateRange = dateRange,
                          rectHeight = rectHeight)
  }

  #Create Label Table 
  labelTable <- createLabelTable(tableLabels)

  return(list(timeline = timeline, tableOfLabels = labelTable))
}

#' Do add to plot
#'
#' @description Returns whether or not the provided lane data should be added to the plot
#' @param data The lane data to check
doAddToPlot <- function(data){
  return(!isEmptyOrBlank(data[['startDates']]))
}

#' Has Valid Data to Plot
#'
#' @description Returns whether or not there is any lane data to plot
#' @param allLaneData A list containing all of the created data lanes
hasValidDataToPlot <- function(allLaneData){
  startDates <- sapply(allLaneData, function(o){o[['startDates']]})

  return(length(unlist(startDates) > 0))
}

#' Plot Lanes
#' @description Given the gsplot object this plots the specified lane data using the
#' provided additional parameters.
#' @param gsplotObject The gsplot object to plot the lane onto
#' @param laneData The lane data to plot
#' @param laneName The variable name of the lane being plotted
#' @param dateRange The date range of the report
#' @param rectHeight The height to use for rendering the lane rectangles
#' @return The gsplot object with the lane data plotted onto it
plotLanes <- function(gsplotObject, laneData, laneName, dateRange, rectHeight){  
  #add rect background for processing order + any other existing lanes
  if(laneData[['required']] || doAddToPlot(laneData)){
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
            side=2, cex=0.75)
    
    if(laneName != "preData"){
      gsplotObject <- abline(gsplotObject, h = ytop_rect+(rectHeight/2), lwd = 4, col="black")
    }
    
  }
  
  #add data to lanes
  if(doAddToPlot(laneData)){
    #Add Data Rectangles
    gsplotObject <- gsplotObject %>%
      rect(xleft = laneData$startDates,
           xright = laneData$endDates,
           ybottom = laneData[['laneYBottom']],
           ytop = laneData[['laneYTop']]) 
    
    #Add Data Labels
    if(!isEmptyOrBlank(laneData[['labels']])){
      shiftedIndex <- which(laneData[['labels']][['shift']])
      staticIndex <- which(!laneData[['labels']][['shift']])

      #Plot Shifted Labels
      if(!isEmptyOrBlank(shiftedIndex)){
        shiftedLabels <- laneData[['labels']][shiftedIndex,]
        pos <- NA
        #get the full range of dates for the lane
        dateRangeFormatted <- format(dateRange, "%m/%d/%Y")
        #get the dates where we have labels/footnotes
        labelDate <- format(laneData[['labels']][['x']], "%m/%d/%Y")

        gsplotObject <- gsplotObject %>%
          points(x = shiftedLabels[['x']],
                 y = shiftedLabels[['y']], pch = 8, col = 'dodgerblue') %>% 
          text(x = shiftedLabels[['x']],
               y = shiftedLabels[['y']], 
               labels = shiftedLabels[['text']], cex = 1, pos = shiftedLabels[['pos']])
      }

      #Plot Static Labels
      if(!isEmptyOrBlank(staticIndex)){
        staticLabels <- laneData[['labels']][staticIndex,]

        gsplotObject <- gsplotObject %>%
        text(x = staticLabels[['x']], 
             y = staticLabels[['y']], 
             labels = staticLabels[['text']], cex=1) 
      }
    }
  }
  
  
  return(gsplotObject)    
}
