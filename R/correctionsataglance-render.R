correctionsataglanceReport <- function(data) {
  
  parseData <- parseCorrectionsData(data)
  
  timeline <- gsplot() %>% 
    
    #initial setup for plot
    axis(side=1, labels=FALSE, tick=FALSE) %>%
    axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
    axis(side=3, labels=FALSE, tick=FALSE) %>% 
    points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=parseData$additionalPlotData$dateData$dateRange) %>% 
    mtext(text = "Processing Order", side=2, cex=1.1, line=1.5,
          at=parseData$additionalPlotData$processOrderLabel) %>% 
    legend(x = as.numeric(parseData$additionalPlotData$dateData$middleDate), 
           y = 115, bty = 'n')
  
  #approvals at top bar
  if(!is.null(parseData$apprData)){
    timeline <- timeline %>%
      rect(xleft = parseData$apprData$startDates,
           xright = parseData$apprData$endDates,
           ybottom = parseData$apprData$ybottom,
           ytop = parseData$apprData$ytop, 
           col = parseData$apprData$apprCol,
           border=NA, legend.name = parseData$apprData$apprType) %>% 
    
      rect(xleft = parseData$additionalPlotData$dateData$startMonths,
           xright = parseData$additionalPlotData$dateData$endMonths,
           ybottom = parseData$apprData$ybottom,
           ytop = parseData$apprData$ytop) %>% 
      text(x = parseData$additionalPlotData$dateData$xyText$x, 
           y = parseData$additionalPlotData$dateData$xyText$y, 
           labels = format(parseData$additionalPlotData$dateData$dateSeq, "%m/%Y"))
  }

  timeline <- rmDuplicateLegendItems(timeline)
  
  #field visit points
  if(!is.null(parseData$fieldVisitData)){
    timeline <- timeline %>%
      points(x = parseData$fieldVisitData$startDates, 
             y=rep(unique(parseData$apprData$ybottom), 
                   parseData$fieldVisitData$dataNum), 
             pch=24, col="black", bg="grey", legend.name = "Field Visits")
  }
  
  allLaneNames <- unlist(strsplit(names(parseData$laneData), split="Data"))
  for(lane in seq(length(parseData$laneData))){
    thisLane <- parseData$laneData[[lane]] 
    labelName <- names(thisLane)[grep('Label', names(thisLane))]
    
    timeline <- plotLanes(gsplotObject = timeline,
                          laneData = thisLane, 
                          whichCol = lane,
                          labelName = labelName,
                          laneName = allLaneNames[lane], 
                          addData = parseData$additionalPlotData)
  }

  return(list(timeline = timeline, tableOfLabels = parseData$tableData))
  
}

addToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('ytop', 'ybottom', 'ylaneName'))
  return(addLogic)
}

plotLanes <- function(gsplotObject, laneData, whichCol, 
                      labelName, laneName, addData){
  
  notOptionalLanes <- c('Pre', 'Normal', 'Post')
  
  #add rect background for processing order + any other existing lanes
  if(laneName %in% notOptionalLanes || addToPlot(laneData)){
    ytop_rect <- max(laneData$ytop)
    ybottom_rect <- min(laneData$ybottom)
    
    gsplotObject <- gsplotObject %>%
      
      #background
      rect(xleft = addData$dateData$dateRange[1],
           xright = addData$dateData$dateRange[2],
           ybottom = ybottom_rect-(addData$rectHeight/2),
           ytop = ytop_rect+(addData$rectHeight/2), 
           border = NA, col=addData$bgColors[whichCol]) %>% 
      
      mtext(text = laneName, 
            at=laneData$ylaneName,
            side=2, cex=0.9)
    
    if(laneName != "Pre"){
      gsplotObject <- abline(gsplotObject, h = ytop_rect+(addData$rectHeight/2), lwd = 4, col="black")
    }
    
  }
  
  #add data to lanes
  if(addToPlot(laneData)){

    #Fix for Year 9999 not rendering properly
    if(laneData$endDates > addData$dateData$dateRange[2]){
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
        dateRange <- format(addData$dateData$dateRange, "%m/%d/%Y")
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
