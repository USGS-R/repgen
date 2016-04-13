
correctionsataglanceReport <- function(data){
  
  parseData <- parseCorrectionsData(data)
  
  timeline <- gsplot() %>% 
    
    #HACKY FIX FOR ISSUE #298 of gsplot
    # bgCol(border=NA, col=NULL) %>% 
    
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
           labels = format(parseData$additionalPlotData$dateData$dateSeq, "%b %Y"), cex = 1.1)
  }

  timeline <- removeApprovalDuplicates(timeline)
  
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
  
  notOptionalLanes <- c('pre', 'normal', 'post')
  
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
    
    if(laneName != "pre"){
      gsplotObject <- abline(gsplotObject, h = ytop_rect+(addData$rectHeight/2), lwd = 4, col="black")
    }
    
  }
  
  #add data to lanes
  if(addToPlot(laneData)){
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
      gsplotObject <- gsplotObject %>%
        points(x = laneData$xyText$x[i],
               y = laneData$xyText$y[i], pch = 8, col = 'dodgerblue') %>% 
        text(x = laneData$xyText$x[i],
             y = laneData$xyText$y[i], 
             labels = laneData$numText[i], cex = 1, pos = 4)
    }
  }
  
  
  return(gsplotObject)    
}

removeApprovalDuplicates <- function(gsplotObject){
  i <- which(names(gsplotObject$legend) == 'legend.args')
  all_legend_names <- unlist(lapply(gsplotObject$legend[i], function(l) {l$legend}))
  i_duplicated <- i[which(duplicated(all_legend_names))]
  gsplotObject$legend[i_duplicated] <- NULL
  return(gsplotObject)
}
