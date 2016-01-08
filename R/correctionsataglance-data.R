# library(timeline)

parseCorrectionsData <- function(data){

  allDataRange <- c(formatDates(data$primarySeries$requestedStartTime), 
                    formatDates(data$primarySeries$requestedEndTime))

  apprData <- formatDataList(data$primarySeries$approvals, 'APPROVALS', datesRange = allDataRange) #top bar = primary series approvals
  fieldVisitData <- list(startDates = formatDates(data$fieldVisits$startTime),
                         dataNum = length(data$fieldVisits$startTime)) #points on top bar = field visits
  preproData <- formatDataList(data$corrections, "PRE_PROCESSING") #lane one = pre-processing
  normData <- formatDataList(data$corrections, "NORMAL") #lane two = normal
  postproData <- formatDataList(data$corrections, "POST_PROCESSING") #lane three = post-processing

  #lines between three and four = ?
  
  #lane four = meta data
  #qualifierLane = data$primarySeries$qualifiers
  #note lane = ?
  #grade lane = ?
  
  parsedDataList <- list(apprData = apprData,
                         preproData = preproData,
                         normData = normData,
                         postproData = postproData)
  fixOverlap <- fixOverlap(parsedDataList)
  
  #number of horizontal lines = date bar + field visits + pre-processing + normal + post-processing
  #still need to add in threshold lines and metadata lines
  numPlotLines <- 5
  numPlotLines <- numPlotLines + fixOverlap$numToAdd
  rectHeights <- 100/numPlotLines
  
  
  xyText = findTextLocations(typeData)
  
  
  allVars <- as.list(environment())
  plotData <- allVars[which(!names(allVars) %in% c("data", "dataList"))]
  
  return(plotData)

  ########## need to get a number of rectangles vertically 
  ########## compare x overlap to determine when to start a new line 
}

formatDataList <- function(dataIn, type, ...){
  args <- list(...)
  is.corrData <- type != 'APPROVALS'
  
  if(is.corrData){
    i <- which(dataIn$processingOrder == type)
  } else {
    i <- seq(length(dataIn$startTime))
  }  
         
  typeData <- list(startDates = formatDates(dataIn$startTime[i]),
                   endDates = formatDates(dataIn$endTime[i]),
                   dataNum = length(dataIn$startTime[i]))
  
  if(is.corrData){
    extraData <- list(corrLabel = dataIn$type[i])
  } else {
    extraData <- list(apprCol = getApprovalColors(dataIn$description),
                      apprDates = seq(args$datesRange[1], args$datesRange[2], by="month"))
  }
  
  typeData <- append(typeData, extraData)

  return(typeData)
}

getApprovalColors <- function(approvals){
  approvalType <- c("Working", "In-review", "Approved")
  approvalColors <- c("red", "yellow", "green")
  matchApproval <- match(approvals, approvalType)
  rect_colors <- approvalColors[matchApproval]
  return(rect_colors)
}

fixOverlap <- function(dataList){
  
  fixLines <- lapply(parsedDataList, function(dataIn) {
    new_line <- FALSE
    if(dataIn$dataNum > 1){
      for(n in 2:dataIn$dataNum){
        before_n <- seq((n-1))
        is_overlap <- dataIn$startDate[n] < dataIn$endDate[before_n]
        new_line[n] <- all(is_overlap)
      }
    }
    rectToShift <- which(new_line)
    addLines <- list(rectToShift = rectToShift, 
                     numNewLines = length(rectToShift))
    
    if(addLines$numNewLines == 0){
      addLines <- NULL
    } 
    
    return(addLines)
  })
  
  lineUnlist <- unlist(fixLines)
  linesToAdd <- grep('numNewLines', names(lineUnlist))
  numToAdd <- unname(lineUnlist[linesToAdd])
  
  return(list(numToAdd = sum(numToAdd), dataShiftInfo = fixLines))
}

addYData <- function(){
  
  
}

findTextLocations <- function(dataIn){
  xl <- dataIn$startDates
  xr <- dataIn$endDates
  yb <- rep(85, dataIn$dataNum)
  yt <- rep(90, dataIn$dataNum)
  
  ctrs <- c()
  for(n in seq(dataIn$dataNum)){
    ctrs$x <- c(ctrs$x, mean(c(xl[n], xr[n])))
    ctrs$y <- c(ctrs$y, mean(c(yb[n], yt[n])))
  }
  
  return(ctrs)
}
