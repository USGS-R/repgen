# library(timeline)

parseCorrectionsData <- function(data){

  allDataRange <- c(formatDates(data$primarySeries$requestedStartTime), 
                    formatDates(data$primarySeries$requestedEndTime))

  #top bar = primary series approvals
  #points on top bar = field visits
  apprData <- formatDataList(data$primarySeries$approvals, 'APPROVALS', datesRange = allDataRange)
  
  #lane one = pre-processing
  preproData <- formatDataList(data$corrections, "PRE_PROCESSING")
  #lane two = normal
  normData <- formatDataList(data$corrections, "NORMAL")
  #lane three = post-processing
  postproData <- formatDataList(data$corrections, "POST_PROCESSING")
  
  #lines between three and four = ?
  
  #lane four = meta data
  #qualifierLane = data$primarySeries$qualifiers
  #note lane = ?
  #grade lane = ?
  
  allVars <- as.list(environment())
  plotData <- allVars[which(!names(allVars) %in% c("data", "i_prepro", "i_norm", "i_postpro"))]
  
  return(plotData)

  ########## need to get a number of rectangles vertically 
  ########## compare x overlap to determine when to start a new line 
}

#get colors for approvals
getApprovalColors <- function(approvals){
  approvalType <- c("Working", "In-review", "Approved")
  approvalColors <- c("red", "yellow", "green")
  matchApproval <- match(approvals, approvalType)
  rect_colors <- approvalColors[matchApproval]
  return(rect_colors)
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
    extraData <- list(corrLabel = dataIn$type[i],
                      xyText = findTextLocations(typeData))
  } else {
    extraData <- list(apprCol = getApprovalColors(dataIn$description),
                      apprDates = seq(args$datesRange[1], args$datesRange[2], by="month"))
  }
  
  typeData <- append(typeData, extraData)

  return(typeData)
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
