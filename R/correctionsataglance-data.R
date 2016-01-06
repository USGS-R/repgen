# library(timeline)

parseCorrectionsData <- function(data){

  allDataRange <- c(as.POSIXct(data$primarySeries$requestedStartTime), 
                    as.POSIXct(data$primarySeries$requestedEndTime))
  
  #top bar = primary series approvals
  #points on top bar = field visits
  
  apprData <- list(apprCol = getApprovalColors(data$primarySeries$approvals$description),
                   apprNum = length(data$primarySeries$approvals$startTime),
                   startDates = data$primarySeries$approvals$startTime,
                   endDates = data$primarySeries$approvals$endTime,
                   apprDates = seq(allDataRange[1], allDataRange[2], by="month"))

  
  #lane one = pre-processing
  which(data$corrections$processingOrder == "PRE_PROCESSING")
  
  #lane two = normal
  which(data$corrections$processingOrder == "NORMAL")
  
  #lane three = post-processing
  which(data$corrections$processingOrder == "POST_PROCESSING")
  
  #lines between three and four = ?
  
  #lane four = meta data
  #qualifierLane = data$primarySeries$qualifiers
  #note lane = ?
  #grade lane = ?
  
  allVars <- as.list(environment())
  plotData <- rev(allVars[which(!names(allVars) %in% c("data"))])
  
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
