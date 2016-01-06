# library(timeline)

parseCorrectionsData <- function(data){

  allDataRange <- c(formatDates(data$primarySeries$requestedStartTime), 
                    formatDates(data$primarySeries$requestedEndTime))
  
  #top bar = primary series approvals
  #points on top bar = field visits
  
  apprData <- list(apprCol = getApprovalColors(data$primarySeries$approvals$description),
                   apprNum = length(data$primarySeries$approvals$startTime),
                   startDates = formatDates(data$primarySeries$approvals$startTime),
                   endDates = formatDates(data$primarySeries$approvals$endTime),
                   apprDates = seq(allDataRange[1], allDataRange[2], by="month"))

  
  #lane one = pre-processing
  preproData <- getCorrectionsData(data$corrections, "PRE_PROCESSING")
  #lane two = normal
  normData <- getCorrectionsData(data$corrections, "NORMAL")
  #lane three = post-processing
  postproData <- getCorrectionsData(data$corrections, "POST_PROCESSING")
  
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

getCorrectionsData <- function(corr, type){
  i <- which(corr$processingOrder == type)
  typeData <- data.frame(startDates = formatDates(corr$startTime[i]),
                         endDates = formatDates(corr$endTime[i]),
                         corrLabel = corr$type[i])
  return(typeData)
}
