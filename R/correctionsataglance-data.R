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
  qualifierData <- formatDataList(data$primarySeries$qualifiers, 'QUALIFIERS')
  #note lane = ?
  #grade lane = ?
  
  parsedDataList <- list(apprData = apprData,
                         preproData = preproData,
                         normData = normData,
                         postproData = postproData,
                         qualifierData = qualifierData)
  #remove empty data
  parsedDataList <- parsedDataList[unname(unlist(lapply(parsedDataList, function(x) {!is.null(x)} )))]
  
  findOverlap <- findOverlap(parsedDataList)
  
  #number of horizontal lines = date bar + field visits + pre-processing + normal + post-processing
  #plus one line inbetween each data lane (except data bar & field visits) = 3
  #still need to add in threshold lines and metadata lines
  numPlotLines <- 2 + 2*length(parsedDataList)
  numPlotLines <- numPlotLines + findOverlap$numToAdd
  rectHeight <- 100/numPlotLines
  
  parsedDataList <- addYData(parsedDataList, rectHeight, findOverlap$dataShiftInfo)

  plotData <- append(parsedDataList,
                     list(fieldVisitData = fieldVisitData,
                          numPlotLines = numPlotLines,
                          allDataRange = allDataRange,
                          rectHeight = rectHeight))
                          
  return(plotData)

}

formatDataList <- function(dataIn, type, ...){
  args <- list(...)
  
  start <- which(names(dataIn) %in% c('startTime', 'startDate'))
  end <- which(names(dataIn) %in% c('endTime', 'endDate'))  
  
  if(!type %in% c('APPROVALS', 'QUALIFIERS')){
    i <- which(dataIn$processingOrder == type)
  } else {
    i <- seq(length(dataIn[[start]]))
  }  
  
  typeData <- list(startDates = formatDates(dataIn[[start]][i]),
                   endDates = formatDates(dataIn[[end]][i]),
                   dataNum = length(dataIn[[start]][i]))
  
  if(type == 'APPROVALS'){
    extraData <- list(apprCol = getApprovalColors(dataIn$description),
                      apprDates = seq(args$datesRange[1], args$datesRange[2], by="month"))
  } else if(type == 'QUALIFIERS') {
    extraData <- list(qualLabel = dataIn$identifier)
  } else {
    extraData <- list(corrLabel = dataIn$type[i])
  }
  
  typeData <- append(typeData, extraData)
  
  if(typeData$dataNum == 0){
    typeData <- NULL
  }

  return(typeData)
}

getApprovalColors <- function(approvals){
  approvalType <- c("Working", "In-review", "Approved")
  approvalColors <- c("red", "yellow", "green")
  matchApproval <- match(approvals, approvalType)
  rect_colors <- approvalColors[matchApproval]
  return(rect_colors)
}

findOverlap <- function(dataList){
  
  fixLines <- lapply(dataList, function(dataIn) {
  
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

addYData <- function(allData, height, overlapInfo){
  startH <- 100
  for(d in seq(length(allData))){
    ytop <- vector(mode = "numeric", length = allData[[d]][['dataNum']])
    ybottom <- ytop
    
    ytop[] <- startH
    ybottom[] <- startH - height
    
    if(!is.null(overlapInfo[[d]])){

      newLine_i <- overlapInfo[[d]]$rectToShift
      
      ########## ~~~~~~ NOT FINISHED ~~~~~~ ##########
      
      ### HAVEN'T FIGURED OUT WHAT HAPPENS WHEN THERE IS MORE THAN ONE NEW LINE
      ### HOW DO YOU KNOW WHICH LINE HAS WHICH POINT??
      ### PROBABLY ADD A "line_num" ARG TO THE overlapInfo FUNCTION
      for(line in seq(overlapInfo[[d]]$numNewLines)){
        startH <- startH - height*line
        ytop[newLine_i] <- startH
        ybottom[newLine_i] <- startH - height
      }

    }
    
    allData[[d]][['ytop']] <- ytop
    allData[[d]][['ybottom']] <- ybottom
    
    if(names(allData[d]) != 'apprData') {allData[[d]][['xyText']] <- findTextLocations(allData[[d]])}
    
    startH <- startH - 2*height #shift down below rect + add space between data lanes
  }
  return(allData)
}

findTextLocations <- function(dataIn){
  #put text in the center of the rectangles
  
  xl <- dataIn$startDates
  xr <- dataIn$endDates
  yb <- dataIn$ybottom
  yt <- dataIn$ytop
  
  x <- as.POSIXct(character()) 
  y <- as.numeric()
  for(n in seq(dataIn$dataNum)){
    x <- c(x, mean(c(xl[n], xr[n])))
    y <- c(y, mean(c(yb[n], yt[n])))
  }

  return(list(x = x, y = y))
}
