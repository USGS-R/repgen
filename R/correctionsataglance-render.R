
correctionsataglanceReport <- function(data){
  
  parseData <- parseCorrectionsData(data)
  
  timeline <- gsplot() %>% 
    
    #HACKY FIX FOR ISSUE #298 of gsplot
    bgCol(border=NA, col=NULL) %>% 
    
    #initial setup for plot
    axis(side=1, labels=FALSE, tick=FALSE) %>%
    axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
    axis(side=3, labels=FALSE, tick=FALSE) %>% 
    points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=parseData$dateData$dateRange)
  
    #approvals at top bar
    if(!is.null(parseData$apprData)){
      timeline <- timeline %>%
        rect(xleft = parseData$apprData$startDates,
             xright = parseData$apprData$endDates,
             ybottom = parseData$apprData$ybottom,
             ytop = parseData$apprData$ytop, 
             col = parseData$apprData$apprCol,
             border=NA) %>% 
      
        rect(xleft = parseData$dateData$startMonths,
             xright = parseData$dateData$endMonths,
             ybottom = parseData$apprData$ybottom,
             ytop = parseData$apprData$ytop) %>% 
        text(x = parseData$dateData$xyText$x, 
             y = parseData$dateData$xyText$y, 
             labels = format(parseData$dateData$dateSeq, "%b %Y"), cex=0.8)
    }
  
    #field visit points
    if(!is.null(parseData$fieldVisitData)){
      timeline <- timeline %>%
        points(x = parseData$fieldVisitData$startDates, 
               y=rep(unique(parseData$apprData$ybottom), 
                     parseData$fieldVisitData$dataNum), 
               pch=24, col="black", bg="grey")
    }
  
    #background of the process order lanes
    timeline <- timeline %>%
    
      #pre-processing background
      rect(xleft = parseData$dateData$dateRange[1],
           xright = parseData$dateData$dateRange[2],
           ybottom = parseData$preproData$ybottom-(parseData$rectHeight/2),
           ytop = parseData$preproData$ytop+(parseData$rectHeight/2), 
           border = NA, col="azure2") %>% 
      #normal background
      rect(xleft = parseData$dateData$dateRange[1],
           xright = parseData$dateData$dateRange[2],
           ybottom = parseData$normData$ybottom-(parseData$rectHeight/2),
           ytop = parseData$normData$ytop+(parseData$rectHeight/2), 
           border = NA, col="azure3") %>% 
      #post-processing background
      rect(xleft = parseData$dateData$dateRange[1],
           xright = parseData$dateData$dateRange[2],
           ybottom = parseData$postproData$ybottom-(parseData$rectHeight/2),
           ytop = parseData$postproData$ytop+(parseData$rectHeight/2), 
           border = NA, col="azure4") %>% 
      
      #add text labels
      mtext("PRE", side=2, cex=0.5, 
            at=parseData$preproData$ylaneName) %>% 
      mtext("NORMAL", side=2, cex=0.5, 
            at=parseData$normData$ylaneName) %>% 
      mtext("POST", side=2, cex=0.5, 
            at=parseData$postproData$ylaneName)
      
    #pre-processing corrections
    if(addToPlot(parseData$preproData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$preproData$startDates,
             xright = parseData$preproData$endDates,
             ybottom = parseData$preproData$ybottom,
             ytop = parseData$preproData$ytop) %>% 
        text(x = parseData$preproData$xyText$x, 
             y = parseData$preproData$xyText$y, 
             labels = parseData$preproData$corrLabel, cex=0.5)
        
    }
      
    #normal corrections
    if(addToPlot(parseData$normData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$normData$startDates,
             xright = parseData$normData$endDates,
             ybottom = parseData$normData$ybottom,
             ytop = parseData$normData$ytop) %>% 
        text(x = parseData$normData$xyText$x, 
             y = parseData$normData$xyText$y, 
             labels = parseData$normData$corrLabel, cex=0.5) 
    }
    
    #post-processing corrections
    if(addToPlot(parseData$postproData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$postproData$startDates,
             xright = parseData$postproData$endDates,
             ybottom = parseData$postproData$ybottom,
             ytop = parseData$postproData$ytop) %>% 
        text(x = parseData$postproData$xyText$x, 
             y = parseData$postproData$xyText$y, 
             labels = parseData$postproData$corrLabel, cex=0.5)
    }
  
    #Thresholds
    if(addToPlot(parseData$thresholdData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$thresholdData$startDates,
             xright = parseData$thresholdData$endDates,
             ybottom = parseData$thresholdData$ybottom,
             ytop = parseData$thresholdData$ytop) %>% 
        text(x = parseData$thresholdData$xyText$x, 
             y = parseData$thresholdData$xyText$y, 
             labels = parseData$thresholdData$metaLabel, cex=0.5)
    }
    
    #Meta-data lanes 
    if(addToPlot(parseData$qualifierData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$qualifierData$startDates,
             xright = parseData$qualifierData$endDates,
             ybottom = parseData$qualifierData$ybottom,
             ytop = parseData$qualifierData$ytop) %>% 
        text(x = parseData$qualifierData$xyText$x, 
             y = parseData$qualifierData$xyText$y, 
             labels = parseData$qualifierData$metaLabel, cex=0.5) %>% 
        mtext("Qualifiers", side=2, cex=0.5, 
              at=parseData$qualifierData$ylaneName)
    }
    
    if(addToPlot(parseData$noteData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$noteData$startDates,
             xright = parseData$noteData$endDates,
             ybottom = parseData$noteData$ybottom,
             ytop = parseData$noteData$ytop) %>% 
        text(x = parseData$noteData$xyText$x, 
             y = parseData$noteData$xyText$y, 
             labels = parseData$noteData$metaLabel, cex=0.5) %>% 
        mtext("Notes", side=2, cex=0.5, 
              at=parseData$noteData$ylaneName)
    }
    
    if(addToPlot(parseData$gradeData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$gradeData$startDates,
             xright = parseData$gradeData$endDates,
             ybottom = parseData$gradeData$ybottom,
             ytop = parseData$gradeData$ytop) %>% 
        text(x = parseData$gradeData$xyText$x, 
             y = parseData$gradeData$xyText$y, 
             labels = parseData$gradeData$metaLabel, cex=0.5) %>% 
        mtext("Grades", side=2, cex=0.5, 
              at=parseData$gradeData$ylaneName)
    }
    
  return(timeline)
  
}

addToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('ytop', 'ybottom', 'ylaneName'))
  return(addLogic)
}

