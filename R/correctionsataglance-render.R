
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
             labels = format(parseData$dateData$dateSeq, "%b %Y"), cex = 0.8)
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
      mtext("Processing Order", side=2, cex=0.7, line=1,
            at=mean(c(parseData$preproData$ylaneName,
                      parseData$normData$ylaneName,
                      parseData$postproData$ylaneName))) %>%
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
      
      if(any(names(parseData$preproData) %in% 'numText')){  
        i <- which(!is.na(parseData$preproData$numText))
        timeline <- timeline %>%
          points(x = parseData$preproData$xyText$x[i],
                 y = parseData$preproData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$preproData$xyText$x[i],
               y = parseData$preproData$xyText$y[i], 
               labels = parseData$preproData$numText[i], cex = 0.5, pos = 4)
      }
          
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
      
      if(any(names(parseData$normData) %in% 'numText')){
        i <- which(!is.na(parseData$normData$numText))
        timeline <- timeline %>%
          points(x = parseData$normData$xyText$x[i],
                 y = parseData$normData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$normData$xyText$x[i],
               y = parseData$normData$xyText$y[i], 
               labels = parseData$normData$numText[i], cex = 0.5, pos = 4)
      }
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
      
      if(any(names(parseData$postproData) %in% 'numText')){
        i <- which(!is.na(parseData$postproData$numText))
        timeline <- timeline %>%
          points(x = parseData$postproData$xyText$x[i],
                 y = parseData$postproData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$postproData$xyText$x[i],
               y = parseData$postproData$xyText$y[i], 
               labels = parseData$postproData$numText[i], cex = 0.5, pos = 4)
      }
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
             labels = parseData$thresholdData$metaLabel, cex=0.5) %>% 
        mtext("Thresholds", side=2, cex=0.5, 
              at=parseData$thresholdData$ylaneName)
      
      if(any(names(parseData$thresholdData) %in% 'numText')){
        i <- which(!is.na(parseData$thresholdData$numText))
        timeline <- timeline %>%
          points(x = parseData$thresholdData$xyText$x[i],
                 y = parseData$thresholdData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$thresholdData$xyText$x[i],
               y = parseData$thresholdData$xyText$y[i], 
               labels = parseData$thresholdData$numText[i], cex = 0.5, pos = 4)
      }
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
      
      if(any(names(parseData$qualifierData) %in% 'numText')){
        i <- which(!is.na(parseData$qualifierData$numText))
        timeline <- timeline %>%
          points(x = parseData$qualifierData$xyText$x[i],
                 y = parseData$qualifierData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$qualifierData$xyText$x[i],
               y = parseData$qualifierData$xyText$y[i], 
               labels = parseData$qualifierData$numText[i], cex = 0.5, pos = 4)
      }
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
      
      if(any(names(parseData$noteData) %in% 'numText')){
        i <- which(!is.na(parseData$noteData$numText))
        timeline <- timeline %>%
          points(x = parseData$noteData$xyText$x[i],
                 y = parseData$noteData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$noteData$xyText$x[i],
               y = parseData$noteData$xyText$y[i], 
               labels = parseData$noteData$numText[i], cex = 0.5, pos = 4)
      }
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
      
      if(any(names(parseData$gradeData) %in% 'numText')){
        i <- which(!is.na(parseData$gradeData$numText))
        timeline <- timeline %>%
          points(x = parseData$gradeData$xyText$x[i],
                 y = parseData$gradeData$xyText$y[i], pch = 8) %>% 
          text(x = parseData$gradeData$xyText$x[i],
               y = parseData$gradeData$xyText$y[i], 
               labels = parseData$gradeData$numText[i], cex = 0.5, pos = 4)
      }
    }
    
  return(list(timeline = timeline, tableOfLabels = parseData$tableData))
  
}

addToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('ytop', 'ybottom', 'ylaneName'))
  return(addLogic)
}

