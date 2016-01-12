
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
           border = NA, col="azure4") 
      
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
  
    #Meta-data lane -- qualifiers
    if(addToPlot(parseData$qualifierData)){
      timeline <- timeline %>%
        
        rect(xleft = parseData$qualifierData$startDates,
             xright = parseData$qualifierData$endDates,
             ybottom = parseData$qualifierData$ybottom,
             ytop = parseData$qualifierData$ytop) %>% 
        text(x = parseData$qualifierData$xyText$x, 
             y = parseData$qualifierData$xyText$y, 
             labels = parseData$qualifierData$qualLabel, cex=0.5)
    }
    
  return(timeline)
  
}

addToPlot <- function(data){
  nms <- names(data)
  addLogic <- any(!nms %in% c('ytop', 'ybottom'))
  return(addLogic)
}

