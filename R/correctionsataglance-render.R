
correctionsataglanceReport <- function(data){
  
  parseData <- parseCorrectionsData(data)
  
  timeline <- gsplot() %>% 
    
    #HACKY FIX FOR ISSUE #298 of gsplot
    bgCol(border=NA, col=NULL) %>% 
    
    #initial setup for plot
    axis(side=1, labels=FALSE, tick=FALSE) %>%
    axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
    axis(side=3, tick=FALSE, at=parseData$apprData$apprDates, 
         labels=format(parseData$apprData$apprDates, "%b %Y")) %>% 
    points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=parseData$allDataRange)
    
    #approvals at top bar
    if(!is.null(parseData$apprData)){
      timeline <- timeline %>%
          rect(xleft = parseData$apprData$startDates,
               xright = parseData$apprData$endDates,
               ybottom = parseData$apprData$ybottom,
               ytop = parseData$apprData$ytop, 
               col = parseData$apprData$apprCol) 
    }
      
    #field visit points
    if(!is.null(parseData$fieldVisitData)){
      timeline <- timeline %>%
        points(x = parseData$fieldVisitData$startDates, 
               y=rep(unique(parseData$apprData$ybottom), 
                     parseData$fieldVisitData$dataNum), 
               pch=24, col="black", bg="grey")
    }
      
    #pre-processing corrections
    if(!is.null(parseData$preproData)){
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
    if(!is.null(parseData$normData)){
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
    if(!is.null(parseData$postproData)){
      timeline <- timeline %>%
        rect(xleft = parseData$postproData$startDates,
             xright = parseData$postproData$endDates,
             ybottom = parseData$postproData$ybottom,
             ytop = parseData$postproData$ytop) %>% 
        text(x = parseData$postproData$xyText$x, 
             y = parseData$postproData$xyText$y, 
             labels = parseData$postproData$corrLabel, cex=0.5)
    }
    
  return(timeline)
  
}

addToPlot <- function(data){
  !is.null(data)
}

