
#'@export
uvhydrographPlot <- function(data){
  
  discharge <- data$discharge
  gage <- data$gage
  
  par(omi=c(0,0,0,0), mai = c(1, 1, 0.1, 0.0))
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  
  
  # need same number of date obs?
  plot(seq(1,length(discharge)), discharge, log="y", panel.first=grid(equilogs=FALSE), 
       type = 'o', pch = 4, lty = 1, col = 'blue',
       ylab = 'Discharge in CFS', xlab = 'March', cex = 0.5,cex.axis = 0.5)
  plot(seq(1,length(discharge)), gage, panel.first=grid(equilogs=FALSE),
       type = 'l',
       ylab = 'Gage height in feet', xlab = 'March', cex = 0.5)
  
}