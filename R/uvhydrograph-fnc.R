
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

layout_uvhydro <- function(lims){
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  
  par(omi=c(0,0,0,0), mai = c(1, 1, 0.1, 0.0))
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  panels <- matrix(c(1,2), nrow = 2)
  layout(panels)
  
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, log = 'y',
       xlab="March", ylab='Discharge in CFS', xaxt="n", yaxt="n", mgp=mgp$y)
  actualX = par("usr")[1:2]
  actualY = par("usr")[3:4]
  
  # gridlines
  abline(
    h   = c( seq( 1, 9, 1 ), seq( 10, 90, 10 ), seq( 100, 900, 100 ) , seq( 1000, 9000, 1000 ), seq( 10000, 100000, 10000 )),
    lty = 3, col = "lightgray")

  # minor axes
  axis(side=1, at=pretty(xaxis,mn_tck), tck=mn_tkL, labels=FALSE)
  axis(side=2, at=pretty(xaxis,mn_tck), tck=mn_tkL, labels=FALSE)
  
  # major axes
  majorX = pretty(xaxis,mj_tck)
  majorY = pretty(yaxis,mj_tck)
  axis(side=1, at=majorX, cex.axis=0.5, tck=mj_tkL, mgp=mgp$x, labels=sprintf("%.2f", majorX))
  axis(side=2, at=majorY, cex.axis=0.5, las=2, tck=mj_tkL, mgp=mgp$y, labels=sprintf("%.2f", majorY))
  
  # edges
  axis(side=1, at=actualX, cex.axis=0.5, tck=0, mgp=mgp$x, labels=sprintf("%.2f", actualX))
  axis(side=2, at=actualY, cex.axis=0.5, las=2, tck=0, mgp=mgp$y, labels=sprintf("%.2f", actualY))
  
  # zero line
  abline(v=0, lwd=1.5)
  
}