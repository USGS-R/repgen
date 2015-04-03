

set_up_plot <- function(lims) {
  xaxis <- lims$xlim
  yaxis <- lims$ylim
  par(omi=c(0,0,0,0), mai = c(0.5, 0.5, 0.1, 0.5))
  
  mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
  mn_tck = 50
  mn_tkL = 0.005
  mj_tck = 10
  mj_tkL = 0.01
  
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, xlab="Shift, in feet", ylab="Stage, in feet", xaxt="n", yaxt="n", mgp=mgp$y)
  actualX = par("usr")[1:2]
  actualY = par("usr")[3:4]
  
  # gridlines
  abline(v=pretty(xaxis,mn_tck), h=pretty(yaxis,mn_tck), lty="solid", col="lightgray")
  abline(v=pretty(xaxis,mj_tck), h=pretty(yaxis,mj_tck), lty="solid", col="darkgray")
  
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

addRatingShifts <- function(x, y, color) {
  curve_pch = 8
  lines(x, y, type="o", col=color, lwd=1.5, pch=curve_pch)
}

addErrorBars <- function(x, y, xError0, xError1, color = 'black') {

  arrows(xError0, y, xError1, y, angle=90, length=0.1, lwd=1.25, code=3, col=color)
  points(x, y, pch=21, bg = 'white')
} 

add_call_out <- function(x,y, xlim, ylim, call_text){
  x_bmp = diff(xlim)*0.05
  y_bmp = diff(ylim)*0.03
  for (i in 1:length(x)){
    lines(c(x[i],x[i]-x_bmp),c(y[i],y[i]+y_bmp), type = 'l',col='black')
    lines(c(x[i]-x_bmp, x[i]-x_bmp*2),c(y[i]+y_bmp,y[i]+y_bmp), type = 'l',col='black')
    text(x[i]-x_bmp*2, y = y[i]+y_bmp, labels = call_text[i], pos = 2, cex = 0.5)
  }
  
  
}
echo <- function(string) {
  print(string, quote=FALSE)
}


percentError <- function(MeasurementGrade) {
  percents = rep(0, length(MeasurementGrade))
  percents[grep("fair", MeasurementGrade)] = 0.08
  # other options to be added
  return(percents)
}

getLims <- function(shiftPoints, stagePoints, maxShift, minShift){

  # shiftPoints and stagePoints are required, and should not be NA. 
  # maxShift and minShift, if missing from the json, are NA
  x_mx <- max(c(sapply(shiftPoints, FUN = max), maxShift), na.rm = TRUE)
  x_mn <- min(c(sapply(shiftPoints, FUN = min), minShift), na.rm = TRUE)
  y_mx <- max(sapply(stagePoints, FUN = max))
  y_mn <- min(sapply(stagePoints, FUN = min))
  
  if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
    stop('missing or NA values in shiftPoints or stagePoints. check input json.')
  }
  ylim = c(y_mn, y_mx)
  xlim = c(x_mn, x_mx)
  return(list(xlim = xlim, ylim = ylim))

}

