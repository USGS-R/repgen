

set_up_plot <- function(xaxis, yaxis) {
  mgp = c(1.25,0.15,0)
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, xlab="Shift, in feet", ylab="Stage in feet", xaxt="n", yaxt="n", mgp=mgp)
  actualX = par("usr")[1:2]
  actualY = par("usr")[3:4]
  
  # gridlines
  abline(v=make_ticks(xaxis, actualX, 0.02, 0), h=make_ticks(yaxis, actualY, 0.1), lty="solid", col="lightgray")
  abline(v=make_ticks(xaxis, actualX, 0.1, 0), h=make_ticks(yaxis, actualY, 0.5), lty="solid", col="darkgray")
  
  # minor axes
  axis(side=1, at=make_ticks(xaxis, actualX, 0.02, 0), tck=0.005, labels=FALSE)
  axis(side=2, at=make_ticks(yaxis, actualY, 0.5), tck=0.005, labels=FALSE)
  
  # major axes
  majorX = make_ticks(xaxis, actualX, 0.1, 0)
  majorY = make_ticks(yaxis, actualY, 0.5)
  axis(side=1, at=majorX, cex.axis=0.5, tck=0.01, mgp=mgp, labels=sprintf("%.2f", majorX))
  axis(side=2, at=majorY, cex.axis=0.5, las=2, tck=0.01, mgp=mgp, labels=sprintf("%.2f", majorY))
  
  # edges
  axis(side=1, at=actualX, cex.axis=0.5, tck=0, mgp=mgp, labels=sprintf("%.2f", actualX))
  axis(side=2, at=actualY, cex.axis=0.5, las=2, tck=0, mgp=mgp, labels=sprintf("%.2f", actualY))
  
  # zero line
  abline(v=0, lwd=1.5)
}

add_series <- function(series, color) {
  lines(x=series$x, y=series$y, type="o", col=color, lwd=1.5, pch=8)
}

add_ratings <- function(ratings, color) {
    arrows(ratings$xub, ratings$y, ratings$xlb, ratings$y, angle=90, length=0.1, lwd=1.25, code=3, col=color)
    points(ratings$x, ratings$y, pch=1)
} 

make_ticks <- function(userRange, realRange, freq, zero=NA) {

  leftSeq = seq(from=userRange[1], to=realRange[1], by=-freq)
  innerSeq = seq(from=userRange[1], to=userRange[2], by=freq)
  if (!is.na(zero)) {
    innerseq <- c(seq(from=zero, to=userRange[1], by=-freq),
                 seq(from=zero, to=userRange[2], by=freq))
  }
  rightSeq = seq(from=userRange[2], to=realRange[2], by=freq)
  ticks = c(leftSeq, innerSeq, rightSeq)
  return(ticks)
}

echo <- function(string) {
  print(string, quote=FALSE)
}

#'@export
calcShifts <- function(FieldVisits, RatingCurve) {
  lowerBound <- FieldVisits$discharge - (FieldVisits$discharge * percentError(FieldVisits$quality))
  upperBound <- FieldVisits$discharge + (FieldVisits$discharge * percentError(FieldVisits$quality))
  estStage <- approx(x=RatingCurve$discharge, y=RatingCurve$stage, xout=FieldVisits$discharge)
  estLb <- approx(x=RatingCurve$discharge, y=RatingCurve$stage, xout=lowerBound)
  estUb <- approx(x=RatingCurve$discharge, y=RatingCurve$stage, xout=upperBound)
  shifts <- data.frame(stage=FieldVisits$stage, shift=estStage$y-FieldVisits$stage,
                       lb=estLb$y-FieldVisits$stage, ub=estUb$y-FieldVisits$stage)
}

percentError <- function(MeasurementGrade) {
  percents = rep(0, length(MeasurementGrade))
  percents[grep("fair", MeasurementGrade)] = 0.08
  # other options to be added
  return(percents)
}


colrs <- c("blue", "red", "green", "orange")

#'@export
mkPNG <- function(data) {
  
  par(omi=c(0,0,0,0), mai = c(0.5, 0.5, 0.1, 0.5))
  set_up_plot(data$xlim,data$ylim)

  for (i in 1:length(data$Curves)) {
    add_series(data$Curves[[i]], color = colrs[i])
  }
  
  add_ratings(data$Measured, "black")
  #add_ratings(Historical, "blue")
  #dev.off()
}