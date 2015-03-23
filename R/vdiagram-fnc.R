Metadata <- list("gageId"="15052500",
                 "gageName"="OLD TOM C NR KASAAN AK",
                 "period"=as.Date(c("2013-09-30", "2014-09-30")),
                 "ratingId"=10)

BaseRatingTable <- data.frame(stage=c(1.44, 1.46, 1.5, 1.6, 1.9, 2.3, 2.8, 3.68, 7.2), 
                              discharge=c(0.5, 1, 1.92, 4.76, 20.9, 60, 135, 328, 1670))

Ratings <- data.frame("ratingId"=rep(10, 6),
                         "datetime"=as.POSIXct(c("2013-10-29 12:37:00",
                                                 "2013-11-23 04:00:00",
                                                 "2014-01-14 13:15:00",
                                                 "2014-06-05 06:14:00",
                                                 "2014-06-22 02:30:00",
                                                 "2014-08-15 10:07:00")),
                         "curveId"=c(rep(1L, 2), rep(2L, 2), rep(3L, 2)))

Curves <- list(list("x"=c(-0.04, -0.04, 0), "y"=c(1.74, 1.92, 2.7)), 
               list("x"=c(-0.07, -0.05, 0), "y"=c(1.73, 2.78, 3.6)),
               list("x"=c(-0.09, -0.05, 0), "y"=c(1.9, 2.78, 3.6)))

HistoricalFieldVisits <- data.frame(id=c(), stage=c(), discharge=c(), quality=c())
MeasuredFieldVisits <- data.frame(id=c(1,2,3,4,5), stage=c(1.8, 2.78, 1.96, 1.73, 1.9), discharge=c(7.82, 99, 16.05, 4.45, 10.2), quality=c("fair", "fair", "fair", "fair", "fair"))

set_up_plot <- function() {
  xaxis = c(-0.5, 0.5)
  yaxis = c(1.5,4.5)
  mgp = c(1.25,0.15,0)
  
  # main plot area
  plot(type="n", x=NA, y=NA, xlim=xaxis, ylim=yaxis, xlab="Shift, in feet", ylab="Stage in feet", xaxt="n", yaxt="n", mgp=mgp)
  actualX = c(par("usr")[1], par("usr")[2])
  actualY = c(par("usr")[3], par("usr")[4])
  
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

# popColor <- function() {
#   tmp <- colrs[1]
#   colrs <<- colrs[-1]
#   return(tmp)
# }

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

shifts <- calcShifts(MeasuredFieldVisits, BaseRatingTable)

Historical <- data.frame(x=c(), y=c(), xlb=c(), xub=c())
Measured <- data.frame(x=shifts$shift, y=shifts$stage, xlb=shifts$lb, xub=shifts$ub)

colrs <- c("blue", "red", "green", "orange")

#'@export
mkPNG <- function() {
  
  #png(filename="figure/vdiagram.png", width=6, height=6.5, units="in", res=300)
  par(omi=c(0,0,0,0))
  set_up_plot()

  for (i in 1:length(Curves)) {
    add_series(Curves[[i]], color = colrs[i])
  }
  
  #add_ratings(Measured, "black")
  #add_ratings(Historical, "blue")
  #dev.off()
}