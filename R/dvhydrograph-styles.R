getDvStyle <- function(data, info = NULL, ...){
  x <- data[[1]]$time
  y <- data[[1]]$value
  legend.name <- data[[1]]$legend.name
  args <- list(...)
  
  styles <- switch(names(data), 
                   stat1Timeseries = list(lines = list(x=x, y=y, type="s", col="blue", pch=20, cex=0.5, legend.name=legend.name)),
                   stat2Timeseries = list(lines = list(x=x, y=y, type="s", col="maroon", pch=20, cex=0.5, legend.name=legend.name)),
                   stat3Timeseries = list(lines = list(x=x, y=y, type="s", col="orange", pch=20, cex=0.5, legend.name=legend.name)),
                   comparisonTimeseries = list(lines = list(x=x, y=y, type="s", col="green", pch=20, cex=0.5, legend.name=legend.name)),
                   stat1TimeseriesEst = list(lines = list(x=x, y=y, type="s", col="red1", lty=2, pch=20, cex=0.5, legend.name=legend.name)),
                   stat2TimeseriesEst = list(lines = list(x=x, y=y, type="s", col="red2", lty=3, pch=20, cex=0.5, legend.name=legend.name)),
                   stat3TimeseriesEst = list(lines = list(x=x, y=y, type="s", col="red3", lty=6, pch=20, cex=0.5, legend.name=legend.name)),
                   comparisonTimeseriesEst = list(lines = list(x=x, y=y, type="s", col="red4", lty=6, pch=20, cex=0.5, legend.name=legend.name)),
                   estimated1Edges = list(arrows = list(x0=data[[1]]$time, x1=data[[1]]$time, y0=data[[1]]$y0, y1=data[[1]]$y1, lwd=1, lty=ifelse(data[[1]]$newSet == "est", 1, 2), col=ifelse(data[[1]]$newSet == "est", "blue", "red1"), code=1, length = 0)),
                   estimated2Edges = list(arrows = list(x0=data[[1]]$time, x1=data[[1]]$time, y0=data[[1]]$y0, y1=data[[1]]$y1, lwd=1, lty=ifelse(data[[1]]$newSet == "est", 1, 3), col=ifelse(data[[1]]$newSet == "est", "maroon", "red2"), code=1, length = 0)),
                   estimated3Edges = list(arrows = list(x0=data[[1]]$time, x1=data[[1]]$time, y0=data[[1]]$y0, y1=data[[1]]$y1, lwd=1, lty=ifelse(data[[1]]$newSet == "est", 1, 6), col=ifelse(data[[1]]$newSet == "est", "orange", "red3"), code=1, length = 0)),
                   comparisonEdges = list(arrows = list(x0=data[[1]]$time, x1=data[[1]]$time, y0=data[[1]]$y0, y1=data[[1]]$y1, lwd=1, lty=ifelse(data[[1]]$newSet == "est", 1, 6), col=ifelse(data[[1]]$newSet == "est", "green", "red4"), code=1, length = 0)),
                   secondaryRefTimeSeries = list(lines = list(x=x, y=y, type="s", col="blue", lty=1, lwd=1, legend.name=legend.name)),
                   tertiaryRefTimeSeries = list(lines = list(x=x, y=y, type="s", col="orange", lty=1, lwd=1, legend.name=legend.name)),
                   quaternaryRefTimeSeries = list(lines = list(x=x, y=y, type="s", col="purple", lty=1, lwd=1, legend.name=legend.name)),
                   secondaryRefTimeSeriesEst = list(lines = list(x=x, y=y, type="s", col="red1", lty=2, lwd=1, legend.name=legend.name)),
                   tertiaryRefTimeSeriesEst = list(lines = list(x=x, y=y, type="s", col="red2", lty=3, lwd=1, legend.name=legend.name)),
                   quaternaryRefTimeSeriesEst = list(lines = list(x=x, y=y, type="s", col="red3", lty=6, lwd=1, legend.name=legend.name)),
                   
                   meas_Q = list(points=list(x=x, y=y, pch = 21, bg = 'black', col = 'black', cex = .8, lwd=1, axes=FALSE, legend.name="Measured Discharge"),		
                   callouts=list(x=x, y=y, labels = data$meas_Q$n, cex = .75, col='red', length = 0.05)),
                   
                   gw_level = list(points = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (GWSI)")),
                   
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=legend.name)),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=legend.name)))
  
  return(styles)
}
