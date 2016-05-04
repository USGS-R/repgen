getDvStyle <- function(data, info = NULL, ...){
  x <- data[[1]]$time
  y <- data[[1]]$value
  legend.name <- data[[1]]$legend.name
  args <- list(...)
  
  styles <- switch(names(data), 
                   stat1 = list(lines = list(x=x, y=y, type="s", col="blue", pch=20, cex=0.5, legend.name=legend.name)),
                   stat2 = list(lines = list(x=x, y=y, type="s", col="maroon", pch=20, cex=0.5, legend.name=legend.name)),
                   stat3 = list(lines = list(x=x, y=y, type="s", col="orange", pch=20, cex=0.5, legend.name=legend.name)),
                   comp = list(lines = list(x=x, y=y, type="s", col="green", pch=20, cex=0.5, legend.name=legend.name)),
                   est_stat1 = list(lines = list(x=x, y=y, type="s", col="red1", lty=2, pch=20, cex=0.5, legend.name=legend.name)),
                   est_stat2 = list(lines = list(x=x, y=y, type="s", col="red2", lty=3, pch=20, cex=0.5, legend.name=legend.name)),
                   est_stat3 = list(lines = list(x=x, y=y, type="s", col="red3", lty=6, pch=20, cex=0.5, legend.name=legend.name)),
                   est_comp = list(lines = list(x=x, y=y, type="s", col="red4", lty=6, pch=20, cex=0.5, legend.name=legend.name)),
                   secondary_ref = list(lines = list(x=x, y=y, type="s", col="blue", lty=1, lwd=1, legend.name=legend.name)),
                   tertiary_ref = list(lines = list(x=x, y=y, type="s", col="orange", lty=1, lwd=1, legend.name=legend.name)),
                   quaternary_ref = list(lines = list(x=x, y=y, type="s", col="purple", lty=1, lwd=1, legend.name=legend.name)),
                
                   meas_Q = list(points=list(x=x, y=y, pch = 21, bg = 'black', col = 'black', cex = .8, lwd=1, axes=FALSE),
                              callouts=list(x=x, y=y, labels = data$meas_Q$n, cex = .75, col='red', length = 0.05)),
                   
                   gw_level = list(points = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (NWIS-RA)")),
                   
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=paste(args$maxLabel, info$type, ":", y))),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=paste(args$minLabel, info$type, ":", y))), 
                   
                   appr_approved = list(points = list(x=x, y=y, col="lightskyblue", type='l', lwd=15, bg="lightskyblue", legend.name=legend.name)),
                   appr_inreview = list(points = list(x=x, y=y, col="yellow2", type='l', lwd=15, bg="yellow2", legend.name=legend.name)),
                   appr_working = list(points = list(x=x, y=y, col="lightpink", type='l', lwd=15, bg="lightpink", legend.name=legend.name)))
  
  return(styles)
}
