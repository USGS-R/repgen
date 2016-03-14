getDvStyle <- function(data, info = NULL, ...){
  x <- data[[1]]$time
  y <- data[[1]]$value
  args <- list(...)
  
  styles <- switch(names(data), 
                   stat1 = list(lines = list(x=x, y=y, type="s", col="blue", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat2 = list(lines = list(x=x, y=y, type="s", col="maroon", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat3 = list(lines = list(x=x, y=y, type="s", col="orange", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   est_stat1 = list(lines = list(x=x, y=y, type="s", col="red", lty=2, pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   est_stat2 = list(lines = list(x=x, y=y, type="s", col="red", lty=3, pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   est_stat3 = list(lines = list(x=x, y=y, type="s", col="red", lty=6, pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   secondary_ref = list(lines = list(x=x, y=y, type="s", col="blue", lty=1, lwd=1, legend.name=data[[1]]$legend.name)),
                   tertiary_ref = list(lines = list(x=x, y=y, type="s", col="orange", lty=1, lwd=1, legend.name=data[[1]]$legend.name)),
                   quaternary_ref = list(lines = list(x=x, y=y, type="s", col="purple", lty=1, lwd=1, legend.name=data[[1]]$legend.name)),
                   
                   gw_level = list(points = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (NWIS-RA)")),
                   
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=paste(args$maxLabel, info$type, ":", y))),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=paste(args$minLabel, info$type, ":", y))), 
                   
                   appr_approved = list(points = list(x=x, y=y, col="lightcyan", type='l', lwd=15, bg="lightcyan", legend.name="Approved")),
                   appr_inreview = list(points = list(x=x, y=y, col="yellow2", type='l', lwd=15, bg="yellow2", legend.name="In-Review")),
                   appr_working = list(points = list(x=x, y=y, col="lightpink", type='l', lwd=15, bg="lightpink", legend.name="Working")))
  
  return(styles)
}
