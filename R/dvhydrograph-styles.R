getDvStyle <- function(data){
  x <- data[[1]]$time
  y <- data[[1]]$value
  styles <- switch(names(data), 
                   stat1 = list(lines = list(x=x, y=y, type="o", col="blue", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat2 = list(lines = list(x=x, y=y, type="o", col="maroon", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat3 = list(lines = list(x=x, y=y, type="o", col="orange", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   est_stat1 = list(lines = list(x=x, y=y, col="red", lty=2, legend.name=data[[1]]$legend.name)),
                   est_stat2 = list(lines = list(x=x, y=y, col="red", lty=3, legend.name=data[[1]]$legend.name)),
                   est_stat3 = list(lines = list(x=x, y=y, col="red", lty=6, legend.name=data[[1]]$legend.name)),
                   secondary_ref = list(lines = list(x=x, y=y, col="blue", lty=1, lwd=1, legend.name="")),
                   tertiary_ref = list(lines = list(x=x, y=y, col="orange", lty=1, lwd=1, legend.name="")),
                   quaternary_ref = list(lines = list(x=x, y=y, col="purple", lty=1, lwd=1, legend.name="")),
                   
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=paste("Max. Instantaneous Discharge:", y)),
                                 callouts = list(x=x, y=y, labels = y)),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=paste("Min. Instantaneous Discharge:", y)),
                                 callouts = list(x=x, y=y, labels = y)))
  
  return(styles)
}
