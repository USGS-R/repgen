getFiveyearStyle <- function(data, info=NULL, ...) {

    x <- data[[1]]$time
    y <- data[[1]]$value
    legend.name <- data[[1]]$legend.name
    args <- list(...)
    
    styles <- switch(names(data), 
                     stat = list(lines = list(x=x, y=y, type="o", col="black", pch=20, cex=0.5, lwd=0.8, legend.name=legend.name)),
                     est_stat = list(lines = list(x=x, y=y, col="red", lty=2, legend.name=legend.name)),
                     max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=paste(args$maxLabel, info$type, ":", y))),
                     min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=paste(args$minLabel, info$type, ":", y))),
                     gw_level = list(points = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (NWIS-RA)")),
                     appr_approved = list(points = list(x=x, y=y, col="lightcyan", type='l', lwd=15, bg="lightcyan", legend.name=legend.name)),
                     appr_inreview = list(points = list(x=x, y=y, col="yellow2", type='l', lwd=15, bg="yellow2", legend.name=legend.name)),
                     appr_working = list(points = list(x=x, y=y, col="lightpink", type='l', lwd=15, bg="lightpink", legend.name=legend.name))
                     )
    
    return(styles)
}