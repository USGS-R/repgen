getFiveyearStyle <- function(data) {

    x <- data[[1]]$time
    y <- data[[1]]$value
    styles <- switch(names(data), 
                     stat = list(lines = list(x=x, y=y, type="o", col="black", pch=20, cex=0.5, lwd=0.8, legend.name=data[[1]]$legend.name)),
                     est_stat = list(lines = list(x=x, y=y, col="red", lty=2, legend.name=data[[1]]$legend.name)),
                     max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name=paste("Max. Instantaneous Water Level:", y)),
                                   callouts = list(x=x, y=y, labels = y)),
                     min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name=paste("Min. Instantaneous Water Level:", y)),
                                   callouts = list(x=x, y=y, labels = y)),
                     appr_approved = list(points = list(x=x, y=y, col="lightcyan", type='l', pch=15, cex=2, lwd=25, 
                                                        bg="lightcyan", legend.name="Approved")),
                     appr_inreview = list(points = list(x=x, y=y, col="yellow2", type='l', pch=15, cex=2, lwd=25, 
                                                        bg="yellow2", legend.name="In-Review")),
                     appr_working = list(points = list(x=x, y=y, col="lightpink", type='l', pch=15, cex=2, lwd=25, 
                                                       bg="lightpink", legend.name="Working"))
                     )
    
    return(styles)
}