

getDvStyle <- function(data){
  x <- data[[1]]$time
  y <- data[[1]]$value
  styles <- switch(names(data), 
<<<<<<< HEAD
                   
                   measurements = list(points = list(x=x, y=y, col="red", pch='O', cex=1, legend.name="Measurements"),
                                       text = list(x=x, y=y, col="red", pos=3, label=labels)),
                   #comp_dv = list(lines = list(x=x, y=y, col="orange", type='o', legend.name="Computed Daily Values")),
                   
                   est_dv_first = list(lines = list(x=x, y=y, col="black", type='o', legend.name="Computed Daily Values")),
                   est_dv_second = list(lines = list(x=x, y=y, col="darkgreen", type='o', legend.name="Computed Daily Values")),
                   est_dv_third = list(lines = list(x=x, y=y, col="orange", type='o', legend.name="Computed Daily Values")),
                   
                   first_dv = list(lines = list(x=x, y=y, col="black", type='l', lty=1, lwd=1, legend.name="First Down Chain Daily Values")), 
                   second_dv = list(lines = list(x=x, y=y, col="darkgreen", type='l', lty=1, lwd=1, legend.name="Second Down Chain Daily Values")), 
                   third_dv = list(lines = list(x=x, y=y, col="orange", type='l', lty=1, lwd=1, legend.name="Third Down Chain Daily Values")), 
                   
                   secondary_ref = list(lines = list(x=x, y=y, col="blue", type='l', lty=1, lwd=1, legend.name="Secondary Reference Time Series")),
                   tertiary_ref = list(lines = list(x=x, y=y, col="orange", type='l', lty=1, lwd=1, legend.name="Tertiary Reference Time Series")),
                   quaternary_ref = list(lines = list(x=x, y=y, col="purple", type='l', lty=1, lwd=1, legend.name="Quaternary Reference Time Series")),
                   
=======
                   stat1 = list(lines = list(x=x, y=y, type="o", col="blue", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat2 = list(lines = list(x=x, y=y, type="o", col="maroon", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   stat3 = list(lines = list(x=x, y=y, type="o", col="orange", pch=20, cex=0.5, legend.name=data[[1]]$legend.name)),
                   est_stat1 = list(lines = list(x=x, y=y, col="red", lty=2, legend.name=data[[1]]$legend.name)),
                   est_stat2 = list(lines = list(x=x, y=y, col="red", lty=3, legend.name=data[[1]]$legend.name)),
                   est_stat3 = list(lines = list(x=x, y=y, col="red", lty=6, legend.name=data[[1]]$legend.name)),
>>>>>>> 26265eb95c2b8f03755136f03651091e0508fdf0
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name="Max. Instantaneous Discharge"),
                                 callouts = list(x=x, y=y, labels = y, angle=235)),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name="Min. Instantaneous Discharge"),
<<<<<<< HEAD
                                 callouts = list(x=x, y=y, labels = y)))
                   
=======
                                 callouts = list(x=x, y=y, labels = y))
                   )
>>>>>>> 26265eb95c2b8f03755136f03651091e0508fdf0
  
  return(styles)
}


