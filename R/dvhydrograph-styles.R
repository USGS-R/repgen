

getDvStyle <- function(data, x, y){
  primary_lbl <- ""
  styles <- switch(names(data), 
                   measurements = list(points = list(x=x, y=y, col="red", pch='O', cex=1, legend.name="Measurements"),
                                       text = list(x=x, y=y, col="red", pos=3, label=labels)),
                   comp_dv = list(lines = list(x=x, y=y, col="orange", type='o', legend.name="Computed Daily Values")),
                   est_dv = list(lines = list(x=x, y=y, col="red", type='o', legend.name="Computed Daily Values")),
                   final_dv = list(),
                   ref_dv = list(),
                   max_iv = list(points = list(x=x, y=y, col="red", pch=8, cex=2, legend.name="Max. Instantaneous Discharge"),
                                 callouts = list(x=x, y=y, labels = y)),
                   min_iv = list(points = list(x=x, y=y, col="blue", pch=8, cex=2, legend.name="Min. Instantaneous Discharge"),
                                 callouts = list(x=x, y=y, labels = y)))
  
  return(styles)
}


