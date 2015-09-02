getVDiagramStyle <- function() {
    styles=list(
      plot=list(xlab="Shift, in feet",ylab="Stage, in feet"),
      callouts=list(x=c(0,0), labels="", col = 'red', lwd = 3, angle=0, legend.name="Max and min gage height for the period shown"),
      grid=list(lty = "dotted"),
      axis=list(side=c(2,4)),
      err_lines=list(angle=90, lwd=1.25, code=3, col = 'black', length=0.1),
      err_points=list(pch = 21, bg = 'white', legend.name="Historical measurements from the last 2 years"),
      err_lines_historic=list(angle=90, lwd=1.25, code=3, col = 'blue', length=0.05),
      err_points_historic=list(pch = 21, bg = 'black', col = 'black', cex = 0.7),
      rating_shifts=list(extendStageBy=0.5, curve_pch=8, callout_cex = 0.5,
                         from_segment=list(lwd=1.5, pch=8, angle=30, code=1, length=0.1),
                         to_segment=list(lwd=1.5, pch=8, angle=30, code=2, length=0.1),
                         shift_segment=list(lwd=1.5, pch=8))
    )
    
    return(styles)
}

