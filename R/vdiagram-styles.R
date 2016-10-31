getVDiagramStyle <- function() {
    styles=list(
      plot=list(xlab="Shift, in feet",ylab="Stage, in feet"),
      maxStageLine=list(b=0, labels="", col = 'red', lwd = 3, legend.name="Max gage height for the period shown"),
      minStageLine=list(b=0, labels="", col = 'red', lwd = 3, legend.name="Min gage height for the period shown"),
      grid=list(lty = "dotted"),
      ablines=list(lty = "dotted"),
      abline_dark = list(col='gray82',lwd=1),
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

