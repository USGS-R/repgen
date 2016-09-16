
getUvStyle <- function(data, info, correctionLabels, plotName) {
  x <- data[[1]]$time
  y <- data[[1]]$value
  
  comp_lbl <- info$comp_UV_lbl
  comp_type <- info$comp_UV_type
  legend.name <- data[[1]]$legend.name

  if (plotName == "primary") { 
    primary_lbl <- info$primary_lbl
    reference_lbl <- info$reference_lbl
    styles <- switch(names(data),
                corr_UV = list(lines = list(x=x, y=y,ylim=YAxisInterval(data$corr_UV$value, data$uncorr_UV$value),ylab=primary_lbl, col="black", lty=1, legend.name=paste("Corrected UV", primary_lbl))),
                est_UV = list(lines = list(x=x, y=y, col="orange", lty=4, lwd=2, legend.name=paste("Estimated UV", primary_lbl))),
                uncorr_UV = list(lines = list(x=x, y=y, col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", primary_lbl))),
                comp_UV = list(lines = list(x=x, y=y, col="green", lty=1, legend.name=paste("Comparison", comp_type,"@", comp_lbl))), 
                series_corr = list(abline=list(v=x, untf=FALSE, col="blue", legend.name=paste("Data correction entry", primary_lbl)),
                                   text=list(x=x, y=correctionLabels$y, label=correctionLabels$label, pos=4, col="blue")), 

                corr_UV_Qref = list(lines = list(x=x,y=y,side=4,ylim=YAxisInterval(data$corr_UV_Qref$value, data$uncorr_UV_Qref$value),ylab=reference_lbl, col="gray30", lty=1, legend.name=paste("Corrected UV", reference_lbl))),
                est_UV_Qref = list(lines = list(x=x,y=y,side=4, col="violetred", lty=2, lwd=2, legend.name=paste("Estimated UV", reference_lbl))),

                water_qual = list(points = list(x=x, y=y, col="orange", pch=8, bg="orange", cex=1.2, lwd=1, legend.name="NWIS-RA WQ Measurement")), 
                meas_Q = list(error_bar=list(x=x, y=y, y.low=(y-data$meas_Q$minQ), y.high=(data$meas_Q$maxQ-y), col="black", lwd=0.7, epsilon=0.1, legend.name="Discharge measurement and error"),
                              points=list(x=x, y=y, pch = 21, bg = 'black', col = 'black', cex = .8, lwd=1),
                              callouts=list(x=x, y=y, labels = data$meas_Q$n, cex = .75, col='red', length = 0.05)),

                appr_approved_dv = list(points = list(x=x, y=y, col="black", pch=data[[1]]$point_type, bg="lightskyblue", legend.name=legend.name)),
                appr_inreview_dv = list(points = list(x=x, y=y, col="black", pch=data[[1]]$point_type, bg="yellow2", legend.name=legend.name)),
                appr_working_dv = list(points = list(x=x, y=y, col="black", pch=data[[1]]$point_type, bg="lightpink", legend.name=legend.name))
                )
  }
  
  if (plotName == "secondary"){
    secondary_lbl <- info$secondary_lbl
    styles <- switch(names(data),
                corr_UV2 = list(lines = list(x=x,y=y, col="gray30", lty=1, legend.name=paste("Corrected UV", secondary_lbl))), 
                est_UV2 = list(lines = list(x=x,y=y, col="violetred", lty=2, lwd=2, legend.name=paste("Estimated UV", secondary_lbl))),
                uncorr_UV2 = list(lines = list(x=x,y=y, col="palegreen2", lty=4, legend.name=paste("Uncorrected UV", secondary_lbl))),
                series_corr2 = list(abline=list(v=x, untf=FALSE, col="blue", legend.name=paste("Data correction entry", secondary_lbl)),
                                   text=list(x=x, y=correctionLabels$y, label=correctionLabels$label, pos=4, col="blue")),  
                
                effect_shift = list(lines=list(x=x,y=y, type='l', col = 'green3', lty = 1, lwd=2, side=4, legend.name=paste(secondary_lbl, info$tertiary_lbl)),
                                    text=list(x=x[1], y=y[1], labels="", side=4)),
                gage_height = list(points=list(x=x, y=y, pch=21, bg='black', col='black', cex=.8, lwd=1, legend.name="Gage height measurement"),
                                   callouts=list(x=x, y=y, labels=data$gage_height$n)),
                gw_level = list(points = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (NWIS-RA)")), 
                meas_shift = list(points=list(x=x, y=y, pch=21, bg='green', col='green', cex=1, lwd=1, side=4, legend.name="Effective shift and error"),
                                  error_bar=list(x=x, y=y, y.low=(y-data$meas_shift$minShift), y.high=(data$meas_shift$maxShift-y), col='green', lwd=.7, side=4)),
                ref_readings = list(points=list(x=x, y=y, col='darkgreen', pch=13, cex=1, lwd=1, legend.name="Reference Readings"), 
                                    error_bar=list(x=x, y=y, y.low=data$ref_readings$uncertainty, y.high=data$ref_readings$uncertainty, col='black', lwd=.7)),
                csg_readings = list(points=list(x=x, y=y, col='blue', pch=8, cex=1, lwd=1, legend.name="Crest Stage Gage Readings"), 
                                    error_bar=list(x=x, y=y, y.low=data$csg_readings$uncertainty, y.high=data$csg_readings$uncertainty, col='blue', lwd=.7)),
                hwm_readings = list(points=list(x=x, y=y, col='red', pch=10, cex=1, lwd=1, legend.name="High Water Mark Readings"), 
                                    error_bar=list(x=x, y=y, y.low=data$hwm_readings$uncertainty, y.high=data$hwm_readings$uncertainty, col='blue', lwd=.7)))
  } 
  
  return(styles)
}
