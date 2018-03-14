#' Get UV Styles
#' @description Get styling and label information for UV hydrograph report elements
#' @return list of named styling elements
#' @importFrom grDevices rgb
getUvStyles <- function() {
  styles=list(
      #raw signal should be green
      uncorr_UV_lbl="Uncorrected UV",
      uncorr_UV_lines=list(col="green", lty=4),

      #corrected signal should be blue and laid down after raw
      corr_UV_lbl="Corrected UV",
      corr_UV_lines=list(ann=TRUE, col="blue", lty=1),
      corr_UV2_lines=list(col="blue", lty=1),
      
      #estimated ts should be red like dv hydro
      est_UV_lbl="Estimated UV",
      est_UV_lines=list(col="red", lty=4, lwd=2),
      est_UV2_lines=list(col="red", lty=2, lwd=2),
      est_UV_Qref_lbl="Estimated UV",
      est_UV_Qref_lines=list(col="red", lty=2, lwd=2),
      
      #comparison ts
      comp_UV_lines=list(col="lightsalmon", lty=1),
      est_comp_UV_lines=list(col="lightsalmon", lty=4, lwd=2),
      
      #data correction entries
      corrections_lines=list(side=7, axes=FALSE),
      corrections_correction_lbl="Data correction entry",
      corrections_ablines=list(untf=FALSE, col="blue", side=7, axes=FALSE),
      corrections_arrows=list(side=7, axes=FALSE, col="blue", code=1, length = 0),
      corrections_points=list(side=7, axes=FALSE, pch=22, col=rgb(0,0,255,180,maxColorValue=255), bg=rgb(255,255,255,125,maxColorValue=255)),
      corrections_text=list(srt=0, cex=0.6, side=7, axes=FALSE, pos=1, offset = -0.12, col=rgb(0,0,255,240,maxColorValue=255)),
      
      #rating shifts
      ratingShifts_lines=list(side=7, axes=FALSE),
      ratingShifts_lbl="Rating shift entry",
      ratingShifts_ablines=list(untf=FALSE, col="magenta", side=7, axes=FALSE),
      ratingShifts_arrows=list(side=7, axes=FALSE, col="magenta", code=1, length = 0),
      ratingShifts_points=list(side=7, axes=FALSE, pch=22, col=rgb(255,0,255,180,maxColorValue=255), bg=rgb(255,255,255,125,maxColorValue=255)),
      ratingShifts_text=list(srt=0, cex=0.6, side=7, axes=FALSE, pos=1, offset = -0.12, col=rgb(255,0,255,240,maxColorValue=255)),
      
      #effective shift measurements should not be green
      effect_shift_lines=list(type='l', col = 'violet', lty = 1, lwd=1, side=4),
      effect_shift_text=list(labels="", side=4),
      meas_shift_points=list(pch=21, bg='violet', col='violet', cex=1, lwd=1, side=4, legend.name="Effective shift and error limits"),
      meas_shift_error_bars=list(col='violet', lwd=.7, side=4),
      
      #reference time series 
      corr_UV_Qref_lbl="Corrected UV",
      corr_UV_Qref_lines=list(ann=TRUE, col="gray65", lty=1),
      
      #water quality measurements
      water_qual_points=list(col="orange", pch=8, bg="orange", cex=1.2, lwd=1, legend.name="Measured Value (QWDATA)"),
      
      #discharge measurements
      meas_Q_error_bars=list(col="black", lwd=0.7, epsilon=0.1),
      meas_Q_points=list(pch = 21, bg = 'black', col = 'black', cex = .8, lwd=1, legend.name="Measured discharge and error limits"),
      meas_Q_callouts=list(cex = .75, col='red', length = 0.05),
      
      #reference readings
      ref_readings_points=list(col='darkgreen', pch=13, cex=1, lwd=1, legend.name="Reference Readings"),
      ref_readings_error_bars=list(col='black', lwd=.7),
      
      #crest stage gage readings
      csg_readings_points=list(col='blue', pch=8, cex=1, lwd=1, legend.name="Extreme - Max Reading"),
      csg_readings_error_bars=list(col='blue', lwd=.7),
      
      #high water mark readings 
      hwm_readings_points=list(col='red', pch=10, cex=1, lwd=1, legend.name="High Water Mark Readings"),
      hwm_readings_error_bars=list(col='red', lwd=.7),
      
      #approval points
      approved_dv_points=list(col="black", bg="#228B22"),
      analyzed_dv_points=list(col="black", bg="#FFD700"),
      working_dv_points=list(col="black", bg="#DC143C"),
      
      #gage height measurements
      gage_height_points=list(pch=21, bg='black', col='black', cex=.8, lwd=1, legend.name="Measurement gage height"),
      
      #ground water
      gw_level_points = list(pch = 8, bg = 'orange', col = 'orange', cex = 1.2, lwd=1, legend.name="Measured Water Level (GWSI)")
      
  )
  
  return(styles)
}

