
getUvStyle <- function(data, info, x, y, approvalInfo, correctionLabels, plotName) {
  if (plotName == "primary") { 
    primary_lbl <- info$primary_lbl
    styles <- switch(names(data),
                corr_UV = list(x=x, y=y, col="black", lty=1, legend.name=paste("Corrected UV", primary_lbl), axes=FALSE),
                est_UV = list(x=x, y=y, col="orange", lty=4, lwd=2, legend.name=paste("Estimated UV", primary_lbl)),
                uncorr_UV = list(x=x, y=y, col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", primary_lbl), axes=FALSE),
                comp_UV = list(x=x, y=y, col="green", lty=1, legend.name=paste("Comparison", primary_lbl)), 
                water_qual = list(x=x, y=y, col="orange", pch=8, bg="orange", cex=1.2, legend.name="NWIS-RA WQ Measurement"),
                max_DV = list(x=approvalInfo$x, y=approvalInfo$y, pch=24, cex=1, col=approvalInfo$col, bg=approvalInfo$bg, legend.name=paste(approvalInfo$legend.name, "DV Max", primary_lbl)),
                mean_DV = list(x=approvalInfo$x, y=approvalInfo$y, pch=21, cex=1, col=approvalInfo$col, bg=approvalInfo$bg, legend.name=paste(approvalInfo$legend.name, "DV Mean", primary_lbl)),
                median_DV = list(x=approvalInfo$x, y=approvalInfo$y, pch=26, cex=1, col=approvalInfo$col, bg=approvalInfo$bg, legend.name=paste(approvalInfo$legend.name, "DV Median", primary_lbl)),
                min_DV = list(x=approvalInfo$x, y=approvalInfo$y, pch=25, cex=1, col=approvalInfo$col, bg=approvalInfo$bg, legend.name=paste(approvalInfo$legend.name, "DV Min", primary_lbl)),
                series_corr = list(abline=list(v=x, untf=FALSE, col="blue", legend.name="Data correction entry"),
                                   text=list(x=x, y=correctionLabels$y, label=correctionLabels$label, pos=4, col="blue")),  
                meas_Q = list(error_bar=list(x=x, y=y, y.low=data$meas_Q$minQ, y.high=data$meas_Q$maxQ, col="black", lwd=0.7, epsilon=0.1, legend.name="Discharge measurement and error"),
                              points=list(x=x, y=y, pch = 21, bg = 'black', col = 'black', cex = .8, axes=FALSE),
                              callouts=list(x=x, y=y, labels = data$meas_Q$n, cex = .75, col='red', length = 0.05, angle = 30)),
                UV_series = list(x=approvalInfo$x, y=approvalInfo$y, type='l', pch=15, col=approvalInfo$col, cex=2, lwd=25, bg=approvalInfo$bg, legend.name=paste(approvalInfo$legend.name, "UV", primary_lbl))
                )
  }
  
  if (plotName == "secondary"){
    secondary_lbl <- info$secondary_lbl
    styles <- switch(names(data),
                corr_UV2 = list(x=x,y=y, col="black", lty=1, legend.name=paste("Corrected UV", secondary_lbl), axes=FALSE), 
                est_UV2 = list(x=x,y=y, col="orange", lty=2, lwd=2, legend.name=paste("Estimated UV", secondary_lbl)),
                uncorr_UV2 = list(x=x,y=y, col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", secondary_lbl)),
                series_corr2 = list(abline=list(v=x, untf = FALSE, col="blue", legend.name="Data Correction Entry"),
                                    text=list(x=x, y=correctionLabels$y, label=correctionLabels$label, pos=4, col="blue")),
                effect_shift = list(lines=list(x=x,y=y, type='l', col = 'green3', lty = 1, lwd=2, legend.name=paste(secondary_lbl, info$tertiary_lbl)),
                                    text=list(x=x[1], y=y[1], labels="")),
                gage_height = list(points=list(x=x, y=y, pch=21, bg='black', col='black', cex=.8, legend.name="Gage height measurement", axes=FALSE),
                                   text=list(x=x, y=y, labels=data$gage_height$n, pos=4)),
                gw_level = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, legend.name="Measured Water Level (NWIS-RA)", axes=FALSE), 
                meas_shift = list(points=list(x=x, y=y, pch=21, bg='green', col='green', cex=1, legend.name="Effective shift and error", axes=FALSE),
                                  error_bar=list(x=x, y=y, y.low=y, y.high=y, col='green', lwd=.7)),
                ref_readings = list(points=list(x=x, y=y, col='darkgreen', pch=13, cex=1, legend.name="Reference Readings"), 
                                     error_bar=list(x=x, y=y, y.low=data$ref_readings$uncertainty, y.high=data$ref_readings$uncertainty, col='black', lwd=.7)),
                csg_readings = list(points=list(x=x, y=y, col='blue', pch=8, cex=1, legend.name="Crest Stage Gage Readings"), 
                                     error_bar=list(x=x, y=y, y.low=data$csg_readings$uncertainty, y.high=data$csg_readings$uncertainty, col='blue', lwd=.7)),
                hwm_readings = list(points=list(x=x, y=y, col='red', pch=10, cex=1, legend.name="High Water Mark Readings"), 
                                     error_bar=list(x=x, y=y, y.low=data$hwm_readings$uncertainty, y.high=data$hwm_readings$uncertainty, col='blue', lwd=.7))
                )
  } 
  
  return(styles)
}

getPlotType <- function(data, plotName) {
  if (plotName == "primary") {   
    plotTypes <- switch(names(data),
                  corr_UV = "lines",
                  est_UV = "lines",
                  uncorr_UV = "lines",
                  comp_UV = "lines",
                  water_qual = "points",
                  max_DV = "points",
                  mean_DV = "points",
                  median_DV = "points",
                  min_DV = "points",
                  series_corr = c("abline", "text"),  
                  meas_Q = c("error_bar", "points", "callouts"),
                  UV_series = "points"
    )
  }
  
  if (plotName == "secondary") {   
    plotTypes <- switch(names(data),
                  corr_UV2 = "lines",
                  est_UV2 = "lines",
                  uncorr_UV2 = "lines",
                  series_corr2 = c("abline" ,"text"),
                  effect_shift = c("lines", "text"),
                  gage_height = c("points", "text"),
                  gw_level = "points",
                  meas_shift = c("points", "error_bar"),
                  ref_readings = c("points", "error_bar"),
                  csg_readings = c("points", "error_bar"),
                  hwm_readings = c("points", "error_bar")
    )
  }
  
  return(plotTypes)
  
}


#  uvhydrographPlot <- function(data){

#    for (month in months){  


      ###### second plot
 
#    } # month for loop  
    
    ###################### printing
    #!!hackalert!! we are essentially taking out and putting back the approvals line so it ends up at the bottom z level of the plot
#     ylim<-gsplot:::calc_views(uvhplot)$window$ylim
#     series_appr_pts <- list(x=uv_pts$x, y=rep(ylim[1],nrow(uv_pts)), type="l", pch=15, col=NULL, cex=2, lwd=25, bg=NULL, legend.name=paste("UV", primary_lbl))
#     uvhplot <- plotting_appr(uvhplot, series_appr_pts, uv_appr, label=primary_lbl, name="UV", limits=uv_lims)
#     par(mar=c(7, 3, 4, 2))
#     uv <- append(uvhplot[length(uvhplot)], uvhplot)
#     uv[[length(uv)]]<-NULL
#     class(uv) <- "gsplot"
#     print(uv)
#     print(sec_uvhplot)
#     return(corrections_table)  
    
    ############################ stopped here 
    
 # } # end UVhydrograph plot function

  
  
  

