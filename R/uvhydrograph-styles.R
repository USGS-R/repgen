getUvStyle <- function(data, info, x, y, approvalInfo, plotName) {
  if (plotName == "primary") { 
    primary_lbl <- info$primary_lbl
    styles <- switch(names(data),
                corr_UV = list(x=x, y=y, type='l', col="black", lty=1, legend.name=paste("Corrected UV", primary_lbl), axes=FALSE),
                est_UV = list(x=x, y=y, type='l', col="orange", lty=4, lwd=2, legend.name=paste("Estimated UV", primary_lbl)),
                uncorr_UV = list(x=x, y=y, type='l', col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", primary_lbl), axes=FALSE),
                comp_UV = list(x=x, y=y, type='l', col="green", lty=1, legend.name=paste("Comparison", primary_lbl)), 
                water_qual = list(x=x, y=y, type='p', col="orange", pch=8, bg="orange", cex=1.2, legend.name="NWIS-RA WQ Measurement"),
                max_DV = list(x=x, y=y, type='p', pch=24, col=approalInfo$col, bg=approalInfo$bg, legend.name=paste(approalInfo$label, "DV Max", primary_lbl)),
                mean_DV = list(x=x, y=y, type='p', pch=21, col=approalInfo$col, bg=approalInfo$bg, legend.name=paste(approalInfo$label, "DV Mean", primary_lbl)),
                median_DV = list(x=x, y=y, type='p', pch=26, col=approalInfo$col, bg=approalInfo$bg, legend.name=paste(approalInfo$label, "DV Median", primary_lbl)),
                min_DV = list(x=x, y=y, type='p', pch=24, col=approalInfo$col, bg=approalInfo$bg, legend.name=paste(approalInfo$label, "DV Max", primary_lbl)),
                series_corr = list(abline=list(v=x, untf=FALSE, col="blue", legend.name="Data correction entry"),
                                   text=list(x=x, y=mean(info$lims_UV$ylim), label=as.Date(x), srt=90, col="red")),  
                meas_Q = list(error_bar=list(x=x, y=y, y.low=y-data$meas_Q$minQ, y.high=data$meas_Q$maxQ-y, col="black", lwd=0.7, epsilon=0.1, legend.name="Discharge measurement and error"),
                              points=list(x=x, y=y, pch = 21, bg = 'black', col = 'black', cex = .8, axes=FALSE),
                              callouts=list(x=x, y=y, labels = data$meas_Q$n, cex = .75, col='red', length = 0.05, angle = 30)),
                series_appr
                )
  }
  
  if (plotName == "secondary"){
    secondary_lbl <- info$secondary_lbl
    styles <- switch(names(data),
                corr_UV2 = list(x=x,y=y, type='l', col="black", lty=1, legend.name=paste("Corrected UV", secondary_lbl), axes=FALSE), 
                est_UV2 = list(x=x,y=y, type='l', col="orange", lty=2, lwd=2, legend.name=paste("Estimated UV", secondary_lbl)),
                uncorr_UV2 = list(x=x,y=y, type='l', col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", secondary_lbl)),
                series_corr2 = list(abline=list(v=x, untf = FALSE, col="blue", legend.name="Data Correction Entry"),
                                    text=list()),
                effect_shift = list(),
                gage_height = list(points=list(x=x, y=y, pch=21, bg='black', col='black', cex=.8, legend.name="Gage height measurement", axes=FALSE),
                                   text=list(x=x, y=y, labels=data$gage_height$n)),
                gw_level = list(x=x,y=y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, legend.name="Measured Water Level (NWIS-RA)", axes=FALSE), 
                meas_shift = list(error_bar=list(x=x, y=y,y.low=y, y.high=y, col='green', lwd=.7, legend.name="Effective shift and error"),
                                  points=list(x=x, y=y, pch=21, bg='green', col='green', legend.name="Effective shift and error", axes=FALSE))
                )
  } 
  
  return(styles)
}

getPlotType <- function(data, plotName) {
  if (plotName == "primary") {   
    plotTypes <- switch(names(data),
                  corr_UV = "points",
                  est_UV = "points",
                  uncorr_UV = "points",
                  comp_UV = "points",
                  water_qual = "points",
                  max_DV = "points",
                  mean_DV = "points",
                  median_DV = "points",
                  min_DV = "points",
                  series_corr = c("abline", "text"),  
                  meas_Q = c("error_bar", "points", "callouts")
    )
  }
  
  if (plotName == "secondary") {   
    plotTypes <- switch(names(data),
                  corr_UV2 = "points",
                  est_UV2 = "points",
                  uncorr_UV2 = "points",
                  series_corr2 = c("abline" ,"text"),
                  effect_shift = c("points"),
                  gage_height = c("points", "text"),
                  gw_level = "points",
                  meas_shift = c("error_bar", "points")
    )
  }
  
  return(plotTypes)
  
}

    
  
  
  #'@export
  uvhydrographPlot <- function(data){
    #for pagination by month, get all of the month strings for the primary series
    all_primary_pts <- getUvHydro(data, "primarySeries" )
    months <- unique(all_primary_pts$month, incomparables = FALSE)
    
    #breaking up plot according to time period
    
    for (month in months){  

      

      #plotting details for data

      ## DV mean, max, min, median
#       field <- list(mean="derivedSeriesMean", max="derivedSeriesMax", min="derivedSeriesMin", median="derivedSeriesMedian")
#       dv <- lapply(field, function(x) {
#         list <- list(dv_pts=subsetByMonth(getUvHydro(data, x), month),
#                      approvals=getApprovals(data, x)
#         )
#       })
#       

  

      
      
      #used for loop because lapply kept returning uvhplot as list within mean, min, max
      for (i in 1:length(dv_pts)){
        approvals <- dv[[i]][['approvals']]
        uvhplot <- plotting_appr(object=uvhplot, sublist=dv_pts[[i]], approvals, label=primary_lbl, name="DV", limits=uv_limits)
      }
      
      

      ###### second plot

      
      if(!is.null(secondary_corrections) && nrow(secondary_corrections)>0) {
      #  sec_uvhplot <- abline(sec_uvhplot, v=secondary_corrections$x, untf = FALSE, col="blue", legend.name="Data Correction Entry")
        y_positions <- rep(lims_UV2$ylim[2], nrow(secondary_corrections))
        differences <- as.numeric(diff(secondary_corrections$x))
        if(length(differences) > 0) {
          for (i in 1:length(differences)) {
            if(differences[i] < 86400) {y_positions[i+1] <- y_positions[i]-(2*par()$cxy[2])}
            i <- i + 1
          }
        }  
        sec_uvhplot <- text(sec_uvhplot, x=secondary_corrections$x, y=y_positions, 
                            label=seq(nrow(secondary_corrections)), pos=4, col="blue")
        
      } 
      
      #add effective shift axis, timeseries, and shift measurements

      add_uv_shift(sec_uvhplot, secondary_lims = secondary_lims, secondary_lbl=secondary_lbl, tertiary_pts = shift_pts, tertiary_lbl = tertiary_lbl, measured_shift_pts = measuredShifts)
      

      
    } # month for loop  
    
    ###################### printing
    #!!hackalert!! we are essentially taking out and putting back the approvals line so it ends up at the bottom z level of the plot
    ylim<-gsplot:::calc_views(uvhplot)$window$ylim
    series_appr_pts <- list(x=uv_pts$x, y=rep(ylim[1],nrow(uv_pts)), type="l", pch=15, col=NULL, cex=2, lwd=25, bg=NULL, legend.name=paste("UV", primary_lbl))
    uvhplot <- plotting_appr(uvhplot, series_appr_pts, uv_appr, label=primary_lbl, name="UV", limits=uv_lims)
    par(mar=c(7, 3, 4, 2))
    uv <- append(uvhplot[length(uvhplot)], uvhplot)
    uv[[length(uv)]]<-NULL
    class(uv) <- "gsplot"
    print(uv)
    print(sec_uvhplot)
    return(corrections_table)  
    
    ############################ stopped here 
    
  } # end UVhydrograph plot function

  
  
  ############ functions:
  
  getUvhLims <- function(pts = NULL, xMinField = 'x', xMaxField = 'x', yMinField = 'y', yMaxField = 'y'){
    x_mx <- max(pts[[xMaxField]], na.rm = TRUE)
    x_mn <- min(pts[[xMinField]], na.rm = TRUE)
    y_mx <- max(pts[[yMaxField]], na.rm = TRUE)
    y_mn <- min(pts[[yMinField]], na.rm = TRUE)
    if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
      stop('missing or NA values in points. check input json.')
    }
    ylim = c(y_mn, y_mx)
    xlim = c(x_mn, x_mx)
    return(list(xlim = xlim, ylim = ylim))
  }
  
  combineLims<- function(lims1, lims2, ...) {
    x_mx <- max(lims1$xlim[2], lims2$xlim[2], na.rm = TRUE)
    x_mn <- min(lims1$xlim[1], lims2$xlim[1], na.rm = TRUE)
    y_mx <- max(lims1$ylim[2], lims2$ylim[2], na.rm = TRUE)
    y_mn <- min(lims1$ylim[1], lims2$ylim[1], na.rm = TRUE)
    if (any(is.na(c(x_mx, x_mn, y_mx, y_mn)))){
      stop('missing or NA values in points. check input json.')
    }
    ylim = c(y_mn, y_mx)
    xlim = c(x_mn, x_mx)
    return(list(xlim = xlim, ylim = ylim))
  }
  
  
  add_uv_shift <- function(sec_uvhplot, secondary_lims = NULL, secondary_lbl = NULL, tertiary_pts = NULL, tertiary_lbl = NULL, measured_shift_pts = NULL) {
    if(!is.null(tertiary_pts) && nrow(tertiary_pts)>0) {
      lims <- getUvhLims(tertiary_pts)
      xaxis <- lims$xlim
      yaxis <- lims$ylim
      
      #     #expand y limits if error bars bleed over
      #     if(!is.null(measured_shift_pts)) {
      #       errorBarLims <- getUvhLims(measured_shift_pts, yMinField = "minShift", yMaxField = "maxShift")
      #       combinedLims <- combineLims(lims, errorBarLims)
      #       yaxis = combinedLims$ylim
      #     }
      
      mgp = list(y=c(1.25,0.15,0), x = c(-0.1,-0.2,0))
      mn_tck = 50
      mn_tkL = 0.005
      mj_tck = 10
      mj_tkL = 0.01
      ax_lab = 0.75 # scale
      num_maj_y = 7
      num_min_y = 15 #only used when ylog = F
      
      # main plot area
      #par(new = TRUE)
      sec_uvhplot <- points(sec_uvhplot, type="l", x=tertiary_pts$x, y=tertiary_pts$y, xlim=secondary_lims$xlim, ylim=yaxis, log = '', xlab=NA, ylab=NA, xaxt="n", yaxt="n", mgp=mgp$y, xaxs='i', col = 'green3', lty = 1, legend.name=paste(secondary_lbl, tertiary_lbl), axes=FALSE)
      
      yticks <- pretty(par()$usr[3:4], num_maj_y)
      yminor <- pretty(par()$usr[3:4], num_min_y)
      
      # major axes
      sec_uvhplot <- axis(sec_uvhplot, side=4, at=yticks, cex.axis=ax_lab, las=2, tck=mj_tkL, mgp=mgp$y, labels=yticks, ylab=tertiary_lbl)
      sec_uvhplot <- mtext(sec_uvhplot, side = 4, line = 2, tertiary_lbl, cex = .75)
      
      sec_uvhplot <- text(sec_uvhplot, paste(secondary_lbl, tertiary_lbl), NA, "green", 1)
    }
    return(sec_uvhplot)
  }
  
  
  ########################## new functions
   
  
  
  plotting_appr <- function(object, sublist, approvals, label, name, limits){
    approvalColors <- c("lightpink", "yellow2", "lightcyan")
    approvalDescriptions <- c("Working", "In-review", "Approved")
    if (!is.null(sublist[['y']]) && length(sublist[['y']]) > 0){  
      if(is.null(approvals)) { #default to working/red level for all points if no approvals found
        sublist[['col']] <- approvalColors[1]
        sublist[['bg']] <- approvalColors[1]
        sublist[['pt.bg']] <- approvalColors[1]
        sublist[['legend.name']] <- paste(approvalDescriptions[1], "DV", label)
        object <- do.call(points, append(sublist, list(object=object)))
      } else { #for each approval period, plot points in the time range using correct approval color
        for(i in 1:nrow(approvals)) {
          a <- approvals[i,]
          startTime <- a$startTime
          endTime <- a$endTime
          level <- a$level + 1
          pts_subset <- cbind(sublist$x, sublist$y)
          pts_subset <- pts_subset[pts_subset[,1] > startTime & pts_subset[,1] < endTime,]
          sublist[['x']] <- pts_subset[,1]
          sublist[['bg']] <- approvalColors[level]
          sublist[['pt.bg']] <- approvalColors[level]
          sublist[['legend.name']] <- paste(approvalDescriptions[level], sublist[['legend.name']])
          if (name=="DV") {
            sublist[['y']] <- pts_subset[,2]
            sublist[['col']] <- 'black'
          } else if (name=="UV") {
            sublist[['col']] <- approvalColors[level]
          }
          object <- do.call(points, append(sublist, list(object=object)))
        }
      }
    }
    return(object)
  }

