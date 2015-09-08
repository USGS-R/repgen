# getUvStyle <- function(dataType) {
#   styles=list(review_DV=list(pch=,lty=),
#               review_UV=list(),
#               uncorr_DV=list(),
#               uncorr_UV=list(type='l',col="darkturquoise",lty=4),
#               corr_DV=list(),
#               corr_UV=list(type='l',col="black",lty=1),
#               working_DV=list(),
#               working_UV=list(),
#               approved_DV=list(),
#               approved_UV=list(),
#               error_DV=list(),
#               callout_DV=list(),
#               estimate_UV=list(type='l',col="orange",lty=4,lwd=2),
#               comp_UV=list(type='l', col="green", lty=1),
#               waterq=list(type='p', col="orange", pch=8, bg="orange", cex=1.2),
#               dv_mean=list(type='p', pch=21, col=NULL, bg=NULL, pt.bg=NULL),
#               dv_median=list(type='p', pch=26, col=NULL, bg=NULL, pt.bg=NULL),
#               dv_max=list(type='p', pch=24, col=NULL, bg=NULL, pt.bg=NULL),
#               dv_min=list(type='p', pch=25, col=NULL, bg=NULL, pt.bg=NULL),
#             
#               
#               )
#       
#   return(styles[[dataType]])
#   }
# 
#   getLegendName <- function(dataType,stat,primary_label) {
#     #returns In Review DV Mean
#      legend.name <- switch(dataType,
#                            review_DV=sprintf("In Review DV %s %s",stat,primary_label),
#                            error_DV=sprintf("Discharge measurements and error"),
#                            corr_UV=sprintf("Corrected UV %s %s",stat,primary_label),
#                            uncorr_UV=sprintf("Uncorrected UV %s %s",stat,primary_label),
#                            estimate_UV=sprintf("Estimated UV %s %s",stat,primary_label),
#                            comp_UV=sprintf("Comparison UV %s %s",stat,primary_label),
#                            waterq=sprintf("NWIS-RA WQ Measurement"),
#                            dv_mean=sprintf("DV Mean %s %s",stat,primary_label),
#                            dv_median=sprintf("DV Median %s %s",stat,primary_label),
#                            dv_max=sprintf("DV Max %s %s",stat,primary_label),
#                            dv_min=sprintf("DV Minx %s %s",stat,primary_label)                      )
#                            
#      
#      return(legend.name)
#   }
#   
#   #this is where we choose how to layer the data for the report
#   #displayOrder() {
#     
#     
#   #}
  
  
  #'@export
  uvhydrographPlot <- function(data){
    #for pagination by month, get all of the month strings for the primary series
    all_primary_pts <- getUvHydro(data, "primarySeries" )
    months <- unique(all_primary_pts$month, incomparables = FALSE)
    
    #breaking up plot according to time period
    
    for (month in months){  
      
      #primary series data points
      uv_pts <- subsetByMonth(getUvHydro(data, "primarySeries" ), month)
      estimated_uv_pts <- subsetByMonth(getUvHydro(data, "primarySeries", estimatedOnly=TRUE), month)
      uv_pts_raw <- subsetByMonth(getUvHydro(data, "primarySeriesRaw" ), month)
      uv_comp_pts <- subsetByMonth(getUvHydro(data, "comparisonSeries" ), month)
      wq_pts <- subsetByMonth(getWaterQualityMeasurements(data), month)
      
      uv_lims <- getUvhLims(uv_pts)
      primary_lbl <- getUvLabel(data, "primarySeries")
      date_lbl <- paste(uv_lims$xlim[1], "through", uv_lims$xlim[2])
      uv_comp_lbl <- getUvName(data, "comparisonSeries")
      
      dates <- seq(uv_lims$xlim[1], uv_lims$xlim[2], by="days")
      
      #plotting details for data
      plot_data_primary <- list(
        uv_pts=list(x=uv_pts$x, y=uv_pts$y, type='l', col="black", lty=1, legend.name=paste("Corrected UV", primary_lbl), axes=FALSE), 
        uv_pts_raw=list(x=uv_pts_raw$x, y=uv_pts_raw$y, type='l', col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", primary_lbl), axes=FALSE),
        estimated_uv_pts=list(x=estimated_uv_pts$x, y=estimated_uv_pts$y, type='l', col="orange", lty=4, lwd=2, legend.name=paste("Estimated UV", primary_lbl)),
        uv_comp_pts=list(x=uv_comp_pts$x, y=uv_comp_pts$y, type='l', col="green", lty=1, legend.name=paste("Comparison", primary_lbl)),
        wq_pts=list(x=wq_pts$x, y=wq_pts$y, type='p', col="orange", pch=8, bg="orange", cex=1.2, legend.name="NWIS-RA WQ Measurement")
      )  
      
      ## DV mean, max, min, median
      field <- list(mean="derivedSeriesMean", max="derivedSeriesMax", min="derivedSeriesMin", median="derivedSeriesMedian")
      dv <- lapply(field, function(x) {
        list <- list(dv_pts=subsetByMonth(getUvHydro(data, x), month),
                     approvals=getApprovals(data, x)
        )
      })
      
      dv_pts <- list(mean=list(x=dv$mean$dv_pts$x, y=dv$mean$dv_pts$y, type='p', pch=21, col=NULL, bg=NULL, pt.bg=NULL, legend.name=paste("DV Mean", primary_lbl)),
                     median=list(x=dv$median$dv_pts$x, y=dv$median$dv_pts$y, type='p', pch=26, col=NULL, bg=NULL, pt.bg=NULL, legend.name=paste("DV Median", primary_lbl)),
                     max=list(x=dv$max$dv_pts$x, y=dv$max$dv_pts$y, type='p', pch=24, col=NULL, bg=NULL, pt.bg=NULL, legend.name=paste("DV Max", primary_lbl)),
                     min=list(x=dv$min$dv_pts$x, y=dv$min$dv_pts$y, type='p', pch=25, col=NULL, bg=NULL, pt.bg=NULL, legend.name=paste("DV Min", primary_lbl))   
      ) 
      
      ####plotting
      uvhplot <- gsplot()
      
      ##series approvals
      uv_appr <- getApprovals(data, "primarySeries" )
      
      for (i in 1:length(plot_data_primary)) {
        uvhplot <- plotting(uvhplot, sublist=plot_data_primary[[i]])
      }
      #uvhplot <- lapply(plot_data_primary, function(sublist) plotting(uvhplot, sublist))    
      
      primary_corrections <- subsetByMonth(getCorrections(data, "primarySeriesCorrections"), month)
      if(!is.null(primary_corrections) && nrow(primary_corrections)>0) {
        uvhplot <- abline(uvhplot, v=primary_corrections$x, untf=FALSE, col="blue", legend.name="Data correction entry")
        uvhplot <- text(uvhplot, x=primary_corrections$x, y=mean(uv_lims$ylim), label=as.Date(primary_corrections$x), srt=90, col="red")
      }
      
      uvhplot <- lines(uvhplot, NA, NA) %>%
        axis(side=c(1),at=seq(uv_lims$xlim[1], uv_lims$xlim[2], by="days"),labels=as.character(1:length(dates))) %>%
        axis(side=2) %>%
        grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
        abline(v=seq(uv_lims$xlim[1], uv_lims$xlim[2], by="days"), lty=3, col="gray") %>% 
        legend(location="below", title="") %>%
        title(main="", xlab=date_lbl, ylab=primary_lbl) 
      
      #used for loop because lapply kept returning uvhplot as list within mean, min, max
      for (i in 1:length(dv_pts)){
        approvals <- dv[[i]][['approvals']]
        uvhplot <- plotting_appr(object=uvhplot, sublist=dv_pts[[i]], approvals, label=primary_lbl, name="DV", limits=uv_limits)
      }
      
      # discharge measurements and errors
      if(data[['primarySeries']][['type']] == "Discharge") {
        q <- subsetByMonth(getFieldVisitErrorBarsQPoints(data), month)
        if(!is.null(q) && nrow(q)>0) {
          uvhplot <- error_bar(uvhplot, x=q$x, y=q$y, y.low=q$y-q$minQ, y.high=q$maxQ-q$y, col="black", lwd=0.7, epsilon=0.1, legend.name="Discharge measurement and error")
          uvhplot <- points(uvhplot, q$x, q$y, pch = 21, bg = 'black', col = 'black', cex = .8, axes=FALSE)
          uvhplot <- callouts(uvhplot, q$x, q$y, labels = q$n, cex = .75, col='red', length = 0.05, angle = 30)
        }
      }
      
      ###### second plot
      
      uv2_pts <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
      estimated_uv2_pts <- subsetByMonth(getUvHydro(data, "secondarySeries", estimatedOnly=TRUE), month)
      uv2_pts_raw <- subsetByMonth(getUvHydro(data, "secondarySeries"), month)
      secondary_lims <- getUvhLims(uv2_pts)
      sec_date_lbl <- paste(secondary_lims$xlim[1], "through", secondary_lims$xlim[2])
      secondary_lbl <- getUvLabel(data, "secondarySeries")
      
      plot_data_secondary <- list(uv2_pts=list(x=uv2_pts$x, y=uv2_pts$y, type='l', col="black", lty=1, legend.name=paste("Corrected UV", secondary_lbl), axes=FALSE), 
                                  
                                  uv2_pts_raw=list(x=uv2_pts_raw$x, y=uv2_pts_raw$y, type='l', col="darkturquoise", lty=4, legend.name=paste("Uncorrected UV", secondary_lbl), axes=FALSE),
                                  
                                  estimated_uv2_pts=list(x=estimated_uv2_pts$x, y=estimated_uv2_pts$y, type='l', col="orange", lty=2, lwd=2, legend.name=paste("Estimated UV", secondary_lbl))
      )
      
      sec_dates <- seq(secondary_lims$xlim[1], secondary_lims$xlim[2], by="days")
      
      sec_uvhplot <- gsplot()
      
      #lapply returns each gsplot value under a list name, for loop works
      for (i in 1:length(plot_data_secondary)) {
        sec_uvhplot <- plotting(sec_uvhplot, sublist=plot_data_secondary[[i]])
      }
      #sec_uvhplot <- lapply(plot_data_secondary, function(sublist) plotting(sec_uvhplot, sublist))
      
      sec_uvhplot <- legend(sec_uvhplot, location="below", title="") %>%
        axis(side=c(1),at=seq(secondary_lims$xlim[1], secondary_lims$xlim[2], by="days"),labels=as.character(1:length(sec_dates))) %>%
        title(main="", xlab=sec_date_lbl, ylab=secondary_lbl) %>%
        axis(side=2) %>%
        axis(side=4) %>%
        grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
        abline(v=seq(secondary_lims$xlim[1], secondary_lims$xlim[2], by="days"), lty=3, col="gray") 
      
      
      secondary_corrections <- getCorrections(data, "secondarySeriesCorrections")
      secondary_corrections <- subsetByMonth(secondary_corrections, month)
      if(!is.null(secondary_corrections) && nrow(secondary_corrections)>0) {
        sec_uvhplot <- abline(sec_uvhplot, v=secondary_corrections$x, untf = FALSE, col="blue", legend.name="Data Correction Entry")
        y_positions <- rep(secondary_lims$ylim[2], nrow(secondary_corrections))
        differences <- as.numeric(diff(secondary_corrections$x))
        if(length(differences) > 0) {
          for (i in 1:length(differences)) {
            if(differences[i] < 86400) {y_positions[i+1] <- y_positions[i]-(2*par()$cxy[2])}
            i <- i + 1
          }
        }
        sec_uvhplot <- text(sec_uvhplot, x=secondary_corrections$x, y=y_positions, 
                            label=seq(nrow(secondary_corrections)), pos=4, col="blue")
        corrections_table <- as.data.frame(cbind(seq(nrow(secondary_corrections)), secondary_corrections$comment))
        colnames(corrections_table) <- c("", "Comments")
      } else {corrections_table <- NULL}
      
      # gageHeight
      if(data[['secondarySeries']][['type']] == "Gage height") {
        add_stage_measurements(data, month=month, sec_uvhplot)
      }
      
      #GW level
      if(data[['secondarySeries']][['type']] == "WaterLevel, BelowLSD") {
        add_gw_level_measurements(data, month=month, sec_uvhplot)
      }
      
      shift_pts <- subsetByMonth(getUvHydro(data, "effectiveShifts"), month)
      tertiary_lbl <- getUvLabel(data, "effectiveShifts")
      
      #add effective shift axis, timeseries, and shift measurements
      measuredShifts <- subsetByMonth(getFieldVisitErrorBarsShifts(data), month)
      add_uv_shift(sec_uvhplot, secondary_lims = secondary_lims, secondary_lbl=secondary_lbl, tertiary_pts = shift_pts, tertiary_lbl = tertiary_lbl, measured_shift_pts = measuredShifts)
      
      #adding shift measurements
      if(!is.null(measuredShifts) && nrow(measuredShifts)>0) {
        sec_uvhplot <- error_bar(sec_uvhplot, x=measuredShifts$x, measuredShifts$y, y.low=measuredShifts$y, y.high=measuredShifts$y, col='green', lwd=.7, legend.name="Effective shift and error")
        sec_uvhplot <- points(sec_uvhplot, measuredShifts$x, measuredShifts$y, pch=21, bg='green', col='green', legend.name="Effective shift and error", axes=FALSE)
      }
      
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
  
  subsetByMonth <- function(pts, onlyMonth) {
    if(!is.null(pts) && nrow(pts) > 0) {
      return(subset(pts, month == onlyMonth))
    }
    return(pts)
  }
  
  
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
  
  
  add_stage_measurements <- function(data, month, sec_uvhplot, ...) {
    pts <- subsetByMonth(getMeanGageHeights(data), month)
    sec_uvhplot <- points(sec_uvhplot, pts$x, pts$y, pch=21, bg='black', col='black', cex=.8, legend.name="Gage height measurement", axes=FALSE)
    sec_uvhplot <- text(sec_uvhplot, x=pts$x, y=pts$y, labels=pts$n)
    return(sec_uvhplot)
  }
  
  add_gw_level_measurements <- function(data, month, sec_uvhplot, ...) {
    pts <- subsetByMonth(getGroundWaterLevels(data), month)
    sec_uvhplot <- points(sec_uvhplot, pts$x, pts$y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, legend.name="Measured Water Level (NWIS-RA)", axes=FALSE)
    return(sec_uvhplot)
  }
  
  
  ########################## new functions
  plotting <- function(object, sublist) {
    if (!is.null(sublist[['y']]) && length(sublist[['y']]) > 0){
      if (sublist[['type']]=='l'){
        object <- do.call(lines, append(sublist, list(object=object))) 
      } else if (sublist[['type']]=='p'){
        object <- do.call(points, append(sublist, list(object=object)))  
      }
    }
    return(object)
  }  
  
  
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

