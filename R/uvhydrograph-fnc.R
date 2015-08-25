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
      for (i in 1:length(differences)) {
        if(differences[i] < 86400) {y_positions[i+1] <- y_positions[i]-(2*par()$cxy[2])}
        i <- i + 1
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
  par(mar=c(7, 3, 4, 2))
  #!!hackalert!! we are essentially taking out and putting back the approvals line so it ends up at the bottom z level of the plot
  if (!is.null(approvals)>0) {
    ylim<-gsplot:::calc_views(uvhplot)$window$ylim
    series_appr_pts <- list(x=uv_pts$x, y=rep(ylim[1],nrow(uv_pts)), type="l", pch=15, col=NULL, cex=2, lwd=25, bg=NULL, legend.name=paste("UV", primary_lbl))
    uvhplot <- plotting_appr(uvhplot, series_appr_pts, uv_appr, label=primary_lbl, name="UV", limits=uv_lims)
    uv <- append(uvhplot[length(uvhplot)], uvhplot)
    uv[[length(uv)]]<-NULL
    class(uv) <- "gsplot"
    print(uv)
  }
  else {
    print(uvhplot)
  }
  print(sec_uvhplot)
  return(corrections_table)  
  
  ############################ stopped here 
  
} # end UVhydrograph plot function


