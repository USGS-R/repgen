getUvStyle <- function(dataType) {
  styles=list(review_DV=list(pch=,lty=),
              review_UV=list(),
              uncorr_DV=list(),
              uncorr_UV=list(),
              corr_DV=list(),
              corr_UV=list(),
              working_DV=list(),
              working_UV=list(),
              approved_DV=list(),
              approved_UV=list(),
              error_DV=list(),
              callout_DV=list())
  return(styles[[dataType]])
  }

  getLegendName <- function(dataType,stat,primary_label) {
    #returns In Review DV Mean
     legend.name <- switch(dataType,
                           review_DV=sprintf("In Review DV %s %s",stat,primary_label),
                           error_DV=sprintf("Discharge measurements and error"))
     return(legend.name)
  }
  

add_stage_measurements <- function(data, month, sec_uvhplot, ...) {
  pts <- subsetByMonth(getMeanGageHeights(data), month)
  sec_uvhplot <- points(sec_uvhplot, pts$x, pts$y, pch=21, bg='black', col='black', cex=.8, legend.name="Gage height measurement")
  sec_uvhplot <- text(sec_uvhplot, x=pts$x, y=pts$y, labels=pts$n)
  return(sec_uvhplot)
}

add_gw_level_measurements <- function(data, month, sec_uvhplot, ...) {
  pts <- subsetByMonth(getGroundWaterLevels(data), month)
  sec_uvhplot <- points(sec_uvhplot, pts$x, pts$y, pch = 8, bg = 'orange', col = 'orange', cex = 1.2, legend.name="Measured Water Level (NWIS-RA)", axes=FALSE)
  return(sec_uvhplot)
}

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
        pts_subset <- data.frame(sublist$x, sublist$y)
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