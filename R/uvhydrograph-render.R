#'Create a uvhydrograph
#'@export
#'@param data coming in to create a plot
#'@rdname uvhydrographPlot
uvhydrographPlot <- function(data) {
  options(scipen=5) #less likely to give scientific notation

  months <- getMonths(data)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  if(!is.null(months)){
    for (month in months) {
      primaryPlotTable <- createPrimaryPlot(data, month)
      if(!is.null(primaryPlotTable$plot)){
        secondaryPlotTable <- createSecondaryPlot(data, month)
      } else {
        secondaryPlotTable <- list()
      }
      
      renderList[[month]] <- list(plot1=primaryPlotTable$plot, table1=primaryPlotTable$table, 
                         plot2=secondaryPlotTable$plot, table2=secondaryPlotTable$table)
    }
  } else {
    renderList[[1]] <- list(plot1=NULL, table1=NULL, plot2=NULL, table2=NULL)
  }
  
  return(renderList)
  
}


createPrimaryPlot <- function(data, month){ 
  primaryData <- parseUVData(data, "primary", month)

  if(anyDataExist(primaryData)){

    primaryInfo <- parseUVSupplemental(data, "primary", primaryData, zeroValues(primaryData, "y"))
    
    uvhplot <- gsplot(ylog=primaryInfo$logAxis, yaxs='r')
    
    for (i in 1:length(primaryData)) {
      x <- primaryData[[i]]$x
      y <- as.numeric(primaryData[[i]]$y)
      
      correctionLabels <- parseLabelSpacing(primaryData[i], primaryInfo)
      primaryApprovals <-  parseApprovalInfo(primaryData[i], primaryInfo, x, y, uvhplot)
      
      if (length(primaryApprovals) > 0) {
        primaryStyles <- vector("list", length(primaryApprovals))
        primaryPlotTypes <- c()
        notPlot <- c()
        for (k in seq(length(primaryApprovals))) {
          primaryStyles[[k]] <- getUvStyle(primaryData[i], primaryInfo, x, y, primaryApprovals[[k]], correctionLabels, "primary")
          primaryPlotTypes[k] <- getPlotType(primaryData[i], "primary")
          notPlot[k] <- all(is.na(primaryStyles[[k]]$x))
        }
        primaryStyles[which(notPlot)] <- NULL
        primaryPlotTypes <- primaryPlotTypes[which(!notPlot)]
      } else {
        primaryStyles <- getUvStyle(primaryData[i], primaryInfo, x, y, primaryApprovals, correctionLabels, "primary")
        primaryPlotTypes <- getPlotType(primaryData[i], "primary")
      }
      
      if (names(primaryData[i]) %in% c("series_corr", "meas_Q", "max_DV", "min_DV", "median_DV", "mean_DV", "UV_series")) {
        for (j in seq_len(length(primaryStyles))) {
          uvhplot <- do.call(primaryPlotTypes[j], append(list(object=uvhplot), primaryStyles[[j]]))
        }
      } else {
        uvhplot <- do.call(primaryPlotTypes, append(list(object=uvhplot), primaryStyles))
      }  
      
    }
  
    uvhplot <- lines(uvhplot, as.POSIXct(NA), as.POSIXct(NA), 
                     xlim=c(primaryInfo$plotDates[1], tail(primaryInfo$plotDates,1))) %>% 
      axis(side=1,at=primaryInfo$plotDates,labels=as.character(primaryInfo$days)) %>%
      axis(side=2, reverse=primaryInfo$isInverted, las=0) %>%
      grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray", legend.name="horizontalGrids") %>% 
      abline(v=primaryInfo$plotDates, lty=3, col="gray", legend.name="verticalGrids") 
  
    orderLegend <- c("verticalGrids", "Working UV", "In-review UV", "Approved UV", "horizontalGrids")
    uvhplot <- reorderPlot(uvhplot, "view.1.2", "legend.name", orderLegend)
    uvhplot <- reorderPlot(uvhplot, "legend", "legend", orderLegend)
    uvhplot <- rm.duplicates(uvhplot, "view.1.2", "legend.name")
    uvhplot <- rm.duplicates(uvhplot, "legend", "legend")
    
    ncol <- ifelse(length(uvhplot$legend) > 3, 2, 1)
    leg_lines <- ifelse(ncol==2, ceiling((length(uvhplot$legend) - 6)/2), 0) 
    legend_offset <- ifelse(ncol==2, 0.3+(0.005*leg_lines), 0.3)
    uvhplot <- legend(uvhplot, location="below", title="", ncol=ncol, 
                      legend_offset=legend_offset, cex=0.8) %>%
      title(main=format(primaryInfo$plotDates[1], "%B %Y"), 
            xlab=paste("UV Series:", primaryInfo$date_lbl), 
            ylab=primaryInfo$primary_lbl) 
    
    uvhplot <- testCallouts(uvhplot, xlimits = xlim(uvhplot)$side.1)
    
    table <- correctionsTable(primaryData)
  
  } else {
    uvhplot <- NULL
    table <- NULL
  }
    
  return(list(plot=uvhplot, table=table))
}


createSecondaryPlot <- function(data, month){
  secondaryData <- parseUVData(data, "secondary", month)
  secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData, zero_logic=NA)
  
  sec_uvhplot <- gsplot(yaxs='r')
  
  for (i in 1:length(secondaryData)) {
    x <- secondaryData[[i]]$x
    y <- as.numeric(secondaryData[[i]]$y)
    
    correctionLabels <- parseLabelSpacing(secondaryData[i], secondaryInfo)
    secondaryStyles <- getUvStyle(secondaryData[i], secondaryInfo, x, y, approvalInfo=list(), 
                                  correctionLabels, "secondary")
    secondaryPlotTypes <- getPlotType(secondaryData[i], "secondary")
    
    if (names(secondaryData[i]) %in% c("series_corr2", "effect_shift", "gage_height", 
                                       "meas_shift", "ref_readings", "csg_readings", "hwm_readings")) {
      for (j in seq_len(length(secondaryStyles))) {
        sec_uvhplot <- do.call(secondaryPlotTypes[j], append(list(object=sec_uvhplot), secondaryStyles[[j]]))
      }
    } else {
      sec_uvhplot <- do.call(secondaryPlotTypes, append(list(object=sec_uvhplot), secondaryStyles))
    }  
    
  }
  
  ncol <- ifelse(length(sec_uvhplot$legend) > 3, 2, 1)
  leg_lines <- ifelse(ncol==2, ceiling((length(sec_uvhplot$legend) - 6)/2), 0) 
  legend_offset <- ifelse(ncol==2, 0.3+(0.05*leg_lines), 0.3)
  
  sec_uvhplot <- lines(sec_uvhplot, as.POSIXct(NA), as.POSIXct(NA), 
                       xlim=c(secondaryInfo$plotDates[1], tail(secondaryInfo$plotDates,1))) %>%
    mtext(paste0(secondaryInfo$tertiary_lbl, " (", secondaryInfo$sec_units, ")"), 
          side = 4, line = 1.5) %>% 
    axis(side=1, at=secondaryInfo$plotDates, labels=as.character(secondaryInfo$days)) %>%
    axis(side=2, reverse=secondaryInfo$isInverted, las=0) %>%
    axis(side=4, las=0) %>%
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=secondaryInfo$plotDates, lty=3, col="gray") %>% 
    legend(location="below", title="", ncol=ncol, legend_offset=legend_offset, cex=0.8) %>%
    title(main="", xlab=paste("UV Series:", secondaryInfo$date_lbl2), 
          ylab=secondaryInfo$secondary_lbl) 
  
  sec_uvhplot <- testCallouts(sec_uvhplot, xlimits = xlim(sec_uvhplot)$side.1)

  table <- correctionsTable(secondaryData)
  
  ###HACKY FIX FOR OVERLAPPING LABELS###
  sec_uvhplot$view.1.2$window$xlim <- as.numeric(sec_uvhplot$view.1.2$window$xlim)
  sec_uvhplot$view.1.4$window$xlim <- sec_uvhplot$view.1.2$window$xlim
  
  return(list(plot=sec_uvhplot, table=table))
}


