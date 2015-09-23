#'@export

#
# Starting point, creates RMD and runs rendering
#
startUvhydrographRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('uvhydrograph', 'uvhydrograph.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir=output_dir)
  return(out_file)
}

#'@export
#'
uvhydrographPlot <- function(data) {
  options(scipen=5) #less likely to give scientific notation
  all_primary_pts <- getUvHydro(data, "primarySeries" )
  months <- unique(all_primary_pts$month, incomparables = FALSE)
  renderList <- vector("list", length(months))
  names(renderList) <- months
  
  for (month in months) {
    primaryPlotTable <- createPrimaryPlot(data, month)
    secondaryPlotTable <- createSecondaryPlot(data, month)
    
    renderList[[month]] <- list(plot1=primaryPlotTable$plot, table1=primaryPlotTable$table, 
                       plot2=secondaryPlotTable$plot, table2=secondaryPlotTable$table)
  }
  
  return(renderList)
  
}


createPrimaryPlot <- function(data, month){ 
  primaryData <- parseUVData(data, "primary", month)
  primaryInfo <- parseUVSupplemental(data, "primary", primaryData$corr_UV)
  
  #orderData() ?
  
  uvhplot <- gsplot(ylog=primaryInfo$logAxis)
  
  for (i in 1:length(primaryData)) {
    x <- primaryData[[i]]$x
    y <- as.numeric(primaryData[[i]]$y)
    
    correctionLabels <- parseLabelSpacing(primaryData[i], primaryInfo)
    primaryApprovals <-  parseApprovalInfo(primaryData[i], primaryInfo, x, y)
    
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
    axis(side=2) %>% 
    axis(side=1,at=primaryInfo$plotDates,labels=as.character(primaryInfo$days)) %>%
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=primaryInfo$plotDates, lty=3, col="gray", legend.name="verticalGrids") 
    
  #gridlines and approval line behind the other data
  uvhplot <- reorderPlot(uvhplot, c("verticalGrids", "UV Discharge"))  
  
  #remove duplicate legend entries (from approvals) after they are used for reordering
  names <- unlist(unname(sapply(uvhplot, function(x) {
    ifelse(is.null(x$gs.config$legend.name), NA, x$gs.config$legend.name)
  })))
 
  for (k in which(duplicated(names))){   
    uvhplot[[k]]$gs.config$legend.name <- NULL
  }
    
  uvhplot <- legend(uvhplot, location="below", title="") %>%
    title(main="", xlab=primaryInfo$date_lbl, ylab=primaryInfo$primary_lbl) 
  
  table <- correctionsTable(primaryData)
  
  return(list(plot=uvhplot, table=table))
}


createSecondaryPlot <- function(data, month){
  secondaryData <- parseUVData(data, "secondary", month)
  secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData$corr_UV2)
  
  sec_uvhplot <- gsplot()
  
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
  
  sec_uvhplot <- lines(sec_uvhplot, as.POSIXct(NA), as.POSIXct(NA), 
                       xlim=c(secondaryInfo$plotDates[1], tail(secondaryInfo$plotDates,1))) %>% 
    axis(side=2) %>%
    axis(side=1, at=secondaryInfo$plotDates, labels=as.character(secondaryInfo$days)) %>%
    axis(side=4) %>%
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=secondaryInfo$plotDates, lty=3, col="gray") %>% 
    legend(location="below", title="") %>%
    title(main="", xlab=secondaryInfo$date_lbl2, ylab=secondaryInfo$secondary_lbl)
    
  table <- correctionsTable(secondaryData)
  
  return(list(plot=sec_uvhplot, table=table))
}


