#'@export

#
# Starting point, creates RMD and runs rendering
#
startUvhydrographRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- makeUvhydrographRmd(system.file('uvhydrograph', package = 'repgen'), data, output, output_dir)
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir=output_dir)
  return(out_file)
}


createPrimaryPlot <- function(data){ 
  primaryData <- parseUVData(data, "primary")
  primaryInfo <- parseUVSupplemental(data, "primary", primaryData$corr_UV)
  
  #orderData() ?
  
  uvhplot <- gsplot()
  
  for (i in 1:length(primaryData)) {
    x <- primaryData[[i]]$x
    y <- primaryData[[i]]$y
    ##### working on this now ############################################
    primaryApprovals <-  parseApprovalInfo(primaryData[i], primaryInfo)  
    ##### working on this now ############################################
    primaryStyles <- getUvStyle(primaryData[i], primaryInfo, x, y, primaryApprovals, "primary")
    primaryPlotTypes <- getPlotType(primaryData[i], "primary")
   
    
    if (names(primaryData[i]) %in% c("series_corr", "meas_Q")) {
      for (j in 1:length(primaryStyles)) {
        uvhplot <- do.call(primaryPlotTypes[j], append(list(object=uvhplot), primaryStyles[[j]]))
      }
    } else {
      uvhplot <- do.call(primaryPlotTypes, append(list(object=uvhplot), primaryStyles))
    }  
    
  }
  
  uvhplot <- lines(uvhplot, NA, NA) %>%
    axis(side=c(1),at=seq(primaryInfo$lims_UV$xlim[1], primaryInfo$lims_UV$xlim[2], by="days"),labels=as.character(1:length(primaryInfo$dates))) %>%
    axis(side=2) %>%
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=seq(primaryInfo$lims_UV$xlim[1], primaryInfo$lims_UV$xlim[2], by="days"), lty=3, col="gray") %>% 
    legend(location="below", title="") %>%
    title(main="", xlab=primaryInfo$date_lbl, ylab=primaryInfo$primary_lbl) 
  
  return(uvhplot)
}


createSecondaryPlot <- function(data){
  secondaryData <- parseUVData(data, "secondary")
  secondaryInfo <- parseUVSupplemental(data, "secondary", secondaryData$corr_UV2)
  
  sec_uvhplot <- gsplot()
  
  for (i in 1:length(secondaryData)) {
    x <- secondaryData[[i]]$x
    y <- secondaryData[[i]]$y
    secondaryStyles <- getUvStyle(secondaryData[i], secondaryInfo, x, y, "secondary")
    secondaryPlotTypes <- getPlotType(secondaryData[i], "secondary")
    
    if (names(secondaryData[i]) %in% c("series_corr2", "gage_height", "meas_shift")) {
      for (j in 1:length(secondaryStyles)) {
        sec_uvhplot <- do.call(secondaryPlotTypes[j], append(list(object=sec_uvhplot), secondaryStyles[[j]]))
      }
    } else {
      sec_uvhplot <- do.call(secondaryPlotTypes, append(list(object=sec_uvhplot), secondaryStyles))
    }  
    
  }
  
  sec_uvhplot <- legend(sec_uvhplot, location="below", title="") %>%
    axis(side=c(1),at=seq(secondaryInfo$lims_UV2$xlim[1], secondaryInfo$lims_UV2$xlim[2], by="days"),labels=as.character(1:length(secondaryInfo$sec_dates))) %>%
    title(main="", xlab=secondaryInfo$date_lbl2, ylab=secondaryInfo$secondary_lbl) %>%
    axis(side=2) %>%
    axis(side=4) %>%
    grid(nx=0, ny=NULL, equilogs=FALSE, lty=3, col="gray") %>% 
    abline(v=seq(secondaryInfo$lims_UV2$xlim[1], secondaryInfo$lims_UV2$xlim[2], by="days"), lty=3, col="gray") 
  
  correctionsTable(secondaryData)
  
  return(sec_uvhplot)
}


