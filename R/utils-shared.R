#shared functions between reports

#'Starting point, creates RMD and runs rendering
#'@param data coming in to create a plot
#'@param output format (either html or pdf)
#'@param author name of person generating the report
#'@param reportName name of report being generated (current options: dvhydrograph, fiveyruvhydrograph, vdiagram)
#'@rdname startRender 
#'@export 
startRender <- function(data, output, author, reportName){
  data <- data 
  
  wd <- getwd()
  tmp_folder_name <- paste0('tmp-', reportName)
  
  output_dir <- paste0(wd, "/", tmp_folder_name)
  
  dir.create(output_dir)
  
  #copy all shared files to tmp folder
  shared_files <- list.files(system.file('shared', package = 'repgen'), full.names = TRUE)
  file.copy(shared_files, output_dir)
  
  #copy all report inst files to tmp folder
  report_files <- list.files(system.file(reportName, package = 'repgen'), full.names = TRUE)
  file.copy(report_files, output_dir)
  
  if(reportName == "vdiagram"){
    rmd_file <- makeVDiagramRmd(output_dir, data, output, output_dir)
  } else {
    rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
  }
  
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir = output_dir)
  
  return(out_file)
}

printWithThirdYAxis <- function(plot) {
  axisDistance <- 4.5

  mar_vals <- par(plot)$mar
  if(is.null(mar_vals)){
    mar_vals <- par('mar')
  } 
  mar_vals[4] <- mar_vals[4] + axisDistance
  par(mar=mar_vals)

  print(plot)
  plot
  minor.ticks <- pretty(ylim(plot,6))
  major.ticks <- log_tick_marks(ylim(plot,6)[[1]], ylim(plot,6)[[2]])
  major.ticks <- major.ticks[major.ticks <= ylim(plot, 6)[[2]]]
  ticks <- major.ticks[major.ticks >= ylim(plot, 6)[[1]]]

  if(length(ticks) <= 3)
  {
    ticks <- unique(append(major.ticks, minor.ticks))
  }

  ticks <- trunc(append(ticks, c(-1, ylim(plot,6)[[2]]*2)))

  axis(side=4, at=ticks, line=axisDistance, las=0)
  mtext(plot$side.6$label, side=4, line=axisDistance+1.5, padj=0)
}

log_tick_marks <- function(min,max)
{
    nsplit <- abs(round(log10(max-min)))
    i <- 0
    nurange <- c()
    while(i<=nsplit) {
        nurange <- c(nurange,sapply(c(1,2,5),function(x) x*(10^i)))
        i <- i+1;
    }
    nurange
}

printReportFeature <- function(feature, isTable=FALSE, m=NULL, mar_values=c(8, 3, 4, 3)){
  if(!is.null(mar_values)){
    par(mar=mar_values)
  }

  if(!isEmpty(feature)){
    if(isTable){
      print(kable(feature))
      cat("\n\n")
    } else if(!is.null(m)){
      msg <- paste(feature, 'in', m)
      cat(msg)
    } else if(!is.null(feature$side.6)) {
      printWithThirdYAxis(feature)
      cat("\n\n")
    } else {
      print(feature)
      cat("\n\n")
    }
  } else {
    return()
  }
}

reorderPlot <- function(object, list, var_name, elementNames){
  for (i in seq_along(elementNames)){
    
    yes <- grep(elementNames[i], lapply(object[[list]], function(x) {x[[var_name]]}))
    no <- grep(elementNames[i], lapply(object[[list]], function(x) {x[[var_name]]}), invert=TRUE)
    
    #remove grids so they don't appear in the legend
    if (elementNames[i] %in% c("verticalGrids", "horizontalGrids")) { 
      if(list=="view.1.2") {
        for(y in yes){
          object[[list]][[y]][[var_name]] <- NULL
        }
        object[[list]] <- object[[list]][append(yes, no)]
      } else if(list=="legend"){object[[list]][yes] <- NULL}
    } else {
      object[[list]] <- object[[list]][append(yes, no)]
    }
    
  }
  
  class(object) <- "gsplot"
  return(object)
}

isLogged <- function(all_data, ts_data, series){
  
  isVolFlow <- all_data[[series]][['isVolumetricFlow']]
  zero_logic <- zeroValues(ts_data, "value")
  neg_logic <- negValues(ts_data, "value")
  
  if(is.null(isVolFlow) || !isVolFlow || zero_logic || neg_logic){
    logAxis <- FALSE
  } else if(isVolFlow && !zero_logic && !neg_logic){  
    logAxis <- TRUE
  }
  
  return(logAxis)
}

############ used in uvhydrograph-render and vdiagram-render ############ 

testCallouts <- function(plot_obj, xlimits){
  xrange <- diff(xlimits)
  buffer <- 0.04*xrange
  xlow <- xlimits[1]-buffer
  xhigh <- xlimits[2]+buffer
  xlimits_real <- c(xlow, xhigh)
  
  png('TMP_PLOT')
  print(plot_obj)
  width_char <- par("cxy")[1]
  #When you're done
  dev.off()
  #Delete the plot you just generated
  unlink('TMP_PLOT')
  
  i_view12 <- which(names(plot_obj$view.1.2) == "callouts")
  i_view14 <- which(names(plot_obj$view.1.4) == "callouts")
  
  testCalloutsByView <- function(plot_obj, callouts_index, view_num, xlimits_real, width_char, xrange){
    for(i in callouts_index){
        callout_args <- plot_obj[[view_num]][[i]]
        if (!is.na(xtfrm(callout_args$x[i])) |  
            !is.na(xtfrm(callout_args$y[i])) |
            is.null(xtfrm(callout_args$x[i])) |  
            is.null(xtfrm(callout_args$y[i]))) {  
          text_len <- nchar(callout_args$labels)
            
            len <- ifelse(is.null(callout_args$length), 0.1, callout_args$length)
            
            xend <- len * xrange * cos(2*pi*(30/360))
            xnew <- callout_args$x + xend + (width_char * text_len) 
            tooLong <- xnew > xlimits_real[2]
              
            if(any(tooLong)){
              out <- which(tooLong)
              notout <- which(!tooLong)
              plot_obj[[view_num]][[i]]$angle[notout] <- NA
              plot_obj[[view_num]][[i]]$angle[out] <- 150
            }
        }
    }
    return(plot_obj)
  }
  
  plot_obj <- testCalloutsByView(plot_obj, i_view12, 'view.1.2', xlimits_real, width_char, xrange)
  plot_obj <- testCalloutsByView(plot_obj, i_view14, 'view.1.4', xlimits_real, width_char, xrange)
  
  return(plot_obj)
}

############ used in uvhydrograph-render, dvhydrograph-render, fiveyeargwsum-render ############ 

rm.duplicate.legend.items <- function(object){
  
  which.duplicated <- which(duplicated(object$legend$legend.auto$legend))
  if(length(which.duplicated) > 0){
    object$legend$legend.auto <- lapply(object$legend$legend.auto, function(legend.arg, which.duplicated) {
        legend.arg[-which.duplicated]
    }, which.duplicated = which.duplicated)
  }

  return(object)
}

############ used in sitevisitpeak-data and sensorreading-data ############ 

formatComments <- function(comments){
  split_comments <- unlist(comments)
  if(is.null(split_comments) || nchar(split_comments) == 0){return(split_comments)}
  htmlbreaks_inside <- lapply(split_comments, gsub, pattern="\r\n", replacement="</br>")
  htmlbreaks_end <- lapply(htmlbreaks_inside, paste0, "</br>", collapse="")
  table_comments <- do.call(paste0, htmlbreaks_end)
  return(table_comments)
}

############ used in sensorreading-data and sitevisitpeak-data ############ 

isEmpty <- function(val){
  result <- (is.null(val) || is.na(val))
  return(result)
}

############ used in various places ############ 

isEmptyOrBlank <- function(val = NULL, listObjects = NULL, objectName = NULL){
  if(is.null(objectName)){
    result <- (length(val)==0 || isEmpty(val) || as.character(val)=="")
  } else {
    result <- !objectName %in% listObjects
  }
  return(result)
}

############ used in uvhydrograph-data, dvhydrograph-data, fiveyeargwsum-data ############ 

isEmptyVar <- function(variable){
  result <- all(is.null(variable) || nrow(variable) == 0 || is.null(nrow(variable)), 
                is.null(variable) || length(variable$time[!is.na(variable$time)]) == 0)
  return(result)
}

#' if there are gaps in the timeseries, don't connect them
#' this creates multiple line/point calls if there are gaps
#' @param data original list format of JSON
#' @param ts current timeseries data
#' @param isDV logic for whether this plot uses daily values or not
splitDataGaps <- function(data, ts, isDV){
  
  data_list <- data[[ts$field[1]]]
  
  hasGaps <- "gaps"  %in% names(data_list) && !isEmptyOrBlank(data_list$gaps)
  hasEstimatedRangesAsGaps <- !isEmptyOrBlank(ts$estimated) && !ts$estimated && 
    "estimatedPeriods"  %in% names(data_list) && 
    !isEmptyOrBlank(data_list$estimatedPeriods)
  
  if(hasGaps || hasEstimatedRangesAsGaps){

    if(hasGaps) {
      startGaps <- flexibleTimeParse(data_list$gaps$startTime, timezone = data$reportMetadata$timezone)
      endGaps <- flexibleTimeParse(data_list$gaps$endTime, timezone = data$reportMetadata$timezone)
    } else {
      startGaps <- c()
      endGaps <- c()
    }
    
    if(hasEstimatedRangesAsGaps) {

      if(isDV){
        # remove any time value for dv estimated times (should be for a whole day)
        startEstimated <- unlist(strsplit(data_list$estimatedPeriods$startDate, "T"))[1]
        endEstimated <-  unlist(strsplit(data_list$estimatedPeriods$endDate, "T"))[1]
      } else {
        startEstimated <- data_list$estimatedPeriods$startDate
        endEstimated <- data_list$estimatedPeriods$endDate
      }
      
      startEstimated <- flexibleTimeParse(startEstimated, timezone = data$reportMetadata$timezone)
      endEstimated <- flexibleTimeParse(endEstimated, timezone = data$reportMetadata$timezone)
      
      startGaps <- c(startGaps, startEstimated)
      endGaps <- c(endGaps, endEstimated)
    }
    
    #This is causing DV steps to be rendered at noon instead of on the day marks. 
    #Re-enable after refactor of time parsing functions?
    #if(isDV){ ts$time <- flexibleTimeParse(ts$time, timezone = data$reportMetadata$timezone) }
    
    startGaps <- sort(startGaps)
    endGaps <- sort(endGaps)

    # working with list data (fiveyr and dvhydro)
    if(class(ts) == "list"){
      dataWithoutGaps <- data.frame(time = ts$time, value = ts$value,
                                    stringsAsFactors = FALSE)
    } else if(class(ts) == "data.frame"){
      dataWithoutGaps <- ts
    } else {
      dataWithoutGaps <- data.frame()
    }
    
    dataSplit <- list()
    for(g in 1:length(startGaps)){
      
      if(isDV) {
        dataBeforeGap <- dataWithoutGaps[which(dataWithoutGaps[['time']] <= startGaps[g]),]
        dataWithoutGaps <- dataWithoutGaps[which(dataWithoutGaps[['time']] >= endGaps[g]),]
      } else {
        dataBeforeGap <- dataWithoutGaps[which(dataWithoutGaps[['time']] <= startGaps[g]),]
        dataWithoutGaps <- dataWithoutGaps[which(dataWithoutGaps[['time']] >= endGaps[g]),]
      }
      

      # only add dataBeforeGap if it exists, sometimes gap dates are earlier than any data 
      if(!isEmptyVar(dataBeforeGap)) { 
        dataSplit <- append(dataSplit, list(dataBeforeGap))
      }
      
      #leave the loop when there is no data left to split, sometimes gap dates are later than any
      if(isEmptyVar(dataWithoutGaps)) { 
        break  
      }
      
    }
    
    if(!isEmptyVar(dataWithoutGaps)){
      dataSplit <- append(dataSplit, list(dataWithoutGaps))
    }
    
    if(class(ts) == "list"){
      dataSplit <- lapply(dataSplit, function(d, legend.name){
        d <- as.list(d)
        d$legend.name <- legend.name
        return(d)
      }, legend.name = ts$legend.name)
    }
    
  } else {
    dataSplit <- list(ts)
  }

  return(dataSplit)
}

#' use splitDataGaps and format the resulting data correctly
#' @param data original list format of JSON
#' @param relevantData contains all ts/vars that are not empty (equals allVars in the *-data.R script)
#' @param isDV logic for whether this plot uses daily values or not
applyDataGaps <- function(data, relevantData, isDV=FALSE){

  #separate data with gaps
  haveField <- unlist(lapply(relevantData, function(v){"field" %in% names(v)}))
  gapData <- unlist(lapply(relevantData[haveField], splitDataGaps, data=data, isDV=isDV), recursive=FALSE)
  
  if(!isEmptyOrBlank(gapData)){
    pattern <- paste0("(", paste(names(relevantData), collapse="|"), ")")
    names(gapData) <- regmatches(names(gapData), m=regexpr(pattern, names(gapData)))
    #add data back together
    relevantDataWithGaps <- append(relevantData[!haveField], gapData)
  } else {
    relevantDataWithGaps <- relevantData
  }
  
  return(relevantDataWithGaps)
}

#'Put the SIMS url (if it exists) into the base of the report
#'@param data coming in to create a plot which may have sims info
#'@export
#'@rdname getSimsUrl
getSimsUrl<- function(data){
  url <- data$simsUrl
  if(is.null(url) || url == '') {
    url <- "SIMS URL: NA"
  } else {
    url <- paste("<a href='",url,"' target='_blank'>","SIMS URL:",url,"</a>")
  }
  return(url)
}

#'Put the waterdata.usgs.gov url (if it exists) into the base of the report
#'@param data coming in to create a plot which may have waterdata info
#'@export
#'@rdname getWaterDataUrl
getWaterDataUrl <- function(data) {
  url <- data$waterdataUrl
  if (is.null(url) || url == '') {
    url <- "waterdata.usgs.gov URL: NA"
  } else {
    url <- paste("<a href='",url,"' target='_blank'>","waterdata.usgs.gov URL:",url,"</a>")
  }
  return(url)
}

#' Apply styles (and some properties) to approval bar rectangles.
#' @param object A gsplot, plot object.
#' @param data A list of gsplot objects to display on the plot.
#' @return gsplot object with approval bar rectangle styles applied.
ApplyApprovalBarStyles <- function(object, data) {
  # calculate approval bar rectangle, vertical extent
  ybottom <- ApprovalBarYBottom(
    object$side.2$lim, object$global$par$ylog, object$side.2$reverse
  )
  ytop <- ApprovalBarYTop(
    object$side.2$lim, object$global$par$ylog, object$side.2$reverse
  )
  
  # for any approval intervals present...
  for (i in grep("^appr_", names(data))) {
    # look up style
    approvalBarStyles <- getApprovalBarStyle(data[i], ybottom, ytop)
    for (j in names(approvalBarStyles)) {
      # apply the styles
      object <- do.call(names(approvalBarStyles[j]),
                        append(list(object = object), approvalBarStyles[[j]]))
    }
  }
  return(object)
}

#' Compute top position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical top extent, in world coordinates.
ApprovalBarYTop <- function(lim, ylog, reverse) {
  return(ApprovalBarY(lim, ylog, reverse, 0.0245))
}

#' Compute bottom position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @return Approval bar, vertical bottom extent, in world coordinates.
ApprovalBarYBottom <- function(lim, ylog, reverse) {
  return(ApprovalBarY(lim, ylog, reverse, 0.04))
}

#' Compute top or bottom vertical position of approval bars.
#' @param lim The y-axis real interval, as two element vector.
#' @param ylog A Boolean, indicating whether the y-axis is log_10 scale:
#'             TRUE => log_10; FALSE => linear.
#' @param reverse A Boolean, indicating whether the y-axis is inverted:
#'                TRUE => inverted y-axis; FALSE => not inverted.
#' @param ratio A scaling ratio to adjust top or bottom of approval bar rectangle.
#' @return Approval bar, top or bottom y-axis point, in world coordinates.
ApprovalBarY <- function(lim, ylog = NULL, reverse, ratio) {
  if (is.null(ylog)) {
    # presume the semantics of NULL as FALSE, which may or not be correct, but 
    # prevents the code from terminating here
    ylog <- FALSE
  }
  
  e.0 <- lim[1]
  e.1 <- lim[2]
  
  # if this is a log10 y-axis
  if (ylog) {
    # if y-axis is inverted
    if (reverse) {
      y <- 10^(log10(e.1) + ratio * (log10(e.1) - log10(e.0)))
    }
    else {
      y <- 10^(log10(e.0) - ratio * (log10(e.1) - log10(e.0)))
    }
  }
  else {
    if (reverse) {
      y <- e.1 + ratio * (e.1 - e.0)
    }
    else {
      y <- e.0 - ratio * (e.1 - e.0)
    }
  }
  
  return(y)
}

#' Rescale top of y-axis to create ~4% margin between vertical top extent of 
#' plot objects and top edge of plot. This is an inaccurate emulation of (the 
#' top-end-of-plot behavior of) R graphics::par's "yaxs = 'r'" state, because we
#' have to use "yaxs = 'i'" in spots, but still want the ~4% margin at the top 
#' of the plot, so we adjust the y-axis endpoint accordingly after we do what we
#' need.
#' @param object A gsplot, plot object.
#' @return The passed-in gsplot object, with y-axis top augmented (upwards).
RescaleYTop <- function(object) {
  ylog <- par("ylog")
  reverse <- object$side.2$reverse
  
  if (ylog) {
    # if the y-axis is inverted
    if (reverse) {
      # add margin by rescaling -4%
      object$side.2$lim[1] <- 10^(0.96 * log10(object$side.2$lim[1]))
    }
    else {
      # ...as well, but rescale by +4%
      object$side.2$lim[2] <- 10^(1.04 * log10(object$side.2$lim[2]))
    }
  }
  else {
    # if the y-axis is inverted
    if (reverse) {
      # add margin by rescaling -4%
      object$side.2$lim[1] <- 0.96 * object$side.2$lim[1]
    }
    else {
      # ...as well, but rescale by +4%
      object$side.2$lim[2] <- 1.04 * object$side.2$lim[2]
    }
  }

  return(object)
}

