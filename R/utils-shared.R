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

############ used in dvhydrograph-data, fiveyeargwsum-data, uvhydrograph-data ############ 

getGroundWaterLevels<- function(ts, ...){
  y <- as.numeric(ts$gwlevel[['groundWaterLevel']])
  x <- ts$gwlevel[['recordDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, stringsAsFactors = FALSE))
}


getWaterQualityMeasurements<- function(ts, ...){
  if(is.null(ts$waterQuality)) {
    df <- data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))
    df <- na.omit(df)
    return(df)
  }
  y <- ts$waterQuality$value[['value']]
  x <- ts$waterQuality[['sampleStartDateTime']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, month=month, stringsAsFactors = FALSE))
}


getFieldVisitMeasurementsQPoints <- function(ts){
  y <- ts$fieldVisitMeasurements[['discharge']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  minQ <- ts$fieldVisitMeasurements[['errorMinDischarge']]
  maxQ <- ts$fieldVisitMeasurements[['errorMaxDischarge']]
  n <- ts$fieldVisitMeasurements[['measurementNumber']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minQ=minQ, maxQ=maxQ, n=n, month=month, stringsAsFactors = FALSE))
}


getFieldVisitMeasurementsShifts <- function(ts){
  y <- ts$fieldVisitMeasurements[['shiftInFeet']]
  x <- ts$fieldVisitMeasurements[['measurementStartDate']]
  minShift <- ts$fieldVisitMeasurements[['errorMinShiftInFeet']]
  maxShift <- ts$fieldVisitMeasurements[['errorMaxShiftInFeet']]
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month
  return(data.frame(time=time, value=y, minShift=minShift, maxShift=maxShift, month=month, stringsAsFactors = FALSE))
}


getCorrections <- function(ts, field){
  if(length(ts[[field]]) == 0){
    return()
  }
  
  x <- ts[[field]][['startTime']]
  comment <- ts[[field]][['comment']]
  if(!is.null(comment)) {
    comment <- paste("Start", comment, sep=" : ")
  }
  time = as.POSIXct(strptime(x, "%FT%T"))
  month <- format(time, format = "%y%m") #for subsetting later by month

  x2 <- ts[[field]][['endTime']]
  comment2 <- ts[[field]][['comment']]
  if(!is.null(comment2)) {
    comment2 <- paste("End", comment2, sep=" : ")
  }
  time2 = as.POSIXct(strptime(x2, "%FT%T"))
  month2 <- format(time2, format = "%y%m") #for subsetting later by month
  
  #labeled as NA in table:
  if(is.null(comment)){ comment <- "N/A" }
  if(is.null(comment2)){ comment2 <- "N/A" }

  #value needs to be NA in order for series corrections to make it through checks in parseUVData
  return(data.frame(time=c(time, time2), value = NA, month=c(month, month2),
                    comment=c(comment, comment2), stringsAsFactors = FALSE))
  # }
}
getEstimatedDates <- function(data, chain_nm, time_data){
  i <- which(data[[chain_nm]]$qualifiers$identifier == "ESTIMATED")
  startTime <- formatDates(data[[chain_nm]]$qualifiers$startDate[i])
  endTime <- formatDates(data[[chain_nm]]$qualifiers$endDate[i])
  est_dates <- data.frame(start = startTime, end = endTime)
  
  date_index <- c()
  for(n in seq(nrow(est_dates))){
    date_index_n <- which(time_data >= est_dates$start[n] & time_data <= est_dates$end[n])
    date_index <- append(date_index, date_index_n)
  }
  
  return(date_index)
}

getApprovals <- function(data, chain_nm, legend_nm, appr_var_all, plot_type=NULL, month=NULL, point_type=NULL){
  appr_type <- c("Approved", "In Review", "Working")
  approvals_all <- list()
  
  isDV <- grepl("derivedSeries", chain_nm)
  
  for(approval in appr_type){
    appr_var <- appr_var_all[which(appr_type == approval)]
    
    if(plot_type == "uvhydro"){
      points <- subsetByMonth(getUvHydro(data, chain_nm), month)
    } else {
      points <- data[[chain_nm]][['points']]
      points$time <- formatDates(points[['time']], plot_type, type=NA)
    }
    
    appr_dates <- getApprovalDates(data, plot_type, chain_nm, approval)
    date_index <- apply(appr_dates, 1, function(d, points){
      which(points$time >= d[1] & points$time <= d[2])}, 
      points=points)
    
    if(is.list(date_index)){
      date_index_list <- date_index
    } else {
      date_index_list <- list(date_index)
    }
    
    approval_info <- list()
    for(i in seq_along(date_index_list)){
      d <- date_index_list[[i]]
      applicable_dates <- points[['time']][d]
      
      if(isDV){
        applicable_values <- points[['value']][d]
      } else {
        applicable_values <- substitute(getYvals_approvals(plot_object, length(applicable_dates)))
      }
      
      approval_info[[i]] <- list(time = applicable_dates,
                                 value = applicable_values,
                                 legend.name = paste(approval, legend_nm),
                                 point_type = point_type)
    }
    
    names(approval_info) <- rep(appr_var, length(date_index_list))
    
    approvals_all <- append(approvals_all, approval_info)
  }
  
  return(approvals_all)
}

getYvals_approvals <- function(object, num_vals){
  ylim <- ylim(object)$side.2[1]
  yvals <- rep(ylim, num_vals)
}

getApprovalDates <- function(data, plot_type, chain_nm, approval){
  i <- which(data[[chain_nm]]$approvals$description == approval)
  startTime <- formatDates(data[[chain_nm]]$approvals$startTime[i], plot_type, type=NA)
  endTime <- formatDates(data[[chain_nm]]$approvals$endTime[i], plot_type, type=NA)
  return(data.frame(startTime=startTime, endTime=endTime))
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

############ used in dvhydrograph-data, correctionsataglance-data, fiveyeargwsum-data, uvhydrograph-data ############ 

formatDates <- function(char_date, plot_type=NULL, type=NA){
  date_formatted <- as.POSIXct(strptime(char_date, "%FT%T"))
  if(!is.null(plot_type) && plot_type == "fiveyr"){
    if(!is.na(type) && type=="start"){
      date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-01"))
    } else if(!is.na(type) && type=="end"){
      date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-30"))
    }
  }
  return(date_formatted)
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

rm.duplicates <- function(object, list_element, var_name){
  names <- unlist(unname(sapply(object[[list_element]], function(x) {
    ifelse(is.null(x[[var_name]]), NA, x[[var_name]])
  })))
  
  if(grepl("view", list_element)){
    for (k in which(duplicated(names))){   
      if(!is.na(names[k])) {object[[list_element]][[k]][[var_name]] <- NULL}
    }
  } else if(list_element == "legend") {
    object[[list_element]] <- object[[list_element]][which(!duplicated(names))]
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
