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

printReportFeature <- function(feature, isTable=FALSE, m=NULL){
  if(!isEmpty(feature)){
    if(isTable){
      print(kable(feature))
      cat("\n\n")
    } else if(!is.null(m)){
      msg <- paste(feature, 'in', m)
      cat(msg)
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

############ used in dvhydrograph-data, correctionsataglance-data, fiveyeargwsum-data, uvhydrograph-data ############ 

formatDates <- function(char_date, type=NA){
  #attempt DV
  format <- "%F"
  date_formatted <- as.POSIXct(strptime(char_date, format))
  
  #try not dv, use timed format
  if(!is.na(isEmpty(date_formatted)) && isEmpty(date_formatted)) {
  format <- "%FT%T"
    date_formatted <- as.POSIXct(strptime(char_date, format))
  }
  
  if(!is.na(type) && type=="start"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-01"))
  } else if(!is.na(type) && type=="end"){
    date_formatted <- as.POSIXct(format(date_formatted, format="%Y-%m-30"))
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

############ used in sensorreading-data and sitevisitpeak-data ############ 

isEmpty <- function(val){
  result <- (is.null(val) || is.na(val))
  return(result)
}

############ used in various places ############ 

isEmptyOrBlank <- function(val = NULL, listObjects = NULL, objectName = NULL){
  if(is.null(objectName)){
    result <- (length(val)==0 || isEmpty(val) || val=="")
  } else {
    result <- !objectName %in% listObjects
  }
  return(result)
}

############ used in uvhydrograph-data, dvhydrograph-data, fiveyeargwsum-data ############ 

isEmptyVar <- function(variable){
  all(!is.null(variable), 
      nrow(variable) != 0 || is.null(nrow(variable)), 
      length(variable$time[!is.na(variable$time)]) != 0)
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
}
