#' Starting point, creates RMD and runs rendering
#' 
#' @description this function orchestrates the creation of the html reports
#' 
#' @param data coming in to create a plot
#' @param author name of person generating the report
#' @param reportName name of report being generated
#' 
#' @return the html file and other report bits
#' 
startRender <- function(data, author, reportName){
  data <- data 
  
  wd <- getwd()
  tmp_folder_name <- paste0('tmp-', reportName)
  
  output_dir <- paste0(wd, "/", tmp_folder_name)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  #copy all shared files to tmp folder
  shared_files <- list.files(system.file('shared', package = 'repgen'), full.names = TRUE)
  file.copy(shared_files, output_dir)
  
  #copy all report inst files to tmp folder
  report_files <- list.files(system.file(reportName, package = 'repgen'), full.names = TRUE)
  file.copy(report_files, output_dir)
  
  if(reportName == "vdiagram"){
    rmd_file <- makeVDiagramRmd(output_dir, data, output_dir)
  } else {
    rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
  }
  
  out_file <- render(rmd_file, paste0("html_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir = output_dir, 
                     output_options=c(paste0("lib_dir=", output_dir)), 
                     clean=TRUE)
  
  #try to clean up old temp files
  cleanTempSpace()
  
  return(out_file)
}

#' Used by UV Hydrograph report to organize and fit plots and tables in report
#' 
#' @description sets margins and lays out report in an attractive way for UV Hydrograph
#' 
#' @param feature the plot or table to include
#' @param isTable TRUE or FALSE if the feature is a table. defaults to FALSE
#' @param m the month name for the feature
#' @param mar_values the margin values for the feature to ensure space for plot and legend
#' 
#' @return the formatted feature requested
#' 
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

#' Takes comments formatted with special escape characters and converts them to HTML breaks
#' and includes a break at the end between comments
#' 
#' @description Formats comments more effectively for including them in HTML table
#' 
#' @param comments which have \\r\\n indicating line breaks
#' 
#' @return comments formatted with html breaks instead of escape pattern
#' 
formatComments <- function(comments){
  split_comments <- unlist(comments)
  if(isEmptyOrBlank(split_comments)){return(split_comments)}
  htmlbreaks_inside <- lapply(split_comments, gsub, pattern="\r\n", replacement="</br>")
  htmlbreaks_end <- lapply(htmlbreaks_inside, paste0, "</br>", collapse="")
  table_comments <- do.call(paste0, htmlbreaks_end)
  return(table_comments)
}

#' Inserts the SIMS url (if it exists) into the base of the report
#' 
#' @description takes the SIMS url and formats it 
#' for including in the report as a link
#' 
#' @param reportObject coming in to create a plot which may have sims info
#' 
#' @return the HTML link for SIMS url
#' 
getSimsUrl<- function(reportObject){
  url <- reportObject[["simsUrl"]]
  if(isEmptyOrBlank(url)) {
    url <- "SIMS URL: NA"
  } else {
    url <- paste("<a href='",url,"' target='_blank'>","SIMS URL:",url,"</a>")
  }
  return(url)
}

#'Put the waterdata.usgs.gov url (if it exists) into the base of the report
#'
#'@description takes the waterdata url and formats it for including it
#'in the report as a link
#'
#'@param reportObject coming in to create a plot which may have waterdata info
#'
#'@return The HTML link for waterdata url
#'
getWaterDataUrl <- function(reportObject) {
  url <- reportObject[["waterdataUrl"]]
  if (isEmptyOrBlank(url)) {
    url <- "waterdata.usgs.gov URL: NA"
  } else {
    url <- paste("<a href='",url,"' target='_blank'>","waterdata.usgs.gov URL:",url,"</a>")
  }
  return(url)
}

#' Clean up temporary disk space used when rendering reports
#' 
#' @description deletes report components after they are used in render function
#' and in case any old files are lingering (render crashed or errored for some reason) 
#' also removes anything older than 5 minutes when the function is called
#' 
cleanTempSpace <- function() {
  tempdir <- dirname(tempfile())
  allTempFiles <- paste0(tempdir, "/", list.files(tempdir))
  lastAccessTimes <- file.info(allTempFiles)$atime
  
  for (i in 1:length(allTempFiles)) { 
    if(difftime(Sys.time(), lastAccessTimes[i], units="mins") > 5) { #delete anything older than 5 minutes
      unlink(allTempFiles[i], recursive=TRUE)
    }
  }
}

#' Convert the string to the equivalent HTML code
#' 
#' @param characters The string to convert
#' 
#' @return The equivalent HTML codes for that string
#' 
convertStringToTableDisplay <- function(characters){
  characters <- gsub(">", "&gt;", gsub("<", "&lt;", characters))
  return(characters)
}

#' Convert the String from HTML code to the equivalent raw characters
#' 
#' @param characters The characters to convert
#' 
#' @return The equivalent string for the HTML codes
#' 
convertTableDisplayToString <- function(characters){
  characters <- gsub("&gt;", ">", gsub("&lt;", "<", characters))
  return(characters)
}

#' shared logo used for reports
#' 
#' @description provides rmarkdown with the image for USGS logo
#'  
#' @return logo to report
getLogo <- function(){
  jpg_filepath <- 'usgs_logo.jpg'
  markdown_text <- noquote(paste0("![](", jpg_filepath, ")"))
  return(markdown_text)
}

#' if a value is null, returns an empty string instead of null, and
#' if a value is present, the value is returned
#' 
#' @description makes sure that the slot in the data frame is not missing by
#' exchanging null values as empty character or the original value if not null
#' 
#' @param val the value you want to check for null and mask
#' 
#' @return either the original value or a null empty character object
#' 
nullMask <- function(val) {
  if(!is.null(val)) {
    result <- val
  } else {
    result <- ""
  }
  return(result)
}

#' @title timeFormatting
#' @description Formats date to passed-in format mask, and time to "(UTC [offset] )"
#' @param timeVals String with format of "YYYY-MM-DDTHH:MM:SS.SSS-UTC offset".
#' @param dateFormatMask String with preferred output date format
#' @return list with date in first position, time in second position.
timeFormatting <- function(timeVals, dateFormatMask){
  if(!isEmpty(timeVals)) {
    dateTime <- (strsplit(timeVals, split="[T]"))
    dateFormat <- strftime(dateTime[[1]][1], dateFormatMask)
    
    #Break apart, format dates/times, put back together.
    timeFormatting <- sapply(dateTime[[1]][2], function(s) {
      #Break apart the date and time into a list of two strings
      m <- regexec("([^-+]+)([+-].*)", s)
      splitTime <- unlist(regmatches(s, m))[2:3]
      return(splitTime)
    })
    timeFormatting[[1]] <- sapply(timeFormatting[[1]], function(s) sub(".000","",s))
    timeFormatting[[2]] <- paste0(" (UTC ",timeFormatting[[2]], ")")
    timeFormatting <-  paste(timeFormatting[[1]],timeFormatting[[2]])
  } else {
    dateFormat <- ""
    timeFormatting <- ""
  }
  return(list(date = dateFormat, time = timeFormatting))
}