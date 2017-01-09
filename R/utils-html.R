#'Starting point, creates RMD and runs rendering
#'@param data coming in to create a plot
#'@param author name of person generating the report
#'@param reportName name of report being generated (current options: dvhydrograph, fiveyruvhydrograph, vdiagram)
#'@rdname startRender 
#'@export 
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

############ used in sitevisitpeak-data and sensorreading-data ############ 

formatComments <- function(comments){
  split_comments <- unlist(comments)
  if(is.null(split_comments) || nchar(split_comments) == 0){return(split_comments)}
  htmlbreaks_inside <- lapply(split_comments, gsub, pattern="\r\n", replacement="</br>")
  htmlbreaks_end <- lapply(htmlbreaks_inside, paste0, "</br>", collapse="")
  table_comments <- do.call(paste0, htmlbreaks_end)
  return(table_comments)
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

#'Put the waterdata.usgs.gov url (
#'if it exists) into the base of the report
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


#Will clear out any folders and files in temp folder that are older than 5 minutes
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
#' @param characters The string to convert
#' @return The equivalent HTML codes for that string
convertStringToTableDisplay <- function(characters){
  characters <- gsub(">", "&gt;", gsub("<", "&lt;", characters))
  return(characters)
}

#' Convert the String from HTML code to the equivalent raw characters
#' @param characters The characters to convert
#' @return The equivalent string for the HTML codes
convertTableDisplayToString <- function(characters){
  characters <- gsub("&gt;", ">", gsub("&lt;", "<", characters))
  return(characters)
}

#' shared logo used for reports
getLogo <- function(){
  jpg_filepath <- 'usgs_logo.jpg'
  markdown_text <- noquote(paste0("![](", jpg_filepath, ")"))
  return(markdown_text)
}