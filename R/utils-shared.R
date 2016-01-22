#shared functions between reports

#'Starting point, creates RMD and runs rendering
#'@param data coming in to create a plot
#'@param output format (either html or pdf)
#'@param author name of person generating the report
#'@param reportName name of report being generated (current options: dvhydrograph, fiveyruvhydrograph, vdiagram)
#'@rdname startRender 
#'@export 
startRender <- function(data, output, author, reportName){
  output_dir <- getwd()
  data <- data 
  
  logo_file <- system.file('shared', 'usgs_logo.jpg', package = 'repgen')
  file.copy(logo_file, output_dir)
  
  if(reportName == "vdiagram"){
    rmd_file <- makeVDiagramRmd(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
  } else {
    rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
  }
  
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir = output_dir)
  return(out_file)
}

############ used in dvhydrograph-data and fiveyeargwsum-data ############ 

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


############ used in dvhydrograph-data and correctionsataglance-data ############ 

formatDates <- function(char_date){
  as.POSIXct(strptime(char_date, "%FT%T"))
}
