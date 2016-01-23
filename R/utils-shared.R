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
  
  renamed_rmd <- NULL
  
  if(reportName == "vdiagram"){
    rmd_file <- makeVDiagramRmd(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
  } else {
    rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
    
    #make a renamed copy of rmd file with a unique name based on the output folder
    folder_name <- basename(output_dir) 
    new_file_name <- paste0(folder_name, ".", reportName, '.Rmd')
    new_file_full_path <- paste0(dirname(file.path(rmd_file)), "/", new_file_name)
    file.copy(rmd_file, new_file_full_path)
    
    #copy renamed file back 
    renamed_rmd <- system.file(reportName, new_file_name, package = 'repgen')
    rmd_file <- renamed_rmd
  }
  
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  
  #delete renamed rmd now that we are done
  if(!is.null(renamed_rmd)) {
    file.remove(renamed_rmd);
  }
  
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
