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
  
  if(reportName == "vdiagram"){
    rmd_file <- makeVDiagramRmd(system.file('vdiagram', package = 'repgen'), data, output, output_dir)
    logo_file <- system.file('shared', 'usgs_logo.jpg', package = 'repgen')
    file.copy(logo_file, output_dir)
  } else {
    rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
  }
  
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}

