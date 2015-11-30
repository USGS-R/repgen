#'Starting point, creates RMD and runs rendering
#'@param data coming in to create a plot
#'@param output format (either html or pdf)
#'@param author name of person generating the report
#'@rdname startFiveYearRender 
#'@export
startFiveYearRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('fiveyeargwsum', 'fiveyrgwsum.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}
