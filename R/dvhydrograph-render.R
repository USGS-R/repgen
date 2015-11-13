#
# Starting point, creates RMD and runs rendering
#
startDvhydrographRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('dvhydrograph', 'dvhydrograph.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir)
  return(out_file)
}

dvhydrographPlot <- function(data) {}
