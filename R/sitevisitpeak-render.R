sitevisitpeakReport <- function(data) {
  
  ts <- data
  tbl <- sitevisitpeakTable(ts)
  formTable <- padTable(tbl)
  cat(formTable)
  
}

#'@export
# Starting point, creates RMD and runs rendering
#
startSiteVisitPeakRender <- function(data, output, author) {
  output_dir <- getwd()
  rmd_file <- system.file('sitevisitpeak', 'sitevisitpeak.Rmd', package = 'repgen')
  out_file <- render(rmd_file, paste0(output,"_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir=output_dir)
  return(out_file)
}

#'@title create a flat text 'sitevisitpeak table' type output table
#'@param rawData sitevisitpeak report json string
#'@importFrom dplyr mutate
#'@return string table
#'@export
#'
sitevisitpeakTable <- function(rawData){
  #Need to modify applyQualifiers to work for this.
  #data <- applyQualifiers(rawData)
  
  columnNames <- c("Visit Status",
                   "Date",
                   "Time",
                   "Party",
                   "Verification Method",
                   "Reading",
                   "Uncertainty",
                   "Estimated Date",
                   "Verification Comments",
                   "Corrected Value",
                   "Qualifier",
                   "Date",
                   "Time",
                   "Difference from Peak Verification Reading")
  
  #Sends in list of readings, and gets pack the formatted data.frame
  results <- formatSVPData(data$readings,columnNames)

  
  return(results)
}