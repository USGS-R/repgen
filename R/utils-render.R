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

#' createOutputDir
#' 
#' @description Creates a unique output director in the current working director
#' @return the output directory created
createOutputDir <- function(name) {
  wd <- getwd()
  tmp_folder_name <- paste0('tmp-', name)
  output_dir <- paste0(wd, "/", tmp_folder_name)
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  return(output_dir)
}

#' DEPRECATED
#' 
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
  warning("This rendering style is deprecated, convert to using whisker style and startTemplatedRender")
  
  data <- data 
  
  output_dir <- createOutputDir(reportName)
  
  #copy all shared files to tmp folder
  shared_files <- list.files(system.file('shared', package = 'repgen'), full.names = TRUE)
  file.copy(shared_files, output_dir)
  
  #copy all report inst files to tmp folder
  report_files <- list.files(system.file(reportName, package = 'repgen'), full.names = TRUE)
  file.copy(report_files, output_dir)
  
  rmd_file <- system.file(reportName, paste0(reportName, '.Rmd'), package = 'repgen')
  
  out_file <- render(rmd_file, paste0("html_document"), params = list(author=author), 
                     output_dir = output_dir, intermediates_dir = output_dir, 
                     output_options=c(paste0("lib_dir=", output_dir)), 
                     clean=TRUE)
  
  #try to clean up old temp files
  cleanTempSpace()
  
  return(out_file)
}


#' Peform the rendering of subfragments for a given report, should return a list of html fragments to 
#' be available to inject into mustach templates
#'
#' @param reportData typed report json, the type should be the name of the report
#' @return list of rendered HTML fragments that will be included in the data available to the final report template
#' 
#' @export
renderCustomFragments <- function(reportData) UseMethod("renderCustomFragments")

#' Peform pulls
#'
#' @param reportData typed report json, the type should be the name of the report
#' @return list of named data elements that should be available to your templates
#' 
#' @export
parseCustomDataElementsForTemplate <- function(reportData) UseMethod("parseCustomDataElementsForTemplate")

#' Starting point for template based rendering, this will load common templates and render a shared header/footer and then load
#' the templates for the report (the reportJson type) and perform execute the renderFragments function to retrieve report-defined
#' fragments.
#' 
#' @description this function orchestrates the creation of the html reports using whisker, the final result is written to a file, and that file name is returned.
#' 
#' @param reportData data coming in to create the report, this needs to be typed and that type needs to have the "renderFragments" function defined for it. The type will also be the name of the report.
#' 
#' @return the html file and other report bits
#' 
startTemplatedRender <- function(reportJson, author){
  reportName <- class(reportJson)
  
  output_dir <- createOutputDir(reportName)
  
  templateData <- list()
  partials <- list()
  
  #load header image
  #TODO
  
  mainReportTemplate <- read_file(system.file('templates', 'common', 'report.mustache', package = 'repgen'))
      
  #load header template
  partials[['header']] <- read_file(system.file('templates', 'common', 'header.mustache', package = 'repgen'))
  templateData <- append(templateData, list(reportMetadata=fetchReportMetadata(reportJson)))
      
  #call custom render function
  templateData[["renderedFragments"]] <- renderCustomFragments(reportJson)
  
  #call custom data parsing
  templateData[["reportData"]] <- parseCustomDataElementsForTemplate(reportJson)
  
  #load shared.css for this report
  templateData[['sharedCss']] <- read_file(system.file('templates', 'common', 'shared.css', package = 'repgen'))
  
  #load all CSS/JS libs for this report
  #TODO
  
  #load custom.js for this report
  templateData[['customJs']] <- read_file(system.file('templates', reportName, 'custom.js', package = 'repgen'))
  
  #load custom.css for this report
  templateData[['customCss']] <- read_file(system.file('templates', reportName, 'custom.css', package = 'repgen'))
  
  #load body template for report
  partials[['body']] <- read_file(system.file('templates', reportName, 'body.mustache', package = 'repgen'))
  
  #render final document
  renderedReport <- whisker.render(template = mainReportTemplate, data = templateData, partials = partials)
 
  #write final HTML to output_file
  out_file <- paste0(output_dir, "/", reportName, ".html")
  writeLines(renderedReport, out_file)
  
  #try to clean up old temp files
  cleanTempSpace()
  
  return(out_file)
}

