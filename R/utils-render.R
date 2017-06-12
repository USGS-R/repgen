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
#' @param name an identifier to use in creating temp dirs
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

#' Starting point for template based rendering, this will load common templates and render a shared header/footer and then load
#' the templates for the report (the reportJson type) and perform execute the renderFragments function to retrieve report-defined
#' fragments.
#' 
#' @description this function orchestrates the creation of the html reports using whisker, the final result is written to a file, and that file name is returned.
#' 
#' @param reportJson data coming in to create the report, this needs to be typed and that type needs to have the "renderFragments" function defined for it. The type will also be the name of the report.
#' @param author string to identifier for person calling report
#' 
#' @return the html file and other report bits
#' 
startTemplatedRender <- function(reportJson, author){
  reportName <- class(reportJson)
  
  output_dir <- createOutputDir(reportName)
  
  templateData <- list()
  partials <- list()
  
  templateData[["version"]] <- printVersionStrings()
  templateData[["created"]] <- sprintf("%s  (%s)", Sys.time(), Sys.timezone())
  
  #load header image
  templateData[["usgsLogoBase64"]] <- loadUsgsLogoBase64()
  
  mainReportTemplate <- loadCommonTemplate('report')
      
  #load header template
  partials[['header']] <- loadCommonTemplate('header')
  templateData <- append(templateData, list(reportMetadata=fetchReportMetadata(reportJson)))
      
  #load shared.css for this report
  templateData[['sharedCssBase64']] <- loadCommonCssBase64()
  
  #call custom render function for the report, it should turn a list of HTML fragments that will available to the templates
  templateData[["renderedFragments"]] <- renderCustomFragments(reportJson)
  templateData[['reportMetadata']][['displayStartDate']] <- format(flexibleTimeParse(templateData[['reportMetadata']][['startDate']], templateData[['reportMetadata']][['timezone']]), "%Y-%m-%d")
  templateData[['reportMetadata']][['displayEndDate']] <- format(flexibleTimeParse(templateData[['reportMetadata']][['endDate']], templateData[['reportMetadata']][['timezone']]), "%Y-%m-%d")
  
  #adding appropriately formatted start and end dates to reportJSON
  templateData[['reportMetadata']][['displayStartDate']] <- format(flexibleTimeParse(templateData[['reportMetadata']][['startDate']], templateData[['reportMetadata']][['timezone']]), "%Y-%m-%d")
  templateData[['reportMetadata']][['displayEndDate']] <- format(flexibleTimeParse(templateData[['reportMetadata']][['endDate']], templateData[['reportMetadata']][['timezone']]), "%Y-%m-%d")
  
  #call custom data parsing, this will allow reports to specify the data structure they want available to the templates
  templateData[["reportData"]] <- parseCustomDataElementsForTemplate(reportJson)
  
  #load all templates/CSS/JS libs for this report
  partials[['body']] <- loadReportTemplate(reportName, 'body')
  partials <- append(partials, loadPartialsListForReport(reportName))
  templateData[['customJsBase64']] <- loadCustomJsBase64(reportName)
  templateData[['customCssBase64']] <- loadCustomCssBase64(reportName)
  templateData[['libsCss']] <- loadImportsIntoListBase64(reportName, "css.imports")
  templateData[['libsJs']] <- loadImportsIntoListBase64(reportName, "js.imports")
  
  #render final document
  renderedReport <- whisker.render(template = mainReportTemplate, data = templateData, partials = partials)
 
  #write final HTML to output_file
  out_file <- paste0(output_dir, "/", reportName, ".html")
  writeLines(renderedReport, out_file)
  
  return(out_file)
}

#' Load Common Template
#' 
#' @description will load a template with the given name from the common set of templates
#' @param templateName name of tempalte not including extension (all templates must have the .mustache extension)
#' @return string template
#' @importFrom readr read_file
loadCommonTemplate <- function(templateName) {
  return(read_file(system.file('templates', 'common', paste0(templateName, '.mustache'), package = 'repgen')))
}

#' Load Common CSS
#' 
#' @description will load a CSS file that is common to all reports and base64 encode it
#' @return base64 string CSS
#' @importFrom base64enc base64encode
loadCommonCssBase64 <- function() {
  return(base64encode(system.file('templates', 'common', 'shared.css', package = 'repgen')))
}

#' Load Report JS
#' 
#' @description will load the main JS file and base64 encode it for the given report. Reports should have files provided in common file structure.
#' @param reportName name of report to load for
#' @return base64 string JS
#' @importFrom base64enc base64encode
loadCustomJsBase64 <- function(reportName) {
  return(base64encode(system.file('templates', reportName, 'custom.js', package = 'repgen')))
}

#' Load Report CSS
#' 
#' @description will load the main CSS file and base64 encode it for the given report. Reports should have files provided in common file structure.
#' @param reportName name of report to load for
#' @return base64 string CSS
#' @importFrom base64enc base64encode
loadCustomCssBase64 <- function(reportName) {
  return(base64encode(system.file('templates', reportName, 'custom.css', package = 'repgen')))
}

#' Load Report Template
#' 
#' @description will load a template with the given name from the report specified
#' @param reportName name of report to load for
#' @param templateName name of tempalte not including extension (all templates must have the .mustache extension)
#' @return string template
#' @importFrom readr read_file
loadReportTemplate <- function(reportName, templateName) {
  return(read_file(system.file('templates', reportName, paste0(templateName, '.mustache'), package = 'repgen')))
}

#' loadUsgsLogoBase64
#' @description will load the logo image and convert to base64 text
#' @return base64 string representation of an image
#' 
#' @importFrom base64enc base64encode
loadUsgsLogoBase64 <- function() {
  return(base64encode(system.file('shared','usgs_logo.jpg', package = 'repgen')))
}

#' loadPartialsListForReport
#' @description Will return a named list of loaded template strings for given report. The name wills be the template file name without the .mustache extensions
#' @param reportName name of report (folder) to load partials from
#' @return named list of loaded templates
#' @importFrom readr read_file
loadPartialsListForReport <- function(reportName) {
  partials = list();
  
  if(file.exists(system.file('templates', reportName, 'partials', package = 'repgen'))) {
    partialsFolder <- system.file('templates', reportName, 'partials', package = 'repgen')
    partialsFileNames <- list.files(system.file('templates', reportName, 'partials', package = 'repgen'))
    
    for (partial in partialsFileNames) {
      name <- strsplit(partial, "\\.")[[1]][[1]]
      partials[[name]] <- read_file(system.file('templates', reportName, 'partials', partial, package = 'repgen'))
    }
  }
  return(partials)
}

#' loadImportsIntoListBase64
#' @description will load a list of imports specified in the reports filename file. the file is a line separted list of file paths relative from the shared/lib folder that will be loaded.
#' @param reportName name of report to load imports for
#' @param filename the file to load with the report's directory structure
#' @return a list of loaded imports in base64
#' @importFrom base64enc base64encode
loadImportsIntoListBase64 <- function(reportName, filename) {
  imports <- list()
  
  if(file.exists(system.file('templates', reportName, filename, package = 'repgen'))) {
    filesListed <- paste0(
        system.file('shared','libs', package = 'repgen'), "/", 
        readLines(system.file('templates', reportName, filename, package = 'repgen')))
    for (import in filesListed) {
      imports <- append(imports, list(list(base64=base64encode(import))))
    }
  }
  return(imports)
}