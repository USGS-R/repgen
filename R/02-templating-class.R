#' renderCustomFragments
#' 
#' Peform the rendering of subfragments for a given report, should return a list of html fragments to 
#' be available to inject into mustach templates
#'
#' @param reportData typed report json, the type should be the name of the report
#' @return vector of rendered HTML fragments that will be included in the data available to the final report template. The fragments will be
#' available to the templates as renderedFragments.html
#' 
#' @export
renderCustomFragments <- function(reportData) 0
setGeneric(name="renderCustomFragments",def=function(reportData){standardGeneric("renderCustomFragments")})

#' parseCustomDataElementsForTemplate
#' 
#' @description call custom data parsing, this will allow reports to specify the data structure they want available to the templates
#'
#' @param reportData typed report json, the type should be the name of the report
#' @return list of named data elements that should be available to your templates. Data will be placed in "reportData"
#' 
#' @export
parseCustomDataElementsForTemplate <- function(reportData) 0
setGeneric(name="parseCustomDataElementsForTemplate",def=function(reportData){standardGeneric("parseCustomDataElementsForTemplate")})

#' CLASSES
setClass("examplereport")
setClass("derivationchain")