#'@aliases parseCustomDataElementsForTemplate
#'@rdname parseCustomDataElementsForTemplate
setMethod("parseCustomDataElementsForTemplate", signature(reportData = "derivationchain"), 
    definition = function(reportData) {
      return(parseCustomDataElementsForTemplateForDerivationChain(reportData))
    }
)

#' parseCustomDataElementsForTemplateForDerivationChain
#' @description Will return the derivations array as a json fragment
#' @param reportData full report data structure 
#' @return list of data elements for template
#' @importFrom jsonlite toJSON
parseCustomDataElementsForTemplateForDerivationChain <- function(reportData) {
  dcJson <- toJSON(reportData[['derivationsInChain']])
  
  return(list(
          derivationsJsonText=dcJson
      ))
}