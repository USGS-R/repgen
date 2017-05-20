#'@aliases parseCustomDataElementsForTemplate
#'@rdname parseCustomDataElementsForTemplate
setMethod("parseCustomDataElementsForTemplate", signature(reportData = "derivationchain"), 
    definition = function(reportData) {
      return(parseCustomDataElementsForTemplateForDerivationChain(reportData))
    }
)

#' parseCustomDataElementsForTemplateForDerivationChain
#' @description Will return the derivations array as a json fragment
#' @param reportData 
#' @return list of data elements for template
parseCustomDataElementsForTemplateForDerivationChain <- function(reportData) {
  dcJson <- toJSON(reportData[['derivationsInChain']])
  
  return(list(
          derivationsJsonText=dcJson
      ))
}