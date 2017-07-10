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
  
  #load symbology processor images for derivation chain report
  derivationChainSymbols <- loadLegendSymbologyDerivationChain()
  
  dcJson <- toJSON(reportData[['derivationsInChain']])
  
  return(list(
          derivationsJsonText=dcJson, derivationChainSymbols=derivationChainSymbols
      ))
}

#' load legend symbology for derivation chain report
#' @description will load the legend icons needed and convert to base64 text
#' @return base64 string representation of legend images
#' @importFrom base64enc base64encode
loadLegendSymbologyDerivationChain <- function() {
  ratingModelDerived <- base64encode(system.file('templates/derivationchain','time-series-icons-rating-model-derived.png', package='repgen'))
  passThrough <- base64encode(system.file('templates/derivationchain','time-series-icons-derived-pass-through.png', package='repgen'))
  calculation <- base64encode(system.file('templates/derivationchain','time-series-icons-calculated.png', package='repgen'))
  basic <- base64encode(system.file('templates/derivationchain','time-series-icons-basic.png', package='repgen'))
  statDerived <- base64encode(system.file('templates/derivationchain','time-series-icons-statistical-derived.png', package='repgen'))
  external <- base64encode(system.file('templates/derivationchain','time-series-icons-external.png', package='repgen'))
  fillMissingData <- base64encode(system.file('templates/derivationchain','time-series-icons-fill-missing-data-derived.png', package='repgen'))
  conditionalFill <- base64encode(system.file('templates/derivationchain','time-series-icons-fill-missing-data-derived.png', package='repgen'))
  verticalDatumConversion <- base64encode(system.file('templates/derivationchain','time-series-icons-datum-conversion-derived.png', package='repgen'))
  noProcessor <- base64encode(system.file('templates/derivationchain','time-series-icons-no-processing-plans.png', package='repgen'))
  basicDescription <- base64encode(system.file('templates/derivationchain','basic-time-series-description.png', package='repgen'))
  processorDerivedDescription <-base64encode(system.file('templates/derivationchain','processor-derived-time-series-description.png', package='repgen'))
  externalTimeSeries <-base64encode(system.file('templates/derivationchain','external-time-series-line.png', package='repgen'))
  
  return(list(ratingModelDerived=ratingModelDerived, passThrough=passThrough, calculation=calculation, basic=basic, statDerived=statDerived, external=external, fillMissingData=fillMissingData, conditionalFill=conditionalFill, verticalDatumConversion=verticalDatumConversion, noProcessor=noProcessor, basicDescription=basicDescription, processorDerivedDescription=processorDerivedDescription, externalTimeSeries=externalTimeSeries))
}
