#'@aliases parseCustomDataElementsForTemplate
#'@rdname parseCustomDataElementsForTemplate
setMethod("parseCustomDataElementsForTemplate", signature(reportData = "examplereport"), 
    definition = function(reportData) {
      return(parseCustomDataElementsForTemplateForExampleReport(reportData))
    }
)

parseCustomDataElementsForTemplateForExampleReport <- function(reportObject) {
  return(list(
          reportField1=reportObject[['reportField1']],
          reportField2=reportObject[['reportField2']]
      ))
}