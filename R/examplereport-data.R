parseCustomDataElementsForTemplate.examplereport <- function(reportObject) {
  return(list(
          reportField1=reportObject[['reportField1']],
          reportField2=reportObject[['reportField2']]
      ))
}