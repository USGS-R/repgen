#'@aliases renderCustomFragments
#'@rdname renderCustomFragments
setMethod("renderCustomFragments", signature(reportData = "examplereport"), 
    definition = function(reportData) {
      return(renderCustomFragmentsForExampleReport(reportData))
    }
)

renderCustomFragmentsForExampleReport <- function(reportObject) {
  return(list(
          list(html=paste("HTML from code ", reportObject[['reportField1']])),
          list(html=paste("HTML from code ", reportObject[['reportField2']]))
          ))
}