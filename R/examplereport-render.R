renderCustomFragments.examplereport <- function(reportObject) {
  return(list(
          list(html=paste("HTML from code ", reportObject[['reportField1']])),
          list(html=paste("HTML from code ", reportObject[['reportField2']]))
          ))
}