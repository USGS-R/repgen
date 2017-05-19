renderCustomFragments.examplereport <- function(reportObject) {
  return(list(
          html=reportObject[['reportField1']],
          html=reportObject[['reportField2']]
          ))
}