getPriorityStat <- function(reportObject){
  descriptions <- c(fetchReportMetadataField(reportObject, 'firstStatDerivedLabel'), 
                    fetchReportMetadataField(reportObject, 'secondStatDerivedLabel'), 
                    fetchReportMetadataField(reportObject, 'thirdStatDerivedLabel'))
  
  match_index <- grep(x = descriptions, pattern = "Mean")
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Max")} 
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Min")}
  
  match_names <- c("firstStatDerived", "secondStatDerived", "thirdStatDerived")
  match_description_nm <- c("firstStatDerivedLabel", "secondStatDerivedLabel", "thirdStatDerivedLabel")
  
  return(list(data_nm = match_names[match_index], descr_nm = match_description_nm[match_index]))
}
