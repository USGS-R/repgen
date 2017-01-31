getPriorityStat <- function(reportObject){
  descriptions <- c(fetchReportMetadataField(reportObject, 'downChainDescriptions1'), 
                    fetchReportMetadataField(reportObject, 'downChainDescriptions2'), 
                    fetchReportMetadataField(reportObject, 'downChainDescriptions3'))
  
  match_index <- grep(x = descriptions, pattern = "Mean")
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Max")} 
  if(length(match_index)==0){match_index <- grep(x = descriptions, pattern = "Min")}
  
  match_names <- c("firstDownChain", "secondDownChain", "thirdDownChain")
  match_description_nm <- c("downChainDescriptions1", "downChainDescriptions2", "downChainDescriptions3")
  
  return(list(data_nm = match_names[match_index], descr_nm = match_description_nm[match_index]))
}
