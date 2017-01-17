context("utils-legend tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('does the remove duplicate legend items remove duplicates?', {
 
  library(jsonlite)
  library(lubridate)
  library(magrittr)
  library(gsplot)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','testsnippets','test-utils-legend.json', package = 'repgen'))
  
  #get the list of legend types from the json data
  legendTypesFromRaw <- c(data$upchainSeries$type, data$primarySeries$type, data$firstDownChain$type, data$upchainSeriesRaw$type, data$effectiveShifts$type, data$primarySeriesRaw$type) 
  duplicatesRaw <- which(duplicated(legendTypesFromRaw)) #should contain duplicates
  
  #pass the data through uvhydrographPlot which will remove duplicates with the call to rmDuplicateLegendItems
  uv_results <- repgen:::uvhydrographPlot(data) 
  which.duplicated <- which(duplicated(uv_results$legend$legend.auto$legend)) #should be empty
  
  #expect that the data going through uvhydrographPlot have duplicates removed
  expect_true(repgen:::isEmptyOrBlank(which.duplicated))
  
  #expect that the list of legend items that doesn't contain duplicates compared to the list that came from raw data do not match
  expect_false(isTRUE(which.duplicated==duplicatesRaw))
})

that_that('', {
  
  
  
})


setwd(dir = wd)