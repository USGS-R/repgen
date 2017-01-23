context("utils-legend tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('does the remove duplicate legend items remove duplicates?', {
 
  library(jsonlite)
  library(lubridate)
  library(magrittr)
  library(gsplot)
  library(dplyr)
  
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-utils-legend.json', package = 'repgen'))
  
  #get the list of legend types from the json data
  legendTypesFromRaw <- c(reportObject$upchainSeries$type, reportObject$primarySeries$type, reportObject$firstDownChain$type, reportObject$upchainSeriesRaw$type, reportObject$effectiveShifts$type, reportObject$primarySeriesRaw$type) 
  duplicatesRaw <- which(duplicated(legendTypesFromRaw)) #should contain duplicates
  
  #pass the data through uvhydrographPlot which will remove duplicates with the call to rmDuplicateLegendItems
  uv_results <- repgen:::uvhydrographPlot(reportObject) 
  which.duplicated <- which(duplicated(uv_results$legend$legend.auto$legend)) #should be empty
  
  #expect that the data going through uvhydrographPlot have duplicates removed
  expect_true(repgen:::isEmptyOrBlank(which.duplicated))
  
  #expect that the list of legend items that doesn't contain duplicates compared to the list that came from raw data do not match
  expect_false(isTRUE(which.duplicated==duplicatesRaw))
})

test_that('getTimeSeriesLabel() returns labels for requested time series and type', {
  library(jsonlite)
  reportObject <- fromJSON('{
    "testSeries1": {
                   "points": [
                   {
                   "time": "2014-11-20",
                   "value": 4510
                   },
                   {
                   "time": "2014-11-21",
                   "value": 3960
                   },
                   {
                   "time": "2014-11-23",
                   "value": 3960
                   },
                   {
                   "time": "2014-11-23",
                   "value": 3960
                   }
                   ],
                   "notes": [],
                   "isVolumetricFlow": true,
                   "description": "From Aquarius",
                   "qualifiers": [
                   {
                   "startDate": "2014-11-20T00:00:00-05:00",
                   "endDate": "2014-11-22T00:00:00-05:00",
                   "identifier": "ESTIMATED",
                   "code": "E",
                   "appliedBy": "admin",
                   "dateApplied": "2016-09-04T19:12:23.2865938Z"
                   }
                   ],
                   "units": "ft^3/s",
                   "grades": [
                   {
                   "startDate": "2014-11-20T00:00:00-05:00",
                   "endDate": "2015-11-21T00:00:00.0000001-05:00",
                   "code": "50"
                   }
                   ],
                   "type": "Discharge",
                   "gaps": [],
                   "estimatedPeriods": [
                   {
                   "startDate": "2014-11-20T00:00:00-05:00",
                   "endDate": "2014-11-22T00:00:00-05:00"
                   }
                   ],
                   "approvals": [
                   {
                   "level": 2,
                   "description": "Approved",
                   "comment": "",
                   "dateApplied": "2016-09-04T23:24:55.0225199Z",
                   "startTime": "1926-10-01T00:00:00-05:00",
                   "endTime": "2015-10-14T00:00:00-05:00"
                   }
                   ],
                   "name": "c0c9b14242ae41b285816918e504fdb6",
                   "startTime": "2014-11-19",
                   "endTime": "2015-11-20",
                   "gapTolerances": [
                   {
                   "startTime": "2014-11-20T00:00:00-05:00",
                   "endTime": "2015-11-21T00:00:00.0000001-05:00",
                   "toleranceInMinutes": 1440
                   }
                   ]
},
                   
                   "reportMetadata": {
                   "timezone": "Etc/GMT+5"
                   }
                   }')
  label <- repgen:::getTimeSeriesLabel(reportObject, "testSeries1")
  expect_equal(label, "Discharge  ( ft^3/s )")
  
})


setwd(dir = wd)