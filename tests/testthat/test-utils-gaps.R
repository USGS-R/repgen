context("utils-gaps tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('splitDataGaps and applyDataGaps work with data.frames (uvhydro)', {
  data <- fromJSON('{
                   "downsampledPrimarySeries": {
                   "gaps": [
                   {
                   "startTime": "2016-06-14T11:00:00-05:00",
                   "endTime": "2016-06-15T08:15:00-05:00"
                   }
                   ],
                   "points": [
                   {
                   "time": "2016-06-14T14:45:00.000Z",
                   "value": 2730.0
                   },
                   {
                   "time": "2016-06-14T16:00:00.000Z",
                   "value": 2770.0
                   },
                   {
                   "time": "2016-06-15T13:30:00.000Z",
                   "value": 2590.0
                   }
                   ]
                   },
                   "reportMetadata": {
                   "timezone": "Etc/GMT+5"
                   }
}')
  
  ts <- repgen:::subsetByMonth(repgen:::getTimeSeries(data, "downsampledPrimarySeries"), "1606")
  gapData <- repgen:::splitDataGaps(data, ts, isDV=FALSE)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 2)
  
  relevantData <- list(corr_UV = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 2)
  expect_true("corr_UV" %in% names(allVars))
  
  })

test_that('splitDataGaps and applyDataGaps work with lists (dvhydro)', {
  data <- fromJSON('{
                   "firstDownChain": {
                   "gaps": [
                   {
                   "startTime": "2016-02-12",
                   "endTime": "2016-04-15"
                   },
                   {
                   "startTime": "2016-06-13",
                   "endTime": "2016-06-16"
                   }
                   ],
                   "points": [
                   {
                   "time": "2016-02-12",
                   "value": 22800
                   },
                   {
                   "time": "2016-04-15",
                   "value": 31000
                   },
                   {
                   "time": "2016-06-13",
                   "value": 12500
                   },
                   {
                   "time": "2016-06-16",
                   "value": 13000
                   },
                   {
                   "time": "2016-06-17",
                   "value": 10800
                   }
                   ]
                   },
                   "reportMetadata": {
                   "timezone": "Etc/GMT+5",
                   "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000"
                   }
}')
  
  ts <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  gapData <- splitDataGaps(data, ts, isDV=TRUE)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 3)
  
  relevantData <- list(stat1 = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 3)
  expect_true("stat1" %in% names(allVars))
  
  })

test_that('splitDataGaps and applyDataGaps work when there are no gaps specified', {
  data <- fromJSON('{
                   "downsampledPrimarySeries": {
                   "gaps": [],
                   "points": [
                   {
                   "time": "2016-06-14T14:45:00.000Z",
                   "value": 2730.0
                   },
                   {
                   "time": "2016-06-14T16:00:00.000Z",
                   "value": 2770.0
                   },
                   {
                   "time": "2016-06-15T13:30:00.000Z",
                   "value": 2590.0
                   }
                   ]
                   },
                   "reportMetadata": {
                   "timezone": "Etc/GMT+5"
                   }
}')
  
  ts <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeries"), "1606")
  gapData <- splitDataGaps(data, ts)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 1)
  
  relevantData <- list(corr_UV = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 1)
  expect_true("corr_UV" %in% names(allVars))
  
  })


setwd(dir = wd)