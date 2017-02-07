context("uvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("unit testing uvhydrograph-data")
test_that("uvhydrograph function breakdown",{
  library(testthat)
  library(jsonlite)
  library(lubridate)
  
  testData <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-example.json', package = 'repgen'))
  
  reportMetadata <- testData$reportMetadata
  months <- repgen:::getMonths(testData, reportMetadata$timezone)
  primarySeriesList <- repgen:::parsePrimarySeriesList(testData, months[[1]], reportMetadata$timezone)
  primaryLims <- repgen:::calculatePrimaryLims(primarySeriesList, repgen:::isPrimaryDischarge(testData))
  upchainSeriesData <- repgen:::readTimeSeriesUvInfo(testData,"upchainSeries")
  primarySeriesData <- repgen:::readTimeSeriesUvInfo(testData,"primarySeries")
  secondaryTimeSeriesInfo <- repgen:::readSecondaryTimeSeriesUvInfo(testData)
  
  expect_is(primaryLims,"list")
  expect_equal(primaryLims$ylim,c(1780, 8920))
  
  expect_is(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone), "list")
  expect_equal(length(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone)$days), 30)
  
  expect_error(repgen:::readSecondaryTimeSeriesUvInfo(NULL))
  expect_equal(upchainSeriesData, secondaryTimeSeriesInfo)
  expect_is(secondaryTimeSeriesInfo,"list")
  expect_equal(secondaryTimeSeriesInfo$label, "Gage height  ( ft )")
  expect_equal(primarySeriesData$label, "Discharge  ( ft^3/s )")
  
  expect_null(repgen:::parseCorrectionsAsTable(NULL))
  
  correctionsTest <- repgen:::readCorrections(testData,"upchainSeriesCorrections")
  toTest <- repgen:::parseCorrectionsAsTable(correctionsTest)
  expect_is(toTest,'data.frame')
  
})

context("testing uvhydrograph")
test_that("uvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data, 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-groundwater.json', package = 'repgen'))
  expect_is(uvhydrograph(data2, 'Author Name'), 'character')
  
  data4 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-hawaii.json', package = 'repgen'))
  expect_is(uvhydrograph(data4, 'Author Name'), 'character')

  data5 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-wq-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data5, 'Author Name'), 'character')
  
  data6 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-missingmonth.json', package = 'repgen'))
  expect_is(uvhydrograph(data6, 'Author Name'), 'character')
  
  data7 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-allapprovals.json', package = 'repgen'))
  expect_is(uvhydrograph(data7, 'Author Name'), 'character')
  
  data8 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-3-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data8, 'Author Name'), 'character')
  
  data9 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-3-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data9, 'Author Name'), 'character')
  
  data10 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-comp-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data10, 'Author Name'), 'character')
  
  data11 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-ref-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data11, 'Author Name'), 'character')
  
  data12 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-ref-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data12, 'Author Name'), 'character')
  
  data13 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-ref-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data13, 'Author Name'), 'character')
  
  data14 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-ref-comp-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data14, 'Author Name'), 'character')
  
  data15 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-comp-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data15, 'Author Name'), 'character')

  data16 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-no-primary-data.json', package = 'repgen'))
  expect_is(uvhydrograph(data16, 'Author Name'), 'character')
  
  data17 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-gh-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data17, 'Author Name'), 'character')
  
  data18 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-watertemp-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data18, 'Author Name'), 'character')
})

test_that("excludeZeroNegative flag works", {
  
  data_neg <- fromJSON(' { 
     "primarySeries": {
       "isVolumetricFlow": true,
       "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": -3960
         },
         {
         "time": "2014-11-22",
         "value": 3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": true,
       "timezone": "Etc/GMT+5"
     }}')
  
  data_zero <- fromJSON(' { 
     "primarySeries": {
       "isVolumetricFlow": true,
       "points": [
         {
         "time": "2014-11-20",
         "value": 0
         },
         {
         "time": "2014-11-21",
         "value": 3960
         },
         {
         "time": "2014-11-22",
         "value": 3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": true,
       "timezone": "Etc/GMT+5"
     }}')
  
  data_notVol <- fromJSON(' { 
     "primarySeries": {
       "isVolumetricFlow": false,
       "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": -3960
         },
         {
         "time": "2014-11-22",
         "value": 3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": true,
       "timezone": "Etc/GMT+5"
     }}')
  
  data_notExclu <- fromJSON(' { 
     "primarySeries": {
       "isVolumetricFlow": true,
       "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": -3960
         },
         {
         "time": "2014-11-22",
         "value": 3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": false,
       "timezone": "Etc/GMT+5"
     }}')
  
  neg <- getTimeSeries(data_neg, "primarySeries")
  expect_equal(nrow(neg), 2)
  
  zero <- getTimeSeries(data_zero, "primarySeries")
  expect_equal(nrow(zero), 2)
  
  notVol <- getTimeSeries(data_notVol, "primarySeries")
  expect_equal(nrow(notVol), 3)
  
  notExclu <- getTimeSeries(data_notExclu, "primarySeries")
  expect_true(any(notExclu$value < 0))
  
})

test_that("getMonths flag works", {
  library(jsonlite)
  reportObject <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "type" : "type", 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-11-20T00:00:00-05:00",
         "value": 4510
         },
         {
         "time": "2014-11-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": 3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "type" : "type", 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-12-20T00:00:00-05:00",
         "value": 4510
         },
         {
         "time": "2014-12-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": 3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": false,
       "timezone": "Etc/GMT+5"
     }}')
  
  months <- repgen:::getMonths(reportObject, "Etc/GMT+5")
  
  expect_equal(length(months), 3)
  expect_equal(months[[1]], "1411")
  expect_equal(months[[2]], "1412")
  expect_equal(months[[3]], "1501")
})

test_that("parseCorrectionsByMonth works", {
  library(jsonlite)
  reportObject <- fromJSON('{   "exampleCorrections": [
    {
      "appliedTimeUtc": "2012-02-29T19:18:25Z",
      "comment": "Example correction 1",
      "startTime": "2012-06-29T10:17:00-05:00",
      "endTime": "2012-07-30T22:59:00-05:00",
      "type": "USGS_MULTI_POINT",
      "parameters": "{}",
      "user": "admin",
      "processingOrder": "PRE_PROCESSING"
    },
    {
      "appliedTimeUtc": "2012-03-29T19:18:25Z",
      "comment": "Example correction 2",
      "startTime": "2012-07-29T10:17:00-05:00",
      "endTime": "2012-08-30T22:59:00-05:00",
      "type": "USGS_MULTI_POINT",
      "parameters": "{}",
      "user": "admin",
      "processingOrder": "PRE_PROCESSING"
    }
    ],
    "emptyCorrections" : []
	}')
  
  correctionsEmpty <- repgen:::parseCorrectionsByMonth(reportObject, "emptyCorrections", "1207")
  expect_equal(nrow(correctionsEmpty), 0)
  
  correctionsDoesNotExist <- repgen:::parseCorrectionsByMonth(reportObject, "nameNoteInJson", "1207")
  expect_equal(nrow(correctionsDoesNotExist), 0)
  
  correctionsMonthNotInData <- repgen:::parseCorrectionsByMonth(reportObject, "exampleCorrections", "1307")
  expect_equal(nrow(correctionsMonthNotInData), 0)
  
  correctionsJune <- repgen:::parseCorrectionsByMonth(reportObject, "exampleCorrections", "1206")
  expect_equal(nrow(correctionsJune), 1)
  expect_equal(correctionsJune[1,]$comment, "Start : Example correction 1")
  
  correctionsJuly <- repgen:::parseCorrectionsByMonth(reportObject, "exampleCorrections", "1207")
  expect_equal(nrow(correctionsJuly), 2)
  expect_equal(correctionsJuly[1,]$comment, "Start : Example correction 2")
  expect_equal(correctionsJuly[2,]$comment, "End : Example correction 1")
  
  correctionsAugust <- repgen:::parseCorrectionsByMonth(reportObject, "exampleCorrections", "1208")
  expect_equal(nrow(correctionsAugust), 1)
  expect_equal(correctionsAugust[1,]$comment, "End : Example correction 2")
})

test_that("parseUvComparisonSeriesByMonth works", {
  library(jsonlite)
  reportNoComparison <- fromJSON('{ 
      "primarySeries": {
        "isVolumetricFlow": true,
        "approvals" : [],
        "qualifiers" : [],
        "units" : "unit", 
        "grades" : [], 
        "type" : "type", 
        "gaps" : [], 
        "gapTolerances" : [], 
        "name" : "test series",
        "points": [
        {
          "time": "2014-11-20T00:00:00-05:00",
          "value": 4510
        },
        {
          "time": "2014-11-21T00:00:00-05:00",
          "value": -3960
        },
        {
          "time": "2015-01-22T00:00:00-05:00",
          "value": 3840
        }] 
      }
    }')
      
  expect_equal(repgen:::parseUvComparisonSeriesByMonth(reportNoComparison, "1201", "Etc/GMT+5"), NULL)
  
  reportComparison <- fromJSON('{ 
    "comparisonSeries": {
      "isVolumetricFlow": true,
      "approvals" : [],
      "qualifiers" : [],
      "units" : "unit", 
      "grades" : [], 
      "type" : "type", 
      "gaps" : [], 
      "gapTolerances" : [], 
      "name" : "test series",
      "points": [
      {
        "time": "2014-11-20T00:00:00-05:00",
        "value": 4510
      },
      {
        "time": "2014-11-21T00:00:00-05:00",
        "value": -3960
      },
      {
        "time": "2015-01-22T00:00:00-05:00",
        "value": 3840
        }] 
      }
    }')

  comparisonSeriesNoMonth <- repgen:::parseUvComparisonSeriesByMonth(reportComparison, "1201", "Etc/GMT+5")
  expect_equal(nrow(comparisonSeriesNoMonth[['points']]), 0)
  
  comparisonSeriesNov <- repgen:::parseUvComparisonSeriesByMonth(reportComparison, "1411", "Etc/GMT+5")
  expect_equal(nrow(comparisonSeriesNov[['points']]), 2)

  comparisonSeriesJan <- repgen:::parseUvComparisonSeriesByMonth(reportComparison, "1501", "Etc/GMT+5")
  expect_equal(nrow(comparisonSeriesJan[['points']]), 1)
})

test_that("parseUvNonEstimatedSeries and parseUvEstimatedSeries works", {
  library(jsonlite)
  reportObject <- fromJSON('{ 
      "exampleSeries": {
        "isVolumetricFlow": true,
        "approvals" : [],
        "qualifiers" : [],
        "units" : "unit", 
        "grades" : [], 
        "type" : "type", 
        "gaps" : [], 
        "gapTolerances" : [], 
        "name" : "test series",
        "points": [
        {
          "time": "2014-11-20T00:00:00-05:00",
          "value": 4510
        },
        {
          "time": "2014-11-21T00:00:00-05:00",
          "value": -3960
        },
        {
          "time": "2014-11-25T00:00:00-05:00",
          "value": 3840
        }],
        "estimatedPeriods" : [{
          "startDate": "2014-11-20T15:54:41-06:00",
          "endDate": "2014-11-22T15:54:41.0000001-06:00"
        }]
      }
    }')

  noMonth <- repgen:::parseUvNonEstimatedSeries(reportObject, "exampleSeries", "1201", "Etc/GMT+5")
  expect_equal(nrow(noMonth[['points']]), 0)
  
  noSeries <- repgen:::parseUvNonEstimatedSeries(reportObject, "doesNotExist", "1411", "Etc/GMT+5")
  expect_equal(noSeries, NULL)
  
  nonEstimated <- repgen:::parseUvNonEstimatedSeries(reportObject, "exampleSeries", "1411", "Etc/GMT+5")
  expect_equal(nrow(nonEstimated[['points']]), 2)
  expect_equal(nonEstimated[['points']][1,][['value']], 4510)
  expect_equal(nonEstimated[['points']][2,][['value']], 3840)
  
  noMonth2 <- repgen:::parseUvEstimatedSeries(reportObject, "exampleSeries", "1201", "Etc/GMT+5")
  expect_equal(nrow(noMonth2[['points']]), 0)
  
  noSeries2 <- repgen:::parseUvEstimatedSeries(reportObject, "doesNotExist", "1411", "Etc/GMT+5")
  expect_equal(noSeries2, NULL)
  
  estimated <- repgen:::parseUvEstimatedSeries(reportObject, "exampleSeries", "1411", "Etc/GMT+5")
  expect_equal(nrow(estimated[['points']]), 1)
  expect_equal(estimated[['points']][1,][['value']], -3960)
})

setwd(dir = wd)
