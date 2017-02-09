context("uvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

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

test_that("excludeZeroNegative flag correctly tells the report to exclude 0 and negative points", {
  
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

test_that("getMonths correctly identifies all months with relevant data for a UV report", {
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

test_that("parseCorrectionsByMonth correctly retrieves a field of corrections, filtered by month", {
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

test_that("parseUvComparisonSeriesByMonth correctly pulls the comparison series with points filtered by month", {
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

test_that("parseUvNonEstimatedSeries and parseUvEstimatedSeries correclty returns timeseries objecs with either estimated or non-estimated points filtered by month", {
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

test_that("getCorrectionArrows correctly creates arrow plotting information from correctionLabel objects",{
      #this test data mimics how correction labels should be. xorigin is a datatime, x is the millis version of a datetime to the right or left of xorigin
      xorigin1 <- as.POSIXct("2016-05-01 18:00:00")
      xorigin2 <- as.POSIXct("2016-05-01 18:45:00")
      xorigin3 <- as.POSIXct("2016-05-23 17:00:00")
      xorigin4 <- as.POSIXct("2016-05-23 17:45:00")
      
      x1 <- as.integer(xorigin1) + 10000 #to the right
      x2 <- as.integer(xorigin2) + 10000 #to the right
      x3 <- as.integer(xorigin3) - 10000 #to the left
      x4 <- as.integer(xorigin4) - 10000 #to the left
      
      testCorrectionLabels <- data.frame(
          x=c(x1, x2, x3, x4),
          xorigin=c(xorigin1, xorigin2, xorigin3, xorigin4), 
          y=c(10, 10, 10, 10), 
          r=c(1, 1, 1, 1), 
          label=c(1, 2, 3, 4), 
          stringsAsFactors=FALSE)
      
      arrows <- repgen:::getCorrectionArrows(testCorrectionLabels)
      
      #all y positions stay the same
      expect_equal(arrows[['y']][1], 10)
      expect_equal(arrows[['y']][2], 10)
      expect_equal(arrows[['y']][3], 10)
      expect_equal(arrows[['y']][4], 10)
      
      #all xorigin positions stay the same
      expect_equal(arrows[['xorigin']][1], xorigin1)
      expect_equal(arrows[['xorigin']][2], xorigin2)
      expect_equal(arrows[['xorigin']][3], xorigin3)
      expect_equal(arrows[['xorigin']][4], xorigin4)
      
      #all x's got shifted so that x the arrow (x-xorigin) is shorter
      expect_equal(arrows[['x']][1] < x1, TRUE)
      expect_equal(arrows[['x']][2] < x2, TRUE)
      expect_equal(arrows[['x']][3] > x3, TRUE)
      expect_equal(arrows[['x']][4] > x4, TRUE)
    })

test_that("getCorrectionPositions returns only the non-redundant x positions all corrections",{
      testCorrections <- data.frame(
          time=c(as.POSIXct("2016-05-23 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(NA, NA, NA), 
          month=c("1605", "1605", "1605"), 
          comment=c("correction 1", "correction 2", "correction 3"), 
          stringsAsFactors=FALSE)
      
      abLines <- repgen:::getCorrectionPositions(testCorrections)
      expect_equal(length(abLines), 2) #dupe position removed
      expect_equal(abLines[[1]], as.POSIXct("2016-05-23 17:00:00"))
      expect_equal(abLines[[2]], as.POSIXct("2016-05-23 17:45:00")) 
      
      #null supported
      expect_equal(length(repgen:::getCorrectionPositions(NULL)), 0)
      
      #empty frame supported
      expect_equal(length(repgen:::getCorrectionPositions(
                  na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)))
          ), 0)
    })

setwd(dir = wd)
