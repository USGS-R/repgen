context("uvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("unit testing uvhydrograph-data")
test_that("uvhydrograph-data functions",{
  library(testthat)
  library(jsonlite)
  library(lubridate)
  
  testData <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-example.json', package = 'repgen'))
  
  reportMetadata <- testData$reportMetadata
  months <- repgen:::getMonths(testData, reportMetadata$timezone)
  
  primarySeriesList <- repgen:::parsePrimarySeriesList(testData, months[[1]], reportMetadata$timezone)
  secondarySeriesList <- repgen:::parseSecondarySeriesList(testData, months[[1]], reportMetadata$timezone)
  
  sortedData <- repgen:::sortDataAndSides(primarySeriesList, 
                                          repgen:::readTimeSeriesUvInfo(testData, "primarySeries"),
                                          repgen:::readTimeSeriesUvInfo(testData, "referenceSeries"),
                                          repgen:::readTimeSeriesUvInfo(testData, "comparisonSeries"))
  
  primaryLims <- repgen:::calculateLims(rbind(primarySeriesList[['corrected']][['points']], 
                                              primarySeriesList[['estimated']][['points']]))
  primaryLims[['ylim']] <- repgen:::bufferLims(primaryLims[['ylim']], primarySeriesList[['uncorrected']][['points']][['value']])
  upchainSeriesData <- repgen:::readTimeSeriesUvInfo(testData,"upchainSeries")
  primarySeriesData <- repgen:::readTimeSeriesUvInfo(testData,"primarySeries")
  secondaryTimeSeriesInfo <- repgen:::readSecondaryTimeSeriesUvInfo(testData)
  
  ###Secondary series list
  expect_is(secondarySeriesList,"list")
  expect_equal(length(secondarySeriesList),5)
  expect_equal(length(secondarySeriesList$corrected),20)
  expect_equal(length(secondarySeriesList$uncorrected),20)
  expect_equal(length(secondarySeriesList$estimated),20)
  expect_false(secondarySeriesList$inverted)
  
  ###Dv series list
  dvData <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-allapprovals.json', package = 'repgen')) #better test file for DVs
  dvSeriesList <- repgen:::parsePrimaryDvList(dvData, repgen:::getMonths(dvData, dvData$reportMetadata$timezone)[[1]], dvData$reportMetadata$timezone)
  expect_is(dvSeriesList,"list")
  expect_equal(length(dvSeriesList),3) # should have an entry for every approval level
  expect_is(dvSeriesList[[1]],"data.frame")
  expect_is(dvSeriesList[[2]],"data.frame")
  expect_is(dvSeriesList[[3]],"data.frame")
  expect_equal(names(dvSeriesList[1]),"approved_dv")
  expect_equal(names(dvSeriesList[2]),"inreview_dv")
  expect_equal(names(dvSeriesList[3]),"working_dv")
  expect_equal(nrow(dvSeriesList[[1]]),20) #4 stats in test file, each with 5 approved dvs
  expect_equal(nrow(dvSeriesList[[2]]),12) #4 stats in test file, each with 3 in-review dvs
  expect_equal(nrow(dvSeriesList[[3]]),28) #4 stats in test file, each with 7 working dvs
  
  ###Primary uvHydro approval bars
  expect_is(repgen:::readPrimaryUvHydroApprovalBars(testData,reportMetadata$timezone,months[1]),"list")
  expect_equal(length(repgen:::readPrimaryUvHydroApprovalBars(testData,reportMetadata$timezone,months[1])),1)
  expect_equal(repgen:::readPrimaryUvHydroApprovalBars(testData,reportMetadata$timezone,months[1])$appr_working_uv$legend.name,"Working UV Discharge  ( ft^3/s )")
  expect_error(repgen:::readPrimaryUvHydroApprovalBars(NULL,reportMetadata$timezone,months[1]))
  
  ###Secondary uvHydro approval bars
  expect_is(repgen:::readSecondaryUvHydroApprovalBars(testData,reportMetadata$timezone),"list")
  expect_equal(length(repgen:::readSecondaryUvHydroApprovalBars(testData,reportMetadata$timezone)),1)
  expect_equal(repgen:::readSecondaryUvHydroApprovalBars(testData,reportMetadata$timezone)$appr_working_uv$legend.name,"Working Gage height  ( ft )")
  
  ###Read UV Readings
  expect_is(repgen:::readAllUvReadings(testData,months[1],"readings"),"list")
  expect_is(repgen:::readAllUvReadings(NULL,months[1],"readings"),"list")
  expect_equal(length(repgen:::readAllUvReadings(testData,months[1],"readings")),3)
  expect_equal(length(repgen:::readAllUvReadings(NULL,months[1],"readings")),3)
  
  #Read UV Q Measurements
  expect_is(repgen:::isEmptyOrBlank(repgen:::readUvQMeasurements(NULL,months[1])[1]),"logical")
  expect_equal(length(repgen:::readUvQMeasurements(testData,months[1])[1]),1)
  expect_is(repgen:::readUvQMeasurements(testData,months[1]),"data.frame")
  expect_equal(length(repgen:::readUvQMeasurements(testData,months[1])),6)
  expect_equal(repgen:::readUvQMeasurements(testData,months[1])$value,2410)
  
  ###Read effective shifts
  expect_is(repgen:::readEffectiveShifts(testData,reportMetadata$timezone,months[1]),"data.frame")
  expect_equal(nrow(repgen:::readEffectiveShifts(testData,reportMetadata$timezone,months[1])),2880)
  expect_equal(length(repgen:::readEffectiveShifts(testData,reportMetadata$timezone,months[1])),3)
  
  ###Read UV GW Level
  expect_equal(repgen:::isEmptyOrBlank(repgen:::readUvGwLevel(NULL,months[1])),NA)
  expect_equal(repgen:::isEmptyOrBlank(repgen:::readUvGwLevel(testData,months[1])),NA)
  expect_equal(length(repgen:::readUvGwLevel(NULL,months[1])),3)
  
  ###Read UV Measurement shifts
  expect_equal(repgen:::isEmptyOrBlank(repgen:::readUvMeasurementShifts(NULL,months[1])),NA)
  expect_equal(length(repgen:::readUvMeasurementShifts(testData,months[1])),5)
  expect_equal(repgen:::readUvMeasurementShifts(testData,months[1])$value, 0.05744612)
  expect_is(repgen:::readUvMeasurementShifts(testData,months[1]),"data.frame")
  
  ###Read Uv Gage Heights
  expect_equal(repgen:::readUvGageHeight(testData,months[1])$value, 7.71)
  expect_equal(length(repgen:::readUvGageHeight(testData,months[1])), 4)
  expect_equal(length(repgen:::readUvGageHeight(NULL,months[1])), 3)
  expect_equal(repgen:::isEmptyOrBlank(repgen:::readUvGageHeight(NULL,months[1])), NA)
  
  expect_true(repgen:::isPrimaryDischarge(testData))
  expect_false(repgen:::isPrimaryDischarge(NULL))
  
  expect_false(repgen:::hasReferenceSeries(NULL))
  expect_false(repgen:::hasReferenceSeries(testData))
  
  expect_true(repgen:::hasUpchainSeries(testData))
  expect_false(repgen:::hasUpchainSeries(NULL))
  
  ###Sort sides out
  expect_equal(nrow(sortedData[['data']][['primary']]), 2880)
  expect_null(sortedData[['data']][['reference']])
  expect_null(sortedData[['data']][['comparison']])
  expect_equal(sortedData[['sides']][['primary']], 2)
  expect_equal(sortedData[['sides']][['reference']], 0)
  expect_equal(sortedData[['sides']][['comparison']], 0)
  
  ###Test limits
  expect_is(primaryLims,"list")
  expect_equal(primaryLims$ylim ,c(1780, 8920))
  expect_equal(length(primaryLims),2)
  
  ###Parse Time Info from Lims
  expect_is(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone), "list")
  expect_equal(length(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone)$days), 30)
  
  ###Large Data grab checks
  expect_error(repgen:::readSecondaryTimeSeriesUvInfo(NULL))
  expect_equal(upchainSeriesData, secondaryTimeSeriesInfo)
  expect_is(secondaryTimeSeriesInfo,"list")
  expect_equal(secondaryTimeSeriesInfo$label, "Gage height  ( ft )")
  expect_equal(primarySeriesData$label, "Discharge  ( ft^3/s )")
  
  ###Parse Corrections as Table: NULL check
  expect_null(repgen:::parseCorrectionsAsTable(NULL))
  
  ###Checking corrections parsers
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

test_that("getVerticalFlagArrows correctly creates arrow plotting information from correctionLabel objects",{
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
      
      arrows <- repgen:::getVerticalFlagArrows(testCorrectionLabels)
      
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

test_that("getVerticalFlagPositions returns only the non-redundant x positions all corrections",{
      testCorrections <- data.frame(
          time=c(as.POSIXct("2016-05-23 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(NA, NA, NA), 
          month=c("1605", "1605", "1605"), 
          comment=c("correction 1", "correction 2", "correction 3"), 
          stringsAsFactors=FALSE)
      
      abLines <- repgen:::getVerticalFlagPositions(testCorrections)
      expect_equal(length(abLines), 2) #dupe position removed
      expect_equal(abLines[[1]], as.POSIXct("2016-05-23 17:00:00"))
      expect_equal(abLines[[2]], as.POSIXct("2016-05-23 17:45:00")) 
      
      #null supported
      expect_equal(length(repgen:::getVerticalFlagPositions(NULL)), 0)
      
      #empty frame supported
      expect_equal(length(repgen:::getVerticalFlagPositions(
                  na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)))
          ), 0)
    })

test_that("addGroupCol properly adds a new column by group value using the proper function for the column value", {
  groupList <- data.frame(x=c(2,2,6), y=c(9,6,3))
  groupList <- groupList %>%  mutate(label = row_number()) %>% arrange(x, label)

  groupList <- repgen:::addGroupCol(groupList, 'testCol1',  isNewCol = function(data, r, vars){data[r-1, 'x'] != data[r, 'x']}, newGroupValue=function(data, prev, r, build_vec, vars){c(value=data[r, 'y']/3, vars=list())}, groupChildValue=function(data,build_vec,r,vars){build_vec[r-1]})
  groupList <- repgen:::addGroupCol(groupList, 'testCol2',  isNewCol = function(data, r, vars){data[r-1, 'x'] != data[r, 'x']}, newGroupValue=function(data, prev, r, build_vec, vars){c(value=data[r, 'y']/3, vars=list())}, groupChildValue=function(data,build_vec,r,vars){data[r, 'y']/3})
  expect_is(groupList, 'data.frame')
  expect_equal(groupList[[4]], c(3,3,1))
  expect_equal(groupList[[5]], c(3,2,1))
})

test_that("xposGroupValue properly denotes new groups based on x position", {
  data1 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-23 17:45:00")
    ),
    label = c(1,2),
    boxWidth = c(28800, 28800),
    colNum = c(1,1)
  )
  r <- 1
  vars1 <- list(
    secondOffset = c(14400),
    limits = list(
      xlim = c(
        as.POSIXct("2016-05-01 00:00:00"),
        as.POSIXct("2016-05-23 17:45:00")
      ),
      ylim = c(6.24, 7.63)
    )
  )

  data2 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-23 17:45:00")
    ),
    label = c(1,2),
    boxWidth = c(28800, 28800),
    colNum = c(1,1)
  )
  r <- 1
  vars2 <- list(
    secondOffset = c(14400),
    limits = list(
      xlim = c(
        as.POSIXct("2016-05-01 00:00:00"),
        as.POSIXct("2016-05-27 17:45:00")
      ),
      ylim = c(6.24, 7.63)
    )
  )

  xPosVal1 <- repgen:::xposGroupValue(data1, NULL, r, NULL, vars1)
  xPosVal2 <- repgen:::xposGroupValue(data2, NULL, r, NULL, vars2)

  expect_is(xPosVal1, 'list')
  expect_is(xPosVal2, 'list')
  expect_true(xPosVal1[[1]] < xPosVal2[[1]])
})

test_that("yposGroupValue", {
  data1 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-23 17:45:00")
    ),
    label = c(1,2),
    boxWidth = c(28800, 28800),
    colNum = c(1,1),
    xpos = c(1464022800, 1464022800)
  )
  r1 <- 1
  vars1 <- list(
    secondOffset = c(14400),
    subtractor = 1,
    limits = list(
      xlim = c(
        as.POSIXct("2016-05-01 00:00:00"),
        as.POSIXct("2016-05-23 17:45:00")
      ),
      ylim = c(6.24, 7.63)
    )
  )

  data2 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-23 17:45:00")
    ),
    label = c(1,2),
    boxWidth = c(28800, 28800),
    colNum = c(1,1),
    xpos = c(1464022800, 1464022800)
  )
  r2 <- 2
  vars2 <- list(
    secondOffset = c(14400),
    subtractor = 1,
    limits = list(
      xlim = c(
        as.POSIXct("2016-05-01 00:00:00"),
        as.POSIXct("2016-05-27 17:45:00")
      ),
      ylim = c(6.24, 7.63)
    )
  )
  build_vec2 <- c(7.63)

  yPosVal1 <- repgen:::yposGroupValue(data1, NULL, r1, NULL, vars1)
  yPosVal2 <- repgen:::yposGroupValue(data2, NULL, r2, build_vec2, vars2)

  expect_is(yPosVal1, 'list')
  expect_is(yPosVal2, 'list')
  expect_equal(yPosVal1[[1]], 7.63)
  expect_equal(yPosVal2[[1]], 6.63)
})

test_that("parseVerticalFlagLabelSpacing", {
  corrections1 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-23 17:45:00")
    ),
    value = c(NA, NA),
    month = c(1605, 1605),
    comment = c(
      "Test1",
      "Test2"
    )
  )

  corrections2 <- data.frame(
    time = c(
      as.POSIXct("2016-05-23 17:00:00"),
      as.POSIXct("2016-05-29 17:45:00")
    ),
    value = c(NA, NA),
    month = c(1605, 1605),
    comment = c(
      "Test1",
      "Test2"
    )
  )

  limits <- list(
    xlim = c(
      as.POSIXct("2016-05-01 00:00:00"),
      as.POSIXct("2016-06-20 17:45:00")
    ),
    ylim = c(6.24, 7.63)
  )

  corrLabels1 <- repgen:::parseVerticalFlagLabelSpacing(corrections1, limits)
  corrLabels2 <- repgen:::parseVerticalFlagLabelSpacing(corrections2, limits)

  expect_is(corrLabels1, 'list')
  expect_is(corrLabels2, 'list')
  expect_equal(corrLabels1$y, c(7.63, 7.53965))
  expect_equal(corrLabels2$y, c(7.63, 7.63))
})

setwd(dir = wd)
