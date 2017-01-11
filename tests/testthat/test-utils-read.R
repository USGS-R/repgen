context("utils-read tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('getGroundWaterLevels data returns as expected', {
  expect_equal(nrow(getGroundWaterLevels(fromJSON('{}'))), 0)
  expect_equal(nrow(getGroundWaterLevels(fromJSON('{ "gwlevel": [] }'))), 0)
  
  gwLevels <- getGroundWaterLevels(fromJSON('{ "gwlevel": [
                                            { 
                                            "siteNumber": "353922083345600", 
                                            "groundWaterLevel": 9.14, 
                                            "recordDateTime": "2012-06-22T00:00:00.000-06:00" 
                                            }]}'))
  expect_equal(nrow(gwLevels), 1)
  expect_equal(gwLevels$month, "1206")
  expect_equal(gwLevels$time, as.POSIXct("2012-06-22 00:00:00 UTC"))
  expect_equal(gwLevels$value, 9.14)
  })

test_that('getWaterQualityMeasurements data returns as expected', {
  expect_equal(nrow(getWaterQualityMeasurements(fromJSON('{}'))), 0)
  expect_equal(nrow(getWaterQualityMeasurements(fromJSON('{ "waterQuality": [] }'))), 0)
  
  waterQuality <- getWaterQualityMeasurements(fromJSON('{ "waterQuality": [
                                                       {
                                                       "recordNumber": "01005171",
                                                       "medium": "Periphyton (quantitative)",
                                                       "sampleStartDateTime": "2010-09-14T08:10:00.000-06:00",
                                                       "value": { 
                                                       "parameter": "00400",
                                                       "remark": "",
                                                       "value": 7.8
                                                       }
                                                       }
                                                       ]}'))
  expect_equal(nrow(waterQuality), 1)
  expect_equal(waterQuality$month, "1009")
  expect_equal(waterQuality$time, as.POSIXct("2010-09-14 08:10:00 UTC"))
  expect_equal(waterQuality$value, 7.8)
  })

test_that('getFieldVisitMeasurementsQPoints data returns as expected', {
  expect_equal(nrow(getFieldVisitMeasurementsQPoints(fromJSON('{}'))), 0)
  expect_equal(nrow(getFieldVisitMeasurementsQPoints(fromJSON('{ "fieldVisitMeasurements": [] }'))), 0)
  
  measurements <- getFieldVisitMeasurementsQPoints(fromJSON('{ "fieldVisitMeasurements": [
                                                            {
                                                            "shiftInFeet": -0.2979136203456,
                                                            "errorMinShiftInFeet": -0.97370692963881,
                                                            "errorMaxShiftInFeet": 0.3430937048683,
                                                            "identifier": "2398D959FFB03786E0530100007FBAFF",
                                                            "controlCondition": "SUBMERGED",
                                                            "measurementStartDate": "2013-10-18T09:23:17.000-06:00",
                                                            "ratingModelIdentifier": "Gage height-Discharge.STGQ@06934500",
                                                            "discharge": 40500,
                                                            "dischargeUnits": "ft^3/s",
                                                            "errorMinDischarge": 37260.0000,
                                                            "errorMaxDischarge": 43740.0000,
                                                            "measurementNumber": "3825",
                                                            "qualityRating": "FAIR",
                                                            "historic": false,
                                                            "meanGageHeight": 4.45,
                                                            "meanGageHeightUnits": "ft",
                                                            "shiftNumber": 0
                                                            }]}'))
    expect_equal(nrow(measurements), 1)
    expect_equal(measurements$month, "1310")
    expect_equal(measurements$time, as.POSIXct("2013-10-18 09:23:17 UTC"))
    expect_equal(measurements$value, 40500)
    expect_equal(measurements$n, "3825")
    expect_equal(measurements$minQ, 37260.0000)
    expect_equal(measurements$maxQ, 43740.0000)
    })

test_that('getFieldVisitMeasurementsShifts data returns as expected', {
  expect_equal(nrow(getFieldVisitMeasurementsShifts(fromJSON('{}'))), 0)
  expect_equal(nrow(getFieldVisitMeasurementsShifts(fromJSON('{ "fieldVisitMeasurements": [] }'))), 0)
  
  shifts <- getFieldVisitMeasurementsShifts(fromJSON('{ "fieldVisitMeasurements": [
                                                     {
                                                     "shiftInFeet": -0.2979136203456,
                                                     "errorMinShiftInFeet": -0.97370692963881,
                                                     "errorMaxShiftInFeet": 0.3430937048683,
                                                     "identifier": "2398D959FFB03786E0530100007FBAFF",
                                                     "controlCondition": "SUBMERGED",
                                                     "measurementStartDate": "2013-10-18T09:23:17.000-06:00",
                                                     "ratingModelIdentifier": "Gage height-Discharge.STGQ@06934500",
                                                     "discharge": 40500,
                                                     "dischargeUnits": "ft^3/s",
                                                     "errorMinDischarge": 37260.0000,
                                                     "errorMaxDischarge": 43740.0000,
                                                     "measurementNumber": "3825",
                                                     "qualityRating": "FAIR",
                                                     "historic": false,
                                                     "meanGageHeight": 4.45,
                                                     "meanGageHeightUnits": "ft",
                                                     "shiftNumber": 0
                                                     }]}'))
  expect_equal(nrow(shifts), 1)
  expect_equal(shifts$month, "1310")
  expect_equal(shifts$time, as.POSIXct("2013-10-18 09:23:17 UTC"))
  expect_equal(shifts$value, -0.2979136203456)
  expect_equal(shifts$minShift, -0.97370692963881)
  expect_equal(shifts$maxShift, 0.3430937048683)
  })

test_that('getCorrections data returns as expected', {
  expect_equal(nrow(getCorrections(fromJSON('{}'), "aField")), NULL)
  expect_equal(nrow(getCorrections(fromJSON('{ "aField": [] }'), "aField")), NULL)
  
  rawJsonString <- '{ "aField": [
  {
  "appliedTimeUtc": "2015-10-31T18:03:16.676Z",
  "comment": "continued drawdown curve",
  "endTime": "2015-09-30T22:59:00.000-06:00",
  "parameters": {},
  "processingOrder": "POST_PROCESSING",
  "startTime": "2011-09-30T22:59:00.000-06:00",
  "type": "USGS_MULTI_POINT",
  "user": "admin"
  }
  ]}'
  
  corrections <- getCorrections(fromJSON(rawJsonString), "aField")
  
  expect_equal(nrow(corrections), 2)
  expect_equal(corrections$month, c("1109", "1509"))
  expect_equal(corrections$time, c(
    as.POSIXct("2011-09-30 22:59:00 UTC"),
    as.POSIXct("2015-09-30 22:59:00 UTC")))
  expect_equal(corrections$comment, c("Start : continued drawdown curve", "End : continued drawdown curve"))
  })

test_that('getEstimatedDates data returns as expected', {
  #TODO fabricate test data and use to call getEstimatedDates
  #TODO don't know what third param is
})

test_that('getApprovals data returns as expected', {
  #TODO fabricate test data and use to call getApprovals
})

test_that('getApprovalDates return correct data', {
  #TODO fabricate test data and use to call getApprovalDates
})

test_that("sizeOf function works", {
  expect_error(repgen:::sizeOf(NULL), "data frame is null, cannot determine size") 
  
  emptyFrame <- data.frame( 
      randoField=character(), 
      stringsAsFactors=FALSE) 
  expect_equal(repgen:::sizeOf(emptyFrame), 0) 
  
  #using fromJSON out of laziness
  library("jsonlite")
  listOf2 <- fromJSON('[{ "value" : 1 }, { "value" : 2 } ]') 
  expect_equal(repgen:::sizeOf(listOf2), 2) 
})

test_that('readTimeSeries returns valid data for a valid time series', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  series <- repgen:::readTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"))
  
  expect_is(series$startTime, 'POSIXct')
  expect_is(series$endTime, 'POSIXct')
  expect_is(series$points, 'data.frame')
  expect_is(series$approvals, 'data.frame')

  expect_equal(nrow(series$points), 4)
  expect_equal(series$estimated, FALSE)
  expect_equal(series$isDV, FALSE)
  expect_equal(series$startTime, repgen:::flexibleTimeParse('2014-11-19', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$endTime, repgen:::flexibleTimeParse('2015-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$points$value[[1]], 4510)
  expect_equal(series$points$time[[1]], repgen:::flexibleTimeParse('2014-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
})

test_that('readTimeSeries returns valid data for a DV series', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  series <- repgen:::readTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"), isDV=TRUE)

  expect_equal(series$isDV, TRUE)
})

test_that('readTimeSeries throws errors for invalid time series data', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  expect_error(repgen:::readTimeSeries(reportObject, "testSeries2", repgen:::fetchReportMetadataField(reportObject, "timezone")), "*is missing required fields*")
  expect_error(repgen:::readTimeSeries(reportObject, "missingSeries", repgen:::fetchReportMetadataField(reportObject, "timezone")), "*not found in JSON data.")
})

test_that('readEstimatedTimeSeries returns only estimated data for given time series',{
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))

  series <- repgen:::readEstimatedTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"))

  expect_equal(nrow(series$points), 2)
  expect_equal(series$estimated, TRUE)
  expect_equal(series$isDV, FALSE)
  expect_equal(series$points$value[[1]], 4510)
  expect_equal(series$points$time[[1]], repgen:::flexibleTimeParse('2014-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$points$value[[length(series$points$value)]], 3960)
  expect_equal(series$points$time[[length(series$points$time)]], repgen:::flexibleTimeParse('2014-11-21', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
})

test_that('readGroundWaterLevels returns valid and properly formatted data when given valid JSON', {
  library(jsonlite)

  reportObject1 <- fromJSON('{
      "gwlevel": [
        {
          "siteNumber": "12345",
          "groundWaterLevel": 2,
          "recordDateTime": "2015-07-16T01:00:00-06:00",
          "timeZone": "EDT"
        },
        {
          "siteNumber": "12345",
          "groundWaterLevel": 3,
          "recordDateTime": "2015-07-16T02:00:00-06:00",
          "timeZone": "EDT"
        }
      ]
  }')

  reportObject2 <- fromJSON('{
      "gwlevel": []
  }')

  gwData <- repgen:::readGroundWaterLevels(reportObject1)
  blankData <- repgen:::readGroundWaterLevels(reportObject2)

  expect_is(gwData, 'data.frame')
  expect_is(gwData$value, 'numeric')
  expect_is(gwData$time, 'POSIXct')
  expect_is(gwData$month, 'factor')

  expect_equal(gwData$value[[1]], 2)
  expect_equal(gwData$time[[2]], as.POSIXct(strptime("2015-07-16T02:00:00-06:00", "%FT%T")))

  expect_is(blankData, 'data.frame')

  expect_equal(nrow(blankData), 0)
})

test_that('readGroundWaterLevels errors when given invalid JSON', {
  library(jsonlite)

  reportObject1 <- fromJSON('{
      "gwlevel": [
        {
          "siteNumber": "12345",
          "recordDateTime": "2015-07-16T01:00:00-06:00"
        },
        {
          "siteNumber": "12345",
          "groundWaterLevel": 3,
          "recordDateTime": "2015-07-16T02:00:00-06:00",
          "timeZone": "EDT"
        }
      ]
  }')

  reportObject2 <- fromJSON('{ }')

  expect_error(repgen:::readGroundWaterLevels(reportObject1), "*missing required fields*")
  expect_error(repgen:::readGroundWaterLevels(reportObject2), "*not found in report JSON.")
})

test_that('readWaterQualityMeasurements returns valid and properly formatted data when given valid JSON', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "waterQuality": [
        {
          "recordNumber": "01501684",
          "medium": "Surface water",
          "sampleStartDateTime": "2015-07-15T10:50:00-06:00",
          "value": {
            "parameter": "00300",
            "remark": "",
            "value": 5.3
          },
          "timeZone": "CST"
        },
        {
          "recordNumber": "01501779",
          "medium": "Surface water",
          "sampleStartDateTime": "2015-07-29T13:30:00-06:00",
          "value": {
            "parameter": "00300",
            "remark": "",
            "value": 4.0
          },
          "timeZone": "CST"
        }
      ]
  }')

  wqData <- repgen:::readWaterQualityMeasurements(reportObject)

  expect_is(wqData, 'data.frame')
  expect_is(wqData$value, 'numeric')
  expect_is(wqData$time, 'POSIXct')
  expect_is(wqData$month, 'factor')

  expect_equal(wqData$value[[1]], 5.3)
  expect_equal(wqData$time[[2]], as.POSIXct(strptime("2015-07-29T13:30:00-06:00", "%FT%T")))
})

test_that('readWaterQualityMeasurements errors when given invalid JSON', {
  library(jsonlite)

  reportObject1 <- fromJSON('{
      "waterQuality": [
        {
          "recordNumber": "01501684",
          "medium": "Surface water",
          "value": {
            "parameter": "00300",
            "remark": "",
            "value": 5.3
          },
          "timeZone": "CST"
        },
        {
          "recordNumber": "01501779",
          "medium": "Surface water",
          "sampleStartDateTime": "2015-07-29T13:30:00-06:00",
          "value": {
            "parameter": "00300",
            "remark": ""
          },
          "timeZone": "CST"
        }
      ]
  }')

  reportObject2 <- fromJSON('{ }')

  expect_error(repgen:::readWaterQualityMeasurements(reportObject1), "*missing required fields*")
  expect_error(repgen:::readWaterQualityMeasurements(reportObject2), "*not found in report JSON.")
})

setwd(dir = wd)