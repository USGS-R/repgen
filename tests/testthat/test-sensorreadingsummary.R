context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  reportObject <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject, 'Author Name'), 'character')
  
  reportObject2 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example-exc-comm.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject2, 'Author Name'), 'character')
  
  reportObject3 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example_w_quals.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject3, 'Author Name'), 'character')
})

test_that("get unique qualifiers", {
  library(jsonlite)
  reportObject <- fromJSON('{
    "readings": [
      {
      "displayTime": "2016-02-29T10:57:00-05:00",
      "recorderComments": [
      "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
      ],
      "visitStatus": "TODO",
      "recorderMethod": "Non-subm pressure  transducer",
      "recorderValue": "5.24",
      "recorderType": "Routine",
      "party": "ARC",
      "nearestcorrectedValue": "5.24",
      "qualifiers": [
        {
        "startDate": "2016-02-29T10:54:41.000-05:00",
        "endDate": "2016-02-29T20:54:41.000-05:00",
        "identifier": "ESTIMATED",
        "code": "E",
        "displayName": "Estimated",
        "appliedBy": "admin",
        "dateApplied": "2016-03-01T22:35:14.957-06:00"
        }
      ],
      "nearestcorrectedTime": "2016-02-29T11:00:00-05:00",
      "nearestrawTime": "2016-02-29T11:00:00-05:00",
      "nearestrawValue": "5.24"
      },
      {
      "displayTime": "2016-02-29T12:31:00-05:00",
      "recorderComments": [
      "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
      ],
      "visitStatus": "TODO",
      "recorderMethod": "Non-subm pressure  transducer",
      "recorderValue": "5.21",
      "recorderType": "Routine",
      "party": "ARC",
      "nearestcorrectedValue": "5.19",
      "qualifiers": [
        {
        "startDate": "2016-02-29T16:54:41.000-05:00",
        "endDate": "2016-02-29T20:54:41.000-05:00",
        "identifier": "ESTIMATED",
        "code": "E",
        "displayName": "Estimated",
        "appliedBy": "admin",
        "dateApplied": "2016-03-01T22:35:14.957-06:00"
        }
      ],
      "nearestcorrectedTime": "2016-02-29T12:45:00-05:00",
      "nearestrawTime": "2016-02-29T12:45:00-05:00",
      "nearestrawValue": "5.19"
      }
    ],
      "reportMetadata": {
      "allowedTypes": "Routine,Reset,Cleaning,After,ReferencePrimary,Reference,Unknown,BubbleGage",
      "country": "United States of America",
      "altitude": "     930",
      "requestingUser": "thongsav",
      "endDate": "2016-11-14T23:59:59.999999999Z",
      "timezone": "Etc/GMT+5",
      "latitude": "44.8577777777778",
      "county": "Franklin County",
      "description": "Compares continuous sensors with readings from known good sensors",
      "title": "Sensor Reading Summary",
      "requestId": "SensorReadingSummaryChoreographer-5b3e0fa8-e154-434c-9aa3-88f636a2a394",
      "siteNumber": "01047200       ",
      "stationName": "Sandy River near Madrid, Maine",
      "coordinateDatumCode": "NAD83     ",
      "state": "Maine",
      "requestTemporal": "2016-11-14T13:17:54.745",
      "stationId": "01047200",
      "excludeComments": false,
      "agency": "USGS ",
      "drainageArea": "    25.3",
      "nwisRaAuthToken": "bce59bfa-142e-4c67-8c46-fa17e86d2748",
      "timeSeriesParams": "Gage height",
      "timeSeriesUniqueId": "3ec1f3f59fe944079fc0ea5a4eb03e83",
      "timeseriesLabel": "Gage height.ft@01047200",
      "longitude": "-70.4852777777778",
      "contributingDrainageArea": "    25.3",
      "altitudeDatumCode": "NGVD29    ",
      "startDate": "2015-11-01T00:00:00Z"
      }
  }')
   
  quals <- as.data.frame(reportObject$readings$qualifiers)
  quals <- quals[, c("code", "identifier", "displayName")]
  uniqueSRSQualifiers <- repgen:::formatQualifiersTable(quals)
  expect_true(nrow(uniqueSRSQualifiers)==1)

  })

test_that('do all expected columns exist in formatted results', {
  library(jsonlite)
  reportObject <- fromJSON('{
    "readings": [
                           {
                           "displayTime": "2016-02-29T10:57:00-05:00",
                           "recorderComments": [
                           "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                           ],
                           "visitStatus": "TODO",
                           "recorderMethod": "Non-subm pressure  transducer",
                           "recorderValue": "5.24",
                           "recorderType": "Routine",
                           "party": "ARC",
                           "nearestcorrectedValue": "5.24",
                           "qualifiers": [
                           {
                           "startDate": "2016-02-29T10:54:41.000-05:00",
                           "endDate": "2016-02-29T20:54:41.000-05:00",
                           "identifier": "ESTIMATED",
                           "code": "E",
                           "displayName": "Estimated",
                           "appliedBy": "admin",
                           "dateApplied": "2016-03-01T22:35:14.957-06:00"
                           }
                           ],
                           "nearestcorrectedTime": "2016-02-29T11:00:00-05:00",
                           "nearestrawTime": "2016-02-29T11:00:00-05:00",
                           "nearestrawValue": "5.24"
                           },
                           {
                           "displayTime": "2016-02-29T12:31:00-05:00",
                           "recorderComments": [
                           "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                           ],
                           "visitStatus": "TODO",
                           "recorderMethod": "Non-subm pressure  transducer",
                           "recorderValue": "5.21",
                           "recorderType": "Routine",
                           "party": "ARC",
                           "nearestcorrectedValue": "5.19",
                           "qualifiers": [
                           {
                           "startDate": "2016-02-29T16:54:41.000-05:00",
                           "endDate": "2016-02-29T20:54:41.000-05:00",
                           "identifier": "ESTIMATED",
                           "code": "E",
                           "displayName": "Estimated",
                           "appliedBy": "admin",
                           "dateApplied": "2016-03-01T22:35:14.957-06:00"
                           }
                           ],
                           "nearestcorrectedTime": "2016-02-29T12:45:00-05:00",
                           "nearestrawTime": "2016-02-29T12:45:00-05:00",
                           "nearestrawValue": "5.19"
                           }
                           ],
                           "reportMetadata": {
                           "allowedTypes": "Routine,Reset,Cleaning,After,ReferencePrimary,Reference,Unknown,BubbleGage",
                           "country": "United States of America",
                           "altitude": "     930",
                           "requestingUser": "thongsav",
                           "endDate": "2016-11-14T23:59:59.999999999Z",
                           "timezone": "Etc/GMT+5",
                           "latitude": "44.8577777777778",
                           "county": "Franklin County",
                           "description": "Compares continuous sensors with readings from known good sensors",
                           "title": "Sensor Reading Summary",
                           "requestId": "SensorReadingSummaryChoreographer-5b3e0fa8-e154-434c-9aa3-88f636a2a394",
                           "siteNumber": "01047200       ",
                           "stationName": "Sandy River near Madrid, Maine",
                           "coordinateDatumCode": "NAD83     ",
                           "state": "Maine",
                           "requestTemporal": "2016-11-14T13:17:54.745",
                           "stationId": "01047200",
                           "excludeComments": false,
                           "agency": "USGS ",
                           "drainageArea": "    25.3",
                           "nwisRaAuthToken": "bce59bfa-142e-4c67-8c46-fa17e86d2748",
                           "timeSeriesParams": "Gage height",
                           "timeSeriesUniqueId": "3ec1f3f59fe944079fc0ea5a4eb03e83",
                           "timeseriesLabel": "Gage height.ft@01047200",
                           "longitude": "-70.4852777777778",
                           "contributingDrainageArea": "    25.3",
                           "altitudeDatumCode": "NGVD29    ",
                           "startDate": "2015-11-01T00:00:00Z"
                           }
}')
  includeComments <- repgen:::isNullOrFalse(repgen:::fetchReportMetadataField(reportObject, 'excludeComments'))
  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Recorder w/in Uncertainty?", 
                   "Indicated Correction",
                   "Applied Correction",
                   "Corrected w/in Reference?",
                   "Value",
                   "Time",
                   "Qualifier"
  )
  results <- repgen:::formatSensorData(reportObject[["readings"]], columnNames, includeComments)
  expect_equal(length(results$toRet),19)
})

test_that('getUniqueComments works for removing redundant comments', {
  library(jsonlite)
  lastDate <- "09/29/2014"
  lastComm <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  date <- "09/29/2014"
  comments <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  lastRefComm <- "Comment = AF RP1 4.046 - 0.160 WS 3.886 // AL - 0.170 WS 3.876<br/>"
  selectedComm <- repgen:::getUniqueComments(comments, date, lastDate, lastComm)
  expect_equal(selectedComm,"")
})

test_that('getUniqueComments works for non-redundant comments', {
  library(jsonlite)
  lastDate <- "09/29/2014"
  lastComm <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  date <- "09/29/2014"
  comments <- "Test<br/>Test<br/>Test<br/>"
  selectedComm <- repgen:::getUniqueComments(comments, date, lastDate, lastComm)
  expect_equal(selectedComm,"Test<br/>Test<br/>Test<br/>")
})

test_that('recorder uncertainty function works as expected, delivers No', {
  recorderValue <- "6.23"
  uncertainty <- "0.05"
  value <- "6.42"
  recorderWithin <- repgen:::getRecorderWithinUncertainty(uncertainty, value, recorderValue)
  expect_equal(recorderWithin,"No")
})

test_that('recorder uncertainty function works as expected, delivers Yes', {
  recorderValue <- "6.44"
  uncertainty <- "0.05"
  value <- "6.42"
  recorderWithin <- repgen:::getRecorderWithinUncertainty(uncertainty, value, recorderValue)
  expect_equal(recorderWithin,"Yes")
})

test_that('indicated correction returns correctly',{
  recorderValue <- "6.44"
  value <- "6.42"
  indicatedCorrection <- repgen:::getIndicatedCorrection(recorderValue, value)
  expect_equal(indicatedCorrection,"-0.02")
  
})

setwd(dir = wd)
