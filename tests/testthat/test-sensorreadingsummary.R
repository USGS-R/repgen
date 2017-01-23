context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  reportData <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportData, 'Author Name'), 'character')
  
  reportData2 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example-exc-comm.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportData, 'Author Name'), 'character')
})

test_that("get unique qualifiers", {

  reportObject <- fromJSON('{
    "readings": [
    {
    "displayTime": "2016-01-06T10:21:00-05:00",
    "recorderComments": [
    "Comment \\u003d Sensor readings were from the H522, which agreed with Sutron combo unit. no AF readings were taken during this SV.\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
    ],
    "visitStatus": "TODO",
    "recorderMethod": "Non-subm pressure  transducer",
    "recorderValue": "5.02",
    "recorderType": "Routine",
    "party": "ARC/SGS",
    "nearestcorrectedValue": "5.02",
    "qualifiers": [],
    "nearestcorrectedTime": "2016-01-06T10:15:00-05:00",
    "nearestrawTime": "2016-01-06T10:15:00-05:00",
    "nearestrawValue": "5.02"
    },
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
  sensorreadingTable <- repgen:::sensorreadingTable(reportObject)
  uniqueSRSQualifiers <- repgen:::getSrsTableQualifiers(sensorreadingTable)
  expect_true(length(uniqueSRSQualifiers)==1)

  })

setwd(dir = wd)
