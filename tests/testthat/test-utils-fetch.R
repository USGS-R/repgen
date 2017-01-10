context("utils-fetch tests")

test_that('fetchRatingShifts data returns as expected', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
    "ratingShifts" : [
        {
          "curveNumber": "9",
          "shiftPoints": [
            0,
            0
          ],
          "stagePoints": [
            3.5,
            5
          ],
          "applicableStartDateTime": "2014-10-09T10:50:00.000-05:00",
          "shiftNumber": 1
        }
      ]
  }')

  ratingShifts <- repgen:::fetchRatingShifts(reportObject)
  expect_equal(length(ratingShifts$shiftPoints[[1]]), 2)
  expect_equal(length(ratingShifts$stagePoints[[1]]), 2)
  expect_equal(ratingShifts$curveNumber, "9")
  expect_equal(ratingShifts$shiftNumber, 1)
  expect_equal(ratingShifts$applicableStartDateTime, "2014-10-09T10:50:00.000-05:00")
})


test_that('fetchMeasurements data returns as expected', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
      "measurements": [
        {
          "shiftInFeet": -0.66430119718037,
          "errorMinShiftInFeet": -1.09075307722521,
          "errorMaxShiftInFeet": -0.25187660812384,
          "identifier": "1BABC37B0CE3B19CE05322EB3D985005",
          "measurementStartDate": "2015-06-23T05:56:07.000-05:00",
          "ratingModelIdentifier": "Gage height-Discharge.STGQ@06933500",
          "discharge": 12300,
          "dischargeUnits": "ft^3/s",
          "errorMinDischarge": 11316.0000,
          "errorMaxDischarge": 13284.0000,
          "measurementNumber": "963",
          "qualityRating": "FAIR",
          "historic": false,
          "meanGageHeight": 9.46,
          "meanGageHeightUnits": "ft",
          "shiftNumber": 3
        }]
	}')

  measurements <- repgen:::fetchMeasurements(reportObject)
  expect_equal(measurements$shiftNumber, 3)
})

test_that('fetchMaxStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(repgen:::fetchMaxStage(empty), numeric(0))
  expect_is(repgen:::fetchMaxStage(empty), 'numeric')
  
  reportObject <- fromJSON('{ "maximumStageHeight" : 1 }')
  expect_equal(repgen:::fetchMaxStage(reportObject), 1)
})

test_that('fetchMinStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(repgen:::fetchMinStage(empty), numeric(0))
  expect_is(repgen:::fetchMinStage(empty), 'numeric')
  
  reportObject <- fromJSON('{ "minimumStageHeight" : 1 }')
  expect_equal(repgen:::fetchMinStage(reportObject), 1)
})

test_that('fetchReportMetadataField return values and empty string if not found', {
  library(jsonlite)
  
  data <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  val1 <- fetchReportMetadataField(data, "field1")
  val2 <- fetchReportMetadataField(data, "field2")
  val3 <- fetchReportMetadataField(data, "field3")
  
  expect_is(val1, 'character')
  expect_is(val2, 'character')
  expect_is(val3, 'NULL')
  
  expect_equal(val1, "value1")
  expect_equal(val2, "value2")
  expect_equal(val3, NULL)
})

test_that('fetchReportMetadata returns all of the report metadata', {
  library(jsonlite)
  
  data <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  metadata <- fetchReportMetadata(data)
  
  expect_is(metadata$field1, 'character')
  expect_is(metadata$field2, 'character')
  expect_is(metadata$field3, 'NULL')
  
  expect_equal(metadata$field1, "value1")
  expect_equal(metadata$field2, "value2")
  expect_equal(metadata$field3, NULL)
})

test_that('fetchTimeSeries returns all of the data for the specified series name', {
  library(jsonlite)

  data <- fromJSON()
})
