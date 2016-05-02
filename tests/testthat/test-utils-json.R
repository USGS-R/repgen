context("utils-json tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('getLims handles missing data', {
  expect_error(repgen:::getLims(NA,NA,NA,NA)) 
})
test_that('getRatingShifts data returns as expected', {
  library(jsonlite)

  empty <- list('data'=c(0,0,0))
  expect_equal(getRatingShifts(empty, 'shiftPoints'), " ")
  expect_error(getRatingShifts(empty, 'shiftPoints', required = TRUE))
  expect_is(getRatingShifts(empty, 'shiftPoints', as.numeric = T), 'numeric')
  expect_is(getRatingShifts(empty, 'shiftPoints', as.numeric = F), 'character')
  
  data <- fromJSON('{ "ratingShifts" : { "shiftPoints" : 1 } }')
  expect_equal(getRatingShifts(data, 'shiftPoints'), 1)
})

test_that('getMeasurements data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(getMeasurements(empty, 'shiftInFeet'), " ")
  expect_error(getMeasurements(empty, 'shiftInFeet', required = TRUE))
  expect_is(getMeasurements(empty, 'shiftInFeet', as.numeric = T), 'numeric')
  expect_is(getMeasurements(empty, 'shiftInFeet', as.numeric = F), 'character')
  
  data <- fromJSON('{ "measurements" : { "shiftInFeet" : 1 } }')
  expect_equal(getMeasurements(data, 'shiftInFeet'), 1)
})

test_that('getMaxStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(getMaxStage(empty), numeric(0))
  expect_is(getMaxStage(empty, as.numeric = T), 'numeric')
  
  data <- fromJSON('{ "maximumStageHeight" : 1 }')
  expect_equal(getMaxStage(data), 1)
})

test_that('getMinStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(getMinStage(empty), numeric(0))
  expect_is(getMinStage(empty, as.numeric = T), 'numeric')
  
  data <- fromJSON('{ "minimumStageHeight" : 1 }')
  expect_equal(getMinStage(data), 1)
})


test_that('getGroundWaterLevels data returns as expected', {
  expect_equal(nrow(getGroundWaterLevels(fromJSON('{}'))), 0)
  expect_equal(nrow(getGroundWaterLevels(fromJSON('{ "gwlevel": [] }'))), 0)
  
  gwLevels <- getGroundWaterLevels(fromJSON('{ "gwlevel": [
    { 
      "siteNumber": "353922083345600", 
      "groundWaterLevel": 9.14, 
      "dateString": "2012-06-22T00:00:00.000-06:00" 
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
  #TODO
})

test_that('getApprovals data returns as expected', {
  #TODO
})

test_that('getApprovalDates return correct data', {
    #TODO
})

test_that('getReportMetadata return values and empty string if not found', {
  library(jsonlite)
  
  data <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  val1 <- getReportMetadata(data, "field1")
  val2 <- getReportMetadata(data, "field2")
  val3 <- getReportMetadata(data, "field3")
  
  expect_is(val1, 'character')
  expect_is(val2, 'character')
  expect_is(val3, 'character')
  
  expect_equal(val1, "value1")
  expect_equal(val2, "value2")
  expect_equal(val3, " ")
})

setwd(dir = wd)
