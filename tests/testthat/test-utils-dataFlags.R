context("utils-dataFlags")

wd <- getwd()
setwd(dir = tempdir())

negativeData <- fromJSON('{
                           "tsField": {
                         "notes": [],
                         "isVolumetricFlow": true,
                         "description": "From Aquarius",
                         "qualifiers": [],
                         "units": "ft^3/s",
                         "grades": [],
                         "type": "Discharge",
                         "points": [
                         {
                         "time": "2013-10-01T00:00:00.000-06:00",
                         "value": -5
                         }],
                         "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
                         "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
                         "approvals": [],
                         "name": "d10cfa498ed248de983cbeaf0a75c14b",
                         "startTime": "2013-10-01T00:00:00.000-06:00",
                         "endTime": "2013-10-30T00:00:00.000-06:00",
                         "inverted": false,
                         "excludeZeroNeg": true
                         }}')
loggedData <- fromJSON('{
                         "tsField": {
                       "notes": [],
                       "isVolumetricFlow": true,
                       "description": "From Aquarius",
                       "qualifiers": [],
                       "units": "ft^3/s",
                       "grades": [],
                       "type": "Discharge",
                       "points": [
                       {
                       "time": "2013-10-02T00:00:00.000-06:00",
                       "value": 5
                       }],
                       "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
                       "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
                       "approvals": [],
                       "name": "d10cfa498ed248de983cbeaf0a75c14b",
                       "startTime": "2013-10-01T00:00:00.000-06:00",
                       "endTime": "2013-10-30T00:00:00.000-06:00",
                       "inverted": false,
                       "excludeZeroNeg": true
                       }}')
zeroData <- fromJSON('{
                       "tsField": {
                       "notes": [],
                       "isVolumetricFlow": true,
                       "description": "From Aquarius",
                       "qualifiers": [],
                       "units": "ft^3/s",
                       "grades": [],
                       "type": "Discharge",
                       "points": [
                       {
                       "time": "2013-10-01T00:00:00.000-06:00",
                       "value": 0
                       }],
                       "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
                       "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
                       "approvals": [],
                       "name": "d10cfa498ed248de983cbeaf0a75c14b",
                       "startTime": "2013-10-01T00:00:00.000-06:00",
                       "endTime": "2013-10-30T00:00:00.000-06:00",
                       "inverted": false,
                       "excludeZeroNeg": true
                       }}')

context("zeroValues")
test_that("can detect if values are negative", {
  expect_false(repgen:::zeroValues(loggedData$tsField$points))
  expect_true(repgen:::zeroValues(zeroData$tsField$points))
  expect_false(repgen:::zeroValues(negativeData$tsField$points))
})

context("negValues")
test_that("can detect if values are negative", {
  expect_false(repgen:::negValues(loggedData$tsField$points))
  expect_false(repgen:::negValues(zeroData$tsField$points))
  expect_true(repgen:::negValues(negativeData$tsField$points))
})

context("isTimeSeriesInverted")
test_that("can detect if timeseries is inverted", {
  #note these json fragments contain only a subset of what a full timeseries json object would
  invertedTS <- fromJSON('{
                         "inverted": true,
                         "points": [] 
}')
  expect_true(repgen:::isTimeSeriesInverted(invertedTS))
  
  regularTS <- fromJSON('{
                        "inverted": false,
                        "points": [] 
                        }')
  expect_false(repgen:::isTimeSeriesInverted(regularTS))
  
  # defaults to false if prop does not exist
  invertedUndefinedTS <- fromJSON('{
                                  "points": [] 
                                  }')
  expect_false(repgen:::isTimeSeriesInverted(invertedUndefinedTS))
})


context("isLogged")
test_that('isLogged properly detects if a TS should be plotted with logarithmic axes', {
  
  testthat::expect_true(repgen:::isLogged(loggedData$tsField$points, 
                                           zeroData$tsField[['isVolumetricFlow']],
                                           zeroData$tsField[['excludeZeroNeg']]))
  
  context("False if excludeZeroNeg, isVolFlow, or both are false")
  testthat::expect_false(repgen:::isLogged(zeroData$tsField$points, 
                                           zeroData$tsField[['isVolumetricFlow']],
                                           FALSE), info = "excludeZeroNeg false")
  testthat::expect_false(repgen:::isLogged(loggedData$tsField$points, 
                                           FALSE,
                                           zeroData$tsField[['excludeZeroNeg']]), info = "isVolFlow false")
  testthat::expect_false(repgen:::isLogged(loggedData$tsField$points, 
                                           FALSE,
                                           FALSE), info = "Both false")
  
  context("Returns false if data is null")
  
  ##### Do we want to default this to true if NULL? ####
  testthat::expect_true(repgen:::isLogged(NULL, 
                                           zeroData$tsField[['isVolumetricFlow']],
                                           zeroData$tsField[['excludeZeroNeg']]), info = "data is null")
  
  context("zeroData and negativeData return false")
  testthat::expect_false(repgen:::isLogged(zeroData$tsField$points, 
                                           zeroData$tsField[['isVolumetricFlow']],
                                           zeroData$tsField[['excludeZeroNeg']]), info = "data is zero")
  
  testthat::expect_false(repgen:::isLogged(negativeData$tsField$points, 
                                           negativeData$tsField[['isVolumetricFlow']],
                                           negativeData$tsField[['excludeZeroNeg']]), info = "data is negative")
  
})