context("utils-filter tests")

wd <- getwd()
setwd(dir = tempdir())


context("isTimeSeriesInverted")
test_that("can detect if timeseries is inverted", {
  #note these json fragments contain only a subset of what a full timeseries json object would
  invertedTS <- fromJSON('{
                         "inverted": true,
                         "points": [] 
}')
  expect_true(isTimeSeriesInverted(invertedTS))
  
  regularTS <- fromJSON('{
                        "inverted": false,
                        "points": [] 
                        }')
  expect_false(isTimeSeriesInverted(regularTS))
  
  # defaults to false if prop does not exist
  invertedUndefinedTS <- fromJSON('{
                                  "points": [] 
                                  }')
  expect_false(isTimeSeriesInverted(invertedUndefinedTS))
  })

test_that('isLogged properly detects if a TS should be plotted with logarithmic axes', {
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
                         "inverted": false
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
                       "inverted": false
                       }}')

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
                           "inverted": false
                           }}')
})

setwd(dir = wd)
