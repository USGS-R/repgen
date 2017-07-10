context("uvhydrograph parsePrimarySeriesList tests")

wd <- getwd()
setwd(dir = tempdir())


test_that("loggedAxis is true if only a primary series exists and is volumetric and has no negative values", {
  #compatible = where isVolumetricFlow=TRUE and all values > 0
  library('jsonlite')
  
  minimalReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "type": "Discharge",
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')
  
  #november is logable
  expect_true(repgen:::parsePrimarySeriesList(minimalReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  
  #jan has a negative value
  expect_false(repgen:::parsePrimarySeriesList(minimalReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false if only a primary series exists and is NOT volumetric and has no negative values", {
  #compatible = where isVolumetricFlow=TRUE and all values > 0
  library('jsonlite')
  
  minimalReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')
  
  #both months not loggable
  expect_false(repgen:::parsePrimarySeriesList(minimalReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(minimalReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})
 

test_that("loggedAxis is true when all series (primary, reference, comparison) are volumetric and have no negative values", {
  library('jsonlite')
  
  withCompatibleReferenceAndComparisonSeriesReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')


  #november is logable
  expect_true(repgen:::parsePrimarySeriesList(withCompatibleReferenceAndComparisonSeriesReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  
  #jan has a negative value
  expect_false(repgen:::parsePrimarySeriesList(withCompatibleReferenceAndComparisonSeriesReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false if primary is NOT volumetric and comparison/reference series are", {
  library('jsonlite')
  primaryNotVolumetric <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')
  
  #both months not logable
  expect_false(repgen:::parsePrimarySeriesList(primaryNotVolumetric, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(primaryNotVolumetric, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false if reference is NOT volumetric and comparison/primary series are", {
  library('jsonlite')
  refNotVolumetric <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')
  
  #both months not logable
  expect_false(repgen:::parsePrimarySeriesList(refNotVolumetric, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(refNotVolumetric, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false if comparison is NOT volumetric and reference/primary series are", {
  library('jsonlite')  
  comparisonNotVolumetric <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": false,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')
  
  #both months not logable
  expect_false(repgen:::parsePrimarySeriesList(comparisonNotVolumetric, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(comparisonNotVolumetric, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false when all series primary series contains negative values", {
  library('jsonlite')
  
  withNegPrimSeriesReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-11-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-11-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-12-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-12-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')


  #both months not logable
  expect_false(repgen:::parsePrimarySeriesList(withNegPrimSeriesReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(withNegPrimSeriesReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false if comparison series has negative values", {
  library('jsonlite')
  
  withNegCompSeriesReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-11-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-11-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-12-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-12-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')

  #both months not loggable
  expect_false(repgen:::parsePrimarySeriesList(withNegCompSeriesReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(withNegCompSeriesReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("loggedAxis is false when reference series has negative values", {
  library('jsonlite')
  
  withNegRefSeriesReport  <- fromJSON('{ 
     "primarySeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "primarySeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "referenceSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-11-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-11-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "referenceSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
       "gaps" : [], 
       "gapTolerances" : [], 
       "name" : "test series",
       "points": [
         {
         "time": "2014-12-20T00:00:00-05:00",
         "value": -4510
         },
         {
         "time": "2014-12-21T00:00:00-05:00",
         "value": -3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeries": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     }, 
     "comparisonSeriesRaw": {
       "isVolumetricFlow": true,
       "approvals" : [],
       "type": "Discharge",
       "qualifiers" : [],
       "units" : "unit", 
       "grades" : [], 
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
         "value": 3960
         },
         {
         "time": "2015-01-22T00:00:00-05:00",
         "value": -3840
         }] 
     },
     "reportMetadata": {
       "excludeZeroNegative": false,
       "primaryParameter" : "Discharge",
     "timezone": "Etc/GMT+5"
     }}')


  #both months not logable
  expect_false(repgen:::parsePrimarySeriesList(withNegRefSeriesReport, "1411", "Etc/GMT+5")[['loggedAxis']])
  expect_false(repgen:::parsePrimarySeriesList(withNegRefSeriesReport, "1501", "Etc/GMT+5")[['loggedAxis']])
})

test_that("useEstimated is true if corrected data exists only as estimated data and non-estimated data",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-only-estimated-no-nonestimated-periods.json', package = 'repgen'))
  
  expect_true(repgen:::parsePrimarySeriesList(reportObject, "1701", "Etc/GMT+5")[['useEstimated']])
})

test_that("useEstimated is false if no corrected data exists",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-no-primary-pts.json', package = 'repgen'))
                           
  expect_false(repgen:::parsePrimarySeriesList(reportObject, "1701", "Etc/GMT+5")[['useEstimated']])
})
  
test_that("useEstimated is false if corrected estimated and non-estimated data exists",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-estimated-and-nonestimated-periods.json', package = 'repgen'))

 expect_false(repgen:::parsePrimarySeriesList(reportObject, "1701", "Etc/GMT+5")[['useEstimated']])
 
})

setwd(dir = wd)