context("uvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("unit testing uvhydrograph-data")
test_that("uvhydrograph function breakdown",{
  library(testthat)
  library(jsonlite)
  library(lubridate)
  
  testData <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-example.json', package = 'repgen'))
  
  reportMetadata <- testData$reportMetadata
  months <- repgen:::getMonths(testData, reportMetadata$timezone)
  primarySeriesList <- repgen:::parsePrimarySeriesList(testData, months[[1]], reportMetadata$timezone)
  primaryLims <- repgen:::calculatePrimaryLims(primarySeriesList, repgen:::isPrimaryDischarge(testData))
  upchainSeriesData <- repgen:::readTimeSeriesUvInfo(testData,"upchainSeries")
  primarySeriesData <- repgen:::readTimeSeriesUvInfo(testData,"primarySeries")
  secondaryTimeSeriesInfo <- repgen:::readSecondaryTimeSeriesUvInfo(testData)
  
  expect_is(primaryLims,"list")
  expect_equal(primaryLims$ylim,c(1780, 8920))
  
  expect_is(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone), "list")
  expect_equal(length(repgen:::parseUvTimeInformationFromLims(primaryLims,reportMetadata$timezone)$days), 30)
  
  expect_error(repgen:::readSecondaryTimeSeriesUvInfo(NULL))
  expect_equal(upchainSeriesData, secondaryTimeSeriesInfo)
  expect_is(secondaryTimeSeriesInfo,"list")
  expect_equal(secondaryTimeSeriesInfo$label, "Gage height  ( ft )")
  expect_equal(primarySeriesData$label, "Discharge  ( ft^3/s )")
  
  expect_null(repgen:::parseCorrectionsAsTable(NULL))
  
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

test_that("excludeZeroNegative flag works", {
  
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

setwd(dir = wd)
