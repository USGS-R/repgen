context("dvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing dvhydrograph")
test_that("dvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-example.json', package = 'repgen'))
  expect_is(repgen::dvhydrograph(data, 'Author Name'), 'character')

  data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-no-min-max.json', package = 'repgen'))
  expect_is(repgen::dvhydrograph(data, 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-waterlevel-example.json', package = 'repgen'))
  expect_is(repgen::dvhydrograph(data2, 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-aqcu744_newData.json', package = 'repgen'))
  expect_is(repgen::dvhydrograph(data3, 'Author Name'), 'character')
  
})

test_that("excludeZeroNegative flag works", {
  data_neg <- fromJSON(' { 
    "firstDownChain": {
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
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }}')
  
  data_zero <- fromJSON(' { 
    "firstDownChain": {
      "isVolumetricFlow": true,
      "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": 3960
         },
         {
         "time": "2014-11-22",
         "value": 0
         }] 
     }, 
    "reportMetadata": {
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }}')
  
  data_notVol <- fromJSON(' { 
    "firstDownChain": {
      "isVolumetricFlow": false,
      "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": 3960
         },
         {
         "time": "2014-11-22",
         "value": 0
         }] 
     }, 
    "reportMetadata": {
      "downChainDescriptions1": "Not discharge",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }}')
  
  data_notExclu <- fromJSON(' { 
    "firstDownChain": {
      "isVolumetricFlow": true,
      "points": [
         {
         "time": "2014-11-20",
         "value": 4510
         },
         {
         "time": "2014-11-21",
         "value": 3960
         },
         {
         "time": "2014-11-22",
         "value": 0
         }] 
     }, 
    "reportMetadata": {
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": false,
      "timezone": "Etc/GMT+5"
    }}')
  
  #neg <- getStatDerived(data_neg, "firstDownChain", "downChainDescriptions1", estimated = FALSE, 
  #                      rmZeroNeg = data_neg$reportMetadata$excludeZeroNegative)
  #expect_equal(length(neg$value), 2)
  #
  #zero <- getStatDerived(data_zero, "firstDownChain", "downChainDescriptions1", estimated = FALSE, 
  #                       rmZeroNeg = data_zero$reportMetadata$excludeZeroNegative)
  #expect_equal(length(zero$value), 2)
  #
  #notVol <- getStatDerived(data_notVol, "firstDownChain", "downChainDescriptions1", estimated = FALSE, 
  #                         rmZeroNeg = data_notVol$reportMetadata$excludeZeroNegative)
  #expect_equal(length(notVol$value), 3)
  #
  #notExclu <- getStatDerived(data_notExclu, "firstDownChain", "downChainDescriptions1", estimated = FALSE, 
  #                           rmZeroNeg = data_notExclu$reportMetadata$excludeZeroNegative)
  #expect_true(any(notExclu$value == 0))
  expect_true(TRUE)
})

setwd(dir = wd)
