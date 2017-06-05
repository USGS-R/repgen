context("timeseriessummary tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing timeseriessummary report")
test_that("timeseriessummary examples work", {
  library(jsonlite)
  library(whisker)
  library(readr)
  
  report1 <- renderReport(fromJSON(system.file('extdata',"timeseriessummary", "timeseriessummary-example.json",package = 'repgen')), "timeseriessummary", "author");
  renderedHtml1 <- read_file(report1)
  expect_is(report1, 'character')
  expect_equal(grep("<title>Time Series Summary</title>", renderedHtml1), 1)
  
  report2 <- renderReport(fromJSON(system.file('extdata',"timeseriessummary", "timeseriessummary-example2.json",package = 'repgen')), "timeseriessummary", "author");
  renderedHtml2 <- read_file(report2)
  expect_is(report2, 'character')
  expect_equal(grep("<title>Time Series Summary</title>", renderedHtml2), 1)
  
  report3 <- renderReport(fromJSON(system.file('extdata',"timeseriessummary", "timeseriessummary-example3.json",package = 'repgen')), "timeseriessummary", "author");
  renderedHtml3 <- read_file(report3)
  expect_is(report2, 'character')
  expect_equal(grep("<title>Time Series Summary</title>", renderedHtml3), 1)
  
  report4 <- renderReport(fromJSON(system.file('extdata',"timeseriessummary", "timeseriessummary-example4.json",package = 'repgen')), "timeseriessummary", "author");
  renderedHtml4 <- read_file(report4)
  expect_is(report4, 'character')
  expect_equal(grep("<title>Time Series Summary</title>", renderedHtml4), 1)
})

test_that('parseTSSRealtedSeries properly retrieves the related upchain series', {
  seriesJson <- fromJSON('{
    "upchainTs": [
      {
      "identifier": "Gage height.ft@01047200",
      "parameter": "Gage height",
      "parameterIdentifier": "Gage height",
      "nwisName": "Gage height",
      "nwisPcode": "00065",
      "unit": "ft",
      "computation": "Instantaneous",
      "timezone": "Etc/GMT+5",
      "inverted": false,
      "groundWater": false,
      "discharge": false,
      "sublocation": "",
      "timeSeriesType": "ProcessorDerived",
      "period": "Points",
      "publish": true,
      "primary": true,
      "uniqueId": "5eb2fdadf2784ebeaed2f64c6d02edf8"
      }
    ],
    "downchainTs": [
      {
      "identifier": "Discharge.ft^3/s.diff_per@01047200",
      "parameter": "Discharge",
      "parameterIdentifier": "Discharge",
      "nwisName": "Discharge",
      "nwisPcode": "00060",
      "unit": "ft^3/s",
      "computation": "Instantaneous",
      "timezone": "Etc/GMT+5",
      "inverted": false,
      "groundWater": false,
      "discharge": true,
      "sublocation": "",
      "timeSeriesType": "ProcessorDerived",
      "period": "Points",
      "publish": false,
      "primary": false,
      "uniqueId": "884fc0c281b14685baf9cbf744f0a606"
      }
    ]
  }')
  
  series <- repgen:::parseTSSRelatedSeries(seriesJson)
  nullSeries <- repgen:::parseTSSRelatedSeries(NULL)
  
  expect_equal(nullSeries, list())
  expect_is(series, 'data.frame')
  expect_equal(nrow(series), 1)
  expect_equal(series[['upchain']][[1]], 'Gage height.ft@01047200')
  expect_equal(series[['downchain']][[1]], "Discharge.ft^3/s.diff_per@01047200")
})

test_that('parseTSSQualifiers properly retrieves the qualifiers', {
  timezone <- "Etc/GMT+5"
  qualsJson <- fromJSON('{
    "primaryTsMetadata": {
      "qualifiers": [
        {
        "startDate": "2017-03-05T18:45:00-05:00",
        "endDate": "2017-03-06T05:45:00.0000001-05:00",
        "identifier": "EQUIP",
        "code": "EQP",
        "displayName": "Equipment malfunction",
        "appliedBy": "system",
        "dateApplied": "2017-03-11T14:57:13.4625975Z"
        },
        {
        "startDate": "2017-02-26T01:30:00-05:00",
        "endDate": "2017-02-26T01:30:00.0000001-05:00",
        "identifier": "EQUIP",
        "code": "EQP",
        "displayName": "Equipment malfunction",
        "appliedBy": "system",
        "dateApplied": "2017-03-11T14:57:13.4625975Z"
        }
      ]
    }
  }')
  
  quals <- repgen:::parseTSSQualifiers(qualsJson, timezone)
  nullQuals <- repgen:::parseTSSQualifiers(NULL)
  
  expect_equal(nullQuals, list())
  expect_is(quals, 'data.frame')
  expect_equal(nrow(quals), 2)
  expect_equal(quals[1,][['startDate']], as.character(flexibleTimeParse('2017-03-05T18:45:00-05:00', timezone)))
  expect_equal(quals[1,][['endDate']], as.character(flexibleTimeParse("2017-03-06T05:45:00.0000001-05:00", timezone)))
  expect_equal(quals[1,][['value']], "EQUIP")
  expect_equal(quals[1,][['code']], "EQP")
})

test_that('parseTSSNotes properly retrieves the notes', {
  timezone <- "Etc/GMT+5"
  notesJson <- fromJSON('{
    "primaryTsMetadata": {
      "notes": [
        {
        "startDate": "2017-02-24T12:30:00-05:00",
        "endDate": "2017-02-24T14:00:00.0000001-05:00",
        "note": "ADAPS Source Flag: *"
        }
      ]
    }
  }')
  
  notes <- repgen:::parseTSSNotes(notesJson, timezone)
  nullNotes <- repgen:::parseTSSNotes(NULL)
  
  expect_equal(nullNotes, list())
  expect_is(notes, 'data.frame')
  expect_equal(length(notes[[1]]), 1)
  expect_equal(notes[['startDate']][[1]], as.character(flexibleTimeParse('2017-02-24T12:30:00-05:00', timezone)))
  expect_equal(notes[['endDate']][[1]], as.character(flexibleTimeParse("2017-02-24T14:00:00.0000001-05:00", timezone)))
  expect_equal(notes[['value']][[1]], "ADAPS Source Flag: *")
})

test_that('parseTSSGrades properly retrieves the grades', {
  timezone <- "Etc/GMT+5"
  gradesJson <- fromJSON('{
                         "primaryTsMetadata": {
                         "grades": [
                         {
                         "startDate": "2016-05-01T00:00:00-05:00",
                         "endDate": "2017-05-31T00:00:00.0000001-05:00",
                         "code": "50"
                         }
                         ]
                         }
  }')
  
  grades <- repgen:::parseTSSGrades(gradesJson, timezone)
  nullGrades <- repgen:::parseTSSGrades(NULL)
  
  expect_equal(nullGrades, list())
  expect_is(grades, 'data.frame')
  expect_equal(length(grades[[1]]), 1)
  expect_equal(grades[['startDate']], as.character(flexibleTimeParse('2016-05-01T00:00:00-05:00', timezone)))
  expect_equal(grades[['endDate']], as.character(flexibleTimeParse("2017-05-31T00:00:00.0000001-05:00", timezone)))
  expect_equal(grades[['value']], "50")
})

test_that('parseTSSRatingCurves properly retrieves the rating cruves', {
  timezone <- "Etc/GMT+5"
  curvesJson <- fromJSON('{
   "ratingCurves": [
     {
       "curveNumber": "6.2",
       "ratingShifts": [
         {
         "curveNumber": "6.2",
         "shiftPoints": [
           0.03,
           0.12,
           0
         ],
         "stagePoints": [
           3.9,
           5.3,
           7.1
         ],
         "applicableStartDateTime": "2015-10-06T16:06:01-05:00",
         "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
         "shiftNumber": 0,
         "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
         }
       ],
       "baseRatingTable": {
       "inputValues": [
         3.6,
         3.79,
         3.9,
         8.3,
         9,
         10,
         11
       ],
       "outputValues": [
         1.473,
         3.629,
         5.53,
         1400,
         2058,
         3433,
         5400
       ]
       },
       "offsets": {
       "inputValues": [
         null
       ],
       "offSetValues": [
         3.05
       ]
       },
       "startOfPeriod": "2015-10-06T16:06:01-05:00",
       "endOfPeriod": "2016-11-16T00:00:00-05:00",
       "remarks": "Lowend extension for coverage in WY2016, base on measurements 104-107. Same as rating 6.1 above 3.90 ft. Was extended -.30 ft below 3.90 ft for low water coverage in WY2016-2017. ARC",
       "ratingType": "LogarithmicTable",
       "applicablePeriods": [
         {
         "startTime": "2015-10-06T16:06:01-05:00",
         "endTime": "2016-11-16T00:00:00-05:00",
         "remarks": "Started rating at beginning of new period worked. ARC"
         },
         {
         "startTime": "2016-11-16T00:00:00-05:00",
         "endTime": "9999-12-31T23:59:59.9999999Z",
         "remarks": "Started rating at beginning of new period worked. ARC"
         }
       ]
     }
   ]
  }')
  
  curves <- repgen:::parseTSSRatingCurves(curvesJson, timezone)
  nullCurves <- repgen:::parseTSSRatingCurves(list(), timezone)
  
  expect_equal(nullCurves, list())
  expect_is(curves, 'data.frame')
  expect_equal(nrow(curves), 2)
  expect_equal(curves[1,][['startOfPeriod']], '2015-10-06T16:06:01-05:00')
  expect_equal(curves[1,][['endOfPeriod']], "2016-11-16T00:00:00-05:00")
  expect_equal(curves[1,][['curveNumber']], "6.2")
})

test_that("parseTSSThresholds properly retrieves the threshold data", {
  timezone <- "Etc/GMT+5"
  thresholdJSON <- fromJSON('{
                            "reportMetadata": {
                            "timezone": "Etc/GMT+5"
                            },
                            "thresholds": [
                            {
                            "name": "VERY HIGH",
                            "referenceCode": "AQUARIUS only",
                            "type": "ThresholdAbove",
                            "severity": 0,
                            "description": "Unspecified threshold value",
                            "periods": [
                            {
                            "startTime": "2000-01-01T00:00:00Z",
                            "endTime": "2015-05-31T23:59:59.9999999Z",
                            "appliedTime": "2016-03-10T02:53:07.8904293Z",
                            "referenceValue": 4000,
                            "suppressData": true
                            },
                            {
                            "startTime": "2015-06-02T00:00:00Z",
                            "endTime": "9999-05-31T23:59:59.9999999Z",
                            "appliedTime": "2016-03-10T02:53:07.8904293Z",
                            "referenceValue": 1234,
                            "suppressData": true
                            }
                            ]
                            },
                            {
                            "name": "VERY LOW",
                            "referenceCode": "AQUARIUS only",
                            "type": "ThresholdBelow",
                            "severity": 0,
                            "description": "Unspecified threshold value",
                            "periods": [
                            {
                            "startTime": "0001-01-01T00:00:00Z",
                            "endTime": "9999-12-31T23:59:59.9999999Z",
                            "appliedTime": "2016-03-10T02:53:08.1400229Z",
                            "referenceValue": 0,
                            "suppressData": true
                            }
                            ]
                            }
                            ]
  }')

  thresholds <- repgen:::parseTSSThresholds(thresholdJSON, timezone)
  nullThresholds <- repgen:::parseTSSThresholds(NULL)
  
  expect_equal(nullThresholds, list())
  expect_is(thresholds, 'data.frame')
  expect_equal(nrow(thresholds), 3)
  expect_equal(thresholds[1,][['type']], 'ThresholdAbove')
  expect_equal(thresholds[2,][['type']], 'ThresholdAbove')
  expect_equal(thresholds[3,][['type']], 'ThresholdBelow')
})

test_that('parseTSSRatingShifts data returns as expected', {
  timezone <- "Etc/GMT+5"
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
          "applicableEndDateTime": "2015-10-09T10:50:00.000-05:00",
          "shiftNumber": 1
        }
      ]
  }')
  
  ratingShifts <- repgen:::parseTSSRatingShifts(reportObject, timezone)
  nullShifts <- repgen:::parseTSSRatingShifts(NULL)
  
  expect_equal(nullShifts, list())
  expect_equal(length(ratingShifts$shiftPoints[[1]]), 2)
  expect_equal(length(ratingShifts$stagePoints[[1]]), 2)
  expect_equal(ratingShifts$curveNumber, "9")
  expect_equal(ratingShifts$shiftNumber, 1)
  expect_equal(ratingShifts$applicableStartDateTime, as.character(flexibleTimeParse("2014-10-09T10:50:00.000-05:00", timezone)))
})

test_that('formatDataTable properly formats a list or data frame into table rows to be rendered by whikser', {
  testDataFrame <- data.frame(testCol1 = c(1,2,3,4,5), testCol2=c(5,4,3,2,1))
  testDataList <- list(testCol1 = c(1,2,3,4,5), testCol2=c(5,4,3,2,1))
  
  testFrameRows <- repgen:::formatDataTable(testDataFrame)
  testListRows <- repgen:::formatDataTable(testDataList)
  nullRows <- repgen:::formatDataTable(NULL)
  
  expect_equal(nullRows, data.frame())
  expect_equal(testFrameRows, testListRows)
  expect_equal(length(testFrameRows), 5)
  expect_equal(testListRows[[3]][['testCol1']], 3)
  expect_equal(testListRows[[3]][['testCol2']], 3)
})

setwd(dir = wd)

