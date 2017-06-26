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
  expect_equal(quals[1,][['startDate']], as.character(flexibleTimeParse('2017-02-26T01:30:00-05:00', timezone)))
  expect_equal(quals[1,][['endDate']], as.character(flexibleTimeParse("2017-02-26T01:30:00.0000001-05:00", timezone)))
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

test_that('parseTSSRatingCurves properly retrieves the rating curves', {
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
  reportData <- fromJSON('{
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
  
  ratingShifts <- repgen:::parseTSSRatingShifts(reportData, timezone)
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

test_that('parseTSSGapTolerances properly retrieves the gap tolerances', {
  timezone <- "Etc/GMT+5"
  tolerancesJson <- fromJSON('{
    "gapTolerances": [
      {
        "startTime": "2016-06-01T00:00:00-05:00",
        "endTime": "2017-06-03T00:00:00.0000001-05:00",
        "toleranceInMinutes": 120
      }
    ]
  }')
  
  tolerances <- repgen:::parseTSSGapTolerances(tolerancesJson, timezone)
  nullTolerances <- repgen:::parseTSSGapTolerances(NULL, timezone)
  
  expect_equal(nullTolerances, NULL)
  expect_is(tolerances, 'data.frame')
  expect_equal(nrow(tolerances), 1)
  expect_equal(tolerances[1,][['startTime']], as.character(repgen:::flexibleTimeParse("2016-06-01T00:00:00-05:00", timezone)))
  expect_equal(tolerances[1,][['endTime']], as.character(repgen:::flexibleTimeParse("2017-06-03T00:00:00.0000001-05:00", timezone)))
  expect_equal(tolerances[1,][['toleranceInMinutes']], 120)
})

test_that('parseTSSApprovals properly retrieves the approvals', {
  timezone <- "Etc/GMT+5"
  approvalsJson <- fromJSON('{
    "approvals": [
      {
        "level": 1200,
        "description": "Approved",
        "comment": "",
        "dateApplied": "2017-02-02T21:16:24.937095Z",
        "startTime": "2007-10-01T00:00:00-05:00",
        "endTime": "2016-11-16T00:00:00-05:00"
      },
      {
        "level": 900,
        "description": "Working",
        "comment": "",
        "dateApplied": "2017-02-02T21:15:49.5368596Z",
        "startTime": "2016-11-16T00:00:00-05:00",
        "endTime": "9999-12-31T23:59:59.9999999Z"
      }
    ]
  }')
  
  approvals <- repgen:::parseTSSApprovals(approvalsJson, timezone)
  nullApprovals <- repgen:::parseTSSApprovals(NULL, timezone)
  
  expect_equal(nullApprovals, NULL)
  expect_is(approvals, 'data.frame')
  expect_equal(nrow(approvals), 2)
  expect_equal(approvals[1,][['startTime']], as.character(flexibleTimeParse('2007-10-01T00:00:00-05:00', timezone)))
  expect_equal(approvals[1,][['description']], "Approved")
})

test_that('parseTSSRatingCurves properly sorts the curves by startPeriod', {
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
          "applicableStartDateTime": "2015-11-25T16:06:01-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
        },
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
          "applicableStartDateTime": "2016-03-09T13:00:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
        },
        {
          "curveNumber": "6.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2016-03-10T23:00:00-05:00",
          "applicableEndDateTime": "2016-11-16T08:10:00-05:00",
          "shiftNumber": 0,
          "shiftRemarks": "prorated to WY2016 BRS during the Mar. 9-10 ice-out event, based on measurements 102-107. ARC"
        },
        {
          "curveNumber": "6.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2016-11-15T08:10:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued BRS into the next period worked. ARC"
        },
        {
          "curveNumber": "6.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2017-02-25T22:30:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continue BRS until ice-out. ARC"
        },
        {
          "curveNumber": "6.2",
          "shiftPoints": [
            0.02,
            0.12,
            0
          ],
          "stagePoints": [
            3.9,
            5.8,
            7.7
          ],
          "applicableStartDateTime": "2017-02-26T02:00:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Prorated to BRS#2 during the Feb. 25-26 ice-out event; based on measurements 113-115. ARC"
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
      "startOfPeriod": "2015-10-25T16:06:01-05:00",
      "endOfPeriod": "2016-11-16T00:00:00-05:00",
      "remarks": "Lowend extension for coverage in WY2016, base on measurements 104-107. Same as rating 6.1 above 3.90 ft. Was extended -.30 ft below 3.90 ft for low water coverage in WY2016-2017. ARC",
      "ratingType": "LogarithmicTable",
      "applicablePeriods": [
        {
          "startTime": "2015-10-25T16:06:01-05:00",
          "endTime": "2016-11-16T00:00:00-05:00",
          "remarks": "Started rating at beginning of new period worked. ARC"
        },
        {
          "startTime": "2016-10-16T00:00:00-05:00",
          "endTime": "9999-12-31T23:59:59.9999999Z",
          "remarks": "Started rating at beginning of new period worked. ARC"
        }
      ]
    },
	{
      "curveNumber": "16.2",
      "ratingShifts": [
        {
          "curveNumber": "16.2",
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
          "applicableStartDateTime": "2015-11-25T16:06:01-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
        },
        {
          "curveNumber": "16.2",
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
          "applicableStartDateTime": "2016-03-09T13:00:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
        },
        {
          "curveNumber": "16.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2016-03-10T23:00:00-05:00",
          "applicableEndDateTime": "2016-11-16T08:10:00-05:00",
          "shiftNumber": 0,
          "shiftRemarks": "prorated to WY2016 BRS during the Mar. 9-10 ice-out event, based on measurements 102-107. ARC"
        },
        {
          "curveNumber": "16.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2016-11-16T08:10:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continued BRS into the next period worked. ARC"
        },
        {
          "curveNumber": "16.2",
          "shiftPoints": [
            0.02,
            0.06,
            0
          ],
          "stagePoints": [
            3.9,
            5.3,
            7.1
          ],
          "applicableStartDateTime": "2017-02-25T22:30:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Continue BRS until ice-out. ARC"
        },
        {
          "curveNumber": "16.2",
          "shiftPoints": [
            0.02,
            0.12,
            0
          ],
          "stagePoints": [
            3.9,
            5.8,
            7.7
          ],
          "applicableStartDateTime": "2017-02-26T02:00:00-05:00",
          "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
          "shiftNumber": 0,
          "shiftRemarks": "Prorated to BRS#2 during the Feb. 25-26 ice-out event; based on measurements 113-115. ARC"
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
      "startOfPeriod": "2015-11-25T16:06:01-05:00",
      "endOfPeriod": "2016-11-16T00:00:00-05:00",
      "remarks": "Lowend extension for coverage in WY2016, base on measurements 104-107. Same as rating 6.1 above 3.90 ft. Was extended -.30 ft below 3.90 ft for low water coverage in WY2016-2017. ARC",
      "ratingType": "LogarithmicTable",
      "applicablePeriods": [
        {
          "startTime": "2015-11-25T16:06:01-05:00",
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
  expect_equal(curves[1,][['startOfPeriod']], as.character("2015-10-25T16:06:01-05:00"))
  expect_equal(curves[2,][['startOfPeriod']], as.character("2015-11-25T16:06:01-05:00"))

})

test_that('parseTSSRatingShifts properly sorts the shifts by applicableStartDateTime', {
  timezone <- "Etc/GMT+5"
  shiftsJson <- fromJSON('{
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
},
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
                         "applicableStartDateTime": "2016-03-09T13:00:00-05:00",
                         "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
                         "shiftNumber": 0,
                         "shiftRemarks": "Continued WY2015 BRS from rating 6.1. ARC"
                         },
                         {
                         "curveNumber": "6.2",
                         "shiftPoints": [
                         0.02,
                         0.06,
                         0
                         ],
                         "stagePoints": [
                         3.9,
                         5.3,
                         7.1
                         ],
                         "applicableStartDateTime": "2016-03-10T23:00:00-05:00",
                         "applicableEndDateTime": "2016-11-16T08:10:00-05:00",
                         "shiftNumber": 0,
                         "shiftRemarks": "prorated to WY2016 BRS during the Mar. 9-10 ice-out event, based on measurements 102-107. ARC"
                         },
                         {
                         "curveNumber": "6.2",
                         "shiftPoints": [
                         0.02,
                         0.06,
                         0
                         ],
                         "stagePoints": [
                         3.9,
                         5.3,
                         7.1
                         ],
                         "applicableStartDateTime": "2016-11-16T08:10:00-05:00",
                         "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
                         "shiftNumber": 0,
                         "shiftRemarks": "Continued BRS into the next period worked. ARC"
                         },
                         {
                         "curveNumber": "6.2",
                         "shiftPoints": [
                         0.02,
                         0.06,
                         0
                         ],
                         "stagePoints": [
                         3.9,
                         5.3,
                         7.1
                         ],
                         "applicableStartDateTime": "2017-02-25T22:30:00-05:00",
                         "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
                         "shiftNumber": 0,
                         "shiftRemarks": "Continue BRS until ice-out. ARC"
                         },
                         {
                         "curveNumber": "6.2",
                         "shiftPoints": [
                         0.02,
                         0.12,
                         0
                         ],
                         "stagePoints": [
                         3.9,
                         5.8,
                         7.7
                         ],
                         "applicableStartDateTime": "2017-02-26T02:00:00-05:00",
                         "applicableEndDateTime": "9999-12-31T23:59:59.9999999Z",
                         "shiftNumber": 0,
                         "shiftRemarks": "Prorated to BRS#2 during the Feb. 25-26 ice-out event; based on measurements 113-115. ARC"
                         }
                         ]
}')
  
  shifts <- repgen:::parseTSSRatingShifts(shiftsJson, timezone)
  expect_equal(shifts[1,][['applicableStartDateTime']], as.character("2015-10-06 16:06:01"))
  expect_equal(shifts[2,][['applicableStartDateTime']], as.character("2016-03-09 13:00:00"))
  expect_equal(shifts[3,][['applicableStartDateTime']], as.character("2016-03-10 23:00:00"))
  expect_equal(shifts[4,][['applicableStartDateTime']], as.character("2016-11-16 08:10:00"))
  expect_equal(shifts[5,][['applicableStartDateTime']], as.character("2017-02-25 22:30:00"))
  expect_equal(shifts[6,][['applicableStartDateTime']], as.character("2017-02-26 02:00:00"))
  
})

test_that('parseTSSQualifiers properly sorts the qualifiers by startDate', {
  timezone <- "Etc/GMT+5"
  qualifiersJson <- fromJSON('{
                         "primaryTsMetadata": {
                         "qualifiers": [
                         {
                         "startDate": "2017-02-26T01:30:00-05:00",
                         "endDate": "2017-02-26T01:30:00.0000001-05:00",
                         "identifier": "EQUIP",
                         "code": "EQP",
                         "displayName": "Equipment malfunction",
                         "appliedBy": "system",
                         "dateApplied": "2017-03-11T14:57:13.4625975Z"
                         },
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
                         "startDate": "2016-11-23T00:00:00-05:00",
                         "endDate": "2016-11-26T12:00:00.0000001-05:00",
                         "identifier": "ESTIMATED",
                         "code": "E",
                         "displayName": "Estimated",
                         "appliedBy": "acloutie",
                         "dateApplied": "2017-02-28T15:42:28.183755Z"
                         },
                         {
                         "startDate": "2016-11-29T12:00:00-05:00",
                         "endDate": "2017-02-23T12:00:00.0000001-05:00",
                         "identifier": "ESTIMATED",
                         "code": "E",
                         "displayName": "Estimated",
                         "appliedBy": "acloutie",
                         "dateApplied": "2017-02-28T16:14:51.1790218Z"
                         },
                         {
                         "startDate": "2017-02-23T12:00:00-05:00",
                         "endDate": "2017-02-28T10:00:00.0000001-05:00",
                         "identifier": "ESTIMATED",
                         "code": "E",
                         "displayName": "Estimated",
                         "appliedBy": "acloutie",
                         "dateApplied": "2017-02-28T20:05:45.8107059Z"
                         },
                         {
                         "startDate": "2017-02-28T10:15:00-05:00",
                         "endDate": "2017-04-03T00:00:00.0000001-05:00",
                         "identifier": "ICE",
                         "code": "ICE",
                         "displayName": "Flow at station affected by ice",
                         "appliedBy": "lflight",
                         "dateApplied": "2017-03-01T12:53:18.2261003Z"
                         }
                         ]
}
}')
  
  qualifiers <- repgen:::parseTSSQualifiers(qualifiersJson, timezone)
  expect_equal(qualifiers[1,][['startDate']], as.character("2016-11-23 00:00:00"))
  expect_equal(qualifiers[2,][['startDate']], as.character("2016-11-29 12:00:00"))
  expect_equal(qualifiers[3,][['startDate']], as.character("2017-02-23 12:00:00"))
  expect_equal(qualifiers[4,][['startDate']], as.character("2017-02-26 01:30:00"))
  expect_equal(qualifiers[5,][['startDate']], as.character("2017-02-28 10:15:00"))
  expect_equal(qualifiers[6,][['startDate']], as.character("2017-03-05 18:45:00"))
})


test_that('parseTSSNotes properly sorts the notes by startDate', {
  timezone <- "Etc/GMT+5"
  notesJson <- fromJSON('{
                             "primaryTsMetadata": {
                              "notes": [
                                {
                                 "startDate": "2017-01-01T00:00:00-05:00",
                                 "endDate": "2017-01-04T12:00:00.0000001-05:00",
                                 "note": "note creator/user test (lflight)"
                                },
                                {
                                  "startDate": "2016-01-01T00:00:00-05:00",
                                  "endDate": "2016-01-04T12:00:00.0000001-05:00",
                                  "note": "note creator/user test (lflight)"
                                },
                                {
                                "startDate": "2012-01-01T00:00:00-05:00",
                                "endDate": "2012-01-04T12:00:00.0000001-05:00",
                                "note": "note creator/user test (lflight)"
                                }
                              ]
                             }
}')
  
  notes <- repgen:::parseTSSNotes(notesJson, timezone)
  expect_equal(notes[1,][['startDate']], as.character("2012-01-01 00:00:00"))
  expect_equal(notes[2,][['startDate']], as.character("2016-01-01 00:00:00"))
  expect_equal(notes[3,][['startDate']], as.character("2017-01-01 00:00:00"))
})

test_that('parseTSSGrades properly sorts the grades by startDate', {
  timezone <- "Etc/GMT+5"
  gradesJson <- fromJSON('{
                        "primaryTsMetadata": {
                        "grades": [
                        {
                          "startDate": "2016-10-01T00:00:00-05:00",
                          "endDate": "2017-06-07T00:00:00.0000001-05:00",
                          "code": "50"
                        },
                        {
                          "startDate": "2015-04-01T00:00:00-05:00",
                          "endDate": "2015-06-07T00:00:00.0000001-05:00",
                          "code": "20"
                        },
                        {
                          "startDate": "2011-05-01T00:00:00-05:00",
                          "endDate": "2011-06-07T00:00:00.0000001-05:00",
                          "code": "10"
                        }
                        ]
                        }
}')
  
  grades <- repgen:::parseTSSGrades(gradesJson, timezone)
  expect_equal(grades[1,][['startDate']], as.character("2011-05-01 00:00:00"))
  expect_equal(grades[2,][['startDate']], as.character("2015-04-01 00:00:00"))
  expect_equal(grades[3,][['startDate']], as.character("2016-10-01 00:00:00"))
})

test_that('parseTSSProcessingCorrections properly sorts the corrections by startTime', {
  timezone <- "Etc/GMT+5"
  correctionsJson <- fromJSON('{
        "corrections": {
                "normal": [
                         {
                         "appliedTimeUtc": "2017-05-31T21:54:23.0717097Z",
                         "startTime": "2017-05-31T08:27:31.0328297-05:00",
                         "comment": "Delete region // Erroneous value during orifice swap. MEB",
                         "endTime": "2017-05-31T08:31:16-05:00",
                         "type": "DeleteRegion",
                         "parameters": "{}",
                         "user": "mbeardsley",
                         "processingOrder": "NORMAL"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:38:52.742006Z",
                         "startTime": "2016-08-04T12:45:00-05:00",
                         "comment": "Delete Region - Sensor reading during purge.",
                         "endTime": "2016-08-04T12:45:00.0000001-05:00",
                         "type": "DeleteRegion",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "NORMAL"
                         }
                         ],
              "postProcessing": [
                         {
                         "appliedTimeUtc": "2017-03-10T13:21:08.1047147Z",
                         "startTime": "2017-03-01T17:00:00-05:00",
                         "comment": "Offset Correction with value of 7.430ft // for sheered orifice line. JMK // Ended when orifice read offset (0.45+7.43=7.88).",
                         "endTime": "2017-03-11T06:30:00.0000001-05:00",
                         "type": "Offset",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "POST_PROCESSING"
                         }
                         ],
              "preProcessing": [
                         {
                         "appliedTimeUtc": "2017-01-30T12:03:38.1722106Z",
                         "startTime": "2016-07-07T16:15:00-05:00",
                         "endTime": "2016-07-07T16:15:00.0000001-05:00",
                         "type": "DeleteRegion",
                         "parameters": "{}",
                         "user": "admin",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:34:17.8751927Z",
                         "startTime": "2016-08-04T10:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-08-04T11:30:00.0000001-05:00",
                         "type": "CopyPaste",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:20:48.140056Z",
                         "startTime": "2016-09-12T08:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-09-12T11:15:00.0000001-05:00",
                         "type": "CopyPaste",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:43:07.4928065Z",
                         "startTime": "2016-11-14T11:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-11-14T12:15:00.0000001-05:00",
                         "type": "CopyPaste",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:45:03.8824656Z",
                         "startTime": "2016-12-29T22:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-12-29T23:45:00.0000001-05:00",
                         "type": "CopyPaste",
                         "parameters": "{}",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-18T11:54:14.8686833Z",
                         "startTime": "2017-03-11T06:00:00-05:00",
                         "comment": "Orifice out of water, as orifice line damaged by ice. ARC // Delete Region",
                         "endTime": "2017-03-17T14:15:00.0000001-05:00",
                         "type": "DeleteRegion",
                         "parameters": "{}",
                         "user": "acloutie",
                         "processingOrder": "PRE_PROCESSING"
                         }
                         ]
        }
  }')
  
  corrections <- repgen:::parseTSSProcessingCorrections(correctionsJson, "pre", timezone)
  expect_equal(corrections[1,][['startTime']], as.character("2016-07-07 16:15:00"))
  expect_equal(corrections[2,][['startTime']], as.character("2016-08-04 10:30:00"))
  expect_equal(corrections[3,][['startTime']], as.character("2016-09-12 08:30:00"))
  expect_equal(corrections[4,][['startTime']], as.character("2016-11-14 11:30:00"))
  expect_equal(corrections[5,][['startTime']], as.character("2016-12-29 22:30:00"))
  expect_equal(corrections[6,][['startTime']], as.character("2017-03-11 06:00:00"))
  
})

test_that('parseTSSGaps properly sorts the gaps by startTime', {
  timezone <- "Etc/GMT+5"
  gapsJson <- fromJSON('{
                        "gaps": [
                          {
                              "startTime": "2017-03-11T05:45:00-05:00",
                              "endTime": "2017-03-17T14:30:00-05:00"
                          },
                          {
                              "startTime": "2015-01-11T05:45:00-05:00",
                              "endTime": "2015-01-17T14:30:00-05:00"
                          },
                          {
                              "startTime": "2016-03-11T05:45:00-05:00",
                              "endTime": "2016-03-17T14:30:00-05:00"
                          }
                        ]
}')
  
  gaps <- repgen:::parseTSSGaps(gapsJson, timezone)
  expect_equal(gaps[1,][['startTime']], as.character("2015-01-11 05:45:00"))
  expect_equal(gaps[2,][['startTime']], as.character("2016-03-11 05:45:00"))
  expect_equal(gaps[3,][['startTime']], as.character("2017-03-11 05:45:00"))
})

test_that('parseTSSApprovals properly sorts the approvals by startTime', {
  timezone <- "Etc/GMT+5"
  approvalsJson <- fromJSON('{
                        "approvals": [
                        {
                         "level": 1200,
                         "description": "Approved",
                         "comment": "Approval changed to Approved by nstasuli.",
                         "dateApplied": "2017-04-13T14:57:21.7884638Z",
                         "startTime": "2016-10-06T00:00:00-05:00",
                         "endTime": "2016-11-14T12:17:00-05:00"
                       },
                       {
                         "level": 900,
                         "description": "Working",
                         "comment": "",
                         "dateApplied": "2017-02-02T21:13:31.788872Z",
                         "startTime": "2013-11-14T12:17:00-05:00",
                         "endTime": "9999-12-31T23:59:59.9999999Z"
                       }
                       ]
}')
  
  approvals <- repgen:::parseTSSApprovals(approvalsJson, timezone)
  expect_equal(approvals[1,][['startTime']], as.character("2013-11-14 12:17:00"))
  expect_equal(approvals[2,][['startTime']], as.character("2016-10-06 00:00:00"))
})

test_that('parseTSSGapTolerances properly sorts the gapTolerances by startTime', {
  timezone <- "Etc/GMT+5"
  gapTolerancesJson <- fromJSON('{
                          "gapTolerances": [
                            {
                                "startTime": "2016-06-01T00:00:00-05:00",
                                "endTime": "2017-06-07T00:00:00.0000001-05:00",
                                "toleranceInMinutes": 120
                            },
                            {
                              "startTime": "2014-06-01T00:00:00-05:00",
                              "endTime": "2015-06-07T00:00:00.0000001-05:00",
                              "toleranceInMinutes": 120
                            },
                                {
                              "startTime": "2011-06-01T00:00:00-05:00",
                              "endTime": "2013-06-07T00:00:00.0000001-05:00",
                              "toleranceInMinutes": 120
                            }
                              ]                                

  }')
  gapTolerances <- repgen:::parseTSSGapTolerances(gapTolerancesJson, timezone)
  expect_equal(gapTolerances[1,][['startTime']], as.character("2011-06-01 00:00:00"))
  expect_equal(gapTolerances[2,][['startTime']], as.character("2014-06-01 00:00:00"))
  expect_equal(gapTolerances[3,][['startTime']], as.character("2016-06-01 00:00:00"))

})

setwd(dir = wd)

