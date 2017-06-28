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
  
  report5 <- renderReport(fromJSON(system.file('extdata',"timeseriessummary", "timeseriessummary-example5.json",package = 'repgen')), "timeseriessummary", "author");
  renderedHtml5 <- read_file(report5)
  expect_is(report5, 'character')
  expect_equal(grep("<title>Time Series Summary</title>", renderedHtml5), 1)
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
    "primaryTsData": {
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
    "primaryTsData": {
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
    "primaryTsData": {
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
    "primaryTsData": {
       "gapTolerances": [
         {
         "startTime": "2016-06-01T00:00:00-05:00",
         "endTime": "2017-06-03T00:00:00.0000001-05:00",
         "toleranceInMinutes": 120
         }
       ]
    }
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
    "primaryTsData": {
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
    }
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
  nullCurves <- repgen:::parseTSSRatingCurves(NULL, timezone)
  
  expect_equal(nullCurves, list())
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
  nullShifts <- repgen:::parseTSSRatingShifts(NULL, timezone)
  
  expect_equal(nullShifts, list())
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
                         "primaryTsData": {
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
  nullQualifiers <- repgen:::parseTSSQualifiers(NULL, timezone)
  
  expect_equal(nullQualifiers, list())
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
                             "primaryTsData": {
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
  nullNotes <- repgen:::parseTSSNotes(NULL, timezone)
  
  expect_equal(nullNotes, list())
  expect_equal(notes[1,][['startDate']], as.character("2012-01-01 00:00:00"))
  expect_equal(notes[2,][['startDate']], as.character("2016-01-01 00:00:00"))
  expect_equal(notes[3,][['startDate']], as.character("2017-01-01 00:00:00"))
})

test_that('parseTSSGrades properly sorts the grades by startDate', {
  timezone <- "Etc/GMT+5"
  gradesJson <- fromJSON('{
                        "primaryTsData": {
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
  nullGrades <- repgen:::parseTSSGrades(NULL, timezone)
  
  expect_equal(nullGrades, list())
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
                         "user": "mbeardsley",
                         "processingOrder": "NORMAL"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:38:52.742006Z",
                         "startTime": "2016-08-04T12:45:00-05:00",
                         "comment": "Delete Region - Sensor reading during purge.",
                         "endTime": "2016-08-04T12:45:00.0000001-05:00",
                         "type": "DeleteRegion",
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
                         "parameters": {
                            "Offset": 7.430
                          },
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
                         "user": "admin",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:34:17.8751927Z",
                         "startTime": "2016-08-04T10:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-08-04T11:30:00.0000001-05:00",
                         "type": "CopyPaste",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:20:48.140056Z",
                         "startTime": "2016-09-12T08:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-09-12T11:15:00.0000001-05:00",
                         "type": "CopyPaste",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:43:07.4928065Z",
                         "startTime": "2016-11-14T11:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-11-14T12:15:00.0000001-05:00",
                         "type": "CopyPaste",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-06T11:45:03.8824656Z",
                         "startTime": "2016-12-29T22:30:00-05:00",
                         "comment": "Copy and Paste from Gage height.ft.EDL@01034500 - JMK",
                         "endTime": "2016-12-29T23:45:00.0000001-05:00",
                         "type": "CopyPaste",
                         "user": "jkinsey",
                         "processingOrder": "PRE_PROCESSING"
                         },
                         {
                         "appliedTimeUtc": "2017-03-18T11:54:14.8686833Z",
                         "startTime": "2017-03-11T06:00:00-05:00",
                         "comment": "Orifice out of water, as orifice line damaged by ice. ARC // Delete Region",
                         "endTime": "2017-03-17T14:15:00.0000001-05:00",
                         "type": "DeleteRegion",
                         "user": "acloutie",
                         "processingOrder": "PRE_PROCESSING"
                         }
                         ]
        }
  }')
  
  corrections <- repgen:::parseTSSProcessingCorrections(correctionsJson, "pre", timezone)
  nullCorrs <- repgen:::parseTSSProcessingCorrections(NULL, "pre", timezone)
  nullCorrs2 <- repgen:::parseTSSProcessingCorrections(correctionsJson, "invalid", timezone)
  
  expect_equal(nullCorrs, NULL)
  expect_equal(nullCorrs2, NULL)
  expect_equal(corrections[1,][['startTime']], as.character("2016-07-07 16:15:00"))
  expect_equal(corrections[2,][['startTime']], as.character("2016-08-04 10:30:00"))
  expect_equal(corrections[3,][['startTime']], as.character("2016-09-12 08:30:00"))
  expect_equal(corrections[4,][['startTime']], as.character("2016-11-14 11:30:00"))
  expect_equal(corrections[5,][['startTime']], as.character("2016-12-29 22:30:00"))
  expect_equal(corrections[6,][['startTime']], as.character("2017-03-11 06:00:00"))
  
})

test_that('parseTSSGaps properly sorts the gaps by startTime', {
  timezone <- "Etc/GMT+5"
  gapJson <- fromJSON('{
    "primaryTsData":{
      "gaps": [
        {
        "startTime": "2016-11-23T00:00:00-05:00",
        "endTime": "2016-11-23T12:00:00-05:00"
        },
        {
        "startTime": "2016-11-24T12:00:00-05:00",
        "endTime": "2016-11-25T00:00:00-05:00"
        },
        {
        "startTime": "2016-11-21T12:00:00-05:00",
        "endTime": "2016-11-22T00:00:00-05:00"
        }
      ]
    }
  }')
  
  gaps <- repgen:::parseTSSGaps(gapJson, timezone)
  nullGaps <- repgen:::parseTSSGaps(NULL, timezone)
  
  expect_equal(nullGaps, NULL)
  expect_equal(gaps[1,][['startTime']], as.character("2016-11-21 12:00:00"))
  expect_equal(gaps[2,][['startTime']], as.character("2016-11-23 00:00:00"))
  expect_equal(gaps[3,][['startTime']], as.character("2016-11-24 12:00:00"))
})

test_that('parseTSSApprovals properly sorts the approvals by startTime', {
  timezone <- "Etc/GMT+5"
  approvalsJson <- fromJSON('{
    "primaryTsData": {
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
    }
  }')
  
  approvals <- repgen:::parseTSSApprovals(approvalsJson, timezone)
  expect_equal(approvals[1,][['startTime']], as.character("2007-10-01 00:00:00"))
  expect_equal(approvals[2,][['startTime']], as.character("2016-11-16 00:00:00"))
})

test_that('parseTSSGapTolerances properly sorts the gapTolerances by startTime', {
  timezone <- "Etc/GMT+5"
  gapTolerancesJson <- fromJSON('{
    "primaryTsData":{
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
    }
  }')
  
  gapTolerances <- repgen:::parseTSSGapTolerances(gapTolerancesJson, timezone)
  nullGapTolerances <- repgen:::parseTSSGapTolerances(NULL, timezone)
  
  expect_equal(nullGapTolerances, NULL)
  expect_equal(gapTolerances[1,][['startTime']], as.character("2011-06-01 00:00:00"))
  expect_equal(gapTolerances[2,][['startTime']], as.character("2014-06-01 00:00:00"))
  expect_equal(gapTolerances[3,][['startTime']], as.character("2016-06-01 00:00:00"))
})

test_that('parseTSSPrimaryTsMetadata properly retrieves the primary TS metadata', {
  metadataJson <- fromJSON('{
                           "primaryTsMetadata": {
                           "identifier": "Gage height.ft.(New site WY2011)@01014000",
                           "period": "Points",
                           "utcOffset": -5,
                           "timezone": "Etc/GMT+5",
                           "groundWater": false,
                           "description": "DD019,(New site WY2011),00065,ft,DCP",
                           "timeSeriesType": "ProcessorDerived",
                           "extendedAttributes": {
                           "ACTIVE_FLAG": "Y",
                           "ADAPS_DD": 19,
                           "PLOT_MEAS": "Y",
                           "PRIMARY_FLAG": "Primary",
                           "ACCESS_LEVEL": "0-Public",
                           "DATA_GAP": 72
                           },
                           "computation": "Instantaneous",
                           "unit": "ft",
                           "nwisName": "Gage height",
                           "parameter": "Gage height",
                           "publish": true,
                           "discharge": false,
                           "sublocation": "",
                           "comment": "",
                           "inverted": false,
                           "parameterIdentifier": "Gage height",
                           "uniqueId": "c289a526bea1493bb33ee6e8dd389b92",
                           "nwisPcode": "00065",
                           "primary": true
                           }
  }')
  
  metadata <- repgen:::parseTSSPrimaryTsMetadata(metadataJson)
  nullMetadata <- repgen:::parseTSSPrimaryTsMetadata(NULL)
  
  expect_equal(nullMetadata, NULL)
  expect_equal(metadata[['period']], "Points")
  expect_equal(metadata[['unit']], "ft")
  expect_equal(metadata[['publish']], TRUE)
  expect_equal(metadata[['parameter']], "Gage height")
  expect_is(metadata[['extendedAttributes']], 'list')
})

test_that('parseTSSMethods properly retrieves the primary ts methods', {
  timezone <- "Etc/GMT+5"
  methodsJson <- fromJSON('{
                          "primaryTsData": {
                          "methods": [
                          {
                          "methodCode": "DefaultNone",
                          "startTime": "2016-06-01T00:00:00-05:00",
                          "endTime": "2017-06-22T00:00:00.0000001-05:00"
                          }
                          ]
                          }
  }')
  
  methods <- repgen:::parseTSSMethods(methodsJson, timezone)
  nullMethods <- repgen:::parseTSSMethods(NULL, timezone)
  
  expect_equal(nullMethods, NULL)
  expect_equal(methods[['methodCode']], "DefaultNone")
  expect_equal(methods[['startTime']], as.character(repgen:::flexibleTimeParse("2016-06-01T00:00:00-05:00", timezone)))
  expect_equal(methods[['endTime']], as.character(repgen:::flexibleTimeParse("2017-06-22T00:00:00.0000001-05:00", timezone)))
})

test_that('parseTSSInterpolationTypes properly retrieves the primary ts interpolation types', {
  timezone <- "Etc/GMT+5"
  itsJson <- fromJSON('{
                      "primaryTsData": {
                      "interpolationTypes": [
                      {
                      "type": "InstantaneousValues",
                      "startTime": "2016-06-01T00:00:00-05:00",
                      "endTime": "2017-06-22T00:00:00.0000001-05:00"
                      }
                      ]
                      }
  }')
  
  its <- repgen:::parseTSSInterpolationTypes(itsJson, timezone)
  nullIts <- repgen:::parseTSSInterpolationTypes(NULL, timezone)
  
  expect_equal(nullIts, NULL)
  expect_equal(its[['type']], "InstantaneousValues")
  expect_equal(its[['startTime']], as.character(repgen:::flexibleTimeParse("2016-06-01T00:00:00-05:00", timezone)))
  expect_equal(its[['endTime']], as.character(repgen:::flexibleTimeParse("2017-06-22T00:00:00.0000001-05:00", timezone)))
})

test_that('parseTSSProcessors properly retrieves the primary ts processors', {
  timezone <- "Etc/GMT+5"
  procsJson <- fromJSON('{
                        "primaryTsData": {
                        "processors": [
                        {
                        "processorType": "correctedpassthrough",
                        "periodStartTime": "2016-06-01T00:00:00-05:00",
                        "periodEndTime": "2017-06-22T00:00:00.0000001-05:00"
                        }
                        ]
                        }
  }')
  
  procs <- repgen:::parseTSSProcessors(procsJson, timezone)
  nullProcs <- repgen:::parseTSSProcessors(NULL, timezone)

  expect_equal(nullProcs, NULL)
  expect_equal(procs[['processorType']], "correctedpassthrough")
  expect_equal(procs[['startTime']], as.character(repgen:::flexibleTimeParse("2016-06-01T00:00:00-05:00", timezone)))
  expect_equal(procs[['endTime']], as.character(repgen:::flexibleTimeParse("2017-06-22T00:00:00.0000001-05:00", timezone)))
})

test_that('constructTSDetails properly constructs the two tables for the TSS details section', {
  timezone <- "Etc/GMT+5"
  reportData <- fromJSON('{
      "primaryTsData":{
        "notes": [],
        "methods": [
           {
           "methodCode": "DefaultNone",
           "startTime": "2016-06-01T00:00:00-05:00",
           "endTime": "2017-06-22T00:00:00.0000001-05:00"
           }
         ],
         "approvals": [
           {
           "level": 1100,
           "description": "In Review",
           "comment": "Approval changed to In Review by lflight.",
           "user": "lflight",
           "dateApplied": "2017-03-30T11:50:39.664749Z",
           "startTime": "2016-01-01T00:00:00-05:00",
           "endTime": "2016-10-06T05:15:00-05:00"
           },
           {
           "level": 900,
           "description": "Working",
           "comment": "",
           "user": "admin",
           "dateApplied": "2017-02-02T21:00:53.1287246Z",
           "startTime": "2016-10-06T05:15:00-05:00",
           "endTime": "9999-12-31T23:59:59.9999999Z"
           }
         ],
         "qualifiers": [],
         "grades": [
           {
           "startDate": "2016-06-01T00:00:00-05:00",
           "endDate": "2017-06-22T00:00:00.0000001-05:00",
           "code": "50"
           }
         ],
         "processors": [
           {
           "processorType": "correctedpassthrough",
           "inputTimeSeriesUniqueIds": [
           "bf943ea7d46e4b30853d5ce0dcd90410"
           ],
           "outputTimeSeriesUniqueId": "c289a526bea1493bb33ee6e8dd389b92",
           "periodStartTime": "0001-01-01T00:00:00Z",
           "periodEndTime": "9999-12-31T23:59:59.9999999Z"
           }
         ],
         "gaps": [
           {
           "startTime": "2016-12-01T14:30:00-05:00",
           "endTime": "2016-12-02T08:45:00-05:00",
           "durationInHours": 18.25,
           "gapExtent": "CONTAINED"
           },
           {
           "startTime": "2016-12-02T22:30:00-05:00",
           "endTime": "2016-12-03T12:30:00-05:00",
           "durationInHours": 14,
           "gapExtent": "CONTAINED"
           },
           {
           "startTime": "2017-04-21T21:30:00-05:00",
           "endTime": "2017-04-21T23:45:00-05:00",
           "durationInHours": 2.25,
           "gapExtent": "CONTAINED"
           }
         ],
         "gapTolerances": [
           {
           "startTime": "2016-06-01T00:00:00-05:00",
           "endTime": "2017-06-22T00:00:00.0000001-05:00",
           "toleranceInMinutes": 120
           }
         ],
         "interpolationTypes": [
           {
           "type": "InstantaneousValues",
           "startTime": "2016-06-01T00:00:00-05:00",
           "endTime": "2017-06-22T00:00:00.0000001-05:00"
           },
           {
             "type": "DailyValues",
             "startTime": "2016-06-23T00:00:00-05:00",
             "endTime": "2017-06-25T00:00:00.0000001-05:00"
           }
         ]
      },
      "primaryTsMetadata": {
        "identifier": "Gage height.ft.(New site WY2011)@01014000",
        "period": "Points",
        "utcOffset": -5,
        "timezone": "Etc/GMT+5",
        "groundWater": false,
        "description": "DD019,(New site WY2011),00065,ft,DCP",
        "timeSeriesType": "ProcessorDerived",
        "extendedAttributes": {
            "ACTIVE_FLAG": "Y",
            "ADAPS_DD": 19,
            "PLOT_MEAS": "Y",
            "PRIMARY_FLAG": "Primary",
            "ACCESS_LEVEL": "0-Public",
            "DATA_GAP": 72
        },
        "computation": "Instantaneous",
        "unit": "ft",
        "nwisName": "Gage height",
        "parameter": "Gage height",
        "publish": true,
        "discharge": false,
        "sublocation": "",
        "comment": "",
        "inverted": false,
        "parameterIdentifier": "Gage height",
        "uniqueId": "c289a526bea1493bb33ee6e8dd389b92",
        "nwisPcode": "00065",
        "primary": true
      }
  }')
  
  reportData2 <- fromJSON('{
      "primaryTsData":{
        "notes": [],
        "methods": [],
        "approvals": [
          {
          "level": 1100,
          "description": "In Review",
          "comment": "Approval changed to In Review by lflight.",
          "user": "lflight",
          "dateApplied": "2017-03-30T11:50:39.664749Z",
          "startTime": "2016-01-01T00:00:00-05:00",
          "endTime": "2016-10-06T05:15:00-05:00"
          },
          {
          "level": 900,
          "description": "Working",
          "comment": "",
          "user": "admin",
          "dateApplied": "2017-02-02T21:00:53.1287246Z",
          "startTime": "2016-10-06T05:15:00-05:00",
          "endTime": "9999-12-31T23:59:59.9999999Z"
          }
        ],
        "qualifiers": [],
        "grades": [
          {
          "startDate": "2016-06-01T00:00:00-05:00",
          "endDate": "2017-06-22T00:00:00.0000001-05:00",
          "code": "50"
          }
        ],
        "processors": [],
        "gaps": [
          {
          "startTime": "2016-12-01T14:30:00-05:00",
          "endTime": "2016-12-02T08:45:00-05:00",
          "durationInHours": 18.25,
          "gapExtent": "CONTAINED"
          },
          {
          "startTime": "2016-12-02T22:30:00-05:00",
          "endTime": "2016-12-03T12:30:00-05:00",
          "durationInHours": 14,
          "gapExtent": "CONTAINED"
          },
          {
          "startTime": "2017-04-21T21:30:00-05:00",
          "endTime": "2017-04-21T23:45:00-05:00",
          "durationInHours": 2.25,
          "gapExtent": "CONTAINED"
          }
        ],
        "gapTolerances": [
          {
          "startTime": "2016-06-01T00:00:00-05:00",
          "endTime": "2017-06-22T00:00:00.0000001-05:00",
          "toleranceInMinutes": 120
          }
          ],
        "interpolationTypes": []
      },
      "primaryTsMetadata": {
        "identifier": "Gage height.ft.(New site WY2011)@01014000",
        "period": "Points",
        "utcOffset": -5,
        "timezone": "Etc/GMT+5",
        "groundWater": false,
        "description": "DD019,(New site WY2011),00065,ft,DCP",
        "timeSeriesType": "ProcessorDerived",
        "extendedAttributes": {},
        "computation": "Instantaneous",
        "unit": "ft",
        "nwisName": "Gage height",
        "parameter": "Gage height",
        "publish": true,
        "discharge": false,
        "sublocation": "",
        "comment": "",
        "inverted": false,
        "parameterIdentifier": "Gage height",
        "uniqueId": "c289a526bea1493bb33ee6e8dd389b92",
        "nwisPcode": "00065",
        "primary": true
      }
  }')
  
  tsDetails <- repgen:::constructTSDetails(reportData, timezone)
  tsDetails2 <- repgen:::constructTSDetails(reportData2, timezone)
  
  expect_is(tsDetails[['tsAttrs']], 'data.frame')
  expect_is(tsDetails[['tsExtAttrs']], 'data.frame')
  expect_equal(length(tsDetails), 3)
  expect_equal(tsDetails[['changeNote']], TRUE)
  expect_equal(nrow(tsDetails[['tsAttrs']]), 16)
  expect_equal(nrow(tsDetails[['tsExtAttrs']]), 10)
  
  expect_is(tsDetails2[['tsAttrs']], 'data.frame')
  expect_is(tsDetails2[['tsExtAttrs']], 'data.frame')
  expect_equal(length(tsDetails2), 3)
  expect_equal(tsDetails2[['changeNote']], FALSE)
  expect_equal(nrow(tsDetails2[['tsAttrs']]), 13)
  expect_equal(nrow(tsDetails2[['tsExtAttrs']]), 10)
  
  row_mm <- which(tsDetails2[['tsAttrs']][['label']] == "Measurement Method")
  expect_equal(tsDetails2[['tsAttrs']][row_mm,][['value']], "")
  row_pt <- which(tsDetails2[['tsAttrs']][['label']] == "Processing Type")
  expect_equal(tsDetails2[['tsAttrs']][row_pt,][['value']], "")
  row_mst <- which(tsDetails2[['tsAttrs']][['label']] == "Method Start Time")
  expect_equal(row_mst, integer(0))
  row_pst <- which(tsDetails2[['tsAttrs']][['label']] == "Period Start Time")
  expect_equal(row_pst, integer(0))
  row_pet <- which(tsDetails2[['tsAttrs']][['label']] == "Period End Time")
  expect_equal(row_pet, integer(0))
  
  expect_equal(tsDetails[['tsAttrs']][1,][['label']], "Label")
  expect_equal(tsDetails[['tsAttrs']][1,][['value']], "Gage height.ft.(New site WY2011)@01014000")
  expect_equal(tsDetails[['tsAttrs']][1,][['indent']], 8)
  expect_equal(tsDetails[['tsAttrs']][6,][['label']], "UTC Offset")
  expect_equal(tsDetails[['tsAttrs']][6,][['value']], "-5")
  expect_equal(tsDetails[['tsAttrs']][6,][['indent']], 8)
  expect_equal(tsDetails[['tsAttrs']][15,][['label']], "Period Start Time")
  expect_equal(tsDetails[['tsAttrs']][15,][['value']], "Open")
  expect_equal(tsDetails[['tsAttrs']][15,][['indent']], 26)
  expect_equal(tsDetails[['tsExtAttrs']][7,][['label']], "ADAPS DD")
  expect_equal(tsDetails[['tsExtAttrs']][7,][['value']], "19")
})

test_that('unNestCorrectionParameters correctly unnests parameters data ', {
  timezone <- "Etc/GMT+5"
  correctionsJson <- fromJSON('{
  "corrections": {
      "normal": [
        {
          "appliedTimeUtc": "2017-06-14T13:20:14.7678867Z",
          "comment": "Offset Correction with value of 0.300ft",
          "startTime": "2017-01-04T05:15:00-05:00",
          "endTime": "2017-01-06T23:00:00.0000001-05:00",
          "type": "Offset",
          "parameters": {
            "Offset": 0.3
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:22:33.7717601Z",
          "comment": "USGS multi-point Start point: (0.000ft, 0.000ft) End point: (0.000ft, 0.500ft)",
          "startTime": "2017-01-07T20:45:00-05:00",
          "endTime": "2017-01-10T09:00:00.0000001-05:00",
          "type": "USGSMultiPoint",
          "parameters": {
            "StartShiftPoints": [
              {
                "Value": 0,
                "Offset": 0
              }
              ],
            "EndShiftPoints": [
              {
                "Value": 0,
                "Offset": 0.5
              }
              ],
            "UsgsType": "Set 2"
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:38:09.4589634Z",
          "comment": "Revert to Raw data",
          "startTime": "2017-02-11T03:30:00-05:00",
          "endTime": "2017-02-11T13:00:00.0000001-05:00",
          "type": "RevertToRaw",
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:29:47.5967321Z",
          "comment": "Delete region",
          "startTime": "2017-02-05T22:00:00-05:00",
          "endTime": "2017-02-07T14:30:00.0000001-05:00",
          "type": "DeleteRegion",
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:29:56.5328236Z",
          "comment": "Freehand Correction without generating points.",
          "startTime": "2017-02-08T06:30:00-05:00",
          "endTime": "2017-02-09T15:00:00.0000001-05:00",
          "type": "Freehand",
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:29:35.8450932Z",
          "comment": "Copy and Paste from Gage height.ft.EDL@01069500",
          "startTime": "2017-02-03T13:15:00-05:00",
          "endTime": "2017-02-05T12:30:00.0000001-05:00",
          "type": "CopyPaste",
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:39:16.8024489Z",
          "comment": "Fill Data Gaps with gap resample period of 30 minutes",
          "startTime": "2017-02-10T06:45:00-05:00",
          "endTime": "2017-02-11T00:15:00.0000001-05:00",
          "type": "FillGaps",
          "parameters": {
            "ResamplePeriod": "PT30M",
            "GapLimit": "MaxDuration"
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:19:48.7283456Z",
          "comment": "Drift Correction of (Date/Time, Diff): (2017-01-01 01:00:00, 0.000ft), (2017-01-03 17:30:00, 0.200ft)",
          "startTime": "2017-01-01T01:00:00-05:00",
          "endTime": "2017-01-03T17:30:00.0000001-05:00",
          "type": "Drift",
          "parameters": {
            "DriftPoints": [
              {
                "Offset": 0,
                "Time": "2017-01-01T06:00:00Z"
              },
              {
                "Offset": 0.2,
                "Time": "2017-01-03T22:30:00Z"
              }
              ]
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:38:54.8184605Z",
          "comment": "Adjustable trim with Upper threshold: (2017-02-10 09:15:00, 1.273ft), (2017-02-10 23:30:00, 1.273ft)",
          "startTime": "2017-02-10T09:15:00-05:00",
          "endTime": "2017-02-10T23:30:00.0000001-05:00",
          "type": "AdjustableTrim",
          "parameters": {
            "UpperThresholdPoints": [
              {
                "Time": "2017-02-10T14:15:00Z",
                "Value": 1.2727854725856091
              },
              {
                "Time": "2017-02-11T04:30:00Z",
                "Value": 1.2727854725856091
              }
              ]
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:29:19.2290281Z",
          "comment": "Adjustable trim with Upper threshold: (2017-02-01 18:15:00, 1.420ft), (2017-02-04 02:45:00, 1.420ft)",
          "startTime": "2017-02-01T18:15:00-05:00",
          "endTime": "2017-02-04T02:45:00.0000001-05:00",
          "type": "AdjustableTrim",
          "parameters": {
            "UpperThresholdPoints": [
              {
                "Time": "2017-02-01T23:15:00Z",
                "Value": 1.4203813559322032
              },
              {
                "Time": "2017-02-04T07:45:00Z",
                "Value": 1.4203813559322032
              }
              ]
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:20:36.1443457Z",
          "comment": "Single point correction. Changed from 1.46 to 1.99",
          "startTime": "2017-01-07T09:00:00-05:00",
          "endTime": "2017-01-07T09:00:00.0000001-05:00",
          "type": "SinglePoint",
          "parameters": {
            "Value": 1.99
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2017-06-14T13:31:17.479716Z",
          "comment": "Outlier trim",
          "startTime": "2017-02-10T08:00:00-05:00",
          "endTime": "2017-02-10T23:30:00.0000001-05:00",
          "type": "Deviation",
          "parameters": {
            "DeviationValue": 0.01,
            "DeviationType": "DeviationFromMinimum",
            "WindowSizeInMinutes": 15
          },
          "user": "lflight",
          "processingOrder": "NORMAL"
        }
        ],
      "postProcessing": [],
      "preProcessing": [],
      "corrUrl": {
        "urlReportType": "correctionsataglance",
        "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
      }
    }
  }')
  corrections <- repgen:::parseTSSProcessingCorrections(correctionsJson, "normal", timezone)
  expect_equal(length(corrections),21)
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","Offset","StartShiftPoints","EndShiftPoints","UsgsType","ResamplePeriod","GapLimit","DriftPoints","UpperThresholdPoints","Value","DeviationValue","DeviationType","WindowSizeInMinutes","timezone","formattedParameters"))
  
})

test_that('formatCorrectionsParamOffset correctly formats offset parameters data ', {
  timezone <- "Etc/GMT+5"
  offsetJson <- fromJSON('{
  "corrections": {
                       "normal": [
                       {
                       "appliedTimeUtc": "2017-06-14T13:20:14.7678867Z",
                       "comment": "Offset Correction with value of 0.300ft",
                       "startTime": "2017-01-04T05:15:00-05:00",
                       "endTime": "2017-01-06T23:00:00.0000001-05:00",
                       "type": "Offset",
                       "parameters": {
                          "Offset": 0.3
                       },
                       "user": "lflight",
                       "processingOrder": "NORMAL"
                       }
                       ],
                       "postProcessing": [],
                       "preProcessing": [],
                       "corrUrl": {
                       "urlReportType": "correctionsataglance",
                       "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                       }
                       }
                       }')
  corrections <- repgen:::parseTSSProcessingCorrections(offsetJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "Offset 0.3")
  expect_equal(names(corrections), c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","Offset","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamDrift correctly formats drift parameters data ', {
  timezone <- "Etc/GMT+5"
  driftJson <- fromJSON('{
  "corrections": {
                       "normal": [
                         {
                        "appliedTimeUtc": "2017-06-14T13:19:48.7283456Z",
                         "comment": "Drift Correction of (Date/Time, Diff): (2017-01-01 01:00:00, 0.000ft), (2017-01-03 17:30:00, 0.200ft)",
                         "startTime": "2017-01-01T01:00:00-05:00",
                         "endTime": "2017-01-03T17:30:00.0000001-05:00",
                         "type": "Drift",
                         "parameters": {
                         "DriftPoints": [
                         {
                         "Offset": 0,
                         "Time": "2017-01-01T06:00:00Z"
                         },
                         {
                         "Offset": 0.2,
                         "Time": "2017-01-03T22:30:00Z"
                         }
                         ]
                         },
                       "user": "lflight",
                       "processingOrder": "NORMAL"
                       }
                       ],
                       "postProcessing": [],
                       "preProcessing": [],
                       "corrUrl": {
                       "urlReportType": "correctionsataglance",
                       "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                       }
                       }
                       }')
  corrections <- repgen:::parseTSSProcessingCorrections(driftJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "Drift correction of (date/time, diff): (2017-01-01 01:00:00, 0ft), (2017-01-03 17:30:00, 0.2ft)")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","DriftPoints","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamSinglePoint correctly formats single point parameters data ', {
  timezone <- "Etc/GMT+5"
  singlePointJson <- fromJSON('{
                        "corrections": {
                        "normal": [
                              {
                              "appliedTimeUtc": "2017-06-14T13:20:36.1443457Z",
                              "comment": "Single point correction. Changed from 1.46 to 1.99",
                              "startTime": "2017-01-07T09:00:00-05:00",
                              "endTime": "2017-01-07T09:00:00.0000001-05:00",
                              "type": "SinglePoint",
                              "parameters": {
                              "Value": 1.99
                              },
                              "user": "lflight",
                              "processingOrder": "NORMAL"
                        }
                        ],
                        "postProcessing": [],
                        "preProcessing": [],
                        "corrUrl": {
                        "urlReportType": "correctionsataglance",
                        "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                        }
                        }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(singlePointJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "SinglePoint 1.99")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","Value","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamUSGSMultiPoint correctly formats USGS Multi Point parameters data ', {
  timezone <- "Etc/GMT+5"
  USGSMultiPointJson <- fromJSON('{
                              "corrections": {
                              "normal": [
                              {
                              "appliedTimeUtc": "2017-06-14T13:22:33.7717601Z",
                              "comment": "USGS multi-point Start point: (0.000ft, 0.000ft) End point: (0.000ft, 0.500ft)",
                              "startTime": "2017-01-07T20:45:00-05:00",
                              "endTime": "2017-01-10T09:00:00.0000001-05:00",
                              "type": "USGSMultiPoint",
                              "parameters": {
                              "StartShiftPoints": [
                              {
                              "Value": 0,
                              "Offset": 0
                              }
                              ],
                              "EndShiftPoints": [
                              {
                              "Value": 0,
                              "Offset": 0.5
                              }
                              ],
                              "UsgsType": "Set 2"
                              }
                              }
                              ],
                              "postProcessing": [],
                              "preProcessing": [],
                              "corrUrl": {
                              "urlReportType": "correctionsataglance",
                              "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                              }
                              }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(USGSMultiPointJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "USGSMultiPoint Start shift points value 0, offset 0. End shift points value 0, offset 0. Set 2")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","StartShiftPoints","EndShiftPoints","UsgsType","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamAdjustableTrim correctly formats adjustable trim parameters data ', {
  timezone <- "Etc/GMT+5"
  adjustableTrimJson <- fromJSON('{
                                 "corrections": {
                                 "normal": [
                                 {
          "appliedTimeUtc": "2017-06-14T13:38:54.8184605Z",
                                 "comment": "Adjustable trim with Upper threshold: (2017-02-10 09:15:00, 1.273ft), (2017-02-10 23:30:00, 1.273ft)",
                                 "startTime": "2017-02-10T09:15:00-05:00",
                                 "endTime": "2017-02-10T23:30:00.0000001-05:00",
                                 "type": "AdjustableTrim",
                                 "parameters": {
                                 "UpperThresholdPoints": [
                                 {
                                 "Time": "2017-02-10T14:15:00Z",
                                 "Value": 1.2727854725856091
                                 },
                                 {
                                 "Time": "2017-02-11T04:30:00Z",
                                 "Value": 1.2727854725856091
                                 }
                                 ]
                                 }
                                  },
                                {
                                  "appliedTimeUtc": "2017-06-01T17:30:39.1830491Z",
                                  "comment": "Delete eronious data",
                                  "startTime": "2017-05-25T16:45:00-07:00",
                                  "endTime": "2017-06-01T00:15:00.0000001-07:00",
                                  "type": "AdjustableTrim",
                                  "parameters": {
                                  "LowerThresholdPoints": [
                                  {
                                  "Value": 8.238427419354839,
                                  "Time": "2017-05-25T23:45:00Z"
                                  },
                                  {
                                  "Value": 8.639435483870969,
                                  "Time": "2017-05-26T04:30:00Z"
                                  },
                                  {
                                  "Value": 8.133548387096775,
                                  "Time": "2017-05-26T12:00:00Z"
                                  },
                                  {
                                  "Value": 8.670282258064518,
                                  "Time": "2017-05-26T21:15:00Z"
                                  },
                                  {
                                  "Value": 8.127379032258066,
                                  "Time": "2017-05-27T12:00:00Z"
                                  },
                                  {
                                  "Value": 8.016330645161291,
                                  "Time": "2017-05-30T06:45:00Z"
                                  },
                                  {
                                  "Value": 7.584475806451614,
                                  "Time": "2017-05-30T12:00:00Z"
                                  },
                                  {
                                  "Value": 7.491935483870968,
                                  "Time": "2017-05-31T14:15:00Z"
                                  },
                                  {
                                  "Value": 8.102701612903227,
                                  "Time": "2017-05-31T19:45:00Z"
                                  },
                                  {
                                  "Value": 7.84975806451613,
                                  "Time": "2017-06-01T07:15:00Z"
                                  }
                                  ]
                                  } 
                                  }
                                 ],
                                 "postProcessing": [],
                                 "preProcessing": [],
                                 "corrUrl": {
                                 "urlReportType": "correctionsataglance",
                                 "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                                 }
                                 }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(adjustableTrimJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']][[1]], "Adjustable trim Upper threshold: 2017-02-10 09:15:00, 1.273ft. Upper threshold: 2017-02-10 23:30:00, 1.273ft. ")
  expect_equal(corrections[['formattedParameters']][[2]], "Adjustable trim Lower threshold: 2017-05-25 18:45:00, 8.238ft. Lower threshold: 2017-05-25 23:30:00, 8.639ft. Lower threshold: 2017-05-26 07:00:00, 8.134ft. Lower threshold: 2017-05-26 16:15:00, 8.67ft. Lower threshold: 2017-05-27 07:00:00, 8.127ft. Lower threshold: 2017-05-30 01:45:00, 8.016ft. Lower threshold: 2017-05-30 07:00:00, 7.584ft. Lower threshold: 2017-05-31 09:15:00, 7.492ft. Lower threshold: 2017-05-31 14:45:00, 8.103ft. Lower threshold: 2017-06-01 02:15:00, 7.85ft. ")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","UpperThresholdPoints","LowerThresholdPoints","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamFillGaps correctly formats fill gaps parameters data ', {
  timezone <- "Etc/GMT+5"
  fillGapsJson <- fromJSON('{
                                 "corrections": {
                                 "normal": [
                           {
                          "appliedTimeUtc": "2017-06-14T13:39:16.8024489Z",
                           "comment": "Fill Data Gaps with gap resample period of 30 minutes",
                           "startTime": "2017-02-10T06:45:00-05:00",
                           "endTime": "2017-02-11T00:15:00.0000001-05:00",
                           "type": "FillGaps",
                           "parameters": {
                           "ResamplePeriod": "PT30M",
                           "GapLimit": "MaxDuration"
                           },
                           "user": "lflight",
                           "processingOrder": "NORMAL"
                            }
                                 ],
                                 "postProcessing": [],
                                 "preProcessing": [],
                                 "corrUrl": {
                                 "urlReportType": "correctionsataglance",
                                 "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                                 }
                                 }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(fillGapsJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "Resample Period PT30M; Gap Limit MaxDuration")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","ResamplePeriod","GapLimit","timezone","formattedParameters"))
})

test_that('formatCorrectionsParamDeviation correctly formats deviation parameters data ', {
  timezone <- "Etc/GMT+5"
  deviationJson <- fromJSON('{
                           "corrections": {
                           "normal": [
                            {
          "appliedTimeUtc": "2017-06-14T13:31:17.479716Z",
                            "comment": "Outlier trim",
                            "startTime": "2017-02-10T08:00:00-05:00",
                            "endTime": "2017-02-10T23:30:00.0000001-05:00",
                            "type": "Deviation",
                            "parameters": {
                            "DeviationValue": 0.01,
                            "DeviationType": "DeviationFromMinimum",
                            "WindowSizeInMinutes": 15
                            },
                            "user": "lflight",
                            "processingOrder": "NORMAL"
                            }
                           ],
                           "postProcessing": [],
                           "preProcessing": [],
                           "corrUrl": {
                           "urlReportType": "correctionsataglance",
                           "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                           }
                           }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(deviationJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']], "Deviation type DeviationFromMinimum; value: 0.01, window size 15 minutes")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","DeviationValue","DeviationType","WindowSizeInMinutes","timezone","formattedParameters"))
})

test_that('unNestCorrectionParameters correctly formats empty parameters data ', {
  timezone <- "Etc/GMT+5"
  emptyParamJson <- fromJSON('{
                            "corrections": {
                            "normal": [
                            {
          "appliedTimeUtc": "2017-06-14T13:38:09.4589634Z",
                            "comment": "Revert to Raw data",
                            "startTime": "2017-02-11T03:30:00-05:00",
                            "endTime": "2017-02-11T13:00:00.0000001-05:00",
                            "type": "RevertToRaw",
                            "user": "lflight",
                            "processingOrder": "NORMAL"
},
                            {
                            "appliedTimeUtc": "2017-06-14T13:29:47.5967321Z",
                            "comment": "Delete region",
                            "startTime": "2017-02-05T22:00:00-05:00",
                            "endTime": "2017-02-07T14:30:00.0000001-05:00",
                            "type": "DeleteRegion",
                            "user": "lflight",
                            "processingOrder": "NORMAL"
                            },
                            {
                            "appliedTimeUtc": "2017-06-14T13:29:56.5328236Z",
                            "comment": "Freehand Correction without generating points.",
                            "startTime": "2017-02-08T06:30:00-05:00",
                            "endTime": "2017-02-09T15:00:00.0000001-05:00",
                            "type": "Freehand",
                            "user": "lflight",
                            "processingOrder": "NORMAL"
                            },
                            {
                            "appliedTimeUtc": "2017-06-14T13:29:35.8450932Z",
                            "comment": "Copy and Paste from Gage height.ft.EDL@01069500",
                            "startTime": "2017-02-03T13:15:00-05:00",
                            "endTime": "2017-02-05T12:30:00.0000001-05:00",
                            "type": "CopyPaste",
                            "user": "lflight",
                            "processingOrder": "NORMAL"
                            }
                            ],
                            "postProcessing": [],
                            "preProcessing": [],
                            "corrUrl": {
                            "urlReportType": "correctionsataglance",
                            "url": "https://cida-eros-aqcudev.er.usgs.gov:8444/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-02-28Z&station=01069500&startDate=2017-01-01Z&primaryTimeseriesIdentifier=efb88ed6b5dc41dcaa33931cd6c144a2"
                            }
                            }
}')
  corrections <- repgen:::parseTSSProcessingCorrections(emptyParamJson, "normal", timezone)
  expect_equal(corrections[['formattedParameters']][1], "CopyPaste")
  expect_equal(corrections[['formattedParameters']][2], "DeleteRegion")
  expect_equal(corrections[['formattedParameters']][3], "Freehand")
  expect_equal(corrections[['formattedParameters']][4], "RevertToRaw")
  expect_equal(names(corrections),c("appliedTimeUtc","comment","startTime","endTime","type","user","processingOrder","timezone","formattedParameters"))
  })

setwd(dir = wd)

