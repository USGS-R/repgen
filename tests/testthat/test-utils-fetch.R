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
  
  reportObject <- fromJSON('{ "maximumStageHeight" : [{ "value" : 1 }] }')
  expect_equal(repgen:::fetchMaxStage(reportObject), 1)
})

test_that('fetchMinStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(repgen:::fetchMinStage(empty), numeric(0))
  expect_is(repgen:::fetchMinStage(empty), 'numeric')
  
  reportObject <- fromJSON('{ "minimumStageHeight" : [{ "value" : 1 }] }')
  expect_equal(repgen:::fetchMinStage(reportObject), 1)
})

test_that('fetchReportMetadataField return values and empty string if not found', {
  library(jsonlite)
  
  reportObject <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  val1 <- repgen:::fetchReportMetadataField(reportObject, "field1")
  val2 <- repgen:::fetchReportMetadataField(reportObject, "field2")
  val3 <- repgen:::fetchReportMetadataField(reportObject, "field3")
  
  expect_is(val1, 'character')
  expect_is(val2, 'character')
  expect_is(val3, 'NULL')
  
  expect_equal(val1, "value1")
  expect_equal(val2, "value2")
  expect_equal(val3, NULL)
})

test_that('fetchReportMetadata returns all of the report metadata', {
  library(jsonlite)
  
  reportObject <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  metadata <- repgen:::fetchReportMetadata(reportObject)
  
  expect_is(metadata$field1, 'character')
  expect_is(metadata$field2, 'character')
  expect_is(metadata$field3, 'NULL')
  
  expect_equal(metadata$field1, "value1")
  expect_equal(metadata$field2, "value2")
  expect_equal(metadata$field3, NULL)
})

test_that('fetchTimeSeries returns all of the data for the specified series name', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-time-series.json', package = 'repgen'))
  
  validSeries <- repgen:::fetchTimeSeries(reportObject, "testSeries1")
  invalidSeries <- repgen:::fetchTimeSeries(reportObject, "testSeries2")

  expect_is(validSeries$startTime, 'character')
  expect_is(validSeries$endTime, 'character')
  expect_is(invalidSeries$startTime, 'NULL')
  expect_is(invalidSeries$endTime, 'character')

  expect_equal(validSeries$startTime, '2014-11-19')
  expect_equal(invalidSeries$startTime, NULL)
  expect_equal(validSeries$endTime, '2015-11-20')
  expect_equal(invalidSeries$endTime, '')

})

test_that('fetchGroundWaterLevels returns the full ground water level data', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "gwlevel": [
        {
          "siteNumber": "12345",
          "groundWaterLevel": 2,
          "recordDateTime": "2015-07-16T01:00:00-06:00",
          "timeZone": "EDT"
        },
        {
          "siteNumber": "12345",
          "groundWaterLevel": 3,
          "recordDateTime": "2015-07-16T02:00:00-06:00",
          "timeZone": "EDT"
        }
      ]
  }')

  gwData <- repgen:::fetchGroundWaterLevels(reportObject)

  expect_is(gwData, 'data.frame')
  expect_is(gwData$siteNumber[[1]], 'character')
  expect_is(gwData$recordDateTime[[length(gwData$recordDateTime)]], 'character')

  expect_equal(nrow(gwData), 2)
  expect_equal(gwData$siteNumber[[1]], '12345')
  expect_equal(gwData$recordDateTime[[length(gwData$recordDateTime)]], '2015-07-16T02:00:00-06:00')
})

test_that('fetchWaterQualityMeasurements returns the full set of water quality measurements data', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "waterQuality": [
        {
          "recordNumber": "01501684",
          "medium": "Surface water",
          "sampleStartDateTime": "2015-07-15T10:50:00-06:00",
          "value": {
            "parameter": "00300",
            "remark": "",
            "value": 5.3
          },
          "timeZone": "CST"
        },
        {
          "recordNumber": "01501779",
          "medium": "Surface water",
          "sampleStartDateTime": "2015-07-29T13:30:00-06:00",
          "value": {
            "parameter": "00300",
            "remark": "",
            "value": 4.0
          },
          "timeZone": "CST"
        }
      ]
  }')

  wqData <- repgen:::fetchWaterQualityMeasurements(reportObject)

  expect_is(wqData, 'data.frame')
  expect_is(wqData$value, 'data.frame')
  expect_is(wqData$recordNumber[[1]], 'character')
  expect_is(wqData$value$value[[1]], 'numeric')
  expect_is(wqData$sampleStartDateTime[[length(wqData$sampleStartDateTime)]], 'character')

  expect_equal(nrow(wqData), 2)
  expect_equal(wqData$recordNumber[[1]], '01501684')
  expect_equal(wqData$sampleStartDateTime[[length(wqData$sampleStartDateTime)]], '2015-07-29T13:30:00-06:00')
  expect_equal(wqData$value$value[[1]], 5.3)
})

test_that('fetchFieldVisitMeasurements returns the full set of field visit measurements data', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "fieldVisitMeasurements": [
        {
          "identifier": "3BBE3CC218E603BAE0530100007FE773",
          "controlCondition": "CLEAR",
          "measurementStartDate": "2015-07-07T15:35:59-05:00",
          "discharge": 4600,
          "dischargeUnits": "ft^3/s",
          "errorMinDischarge": 4140.000,
          "errorMaxDischarge": 5060.000,
          "measurementNumber": "651",
          "qualityRating": "POOR",
          "historic": false,
          "meanGageHeight": 4.91,
          "meanGageHeightUnits": "ft",
          "shiftNumber": 0
        }
      ]
  }')

  fvData <- repgen:::fetchFieldVisitMeasurements(reportObject)

  expect_is(fvData, 'data.frame')
  expect_is(fvData$identifier[[1]], 'character')
  expect_is(fvData$discharge[[1]], 'integer')
  expect_is(fvData$measurementStartDate[[length(fvData$measurementStartDate)]], 'character')

  expect_equal(nrow(fvData), 1)
  expect_equal(fvData$identifier[[1]], '3BBE3CC218E603BAE0530100007FE773')
  expect_equal(fvData$measurementStartDate[[length(fvData$measurementStartDate)]], '2015-07-07T15:35:59-05:00')
  expect_equal(fvData$discharge[[1]], 4600)
})

test_that('fetchFieldVisitReadings returns the full set of field visit readings data', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
      "readings": [
        {
          "time": "2015-08-07T09:26:00.000-05:00",
          "comments": [
            "Comment \u003d Reset to 2.42 after inspection",
            "Comment \u003d This is another comment",
            "Comment \u003d This is another comment that // a break in it"
            ],
          "fieldVisitIdentifier": "1FCDFDC32416F7C4E05322EB3D985BC8",
          "visitStatus": "TODO",
          "party": "CR",
          "monitoringMethod": "Max-min indicator",
          "value": "21.72",
          "parameter": "Gage height",
          "type": "2015-04-03T09:41:00.000-05:00",
          "startTime": "2015-04-03T09:41:00.000-05:00",
          "associatedIvTime": "2015-06-22T00:00:00.000-05:00",
          "associatedIvValue": "21.75",
          "minTime": "2015-05-08T07:15:00.000-05:00",
          "minValue": "2.05",
          "associatedIvQualifiers": [
            {
              "startDate": "2015-06-26T05:00:00.000-05:00",
              "endDate": "2015-08-26T11:00:00.000-05:00",
              "identifier": "EQUIP",
              "code": "EQP",
              "appliedBy": "gwilson",
              "displayName": "Equpment Malfunction",
              "dateApplied": "2015-09-15T06:45:46.130-05:00"
            },
            {
              "startDate": "2015-07-05T09:30:00.000-05:00",
              "endDate": "2015-07-06T15:30:00.000-05:00",
              "identifier": "EQUIP",
              "code": "EQP",
              "appliedBy": "gwilson",
              "displayName": "Equpment Malfunction",
              "dateApplied": "2015-09-15T12:57:22.423-05:00"
            }
        ]
      }
    ]
  }')
  
  fvData <- repgen:::fetchFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_is(fvData$comments, 'list')
  expect_is(fvData$fieldVisitIdentifier, 'character')
  expect_is(fvData$value, 'character')
  expect_is(fvData$time[[length(fvData$time)]], 'character')
  expect_is(fvData$associatedIvQualifiers, 'list')
  expect_is(fvData$associatedIvQualifiers[[1]], 'data.frame')
  
  expect_equal(nrow(fvData), 1)
  expect_equal(fvData$fieldVisitIdentifier, '1FCDFDC32416F7C4E05322EB3D985BC8')
  expect_equal(fvData$time[[length(fvData$time)]], '2015-08-07T09:26:00.000-05:00')
  expect_equal(fvData$value, "21.72")
  expect_equal(fvData$associatedIvQualifiers[[1]]$dateApplied[[1]], "2015-09-15T06:45:46.130-05:00")
  expect_equal(fvData$associatedIvQualifiers[[1]]$dateApplied[[2]], "2015-09-15T12:57:22.423-05:00")
})

test_that('fetchFieldVisitReadings returns multiple readings', {
  reportObject <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  fvData <- repgen:::fetchFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==4)
})

test_that('fetchCorrections returns the full set of corrections data for the specified time series', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "primarySeriesCorrections": [
        {
          "appliedTimeUtc": "2012-02-29T19:18:25Z",
          "startTime": "2011-02-29T10:17:00-05:00",
          "endTime": "2011-09-30T22:59:00-05:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "PRE_PROCESSING"
        },
        {
          "appliedTimeUtc": "2012-02-29T19:18:25Z",
          "startTime": "2012-02-29T10:17:00-05:00",
          "endTime": "2012-09-30T22:59:00-05:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "comment": "test comment",
          "processingOrder": "PRE_PROCESSING"
        }
      ]
  }')

  corrData <- repgen:::fetchCorrections(reportObject, "primarySeriesCorrections")

  expect_is(corrData, 'data.frame')
  expect_is(corrData$startTime[[1]], 'character')
  expect_is(corrData$comment[[2]], 'character')

  expect_equal(nrow(corrData), 2)
  expect_equal(corrData$comment[[2]], 'test comment')
  expect_equal(corrData$startTime[[1]], '2011-02-29T10:17:00-05:00')
})

test_that("fetchMinMaxIVs properly retrieves the min/max IV values", {
  IVs <- fromJSON('{
    "maxMinData": {
      "seriesTimeSeriesPoints": {
        "DataRetrievalRequest-dc10355d-daf8-4aa9-8d8b-c8ab69c16f99": {
          "startTime": "2013-11-10T00:00:00-05:00",
          "endTime": "2013-12-11T23:59:59.999999999-05:00",
          "qualifiers": [],
          "theseTimeSeriesPoints": {
            "MAX": [
              {
                "time": "2013-11-18T12:00:00-05:00",
                "value": 892
              }
            ],
            "MIN": [
              {
                "time": "2013-11-12T22:45:00-05:00",
                "value": 60.5
              }
            ]
          }
        }
      }
    }
  }')

  max_iv <- repgen:::fetchMinMaxIVs(IVs, "MAX")
  min_iv <- repgen:::fetchMinMaxIVs(IVs, "MIN")

  expect_is(max_iv, 'data.frame')
  expect_is(min_iv, 'data.frame')

  expect_equal(max_iv$value, 892)
  expect_equal(min_iv$value, 60.5)

  expect_equal(max_iv$time, "2013-11-18T12:00:00-05:00")
  expect_equal(min_iv$time, "2013-11-12T22:45:00-05:00")
})

test_that("fetchPrimarySeriesApprovals properly retrieves the primary series approvals", {
  approvals <- fromJSON('{
    "primarySeriesApprovals": [
        {
            "level": 0,
            "description": "Working",
            "comment": "",
            "dateApplied": "2016-11-27T17:25:46.7400821Z",
            "startTime": "2015-10-01T00:00:00-06:00",
            "endTime": "9999-12-31T23:59:59.9999999Z"
        }
    ]
  }')

  primary <- repgen:::fetchPrimarySeriesApprovals(approvals)

  expect_is(approvals, 'list')
  expect_equal(approvals[[1]][['level']], 0)
  expect_equal(approvals[[1]][['description']], 'Working')
})

test_that("fetchPrimarySeriesQualifiers properly retrieves the primary series qualifiers", {
  approvals <- fromJSON('{
    "primarySeriesQualifiers": [
        {
            "startDate": "2016-12-01T00:00:00-05:00",
            "endDate": "2017-01-10T00:00:00.0000001-05:00",
            "identifier": "ESTIMATED",
            "code": "E",
            "displayName": "Flow at station affected by ice",
            "appliedBy": "admin",
            "dateApplied": "2016-12-10T11:16:12Z"
        }
    ]
  }')
  
  primary <- repgen:::fetchPrimarySeriesQualifiers(approvals)
  
  expect_is(approvals, 'list')
  expect_equal(approvals[[1]][['code']], "E")
  expect_equal(approvals[[1]][['displayName']], "Flow at station affected by ice")
})


test_that("fetchFieldVists properly retrieves the field vist data", {
  fieldVisits <- fromJSON('{
    "fieldVisits": [
      {
        "locationIdentifier": "06892350",
        "startTime": "2015-01-06T15:00:00-06:00",
        "endTime": "2015-01-06T15:30:00-06:00",
        "identifier": "2DAF1E50CE2228A5E0530100007F57D2",
        "isValid": true,
        "lastModified": "2016-03-10T03:07:43.820683-06:00",
        "party": "MDM LRG",
        "remarks": "Removed EXO and Nitratax to prevent damage from ice. Unable to remove the equipment from the pipe, so left it hanging from bridge, not in stream.",
        "weather": "COLD, ice."
      }
    ]
  }')

  fieldVisitData <- repgen:::fetchFieldVists(fieldVisits)

  expect_is(fieldVisitData, 'data.frame')
  expect_equal(fieldVisitData[1,][['startTime']], "2015-01-06T15:00:00-06:00")
  expect_equal(fieldVisitData[1,][['isValid']], TRUE)
})

test_that("fetchProcessingCorrections properly retrieves the processing correction data", {
  corrJSON <- fromJSON('{
    "corrections": {
      "normal": [
        {
          "appliedTimeUtc": "2015-12-08T15:32:33Z",
          "comment": "Sensor calibrated.",
          "startTime": "2015-11-09T14:15:00-06:00",
          "endTime": "2015-11-09T14:20:00-06:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "NORMAL"
        }
      ],
      "postProcessing": [
        {
          "appliedTimeUtc": "2016-03-09T21:27:53.2181786Z",
          "comment": "Approval period copy paste from Ref",
          "startTime": "2014-12-10T00:00:00-06:00",
          "endTime": "2015-01-29T00:00:00-06:00",
          "type": "COPY_PASTE",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "POST_PROCESSING"
        }
      ],
      "preProcessing": [
        {
          "appliedTimeUtc": "2015-06-10T18:08:11Z",
          "startTime": "2015-03-30T11:00:00-06:00",
          "endTime": "2015-05-08T10:15:00-06:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "PRE_PROCESSING"
        }
      ]
    }
  }')

  preData <- repgen:::fetchProcessingCorrections(corrJSON, "pre")
  normalData <- repgen:::fetchProcessingCorrections(corrJSON, "normal")
  postData <- repgen:::fetchProcessingCorrections(corrJSON, "post")

  expect_equal(preData[1,][['type']], 'USGS_MULTI_POINT')
  expect_equal(preData[1,][['startTime']], '2015-03-30T11:00:00-06:00')

  expect_equal(normalData[1,][['type']], 'USGS_MULTI_POINT')
  expect_equal(normalData[1,][['startTime']], '2015-11-09T14:15:00-06:00')

  expect_equal(postData[1,][['type']], 'COPY_PASTE')
  expect_equal(postData[1,][['startTime']], '2014-12-10T00:00:00-06:00')
})

test_that("fetchThresholds properly retrieves the threshold data", {
  thresholdJSON <- fromJSON('{
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

  thresholds <- repgen:::fetchThresholds(thresholdJSON)

  expect_is(thresholds, 'data.frame')
  expect_is(thresholds[1,][['periods']], 'list')
  expect_equal(nrow(thresholds), 2)
  expect_equal(nrow(thresholds[1,][['periods']][[1]]), 2)
  expect_equal(thresholds[1,][['type']], 'ThresholdAbove')
  expect_equal(thresholds[2,][['type']], 'ThresholdBelow')
})

test_that('fetchExcludedControlConditions properly retrieves the excluded control condition data', {
  controlConditionJSON <- fromJSON('{
     "excludedControlConditions": [
        {
          "value": "Clear",
          "name": "CLEAR"
        },
        {
          "value": "VegetationLight",
          "name": "VEGETATION_LIGHT"
        },
        {
          "value": "VegetationModerate",
          "name": "VEGETATION_MODERATE"
        }
     ]
  }')
  
  conditions <- repgen:::fetchExcludedControlConditions(controlConditionJSON)
  
  expect_is(conditions, 'data.frame')
  expect_equal(nrow(conditions), 3)
  expect_equal(conditions[1,][['value']], 'Clear')
  expect_equal(conditions[1,][['name']], "CLEAR")
})

test_that('fetchGaps properly retrieves the gaps', {
  gapJson <- fromJSON('{
    "primaryTsData":{
      "gaps": [
        {
        "startTime": "2016-11-23T00:00:00-05:00",
        "endTime": "2016-11-23T12:00:00-05:00"
        },
        {
        "startTime": "2016-11-23T12:00:00-05:00",
        "endTime": "2016-11-24T00:00:00-05:00"
        }
      ]
    }
  }')
  
  gaps <- repgen:::fetchGaps(gapJson)
  
  expect_is(gaps, 'data.frame')
  expect_equal(nrow(gaps), 2)
  expect_equal(gaps[1,][['startTime']], '2016-11-23T00:00:00-05:00')
  expect_equal(gaps[1,][['endTime']], "2016-11-23T12:00:00-05:00")
})

test_that('fetchUpchainSeries properly retrieves the related upchain series', {
  upchainJson <- fromJSON('{
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
     ]
  }')
  
  upchain <- repgen:::fetchUpchainSeries(upchainJson)
  
  expect_is(upchain, 'data.frame')
  expect_equal(nrow(upchain), 1)
  expect_equal(upchain[1,][['identifier']], 'Gage height.ft@01047200')
  expect_equal(upchain[1,][['uniqueId']], "5eb2fdadf2784ebeaed2f64c6d02edf8")
})

test_that('fetchDownchainSeries properly retrieves the related upchain series', {
  downchainJson <- fromJSON('{
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
  
  downchain <- repgen:::fetchDownchainSeries(downchainJson)
  
  expect_is(downchain, 'data.frame')
  expect_equal(nrow(downchain), 1)
  expect_equal(downchain[1,][['identifier']], 'Discharge.ft^3/s.diff_per@01047200')
  expect_equal(downchain[1,][['uniqueId']], "884fc0c281b14685baf9cbf744f0a606")
})

test_that('fetchQualifiers properly retrieves the qualifiers', {
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
  
  quals <- repgen:::fetchQualifiers(qualsJson)
  
  expect_is(quals, 'data.frame')
  expect_equal(nrow(quals), 2)
  expect_equal(quals[1,][['startDate']], '2017-03-05T18:45:00-05:00')
  expect_equal(quals[1,][['endDate']], "2017-03-06T05:45:00.0000001-05:00")
  expect_equal(quals[1,][['identifier']], "EQUIP")
  expect_equal(quals[1,][['code']], "EQP")
})

test_that('fetchNotes properly retrieves the notes', {
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
  
  notes <- repgen:::fetchNotes(notesJson)
  
  expect_is(notes, 'data.frame')
  expect_equal(nrow(notes), 1)
  expect_equal(notes[1,][['startDate']], '2017-02-24T12:30:00-05:00')
  expect_equal(notes[1,][['endDate']], "2017-02-24T14:00:00.0000001-05:00")
  expect_equal(notes[1,][['note']], "ADAPS Source Flag: *")
})

test_that('fetchGrades properly retrieves the grades', {
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
  
  grades <- repgen:::fetchGrades(gradesJson)
  
  expect_is(grades, 'data.frame')
  expect_equal(nrow(grades), 1)
  expect_equal(grades[1,][['startDate']], '2016-05-01T00:00:00-05:00')
  expect_equal(grades[1,][['endDate']], "2017-05-31T00:00:00.0000001-05:00")
  expect_equal(grades[1,][['code']], "50")
})

test_that('fetchRatingCurves properly retrieves the rating cruves', {
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
  
  curves <- repgen:::fetchRatingCurves(curvesJson)
  
  expect_is(curves, 'data.frame')
  expect_equal(nrow(curves), 1)
  expect_equal(curves[1,][['startOfPeriod']], '2015-10-06T16:06:01-05:00')
  expect_equal(curves[1,][['endOfPeriod']], "2016-11-16T00:00:00-05:00")
  expect_equal(curves[1,][['curveNumber']], "6.2")
  expect_equal(nrow(curves[1,][['applicablePeriods']][[1]]), 2)
})

test_that('fetchApprovals properly retrieves the approvals', {
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
  
  approvals <- repgen:::fetchApprovals(approvalsJson)
  
  expect_is(approvals, 'data.frame')
  expect_equal(nrow(approvals), 2)
  expect_equal(approvals[1,][['startTime']], '2007-10-01T00:00:00-05:00')
  expect_equal(approvals[1,][['endTime']], "2016-11-16T00:00:00-05:00")
  expect_equal(approvals[1,][['level']], 1200)
  expect_equal(approvals[1,][['description']], "Approved")
})

test_that('fetchGapTolerances properly retrieves the gap tolerances', {
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
  
  tolerances <- repgen:::fetchGapTolerances(tolerancesJson)
  
  expect_is(tolerances, 'data.frame')
  expect_equal(nrow(tolerances), 1)
  expect_equal(tolerances[1,][['startTime']], "2016-06-01T00:00:00-05:00")
  expect_equal(tolerances[1,][['endTime']], "2017-06-03T00:00:00.0000001-05:00")
  expect_equal(tolerances[1,][['toleranceInMinutes']], 120)
})

test_that('fetchCorrReportURL properly retrieves the URL for the CORR report', {
  corrJson <- fromJSON('{
    "corrections": {
      "corrUrl": "https://localhost:8443/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-06-13Z&station=01049320&startDate=2017-05-01Z&primaryTimeseriesIdentifier=b5be2e0d6a12443f80c51aacf28514c6"
      }
  }')
  
  url <- repgen:::fetchCorrReportURL(corrJson)
  
  expect_equal(url, "https://localhost:8443/aqcu-webservice/service/reports/correctionsataglance/?endDate=2017-06-13Z&station=01049320&startDate=2017-05-01Z&primaryTimeseriesIdentifier=b5be2e0d6a12443f80c51aacf28514c6")
})

test_that('fetchPrimaryTSMetadata properly retrieves the primary TS metadata', {
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
  
  metadata <- repgen:::fetchPrimaryTSMetadata(metadataJson)
  
  expect_equal(metadata[['period']], "Points")
  expect_equal(metadata[['unit']], "ft")
  expect_equal(metadata[['publish']], TRUE)
  expect_equal(metadata[['parameter']], "Gage height")
  expect_is(metadata[['extendedAttributes']], 'list')
})

test_that('fetchMethods properly retrieves the primary ts methods', {
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
  
  methods <- repgen:::fetchMethods(methodsJson)
  
  expect_equal(methods[['methodCode']], "DefaultNone")
  expect_equal(methods[['startTime']], "2016-06-01T00:00:00-05:00")
  expect_equal(methods[['endTime']], "2017-06-22T00:00:00.0000001-05:00")
})

test_that('fetchInterpolationTypes properly retrieves the primary ts interpolation types', {
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
  
  its <- repgen:::fetchInterpolationTypes(itsJson)
  
  expect_equal(its[['type']], "InstantaneousValues")
  expect_equal(its[['startTime']], "2016-06-01T00:00:00-05:00")
  expect_equal(its[['endTime']], "2017-06-22T00:00:00.0000001-05:00")
})

test_that('fetchProcessors properly retrieves the primary ts processors', {
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
  
  procs <- repgen:::fetchProcessors(procsJson)
  
  expect_equal(procs[['processorType']], "correctedpassthrough")
  expect_equal(procs[['periodStartTime']], "2016-06-01T00:00:00-05:00")
  expect_equal(procs[['periodEndTime']], "2017-06-22T00:00:00.0000001-05:00")
})
