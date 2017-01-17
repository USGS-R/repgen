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
  
  reportObject <- fromJSON('{ "maximumStageHeight" : 1 }')
  expect_equal(repgen:::fetchMaxStage(reportObject), 1)
})

test_that('fetchMinStage data returns as expected', {
  empty <- list('data'=c(0,0,0))
  expect_equal(repgen:::fetchMinStage(empty), numeric(0))
  expect_is(repgen:::fetchMinStage(empty), 'numeric')
  
  reportObject <- fromJSON('{ "minimumStageHeight" : 1 }')
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

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
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
            "Comment \u003d Reset to 2.42 after inspection"
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
  expect_is(fvData$fieldVisitIdentifier[[1]], 'character')
  expect_is(fvData$value[[1]], 'character')
  expect_is(fvData$time[[length(fvData$time)]], 'character')
  expect_is(fvData$associatedIvQualifiers, 'list')
  expect_is(fvData$associatedIvQualifiers[[1]], 'data.frame')
  
  expect_equal(nrow(fvData), 1)
  expect_equal(fvData$fieldVisitIdentifier[[1]], '1FCDFDC32416F7C4E05322EB3D985BC8')
  expect_equal(fvData$time[[length(fvData$time)]], '2015-08-07T09:26:00.000-05:00')
  expect_equal(fvData$value[[1]], "21.72")
  expect_equal(fvData$associatedIvQualifiers[[1]]$dateApplied[[1]], "2015-09-15T06:45:46.130-05:00")
  expect_equal(fvData$associatedIvQualifiers[[1]]$dateApplied[[2]], "2015-09-15T12:57:22.423-05:00")
})



