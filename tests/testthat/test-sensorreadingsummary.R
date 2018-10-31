context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  reportObject <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject, 'Author Name'), 'character')
  
  reportObject2 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example-exc-comm.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject2, 'Author Name'), 'character')
  
  reportObject3 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example_w_quals.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject3, 'Author Name'), 'character')
})


test_that('parseSRSQualifiers properly retrieves the qualifiers', {
	library(jsonlite)
	timezone <- "Etc/GMT+5"
	qualsJson <- fromJSON('{
		"readings": [{
			"displayTime": "2016-02-29T05:57:00.0000000Z",
			"recorderComments": [
				"Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
				],
			"visitStatus": "TODO",
			"recorderMethod": "Non-subm pressure  transducer",
			"recorderValue": "5.24",
			"recorderType": "Routine",
			"party": "ARC",
			"nearestCorrectedValue": "5.24",
			"qualifiers": [
				{
				"startTime": "2017-03-05T18:45:00-05:00",
				"endTime": "2017-03-06T05:45:00.0000001-05:00",
				"identifier": "EQUIP",
				"user": "system",
				"dateApplied": "2017-03-11T14:57:13.4625975Z"
				},
				{
				"startTime": "2017-02-26T01:30:00-05:00",
				"endTime": "2017-02-26T01:30:00.0000001-05:00",
				"identifier": "EQUIP",
				"user": "system",
				"dateApplied": "2017-03-11T14:57:13.4625975Z"
				}
			]
			}],
			"reportMetadata": {
				"qualifierMetadata": {
				"EQUIP": {
					"identifier": "EQUIP",
					"code": "EQP",
					"displayName": "Equipment malfunction"
					}
				}
			}
}')
	#browser()
	qualsMetadata <- repgen:::fetchQualifierMetadata(qualsJson)
	reading <- qualsJson[["readings"]][1,]
	quals <- repgen:::parseSRSQualifiers(reading, timezone, qualsMetadata)

	expect_is(quals, 'data.frame')
	expect_equal(nrow(quals), 2)
	expect_equal(quals[1,][['startTime']], flexibleTimeParse('2017-03-05T18:45:00-05:00', timezone))
	expect_equal(quals[1,][['endTime']], flexibleTimeParse("2017-03-06T05:45:00.0000001-05:00", timezone))
	expect_equal(quals[1,][['identifier']], "EQUIP")
	expect_equal(quals[1,][['code']], "EQP")
	})

test_that("get unique qualifiers", {
  library(jsonlite)
	library(dplyr)
  reportObject <- fromJSON('{
    "readings": [
      {
      "displayTime": "2016-02-29T05:57:00.0000000Z",
      "recorderComments": [
      "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
      ],
      "visitStatus": "TODO",
      "recorderMethod": "Non-subm pressure  transducer",
      "recorderValue": "5.24",
      "recorderType": "Routine",
      "party": "ARC",
      "nearestCorrectedValue": "5.24",
      "qualifiers": [
        {
        "startDate": "2016-02-29T05:54:41.0000000Z",
        "endDate": "2016-02-29T15:54:41.0000000Z",
        "identifier": "ESTIMATED",
        "appliedBy": "admin",
        "dateApplied": "2016-03-01T16:35:14.0000000Z"
        }
      ],
      "nearestCorrectedTime": "2016-02-29T06:00:00.0000000Z",
      "nearestRawTime": "2016-02-29T06:00:00.0000000Z",
      "nearestRawValue": "5.24"
      },
      {
      "displayTime": "2016-02-29T07:31:00.0000000Z",
      "recorderComments": [
      "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
      ],
      "visitStatus": "TODO",
      "recorderMethod": "Non-subm pressure  transducer",
      "recorderValue": "5.21",
      "recorderType": "Routine",
      "party": "ARC",
      "nearestCorrectedValue": "5.19",
      "qualifiers": [
        {
        "startDate": "2016-02-29T11:54:41.0000000Z",
        "endDate": "2016-02-29T15:54:41.0000000Z",
        "identifier": "ESTIMATED",
        "appliedBy": "admin",
        "dateApplied": "2016-03-01T07:35:14.0000000Z"
        }
      ],
      "nearestCorrectedTime": "2016-02-29T07:45:00.0000000Z",
      "nearestRawTime": "2016-02-29T07:45:00.0000000Z",
      "nearestRawValue": "5.19"
      }
    ],
      "reportMetadata": {
			"requestParameters": {
      	"excludeComments": "",
  			"primaryTimeseriesIdentifier": "3ec1f3f59fe944079fc0ea5a4eb03e83",
  			"startDate": "2015-11-01",
  			"endDate": "2016-11-14",
  			"requestPeriod": {}
			},
      "requestingUser": "thongsav",
      "endDate": "2016-11-14T23:59:59.999999999Z",
      "timezone": "Etc/GMT+5",
      "title": "Sensor Reading Summary",
      "stationName": "Sandy River near Madrid, Maine",
      "stationId": "01047200",
      "excludeComments": false,
      "timeSeriesParams": "Gage height",
      "timeSeriesUniqueId": "3ec1f3f59fe944079fc0ea5a4eb03e83",
      "timeseriesLabel": "Gage height.ft@01047200",
      "startDate": "2015-11-01T00:00:00Z",
			"qualifierMetadata": {
      	"ESTIMATED": {
        	"identifier": "ESTIMATED",
        	"code": "E",
      	 	"displayName": "Estimated"
      	}
			}
    }
  }')

  quals <- bind_rows(repgen:::fetchQualifierMetadata(reportObject)[[1]])
  quals <- quals[, c("code", "identifier", "displayName")]
  uniqueSRSQualifiers <- repgen:::formatQualifiersTable(quals)
  expect_true(nrow(uniqueSRSQualifiers)==1)

  })

test_that('do all expected columns exist in formatted results', {
  library(jsonlite)
  reportObject <- fromJSON('{
    "readings": [
                           {
                           "displayTime": "2016-02-29T05:57:00.0000000Z",
                           "recorderComments": [
                           "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                           ],
                           "visitStatus": "TODO",
                           "recorderMethod": "Non-subm pressure  transducer",
                           "recorderValue": "5.24",
                           "recorderType": "Routine",
                           "party": "ARC",
                           "nearestCorrectedValue": "5.24",
                           "qualifiers": [
                           {
                           "startDate": "2016-02-29T05:54:41.0000000Z",
                           "endDate": "2016-02-29T15:54:41.0000000Z",
                           "identifier": "ESTIMATED",
                           "appliedBy": "admin",
                           "dateApplied": "2016-03-01T16:35:14.0000000Z"
                           }
                           ],
                           "nearestCorrectedTime": "2016-02-29T06:00:00.0000000Z",
                           "nearestRawTime": "2016-02-29T06:00:00.0000000Z",
                           "nearestRawValue": "5.24"
                           },
                           {
                           "displayTime": "2016-02-29T07:31:00.0000000Z",
                           "recorderComments": [
                           "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                           ],
                           "visitStatus": "TODO",
                           "recorderMethod": "Non-subm pressure  transducer",
                           "recorderValue": "5.21",
                           "recorderType": "Routine",
                           "party": "ARC",
                           "nearestCorrectedValue": "5.19",
                           "qualifiers": [
                           {
                           "startDate": "2016-02-29T11:54:41.0000000Z",
                           "endDate": "2016-02-29T15:54:41.0000000Z",
                           "identifier": "ESTIMATED",
                           "appliedBy": "admin",
                           "dateApplied": "2016-03-01T16:35:14.0000000Z"
                           }
                           ],
                           "nearestCorrectedTime": "2016-02-29T07:45:00.0000000Z",
                           "nearestRawTime": "2016-02-29T07:45:00.0000000Z",
                           "nearestRawValue": "5.19"
                           }
                           ],
                           "reportMetadata": {
															"requestParameters": {
      												"excludeComments": "",
      												"primaryTimeseriesIdentifier": "3ec1f3f59fe944079fc0ea5a4eb03e83",
      												"startDate": "2015-11-01",
      												"endDate": "2016-11-14",
      												"requestPeriod": {}
    												},
                           "requestingUser": "thongsav",
                           "endDate": "2016-11-14T23:59:59.999999999Z",
                           "timezone": "Etc/GMT+5",
                           "title": "Sensor Reading Summary",
                           "stationName": "Sandy River near Madrid, Maine",
                           "stationId": "01047200",
                           "timeSeriesParams": "Gage height",
                           "timeSeriesUniqueId": "3ec1f3f59fe944079fc0ea5a4eb03e83",
                           "timeseriesLabel": "Gage height.ft@01047200",
                           "startDate": "2015-11-01T00:00:00Z",
														"qualifierMetadata": {
														      "ESTIMATED": {
														        "identifier": "ESTIMATED",
														        "code": "E",
														        "displayName": "Estimated"
														      	}
														     }
                           }
}')
  includeComments <- repgen:::isNullOrFalse(repgen:::fetchRequestParametersField(reportObject, 'excludeComments'))
  timezone <- repgen:::fetchReportMetadataField(reportObject, 'timezone')
  columnNames <- c("Date",
                   "Time",
                   "Party",
                   "Sublocation",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Method",
                   "Reading Type",
                   "Reading",
                   "Uncertainty",
                   "Recorder w/in Uncertainty?", 
                   "Indicated Correction",
                   "Applied Correction",
                   "Corrected w/in Reference?",
                   "Value",
                   "Time",
                   "Qualifier"
  )
  results <- repgen:::formatSensorData(reportObject[["readings"]], columnNames, includeComments,timezone, repgen:::fetchQualifierMetadata(reportObject))
  expect_equal(length(results$toRet),19)
})

test_that('getUniqueComments works for removing redundant comments', {
  library(jsonlite)
  lastDate <- "09/29/2014"
  lastComm <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  date <- "09/29/2014"
  comments <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  lastRefComm <- "Comment = AF RP1 4.046 - 0.160 WS 3.886 // AL - 0.170 WS 3.876<br/>"
  selectedComm <- repgen:::getUniqueComments(comments, date, lastDate, lastComm)
  expect_equal(selectedComm,"")
})

test_that('getUniqueComments works for non-redundant comments', {
  library(jsonlite)
  lastDate <- "09/29/2014"
  lastComm <- "GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP; OrificeServicedCode = PRGD<br/>OrificeServicedDateTime = 2014-09-29T07:06:00-04:00 (EDT)<br/>DesiccantConditionCode = DESG<br/>DesiccantChangedIndicator = false<br/>GasSystemTypeCode = BAIR<br/>GasTankChangedIndicator = false<br/>Comment = No change noticed from purge. // AF offset 1.580 // AL offset 1.570, reset at -0.01 @ 0708, based on trends and applied correction since June 2013.<br/>"
  date <- "09/29/2014"
  comments <- "Test<br/>Test<br/>Test<br/>"
  selectedComm <- repgen:::getUniqueComments(comments, date, lastDate, lastComm)
  expect_equal(selectedComm,"Test<br/>Test<br/>Test<br/>")
})

test_that('recorder uncertainty function works as expected, delivers No', {
  recorderValue <- "6.23"
  uncertainty <- "0.05"
  value <- "6.42"
  recorderWithin <- repgen:::getRecorderWithinUncertainty(uncertainty, value, recorderValue)
  expect_equal(recorderWithin,"No")
})

test_that('recorder uncertainty function works as expected, delivers Yes', {
  recorderValue <- "6.44"
  uncertainty <- "0.05"
  value <- "6.42"
  recorderWithin <- repgen:::getRecorderWithinUncertainty(uncertainty, value, recorderValue)
  expect_equal(recorderWithin,"Yes")
})

test_that('indicated correction returns correctly',{
  recorderValue <- "6.44"
  value <- "6.42"
  indicatedCorrection <- repgen:::getIndicatedCorrection(recorderValue, value)
  expect_equal(indicatedCorrection,as.numeric("-0.02"))
})

test_that('applied correction returns as expected', {
  raw <- "5.02"
  corrected <- "5.02"
  appliedCorrection <- repgen:::getAppliedCorrection(raw, corrected)
  expect_equal(appliedCorrection,as.numeric("0"))
})

test_that('get Corrected reference returns value as expected', {
  nearestCorrectedValue <- "2.48"
  uncertainty <- "0.01"
  value <- "2.48"
  correctedRef <- repgen:::getCorrectedRef(value, nearestCorrectedValue, uncertainty)
  expect_equal(correctedRef,"Yes")
})

setwd(dir = wd)
