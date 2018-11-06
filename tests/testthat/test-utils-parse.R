wd <- getwd()
setwd(dir = tempdir())

context("testing utils-parse")
library(jsonlite)
parseTestJSON <- fromJSON(system.file('extdata','testsnippets','test-dvhydrograph.json', package = 'repgen'))
test_that("parseTimeSeries correctly parses DV Time Series JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- parseTestJSON[['onlyStat1']]

  timezone <- repgen:::fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- repgen:::parseTimeSeries(onlyStat1, 'firstStatDerived', 'firstStatDerivedLabel', timezone, isDV=TRUE)
  stat1Est <- repgen:::parseTimeSeries(onlyStat1, 'firstStatDerived', 'firstStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)

  stat2 <- repgen:::parseTimeSeries(onlyStat1, 'secondStatDerived', 'secondStatDerivedLabel', timezone, isDV=TRUE)
  stat2Est <- repgen:::parseTimeSeries(onlyStat1, 'secondStatDerived', 'secondStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)

  expect_is(stat1, 'list')
  expect_is(stat1Est, 'list')
  expect_is(stat2, 'NULL')
  expect_is(stat2Est, 'NULL')

  expect_equal(nrow(stat1$points), 2)
  expect_equal(nrow(stat1Est$points), 1)
})

test_that("parseFieldVisitMeasurements returns valid field visit measurements for valid JSON", {
  fieldVisits <- parseTestJSON[['fieldVisits']]
  emptyFieldVisits <- fromJSON('{"fieldVisitMeasurements": []}')
  noFieldVisits <- fromJSON('{}')

  valid <- repgen:::parseFieldVisitMeasurements(fieldVisits)
  empty <- repgen:::parseFieldVisitMeasurements(emptyFieldVisits)
  invalid <- repgen:::parseFieldVisitMeasurements(noFieldVisits)

  expect_warning(repgen:::parseFieldVisitMeasurements(noFieldVisits), "Returning NULL for field visit measurements.")

  expect_is(valid, 'data.frame')
  expect_is(invalid, 'NULL')
  expect_is(empty, 'NULL')
})

test_that("parseGroundWaterLevels returns valid min/max IVs for valid JSON", {
  reportObject1 <- parseTestJSON[['gwLevels']]
  reportObject2 <- fromJSON('{"gwlevel": []}')
  reportObject3 <- fromJSON('{}')

  gwData <- parseGroundWaterLevels(reportObject1)
  blankData <- parseGroundWaterLevels(reportObject2)
  missingData <- parseGroundWaterLevels(reportObject3)

  expect_warning(parseGroundWaterLevels(reportObject3), "Returning NULL for ground water levels.")

  expect_is(gwData, 'data.frame')
  expect_is(blankData, 'NULL')
  expect_is(missingData, 'NULL')
})

test_that("parseFieldVisitReadings returns valid readings for valid JSON", {
  library(dplyr)
  library(jsonlite)
  
  reportObject <- fromJSON('{
                         "readings": [
                          {
                            "visitTime": "2015-08-07T09:26:00.000-05:00",
                            "comments": [
                             "Comment \\u003d CSG still submerged.\\r\\nGageInspectedCode \\u003d NTRD\\r\\nIntakeHoleConditionCode \\u003d UNSP\\r\\nVentHoleConditionCode \\u003d UNSP"
                              ],
                            "fieldVisitIdentifier": "1FCDFDC32416F7C4E05322EB3D985BC8",
                            "visitStatus": "TODO",
                            "party": "CR",
                            "monitoringMethod": "Max-min indicator",
                            "value": "21.72",
                            "parameter": "Gage height",
                            "type": "2015-04-03T09:41:00.000-05:00",
                            "startTime": "2015-04-03T09:41:00.000-05:00",
                            "associatedIvTime": "2015-06-26T07:00:00.000-05:00",
                            "associatedIvValue": "21.75",
                            "minTime": "2015-05-08T07:15:00.000-05:00",
                            "minValue": "2.05",
                            "associatedIvQualifiers": [
                            {
                            "startDate": "2015-06-26T05:00:00.000-05:00",
                            "endDate": "2015-08-26T11:00:00.000-05:00",
                            "identifier": "TESTQUAL",
                            "code": "TQ",
                            "appliedBy": "gwilson",
                            "displayName": "Test Qualifier",
                            "dateApplied": "2015-09-15T06:45:46.130-05:00"
                            },
                            {
                              "startDate": "2015-06-26T02:30:00.000-05:00",
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
  fvData <- repgen:::parseFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_is(fvData[['qualifiers']][[1]], 'data.frame')
  expect_equal(fvData[['qualifiers']][[1]]$code[[1]],"TQ")
  expect_equal(fvData[['qualifiers']][[1]]$identifier[[1]],"TESTQUAL")
  expect_equal(fvData[['qualifiers']][[1]]$description[[1]],"Test Qualifier")
  expect_equal(fvData[['qualifiers']][[1]]$code[[2]], "EQP")
  expect_equal(fvData[['qualifiers']][[1]]$identifier[[2]],"EQUIP")
  expect_equal(fvData[['qualifiers']][[1]]$description[[2]],"Equpment Malfunction")
})

test_that("parseFieldVisitReadings returns NULL for invalid JSON", {
  library(dplyr)
  library(jsonlite)
  
  reportObject <- fromJSON('{"readings": []}')
  expect_equal(repgen:::parseFieldVisitReadings(reportObject), NULL)
})

test_that("parseMinMaxIVs returns valid min/max IVs for valid JSON", {
  IVs <- parseTestJSON[['onlyIVs']]
  onlyMax <- parseTestJSON[['onlyMaxIV']]
  noTSNoIVs <- parseTestJSON[['noData']]

  timezone <- repgen:::fetchReportMetadataField(IVs, 'timezone')
  type <- "Discharge"

  invalid <- repgen:::parseMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  onlyMax <- repgen:::parseMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  normal <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  inverted <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = TRUE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  excludeMinMax <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = TRUE, excludeZeroNegativeFlag = FALSE)
  excludeZeroNegative <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = TRUE)

  expect_is(invalid, 'NULL')
  expect_is(normal, 'list')
  expect_is(onlyMax, 'list')
  expect_is(inverted, 'list')
  expect_is(excludeMinMax, 'list')
  expect_is(excludeZeroNegative, 'list')

  expect_equal(names(onlyMax), c('max_iv', 'canLog'))
  expect_equal(names(normal), c('max_iv', 'min_iv', 'canLog'))
  expect_equal(names(inverted), c('max_iv', 'min_iv', 'canLog'))
  expect_equal(names(excludeMinMax), c('max_iv_label', 'min_iv_label', 'canLog'))
  expect_equal(names(excludeZeroNegative), c('max_iv', 'min_iv_label', 'canLog'))
  expect_equal(onlyMax[['canLog']], TRUE)
  expect_equal(normal[['canLog']], FALSE)
  expect_equal(inverted[['canLog']], FALSE)
  expect_equal(excludeMinMax[['canLog']], TRUE)
  expect_equal(excludeZeroNegative[['canLog']], TRUE)
})

test_that("parseMinMaxIV properly retrieves the min/max IV values", {
  IVs <- parseTestJSON[['onlyIVs']]

  max_iv <- repgen:::parseMinMaxIV(IVs, "max", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(IVs, "min", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(IVs, "max", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(IVs, "min", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv$legend.name, "Max. Instantaneous test : 892")
  expect_equal(min_iv$legend.name, "Min. Instantaneous test : -60.5")
  expect_equal(max_iv_inv$legend.name, "Min. Instantaneous test : 892")
  expect_equal(min_iv_inv$legend.name, "Max. Instantaneous test : -60.5")
})

test_that("parseMinMaxIV returns NULL when given invalid JSON", { 
  noTSNoIVs <- parseTestJSON[['noTSNoIVs']]

  max_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "max", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "min", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "max", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "min", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'NULL')
  expect_is(min_iv, 'NULL')
  expect_is(max_iv_inv, 'NULL')
  expect_is(min_iv_inv, 'NULL')
})

test_that("parsePrimarySeriesApprovals returns the primary series approvals for valid JSON", {
  approvals <- fromJSON('{
    "reportMetadata": {
      "timezone": "Etc/GMT+5"
    },
    "primarySeriesApprovals": [
        {
            "approvalLevel": 0,
            "levelDescription": "Working",
            "comment": "",
            "dateAppliedUtc": "2016-11-27T17:25:46.7400821Z",
            "startTime": "2015-10-01T00:00:00-06:00",
            "endTime": "9999-12-31T23:59:59.9999999Z"
        }
    ]
  }')

  startTime <- repgen:::flexibleTimeParse(repgen:::fetchReportMetadataField(approvals, 'startTime'), repgen:::fetchReportMetadataField(approvals, 'timezone'))
  endTime <- repgen:::flexibleTimeParse(repgen:::fetchReportMetadataField(approvals, 'endTime'), repgen:::fetchReportMetadataField(approvals, 'timezone'))
  primary <- repgen:::parsePrimarySeriesApprovals(approvals, startTime, endTime)

  expect_is(primary, 'list')
  expect_equal(primary[['approvals']][1,][['approvalLevel']], 0)
  expect_equal(primary[['approvals']][1,][['levelDescription']], 'Working')
  expect_equal(primary[['approvals']][1,][['startTime']], "2015-10-01T00:00:00-06:00")
})

test_that("parsePrimarySeriesQualifiers properly retrieves the primary series qualifiers", {
  approvals <- fromJSON('{
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "qualifierMetadata": {
      "ESTIMATED": {
                        "identifier": "ESTIMATED",
                        "code": "E",
                        "displayName": "Estimated"
           },
                        "ICE": {
                        "identifier": "ICE",
                        "code": "I",
                        "displayName": "Ice"
                        }
                        }
                        },
   "primarySeriesQualifiers": [
        {
            "startTime": "2016-12-01T00:00:00-05:00",
            "endTime": "2017-01-10T00:00:00.0000001-05:00",
            "identifier": "ESTIMATED",
            "user": "admin",
            "dateApplied": "2016-12-10T11:16:12Z"
        },
        {
            "startTime": "2016-12-01T00:00:00-05:00",
            "endTime": "2017-01-10T00:00:00.0000001-05:00",
            "identifier": "ICE",
            "user": "admin",
            "dateApplied": "2016-12-10T11:16:12Z"
        }
    ]
  }')
  
  primary <- repgen:::parsePrimarySeriesQualifiers(approvals)
  est <- repgen:::parsePrimarySeriesQualifiers(approvals, filterCode="E")
  
  expect_is(primary, 'data.frame')
  expect_is(est, 'data.frame')
  expect_equal(nrow(primary), 2)
  expect_equal(nrow(est), 1)
  expect_equal(primary[1,][['identifier']], "ESTIMATED")
  expect_equal(primary[1,][['code']], 'E')
  expect_equal(est[1,][['identifier']], "ESTIMATED")
  expect_equal(est[1,][['code']], 'E')
})

test_that('parseWaterQualityMeasurements returns valid and properly formatted data when given valid JSON', {
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

  wqData <- repgen:::parseWaterQualityMeasurements(reportObject)
  expect_is(wqData, 'data.frame')
  expect_is(wqData$value, 'numeric')
  expect_is(wqData$time, 'POSIXct')
  expect_is(wqData$month, 'character')
  expect_equal(wqData$value[[1]], 5.3)
  expect_equal(wqData$time[[2]], as.POSIXct(strptime("2015-07-29T13:30:00-06:00", "%FT%T")))
})

test_that('parseWaterQualityMeasurements doesnt error when given invalid JSON', {
  library(jsonlite)
  
  reportObject1 <- fromJSON('{
                            "waterQuality": [
                            {
                            "recordNumber": "01501684",
                            "medium": "Surface water",
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
                            "value": {
                            "parameter": "00300",
                            "remark": ""
                            },
                            "timeZone": "CST"
                            }
                            ]
  }')

  reportObject2 <- fromJSON('{ }')
  
  expect_equal(repgen:::parseWaterQualityMeasurements(reportObject1), NULL)
  expect_equal(repgen:::parseWaterQualityMeasurements(reportObject2), NULL)
})

test_that('parseExcludedControlConditions properly retrieves the excluded control condition data', {
  controlConditionJSON <- fromJSON('{
     "reportMetadata": {
        "requestParameters": {
          "excludeConditions": [
            "Clear",
            "VegetationLight",
            "VegetationModerate"
      ]
        }
}
  }')
  
  conditions <- repgen:::parseExcludedControlConditions(controlConditionJSON)
  
  expect_equal(repgen:::parseExcludedControlConditions(), NULL)
  expect_is(conditions, 'character')
  expect_equal(length(conditions), 3)
  expect_equal(conditions[1], 'Clear')
  expect_equal(conditions[2], 'VegetationLight')
  expect_equal(conditions[3], 'VegetationModerate')
})

test_that("parsedProcessingCorrections properly retrieves the processing corrections data", {
  corrJSON <- fromJSON('{
                       "reportMetadata": {
                       "timezone": "Etc/GMT+5"
                       },
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
  timezone <- "Etc/GMT+5"

  preData <- repgen:::parseProcessingCorrections(corrJSON, "pre", timezone)
  normalData <- repgen:::parseProcessingCorrections(corrJSON, "normal", timezone)
  postData <- repgen:::parseProcessingCorrections(corrJSON, "post", timezone)
  nullData <- repgen:::parseProcessingCorrections(NULL, "pre", timezone)
  
  expect_equal(nullData, NULL)
  expect_equal(preData[1,][['type']], 'USGS_MULTI_POINT')
  expect_equal(preData[1,][['startTime']], flexibleTimeParse('2015-03-30T11:00:00-06:00', timezone))

  expect_equal(normalData[1,][['type']], 'USGS_MULTI_POINT')
  expect_equal(normalData[1,][['startTime']], flexibleTimeParse('2015-11-09T14:15:00-06:00', timezone))

  expect_equal(postData[1,][['type']], 'COPY_PASTE')
  expect_equal(postData[1,][['startTime']], flexibleTimeParse('2014-12-10T00:00:00-06:00', timezone))
})

test_that('parseGaps properly retrieves and formats the gaps', {
  timezone <- "Etc/GMT+5"
  gapJson <- fromJSON('{
    "primaryTsData": {
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
  
  gaps <- repgen:::parseGaps(gapJson, timezone)
  nullGaps <- repgen:::parseGaps(NULL, timezone)
  
  expect_equal(nullGaps, NULL)
  expect_is(gaps, 'data.frame')
  expect_equal(nrow(gaps), 2)
  expect_equal(gaps[['startTime']][[1]], as.repgendate(flexibleTimeParse('2016-11-23T00:00:00-05:00', timezone)))
  expect_equal(gaps[['endTime']][[1]], as.repgendate(flexibleTimeParse("2016-11-23T12:00:00-05:00", timezone)))
})

test_that("parseThresholds properly retrieves the threshold data", {
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

  thresholds <- repgen:::parseThresholds(thresholdJSON)
  nullThresholds <- repgen:::parseThresholds(NULL, timezone)
  
  expect_equal(nullThresholds, NULL)
  expect_is(thresholds, 'data.frame')
  expect_is(thresholds[1,][['periods']], 'list')
  expect_equal(nrow(thresholds), 2)
  expect_equal(nrow(thresholds[1,][['periods']][[1]]), 2)
  expect_equal(thresholds[1,][['type']], 'ThresholdAbove')
  expect_equal(thresholds[2,][['type']], 'ThresholdBelow')
})

test_that('parseApprovals properly retrieves the approvals', {
  timezone <- "Etc/GMT+5"
  approvalsJson <- fromJSON('{
    "primaryTsData": {
      "approvals": [
        {
        "approvalLevel": 1200,
        "levelDescription": "Approved",
        "comment": "",
        "dateAppliedUtc": "2017-02-02T21:16:24.937095Z",
        "startTime": "2007-10-01T00:00:00-05:00",
        "endTime": "2016-11-16T00:00:00-05:00"
        },
        {
        "approvalLevel": 900,
        "levelDescription": "Working",
        "comment": "",
        "dateAppliedUtc": "2017-02-02T21:15:49.5368596Z",
        "startTime": "2016-11-16T00:00:00-05:00",
        "endTime": "9999-12-31T23:59:59.9999999Z"
        }
      ]
    }
  }')
  
  approvals <- repgen:::parseApprovals(approvalsJson, timezone)
  nullApprovals <- repgen:::parseApprovals(NULL, timezone)
  
  expect_equal(nullApprovals, NULL)
  expect_is(approvals, 'data.frame')
  expect_equal(nrow(approvals), 2)
  expect_equal(approvals[1,][['startTime']], flexibleTimeParse('2007-10-01T00:00:00-05:00', timezone))
  expect_equal(approvals[1,][['endTime']], flexibleTimeParse("2016-11-16T00:00:00-05:00", timezone))
  expect_equal(approvals[1,][['approvalLevel']], 1200)
  expect_equal(approvals[1,][['levelDescription']], "Approved")
})

setwd(dir = wd)