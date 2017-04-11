wd <- getwd()
setwd(dir = tempdir())

context("testing utils-parse")
parseTestJSON <- fromJSON(system.file('extdata','testsnippets','test-dvhydrograph.json', package = 'repgen'))
test_that("parseTimeSeries correctly parses DV Time Series JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- parseTestJSON[['onlyStat1']]

  timezone <- fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- parseTimeSeries(onlyStat1, 'firstStatDerived', 'firstStatDerivedLabel', timezone, isDV=TRUE)
  stat1Est <- parseTimeSeries(onlyStat1, 'firstStatDerived', 'firstStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)

  stat2 <- parseTimeSeries(onlyStat1, 'secondStatDerived', 'secondStatDerivedLabel', timezone, isDV=TRUE)
  stat2Est <- parseTimeSeries(onlyStat1, 'secondStatDerived', 'secondStatDerivedLabel', timezone, estimated=TRUE, isDV=TRUE)

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

  max_iv <- repgen:::parseMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)

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

  max_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)

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
            "level": 0,
            "description": "Working",
            "comment": "",
            "dateApplied": "2016-11-27T17:25:46.7400821Z",
            "startTime": "2015-10-01T00:00:00-06:00",
            "endTime": "9999-12-31T23:59:59.9999999Z"
        }
    ]
  }')

  startTime <- repgen:::flexibleTimeParse(repgen:::fetchReportMetadataField(approvals, 'startTime'), repgen:::fetchReportMetadataField(approvals, 'timezone'))
  endTime <- repgen:::flexibleTimeParse(repgen:::fetchReportMetadataField(approvals, 'endTime'), repgen:::fetchReportMetadataField(approvals, 'timezone'))
  primary <- repgen:::parsePrimarySeriesApprovals(approvals, startTime, endTime)

  expect_is(primary, 'list')
  expect_equal(primary[['approvals']][1,][['level']], 0)
  expect_equal(primary[['approvals']][1,][['description']], 'Working')
  expect_equal(primary[['approvals']][1,][['startTime']], "2015-10-01T00:00:00-06:00")
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

setwd(dir = wd)