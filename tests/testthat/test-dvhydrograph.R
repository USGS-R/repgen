wd <- getwd()
setwd(dir = tempdir())

context("testing DVHydrograph")
test_that("parseDVTimeSeries correctly parses DV Time Series JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- fromJSON('
    {
      "reportMetadata": {
        "timezone": "Etc/GMT+5",
        "secondDownChain": "",
        "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
        "thirdDownChain": "",
        "primaryTimeSeries": "b4216f457d474b6b8d6b0c23cbadbfd3",
        "quaternaryReferenceTimeSeries": "",
        "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200",
        "primaryDescriptions": "Discharge.ft^3/s@01054200",
        "tertiaryReferenceTimeSeries": "",
        "secondaryReferenceTimeSeries": ""
      },
      "firstDownChain": {
        "notes": [],
        "isVolumetricFlow": true,
        "description": "From Aquarius",
        "qualifiers": [
          {
            "startDate": "2013-11-11T00:00:00-05:00",
            "endDate": "2013-11-12T00:00:00-05:00",
            "identifier": "ESTIMATED",
            "code": "E",
            "appliedBy": "admin",
            "dateApplied": "2016-09-04T19:36:45.3185938Z"
          }
        ],
        "units": "ft^3/s",
        "grades": [],
        "type": "Discharge",
        "gaps": [],
        "points": [
          {
            "time": "2013-11-10",
            "value": 66.6
          },
          {
            "time": "2013-11-11",
            "value": 66.5
          },
          {
            "time": "2013-11-12",
            "value": 66.4
          }
        ],
        "requestedStartTime": "2013-11-10T00:00:00-05:00",
        "requestedEndTime": "2013-12-11T23:59:59.999999999-05:00",
        "estimatedPeriods": [
          {
            "startDate": "2013-11-11T00:00:00-05:00",
            "endDate": "2013-11-12T00:00:00-05:00"
          }
        ],
        "approvals": [
          {
            "level": 2,
            "description": "Approved",
            "comment": "",
            "dateApplied": "2016-09-04T23:53:53.2623141Z",
            "startTime": "1963-10-01T00:00:00-05:00",
            "endTime": "2015-10-05T00:00:00-05:00"
          }
        ],
        "name": "24eca840ec914810a88f00a96a70fc88",
        "startTime": "2013-11-09",
        "endTime": "2013-12-11",
        "gapTolerances": []
      }
    }'
  )

  timezone <- fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- parseDVTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone)
  stat1Est <- parseDVTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone, estimated=TRUE)

  stat2 <- parseDVTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone)
  stat2Est <- parseDVTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone, estimated=TRUE)

  expect_is(stat1, 'list')
  expect_is(stat1Est, 'list')
  expect_is(stat2, 'NULL')
  expect_is(stat2Est, 'NULL')

  expect_equal(nrow(stat1$points), 2)
  expect_equal(nrow(stat1Est$points), 1)
})

test_that("parseDVApprovals returns valid approvals for valid JSON", {
  TSData <- fromJSON('
    {
      "reportMetadata": {
        "timezone": "Etc/GMT+5",
        "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
        "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
      },
      "firstDownChain": {
        "notes": [],
        "isVolumetricFlow": true,
        "description": "From Aquarius",
        "qualifiers": [],
        "units": "ft^3/s",
        "grades": [],
        "type": "Discharge",
        "gaps": [],
        "points": [
          {
            "time": "2013-11-10",
            "value": 66.6
          }
        ],
        "requestedStartTime": "2013-11-10T00:00:00-05:00",
        "requestedEndTime": "2013-12-11T23:59:59.999999999-05:00",
        "estimatedPeriods": [],
        "approvals": [
          {
            "level": 2,
            "description": "Approved",
            "comment": "",
            "dateApplied": "2016-09-04T23:53:53.2623141Z",
            "startTime": "1963-10-01T00:00:00-05:00",
            "endTime": "2015-10-05T00:00:00-05:00"
          },
          {
            "level": 1,
            "description": "In Review",
            "comment": "",
            "dateApplied": "2016-09-04T23:53:53.2623141Z",
            "startTime": "2015-10-05T00:00:00-05:00",
            "endTime": "2015-11-30T00:00:00-05:00"
          }
        ],
        "name": "24eca840ec914810a88f00a96a70fc88",
        "startTime": "2013-11-09",
        "endTime": "2013-12-11",
        "gapTolerances": []
      }
    }'
  )

  timezone <- fetchReportMetadataField(TSData, 'timezone')

  stat1 <- parseDVTimeSeries(TSData, 'firstDownChain', 'downChainDescriptions1', timezone)
  approvals <- parseDVApprovals(stat1, timezone)

  expect_is(approvals, 'list')

  expect_equal(length(approvals), 2)
  expect_equal(names(approvals), c('appr_approved_uv', 'appr_inreview_uv'))
})

test_that("parseDVFieldVisitMeasurements returns valid field visit measurements for valid JSON", {
  fieldVisits <- fromJSON('{
      "fieldVisitMeasurements": [
        {
          "shiftInFeet": 0.05744611933222,
          "errorMinShiftInFeet": -0.10928295341418,
          "errorMaxShiftInFeet": 0.21698855520226,
          "identifier": "3BBEDED0E9961692E0530100007FB15C",
          "controlCondition": "CLEAR",
          "measurementStartDate": "2016-04-08T09:02:42-08:00",
          "ratingModelIdentifier": "Gage height-Discharge.STGQ@11532500",
          "discharge": 2410,
          "dischargeUnits": "ft^3/s",
          "errorMinDischarge": 2217.2000,
          "errorMaxDischarge": 2602.8000,
          "measurementNumber": "943",
          "qualityRating": "FAIR",
          "historic": false,
          "meanGageHeight": 7.71,
          "meanGageHeightUnits": "ft",
          "shiftNumber": 0
        }
      ]
  }')

  emptyFieldVisits <- fromJSON('{
    "fieldVisitMeasurements": []
  }')

  noFieldVisits <- fromJSON('{}')

  valid <- parseDVFieldVisitMeasurements(fieldVisits)
  empty <- parseDVFieldVisitMeasurements(emptyFieldVisits)
  invalid <- parseDVFieldVisitMeasurements(noFieldVisits)

  expect_warning(parseDVFieldVisitMeasurements(noFieldVisits), "Returning empty data frame")

  expect_is(valid, 'data.frame')
  expect_is(invalid, 'NULL')
  expect_is(empty, 'NULL')
})

test_that("parseDVGroundWaterLevels returns valid min/max IVs for valid JSON", {
  reportObject1 <- fromJSON('{
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

  reportObject2 <- fromJSON('{
      "gwlevel": []
  }')

  reportObject3 <- fromJSON('{}')

  gwData <- parseDVGroundWaterLevels(reportObject1)
  blankData <- parseDVGroundWaterLevels(reportObject2)
  missingData <- parseDVGroundWaterLevels(reportObject3)

  expect_warning(parseDVGroundWaterLevels(reportObject3))

  expect_is(gwData, 'data.frame')
  expect_is(blankData, 'NULL')
  expect_is(missingData, 'NULL')
})

test_that("parseDVMinMaxIVs returns valid min/max IVs for valid JSON", {
  IVs <- fromJSON('{
    "readings": [],
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
                "value": -10
              }
            ]
          }
        }
      }
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  onlyMax <- fromJSON('{
    "readings": [],
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
            "MIN": []
          }
        }
      }
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  noTSnoIVs <- fromJSON('{
    "readings": [],
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  timezone <- fetchReportMetadataField(IVs, 'timezone')
  type <- "Discharge"

  invalid <- parseDVMinMaxIVs(noTSnoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(parseDVMinMaxIVs(noTSnoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  onlyMax <- parseDVMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(parseDVMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  normal <- parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  inverted <- parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = TRUE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  excludeMinMax <- parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = TRUE, excludeZeroNegativeFlag = FALSE)
  excludeZeroNegative <- parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = TRUE)

  expect_is(invalid, 'NULL')
  expect_is(normal, 'list')
  expect_is(onlyMax, 'list')
  expect_is(inverted, 'list')
  expect_is(excludeMinMax, 'list')
  expect_is(excludeZeroNegative, 'list')

  expect_equal(names(onlyMax), c('max_iv'))
  expect_equal(names(normal), c('max_iv', 'min_iv'))
  expect_equal(names(inverted), c('max_iv', 'min_iv'))
  expect_equal(names(excludeMinMax), c('max_iv_label', 'min_iv_label'))
  expect_equal(names(excludeZeroNegative), c('max_iv', 'min_iv_label'))
})

test_that("getEstimatedEdges properly creates vertical edge lines between estimated and non-estimated time series", {
  estPoints <- data.frame(value=c(1,2,3,2), time=c(as.POSIXct("2017-01-01 Etc/GMT+5"), as.POSIXct("2017-01-03 Etc/GMT+5"), as.POSIXct("2017-01-04 Etc/GMT+5"), as.POSIXct("2017-01-07 Etc/GMT+5")))
  statPoints <- data.frame(value=c(3,2,2,3), time=c(as.POSIXct("2017-01-02 Etc/GMT+5"), as.POSIXct("2017-01-05 Etc/GMT+5"), as.POSIXct("2017-01-06 Etc/GMT+5"), as.POSIXct("2017-01-08 Etc/GMT+5")))
  emptyPoints <- data.frame(value=c(), time=c())
  estEdges <- repgen:::getEstimatedEdges(statPoints, estPoints)
  estEdges2 <- repgen:::getEstimatedEdges(statPoints, emptyPoints)

  expect_is(estEdges, 'list')
  expect_is(estEdges2, 'NULL')
  
  expect_equal(length(estEdges$y0), 5)
  expect_equal(length(estEdges$y1), 5)
  expect_equal(length(estEdges$time), 5)
  expect_equal(length(estEdges$newSet), 5)
  expect_equal(estEdges$y0[1], 1)
  expect_equal(estEdges$y1[1], 3)
  expect_equal(estEdges$y0[3], 3)
  expect_equal(estEdges$y1[3], 2)
  expect_equal(estEdges$newSet[1], "stat")
  expect_equal(estEdges$newSet[4], "est")
  expect_equal(estEdges$time[1], as.POSIXct("2017-01-02 Etc/GMT+5"))
  expect_equal(estEdges$time[4], as.POSIXct("2017-01-07 Etc/GMT+5"))
  expect_equal(estEdges$time[5], as.POSIXct("2017-01-08 Etc/GMT+5"))
})

test_that("getMinMaxIV properly retrieves the min/max IV values", {
  IVs <- fromJSON('{
    "readings": [],
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
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  max_iv <- repgen:::getMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::getMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::getMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::getMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv$legend_nm, "Max. Instantaneous test : 892")
  expect_equal(min_iv$legend_nm, "Min. Instantaneous test : 60.5")
  expect_equal(max_iv_inv$legend_nm, "Min. Instantaneous test : 892")
  expect_equal(min_iv_inv$legend_nm, "Max. Instantaneous test : 60.5")
})

test_that("getMinMaxIV returns NULL when given invalid JSON", { 
  noTSnoIVs <- fromJSON('{
    "readings": [],
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  max_iv <- repgen:::getMinMaxIV(noTSnoIVs, "MAX", repgen:::fetchReportMetadataField(noTSnoIVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::getMinMaxIV(noTSnoIVs, "MIN", repgen:::fetchReportMetadataField(noTSnoIVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::getMinMaxIV(noTSnoIVs, "MAX", repgen:::fetchReportMetadataField(noTSnoIVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::getMinMaxIV(noTSnoIVs, "MIN", repgen:::fetchReportMetadataField(noTSnoIVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'NULL')
  expect_is(min_iv, 'NULL')
  expect_is(max_iv_inv, 'NULL')
  expect_is(min_iv_inv, 'NULL')
})

test_that("extendStep properly extends the last time step by a day", {
  preStepData <- list(
    x=c(as.POSIXct("2014-11-20 GMT+5"), as.POSIXct("2014-11-21 GMT+5")),
    y=c(4510, 4511),
    type=c("s")
  )

  postStepData <- repgen:::extendStep(preStepData)

  expect_is(postStepData, 'list')

  expect_equal(length(postStepData$x), 3)
  expect_equal(length(postStepData$y), 3)
  expect_equal(postStepData$x[1], as.POSIXct("2014-11-20 GMT+5"))
  expect_equal(postStepData$x[2], as.POSIXct("2014-11-21 0GMT+5"))
  expect_equal(postStepData$x[3], as.POSIXct("2014-11-22 0GMT+5"))
  expect_equal(postStepData$y[1], 4510)
  expect_equal(postStepData$y[2], 4511)
  expect_equal(postStepData$y[3], 4511)
})

setwd(dir = wd)
