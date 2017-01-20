wd <- getwd()
setwd(dir = tempdir())

context("testing DVHydrograph")
test_that("parseDVData correctly parses valid DVHydrograph report JSON",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- fromJSON('{
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
    "waterdataUrl": "http://waterdata.usgs.gov/nwis/inventory/?site_no\u003d01054200",
    "fieldVisitMeasurements": [],
    "reportMetadata": {
      "endDate": "2013-12-11T23:59:59.999999999Z",
      "timezone": "Etc/GMT+5",
      "secondDownChain": "",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "description": "Broad brush overviews of a record",
      "title": "DV Hydrograph",
      "thirdDownChain": "",
      "primaryTimeSeries": "b4216f457d474b6b8d6b0c23cbadbfd3",
      "requestId": "DvHydrographChoreographer-4358e078-8d31-4085-84ca-75263f425800",
      "quaternaryReferenceTimeSeries": "",
      "siteNumber": "01054200       ",
      "stationName": "Wild River at Gilead, Maine",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200",
      "primaryDescriptions": "Discharge.ft^3/s@01054200",
      "startDate": "2013-11-10T00:00:00Z",
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
          "endDate": "2013-11-18T00:00:00-05:00",
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
          "endDate": "2013-11-18T00:00:00-05:00"
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
    },
    "simsUrl": "http://sims.water.usgs.gov/SIMSClassic/StationInfo.asp?site_no\u003d01054200"
  }')

  noIVs <- fromJSON('{
    "readings": [],
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
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
        }
      ],
      "name": "24eca840ec914810a88f00a96a70fc88",
      "startTime": "2013-11-09",
      "endTime": "2013-12-11",
      "gapTolerances": []
    }
  }')

  dvData <- repgen:::parseDVData(onlyStat1)
  noIVsData <- repgen:::parseDVData(noIVs)

  expect_is(dvData, 'list')
  expect_is(dvData$dvData, 'list')
  expect_is(dvData$dvInfo, 'list')
  expect_is(noIVsData, 'list')
  expect_is(noIVsData$dvData, 'list')
  expect_is(noIVsData$dvInfo, 'list')

  expect_equal(length(dvData), 2)
  expect_equal(length(dvData$dvInfo), 2)
  expect_equal(nrow(dvData$dvData$stat1Timeseries), 1)
  expect_equal(dvData$dvData$stat1Timeseries$value[1], 66.6)
  expect_equal(dvData$dvData$stat1Timeseries$time, repgen:::flexibleTimeParse("2013-11-10", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(dvData$dvData$appr_approved_uv$x0, repgen:::flexibleTimeParse("2013-11-09T23:59:00", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(dvData$dvData$max_iv$value, 892)
  expect_equal(dvData$dvData$max_iv$time, repgen:::flexibleTimeParse("2013-11-18T12:00:00-05:00", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(length(noIVsData), 2)
  expect_equal(length(noIVsData$dvInfo), 2)
  expect_equal(nrow(noIVsData$dvData$stat1Timeseries), 1)
  expect_equal(noIVsData$dvData$stat1Timeseries$value[1], 66.6)
  expect_equal(noIVsData$dvData$stat1Timeseries$time, repgen:::flexibleTimeParse("2013-11-10", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(noIVsData$dvData$appr_approved_uv$x0, repgen:::flexibleTimeParse("2013-11-09T23:59:00", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(noIVsData$dvData$max_iv, NULL)
})

test_that("parseDVData returns NULL when parsing invalid or valid but empty JSON", {
  noTS <- fromJSON('{
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

  noData <- fromJSON('{}')

  noIVsnoTSData <- parseDVData(noTSnoIVs)
  noTSData <- parseDVData(noTS)
  noData <- parseDVData(noData)

  expect_equal(noIVsnoTSData, NULL)
  expect_equal(noTSData, NULL)
  expect_equal(noData, NULL)
})

test_that("parseDVData handles flags properly", {

})

test_that("getDVHydroTimeSeries properly reads a valid DV time series", {
  valid1 <- fromJSON(' { 
    "firstDownChain": {
      "notes": [],
      "isVolumetricFlow": true,
      "description": "From Aquarius",
      "qualifiers": [
        {
          "startDate": "2013-11-11T00:00:00-05:00",
          "endDate": "2013-11-18T00:00:00-05:00",
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
          "endDate": "2013-11-18T00:00:00-05:00"
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
    },
    "reportMetadata": {
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }
  }')

  valid1Est <- fromJSON(' { 
    "firstDownChain": {
      "notes": [],
      "isVolumetricFlow": true,
      "description": "From Aquarius",
      "qualifiers": [
        {
          "startDate": "2013-11-11T00:00:00-05:00",
          "endDate": "2013-11-18T00:00:00-05:00",
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
          "endDate": "2013-11-18T00:00:00-05:00"
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
    },
    "reportMetadata": {
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }
  }')

  valid1Data <- repgen:::getDVHydroTimeSeries(valid1, "firstDownChain", "downChainDescriptions1", repgen:::fetchReportMetadataField(valid1, 'timezone'))
  valid1EstData <- repgen:::getDVHydroTimeSeries(valid1, "firstDownChain", "downChainDescriptions1", repgen:::fetchReportMetadataField(valid1, 'timezone'), estimated=TRUE)

  expect_is(valid1Data, 'list')
  expect_is(valid1EstData, 'list')

  expect_equal(valid1Data$isDV, TRUE)
  expect_equal(valid1Data$estimated, FALSE)
  expect_equal(nrow(valid1Data$points), 1)
  expect_equal(valid1Data$points$value[1], 66.6)
  expect_equal(valid1Data$points$time[1], repgen:::flexibleTimeParse(valid1$firstDownChain$points$time[1], repgen:::fetchReportMetadataField(valid1, 'timezone'), shiftTimeToNoon=FALSE))
  expect_equal(valid1Data$isVolumetricFlow, TRUE)

  expect_equal(valid1EstData$isDV, TRUE)
  expect_equal(valid1EstData$estimated, TRUE)
  expect_equal(nrow(valid1EstData$points), 2)
  expect_equal(valid1EstData$points$value[1], 66.5)
  expect_equal(valid1EstData$points$time[1], repgen:::flexibleTimeParse(valid1$firstDownChain$points$time[2], repgen:::fetchReportMetadataField(valid1, 'timezone'), shiftTimeToNoon=FALSE))
  expect_equal(valid1EstData$isVolumetricFlow, TRUE)
})

test_that("getDVHydroTimeSeries returns an empty list from reading an invalid TS", {
  invalid <- fromJSON('{
    "firstDownChain": {
      "points": [

      ]
    },
    "secondDownChain": {
      "notes": [],
      "isVolumetricFlow": true,
      "description": "From Aquarius",
      "qualifiers": [
        {
          "startDate": "2013-11-11T00:00:00-05:00",
          "endDate": "2013-11-18T00:00:00-05:00",
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
        }
      ],
      "requestedStartTime": "2013-11-10T00:00:00-05:00",
      "requestedEndTime": "2013-12-11T23:59:59.999999999-05:00",
      "estimatedPeriods": [],
      "approvals": [],
      "name": "24eca840ec914810a88f00a96a70fc88",
      "startTime": "2013-11-09",
      "endTime": "2013-12-11",
      "gapTolerances": []
    },
    "reportMetadata": {
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000",
      "excludeZeroNegative": true,
      "timezone": "Etc/GMT+5"
    }
  }')

  invalidTSData <- repgen:::getDVHydroTimeSeries(invalid, "firstDownChain", "downChainDescriptions1", repgen:::fetchReportMetadataField(invalid, 'timezone'))
  missingDescData <- repgen:::getDVHydroTimeSeries(invalid, "secondDownChain", "missingDescriptions", repgen:::fetchReportMetadataField(invalid, 'timezone'))
  missingTSData <- repgen:::getDVHydroTimeSeries(invalid, "missingTS", "missingDescriptions", repgen:::fetchReportMetadataField(invalid, 'timezone'))

  expect_is(invalidTSData, 'list')
  expect_is(missingDescData, 'list')
  expect_is(missingTSData, 'list')

  expect_equal(invalidTSData, list())
  expect_equal(missingDescData, list())
  expect_equal(missingTSData, list())
})

test_that("parseRefDVData behaves the same as parseDVData when loading valid report JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- fromJSON('{
    "readings": [],
    "waterdataUrl": "http://waterdata.usgs.gov/nwis/inventory/?site_no\u003d01054200",
    "fieldVisitMeasurements": [],
    "reportMetadata": {
      "endDate": "2013-12-11T23:59:59.999999999Z",
      "timezone": "Etc/GMT+5",
      "secondDownChain": "",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "description": "Broad brush overviews of a record",
      "title": "DV Hydrograph",
      "thirdDownChain": "",
      "primaryTimeSeries": "b4216f457d474b6b8d6b0c23cbadbfd3",
      "requestId": "DvHydrographChoreographer-4358e078-8d31-4085-84ca-75263f425800",
      "quaternaryReferenceTimeSeries": "",
      "siteNumber": "01054200       ",
      "stationName": "Wild River at Gilead, Maine",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200",
      "primaryDescriptions": "Discharge.ft^3/s@01054200",
      "startDate": "2013-11-10T00:00:00Z",
      "tertiaryReferenceTimeSeries": "",
      "secondaryReferenceTimeSeries": ""
    },
    "secondaryReferenceTimeSeries": {
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
        }
      ],
      "name": "24eca840ec914810a88f00a96a70fc88",
      "startTime": "2013-11-09",
      "endTime": "2013-12-11",
      "gapTolerances": []
    },
    "simsUrl": "http://sims.water.usgs.gov/SIMSClassic/StationInfo.asp?site_no\u003d01054200"
  }')

  dvData <- repgen:::parseRefDVData(onlyStat1, "secondaryReferenceTimeSeries", "downChainDescriptions1")
  noIVsData <- repgen:::parseRefDVData(noIVs, "secondaryReferenceTimeSeries", "downChainDescriptions1")

  expect_is(dvData, 'list')
  expect_is(dvData$refData, 'list')
  expect_is(dvData$refInfo, 'list')

  expect_equal(length(dvData), 2)
  expect_equal(length(dvData$refInfo), 2)
  expect_equal(nrow(dvData$refData$secondaryRefTimeSeries), 1)
  expect_equal(dvData$refData$secondaryRefTimeSeries$value[1], 66.6)
  expect_equal(dvData$refData$secondaryRefTimeSeries$time, repgen:::flexibleTimeParse("2013-11-10", "Etc/GMT+5", shiftTimeToNoon=FALSE))
  expect_equal(dvData$refData$appr_approved_uv$x0, repgen:::flexibleTimeParse("2013-11-09T23:59:00", "Etc/GMT+5", shiftTimeToNoon=FALSE))
})

test_that("parseDVData returns NULL when parsing invalid or valid but empty JSON", {
  noTS <- fromJSON('{
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

  noData <- fromJSON('{}')

  noData <- repgen:::parseRefDVData(noTS, "secondaryReferenceTimeSeries", "downChainDescriptions1")
  noTSData <- repgen:::parseRefDVData(noTSnoIVs, "secondaryReferenceTimeSeries", "downChainDescriptions1")
  noIVsnoTSData <- repgen:::parseRefDVData(noData, "secondaryReferenceTimeSeries", "downChainDescriptions1")

  expect_equal(noIVsnoTSData, NULL)
  expect_equal(noTSData, NULL)
  expect_equal(noData, NULL)
})

test_that("getEstimatedEdges properly creates vertical edge lines between estimated and non-estimated time series", {
  estPoints <- data.frame(value=c(1,2,3,2), time=c(as.POSIXct("2017-01-01 Etc/GMT+5"), as.POSIXct("2017-01-03 Etc/GMT+5"), as.POSIXct("2017-01-04 Etc/GMT+5"), as.POSIXct("2017-01-07 Etc/GMT+5")))
  statPoints <- data.frame(value=c(3,2,2,3), time=c(as.POSIXct("2017-01-02 Etc/GMT+5"), as.POSIXct("2017-01-05 Etc/GMT+5"), as.POSIXct("2017-01-06 Etc/GMT+5"), as.POSIXct("2017-01-08 Etc/GMT+5")))
  estEdges <- repgen:::getEstimatedEdges(statPoints, estPoints)

  expect_is(estEdges, 'list')
  
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

test_that("getMinMaxIV returns an empty list when given invalid JSON", { 
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

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv, list())
  expect_equal(min_iv, list())
  expect_equal(max_iv_inv, list())
  expect_equal(min_iv_inv, list())
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
