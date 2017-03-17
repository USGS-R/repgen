context("correctionsataglance tests")
wd <- getwd()
setwd(dir = tempdir())
currentTZ <- Sys.getenv('TZ')
Sys.setenv(TZ='Etc/GMT+5')

context("testing correctionsataglance")
test_that("correctionsataglance examples work",{
  library(jsonlite)
  library(lubridate)
  library(dplyr)
  library(gsplot)
  
  data <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data, 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example2.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data2, 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example3.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data3, 'Author Name'), 'character')
  
  data4 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example4.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data4, 'Author Name'), 'character')
  
  data5 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example5.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data5, 'Author Name'), 'character')
  
  data6 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example6.json', package = 'repgen'))
  expect_is(repgen:::correctionsataglance(data6, 'Author Name'), 'character')
})

test_that("correctionsataglance duplicate legend values are removed",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example.json', package = 'repgen'))
  corr_results <- repgen:::correctionsataglanceReport(data)
  corr_plot <- corr_results$timeline
  
  all_legend_names <- corr_plot$legend$legend.auto$legend
  
  expect_equal(anyDuplicated(all_legend_names), 0)
})

#Data Functions
test_that("calcStartSeq properly calculates the sequence of month start dates", {
  timezone <- "Etc/GMT+5"
  startDate1 <- repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)
  startDate2 <- repgen:::flexibleTimeParse("2017-01-02T12:01:00", timezone)
  endDate1 <- repgen:::flexibleTimeParse("2017-01-08T00:12:13", timezone)
  endDate2 <- repgen:::flexibleTimeParse("2017-03-08T00:12:13", timezone)

  startTime1 <- hours(hour(startDate1)) + minutes(minute(startDate1)) + seconds(second(startDate1))
  startTime2 <- hours(hour(startDate2)) + minutes(minute(startDate2)) + seconds(second(startDate2))

  monthDate2 <- repgen:::flexibleTimeParse("2017-02-01", timezone, shiftTimeToNoon=FALSE)
  monthDate3 <- repgen:::flexibleTimeParse("2017-03-01", timezone, shiftTimeToNoon=FALSE)

  startSeq1 <- repgen:::calcStartSeq(startDate1, endDate1)
  startSeq2 <- repgen:::calcStartSeq(startDate1, endDate2)
  startSeq3 <- repgen:::calcStartSeq(startDate2, endDate1)
  startSeq4 <- repgen:::calcStartSeq(startDate2, endDate2)

  expect_equal(length(startSeq1), 1)
  expect_equal(length(startSeq2), 3)
  expect_equal(length(startSeq3), 1)
  expect_equal(length(startSeq4), 3)

  expect_equal(startSeq1, startDate1)
  expect_equal(as.numeric(startSeq2), unlist(list(startDate1, monthDate2+startTime1, monthDate3+startTime1)))
  expect_equal(startSeq3, startDate2)
  expect_equal(startSeq4, c(startDate2, monthDate2+startTime2, monthDate3+startTime2))
})

test_that("calcEndSeq properly calculates the sequence of month end dates", {
  timezone <- "Etc/GMT+5"
  startDate1 <- repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)
  startDate2 <- repgen:::flexibleTimeParse("2017-01-02T12:01:00", timezone)
  endDate1 <- repgen:::flexibleTimeParse("2017-01-08T00:12:13", timezone)
  endDate2 <- repgen:::flexibleTimeParse("2017-03-08T00:12:13", timezone)

  startTime1 <- hours(hour(startDate1)) + minutes(minute(startDate1)) + seconds(second(startDate1))
  startTime2 <- hours(hour(startDate2)) + minutes(minute(startDate2)) + seconds(second(startDate2))

  monthDate2 <- repgen:::flexibleTimeParse("2017-02-01", timezone, shiftTimeToNoon=FALSE)
  monthDate3 <- repgen:::flexibleTimeParse("2017-03-01", timezone, shiftTimeToNoon=FALSE)
  
  startSeq1 <- repgen:::calcStartSeq(startDate1, endDate1)
  startSeq2 <- repgen:::calcStartSeq(startDate1, endDate2)
  startSeq3 <- repgen:::calcStartSeq(startDate2, endDate1)
  startSeq4 <- repgen:::calcStartSeq(startDate2, endDate2)
  
  endSeq1 <- repgen:::calcEndSeq(startSeq1, endDate1)
  endSeq2 <- repgen:::calcEndSeq(startSeq2, endDate2)
  endSeq3 <- repgen:::calcEndSeq(startSeq3, endDate1)
  endSeq4 <- repgen:::calcEndSeq(startSeq4, endDate2)

  expect_equal(length(endSeq1), 1)
  expect_equal(length(endSeq2), 3)
  expect_equal(length(endSeq3), 1)
  expect_equal(length(endSeq4), 3)

  expect_equal(as.numeric(endSeq1), as.numeric(endDate1))
  expect_equal(as.numeric(endSeq2), unlist(list(monthDate2+startTime1, monthDate3+startTime1, endDate2)))
  expect_equal(as.numeric(endSeq3), as.numeric(endDate1))
  expect_equal(as.numeric(endSeq4), as.numeric(c(monthDate2+startTime2, monthDate3+startTime2, endDate2)))
})

test_that("labelDateSeq properly calculates the label dates", {
  timezone <- "Etc/GMT+5"
  startDate1 <- repgen:::flexibleTimeParse("2016-12-28T12:12:13", timezone)
  startDate2 <- repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)
  startDate3 <- repgen:::flexibleTimeParse("2017-01-02T12:01:00", timezone)

  endDate1 <- repgen:::flexibleTimeParse("2017-01-08T00:12:13", timezone)
  endDate2 <- repgen:::flexibleTimeParse("2017-01-08T00:12:13", timezone)
  endDate3 <- repgen:::flexibleTimeParse("2017-03-08T00:12:13", timezone)

  startTime1 <- hours(hour(startDate1)) + minutes(minute(startDate1)) + seconds(second(startDate1))
  startTime2 <- hours(hour(startDate2)) + minutes(minute(startDate2)) + seconds(second(startDate2))
  startTime3 <- hours(hour(startDate3)) + minutes(minute(startDate3)) + seconds(second(startDate3))

  monthDate2 <- repgen:::flexibleTimeParse("2017-02-01", timezone, shiftTimeToNoon=FALSE)
  monthDate3 <- repgen:::flexibleTimeParse("2017-03-01", timezone, shiftTimeToNoon=FALSE)

  numDays1 <- 365
  numDays2 <- repgen:::calculateTotalDays(startDate3, endDate3)
  numDays3 <- 365

  startSeq1 <- repgen:::calcStartSeq(startDate1, endDate1)
  startSeq2 <- repgen:::calcStartSeq(startDate2, endDate2)
  startSeq3 <- repgen:::calcStartSeq(startDate3, endDate3)

  endSeq1 <- repgen:::calcEndSeq(startSeq1, endDate1)
  endSeq2 <- repgen:::calcEndSeq(startSeq2, endDate2)
  endSeq3 <- repgen:::calcEndSeq(startSeq3, endDate3)

  labelSeq1 <- repgen:::labelDateSeq(startSeq1, endSeq1, numDays1)
  labelSeq2 <- repgen:::labelDateSeq(startSeq2, endSeq2, numDays2)
  labelSeq3 <- repgen:::labelDateSeq(startSeq3, endSeq3, numDays3)

  expect_equal(length(labelSeq1), 2)
  expect_equal(length(labelSeq2), 1)
  expect_equal(length(labelSeq3), 3)

  expect_equal(all(is.na(labelSeq1)), TRUE)
  expect_equal(as.numeric(labelSeq2), c(as.numeric(startDate2)))
  expect_equal(as.numeric(labelSeq3), as.numeric(c(startDate3, monthDate2+startTime3, NA)))
})

test_that("findOverlap properly identifies overlapping regions in data", {
  timezone <- "Etc/GMT+5"

  dataList1 <- list(
    preData = list(
      startDates = c(
        repgen:::flexibleTimeParse("2016-12-28T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)
      ),
      endDates = c(
        repgen:::flexibleTimeParse("2016-12-29T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone)
      ),
      applyDates = c(
        repgen:::flexibleTimeParse("2017-02-06T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-02-07T12:12:13", timezone)
      ),
      corrLabel = c(
        "Test",
        "Test"
      )
    ),
    normalData = list(
      startDates = c(
        repgen:::flexibleTimeParse("2016-12-28T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)
      ),
      endDates = c(
        repgen:::flexibleTimeParse("2017-01-04T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone)
      )
    ),
    thresholdData = list(
      startDates = c(
        repgen:::flexibleTimeParse("2016-12-28T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-05T12:12:13", timezone)
      ),
      endDates = c(
        repgen:::flexibleTimeParse("2017-01-04T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-07T12:12:13", timezone)
      )
    ),
    notesData = list(
      startDates = c(
        repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-05T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-06T12:12:13", timezone)
      ),
      endDates = c(
        repgen:::flexibleTimeParse("2017-01-04T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-06T14:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-07T12:12:13", timezone)
      )
    )
  )

  dataList2 <- list(
    notesData = list(
      startDates = c(
        repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-05T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-03T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-06T12:12:13", timezone)
      ),
      endDates = c(
        repgen:::flexibleTimeParse("2017-01-05T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-06T12:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-07T14:12:13", timezone),
        repgen:::flexibleTimeParse("2017-01-08T12:12:13", timezone)
      )
    )
  )

  dataList3 <- list()

  dataList4 <- list(
    preData <- list(),
    normalData <- list(),
    postData <- list(
      startDates <- NULL,
      endDates <- NULL,
      applyDates <- NULL,
      corrLabel <- NULL
    )
  )

  overlap1 <- repgen:::findOverlap(dataList1)
  overlap2 <- repgen:::findOverlap(dataList2)
  overlap3 <- repgen:::findOverlap(dataList3)
  overlap4 <- repgen:::findOverlap(dataList4)

  expect_is(overlap1, 'list')
  expect_is(overlap2, 'list')
  expect_is(overlap3, 'list')
  expect_is(overlap4, 'list')

  expect_equal(overlap1$totalNewLines, 3)
  expect_equal(overlap2$totalNewLines, 1)
  expect_equal(overlap3$totalNewLines, 0)
  expect_equal(overlap4$totalNewLines, 0)

  expect_is(overlap1$dataShiftInfo, 'list')
  expect_is(overlap2$dataShiftInfo, 'list')
  expect_is(overlap3$dataShiftInfo, 'list')
  expect_is(overlap4$dataShiftInfo, 'list')

  shifts1 <- overlap1$dataShiftInfo
  shifts2 <- overlap2$dataShiftInfo
  shifts3 <- overlap3$dataShiftInfo
  shifts4 <- overlap4$dataShiftInfo

  expect_equal(length(shifts1), 4)
  expect_equal(length(shifts2), 1)

  expect_equal(shifts1$preData, NULL)
  expect_equal(shifts1$normalData$rectToShift, 2)
  expect_equal(shifts1$normalData$lineNum, 2)
  expect_equal(shifts1$normalData$numNewLines, 1)
  expect_equal(shifts1$thresholdData$rectToShift, 2)
  expect_equal(shifts1$thresholdData$lineNum, 2)
  expect_equal(shifts1$thresholdData$numNewLines, 1)
  expect_equal(shifts1$notesData$rectToShift, 3)
  expect_equal(shifts1$notesData$lineNum, 2)
  expect_equal(shifts1$notesData$numNewLines, 1)
  
  expect_equal(shifts2$notesData$rectToShift, 3)
  expect_equal(shifts2$notesData$lineNum, 2)
  expect_equal(shifts2$notesData$numNewLines, 1)

  expect_equal(shifts3, list())

  expect_equal(shifts4[[1]], NULL)
  expect_equal(shifts4[[2]], NULL)
  expect_equal(shifts4[[3]], NULL)
})

test_that("parseCorrApprovals properly formats approvals for the CORR report", {
  timezone <- "Etc/GMT+5"

  dateSeq <- c(
    repgen:::flexibleTimeParse("2016-12-01T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone)
  )
  
  timeSeries1 <- fromJSON('{
    "approvals":[
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by gwilson.",
        "dateApplied": "2016-05-19T16:26:58.2093803Z",
        "startTime": "2017-01-01T12:12:13",
        "endTime": "2017-02-01T12:12:13"
      }
    ]
  }')
  timeSeries2 <- fromJSON('{
    "approvals":[]
  }')
  timeSeries3 <- list()
  timeSeries4 <- NULL

  approvals1 <- repgen:::parseCorrApprovals(timeSeries1, timezone, dateSeq)
  approvals2 <- repgen:::parseCorrApprovals(timeSeries2, timezone, dateSeq)
  approvals3 <- repgen:::parseCorrApprovals(timeSeries3, timezone, dateSeq)
  approvals4 <- repgen:::parseCorrApprovals(timeSeries4, timezone, dateSeq)

  expect_is(approvals1, 'list')
  expect_is(approvals2, 'list')
  expect_is(approvals3, 'list')
  expect_is(approvals4, 'list')

  expect_equal(approvals1$startDates, repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone))
  expect_equal(approvals1$endDates, repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone))
  expect_equal(approvals1$type, "Approval: Approved")
  expect_equal(approvals1$colors, "#228B22")
  expect_equal(approvals1$approvalLabel, c("12/2016", "01/2017", "02/2017"))
  expect_equal(approvals2, list())
  expect_equal(approvals3, list())
  expect_equal(approvals4, list())
})

test_that("parseCorrThresholds properly formats threshold data for the CORR report", {
  timezone <- "Etc/GMT+5"
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

  emptyJSON <- fromJSON('{
    "thresholds": []
  }')

  invalidJSON <- fromJSON('{}')

  thresholds1 <- repgen:::parseCorrThresholds(thresholdJSON, timezone)
  thresholds2 <- repgen:::parseCorrThresholds(emptyJSON, timezone)
  thresholds3 <- repgen:::parseCorrThresholds(invalidJSON, timezone)

  expect_is(thresholds1, 'list')
  expect_is(thresholds2, 'list')
  expect_is(thresholds3, 'list')

  expect_equal(length(thresholds1), 3)
  expect_equal(length(thresholds2), 0)
  expect_equal(length(thresholds3), 0)
  
  expect_equal(as.numeric(thresholds1$startDates), as.numeric(c(
    repgen:::flexibleTimeParse("2000-01-01T00:00:00Z", timezone),
    repgen:::flexibleTimeParse("2015-06-02T00:00:00Z", timezone),
    repgen:::flexibleTimeParse("0001-01-01T00:00:00Z", timezone)
  )))

  expect_equal(as.numeric(thresholds1$endDates), as.numeric(c(
    repgen:::flexibleTimeParse("2015-05-31T23:59:59.9999999Z", timezone),
    repgen:::flexibleTimeParse("9999-05-31T23:59:59.9999999Z", timezone),
    repgen:::flexibleTimeParse("9999-12-31T23:59:59.9999999Z", timezone)
  )))

  expect_equal(thresholds1$metaLabel, c("AQUARIUS only | ThresholdAbove 4000 | Suppress: TRUE", "AQUARIUS only | ThresholdAbove 1234 | Suppress: TRUE", "AQUARIUS only | ThresholdBelow 0 | Suppress: TRUE"))
})

test_that("parseCorrQualifiers properly formats qualifier data for the CORR report", {
  timezone <- "Etc/GMT+5"

  timeSeries1 <- fromJSON('{
    "qualifiers": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-02-01T12:12:13",
        "identifier": "ESTIMATED",
        "code": "E",
        "appliedBy": "admin",
        "dateApplied": "2016-03-10T00:49:11.5961786Z"
      }
    ]
  }')
  timeSeries2 <- fromJSON('{
    "qualifiers":[]
  }')
  timeSeries3 <- list()
  timeSeries4 <- NULL

  quals1 <- repgen:::parseCorrQualifiers(timeSeries1, timezone)
  quals2 <- repgen:::parseCorrQualifiers(timeSeries2, timezone)
  quals3 <- repgen:::parseCorrQualifiers(timeSeries3, timezone)
  quals4 <- repgen:::parseCorrQualifiers(timeSeries4, timezone)

  expect_is(quals1, 'list')
  expect_is(quals2, 'list')
  expect_is(quals3, 'list')
  expect_is(quals4, 'list')

  expect_equal(quals1$startDates, repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone))
  expect_equal(quals1$endDates, repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone))
  expect_equal(quals1$metaLabel, "ESTIMATED")
  expect_equal(quals2, list())
  expect_equal(quals3, list())
  expect_equal(quals4, list())
})

test_that("parseCorrGrades properly formats grade data for the CORR report", {
  timezone <- "Etc/GMT+5"

  timeSeries1 <- fromJSON('{
    "grades": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-02-01T12:12:13",
        "code": "0"
      }
    ]
  }')
  timeSeries2 <- fromJSON('{
    "grades":[]
  }')
  timeSeries3 <- list()
  timeSeries4 <- NULL

  grades1 <- repgen:::parseCorrGrades(timeSeries1, timezone)
  grades2 <- repgen:::parseCorrGrades(timeSeries2, timezone)
  grades3 <- repgen:::parseCorrGrades(timeSeries3, timezone)
  grades4 <- repgen:::parseCorrGrades(timeSeries4, timezone)

  expect_is(grades1, 'list')
  expect_is(grades2, 'list')
  expect_is(grades3, 'list')
  expect_is(grades4, 'list')

  expect_equal(grades1$startDates, repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone))
  expect_equal(grades1$endDates, repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone))
  expect_equal(grades1$metaLabel, "Grade 0")
  expect_equal(grades2, list())
  expect_equal(grades3, list())
  expect_equal(grades4, list())
})

test_that("parseCorrNotes properly formats notes data for the CORR report", {
  timezone <- "Etc/GMT+5"

  timeSeries1 <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-02-01T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')
  timeSeries2 <- fromJSON('{
    "notes":[]
  }')
  timeSeries3 <- list()
  timeSeries4 <- NULL

  notes1 <- repgen:::parseCorrNotes(timeSeries1, timezone)
  notes2 <- repgen:::parseCorrNotes(timeSeries2, timezone)
  notes3 <- repgen:::parseCorrNotes(timeSeries3, timezone)
  notes4 <- repgen:::parseCorrNotes(timeSeries4, timezone)

  expect_is(notes1, 'list')
  expect_is(notes2, 'list')
  expect_is(notes3, 'list')
  expect_is(notes4, 'list')

  expect_equal(notes1$startDates, repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone))
  expect_equal(notes1$endDates, repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone))
  expect_equal(notes1$metaLabel, "ADAPS Source Flag: *")
  expect_equal(notes2, list())
  expect_equal(notes3, list())
  expect_equal(notes4, list())
})

test_that("parseCorrProcessingCorrections properly formats processing order corrections for the CORR report", {
  corrJSON1 <- fromJSON('{
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
  corrJSON2 <- fromJSON('{
    "corrections":{}
  }')
  corrJSON3 <- fromJSON('{
    "corrections":{
      "normal": [],
      "preProecssing": [],
      "postProcessing": []
    }
  }')
  corrJSON4 <- list()
  corrJSON5 <- NULL
  timezone <- "Etc/GMT+5"


  preData <- repgen:::parseCorrProcessingCorrections(corrJSON1, "pre", timezone)
  normalData <- repgen:::parseCorrProcessingCorrections(corrJSON1, "normal", timezone)
  postData <- repgen:::parseCorrProcessingCorrections(corrJSON1, "post", timezone)

  testData1 <- repgen:::parseCorrProcessingCorrections(corrJSON1, "invalid", timezone)
  testData2 <- repgen:::parseCorrProcessingCorrections(corrJSON2, "pre", timezone)
  testData3 <- repgen:::parseCorrProcessingCorrections(corrJSON3, "pre", timezone)
  testData4 <- repgen:::parseCorrProcessingCorrections(corrJSON4, "pre", timezone)
  testData5 <- repgen:::parseCorrProcessingCorrections(corrJSON5, "pre", timezone)

  expect_is(preData, 'list')
  expect_is(normalData, 'list')
  expect_is(postData, 'list')
  expect_is(testData1, 'list')
  expect_is(testData2, 'list')
  expect_is(testData3, 'list')
  expect_is(testData4, 'list')
  expect_is(testData5, 'list')

  expect_equal(length(preData), 4)
  expect_equal(length(normalData), 4)
  expect_equal(length(postData), 4)
  expect_equal(length(testData1), 4)
  expect_equal(length(testData2), 4)
  expect_equal(length(testData3), 4)
  expect_equal(length(testData4), 4)
  expect_equal(length(testData5), 4)

  expect_equal(preData$startDates, repgen:::flexibleTimeParse("2015-03-30T11:00:00-06:00", timezone))
  expect_equal(preData$endDates, repgen:::flexibleTimeParse("2015-05-08T10:15:00-06:00", timezone))
  expect_equal(as.numeric(preData$applyDates), as.numeric(repgen:::flexibleTimeParse("2015-06-10T18:08:11Z", timezone)))
  expect_equal(preData$corrLabel, "USGS_MULTI_POINT")

  expect_equal(normalData$startDates, repgen:::flexibleTimeParse("2015-11-09T14:15:00-06:00", timezone))
  expect_equal(normalData$endDates, repgen:::flexibleTimeParse("2015-11-09T14:20:00-06:00", timezone))
  expect_equal(as.numeric(normalData$applyDates), as.numeric(repgen:::flexibleTimeParse("2015-12-08T15:32:33Z", timezone)))
  expect_equal(normalData$corrLabel, "USGS_MULTI_POINT")

  expect_equal(postData$startDates, repgen:::flexibleTimeParse("2014-12-10T00:00:00-06:00", timezone))
  expect_equal(postData$endDates, repgen:::flexibleTimeParse("2015-01-29T00:00:00-06:00", timezone))
  expect_equal(as.numeric(postData$applyDates), as.numeric(repgen:::flexibleTimeParse("2016-03-09T21:27:53.2181786Z", timezone)))
  expect_equal(postData$corrLabel, "COPY_PASTE")

  expect_equal(unlist(testData1), NULL)
  expect_equal(unlist(testData2), NULL)
  expect_equal(unlist(testData3), NULL)
  expect_equal(unlist(testData4), NULL)
  expect_equal(unlist(testData5), NULL)
})

test_that("getLaneYData properly calculates the Y position data for lanes", {
  timezone <- "Etc/GMT+5"

  laneJSON1 <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-02T12:12:13",
        "endDate": "2017-01-04T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-04T12:12:13",
        "endDate": "2017-02-07T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-05T12:12:13",
        "endDate": "2017-02-06T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')

  laneJSON2 <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')

  laneHeight <- 10
  initialHeight <- 100
  laneData1 <- repgen:::parseCorrNotes(laneJSON1, timezone)
  laneData2 <- repgen:::parseCorrNotes(laneJSON2, timezone)
  overlap <- repgen:::findOverlap(list(laneData1=laneData1, laneData2=laneData2))
  yData1 <- repgen:::getLaneYData(laneData1, laneHeight, initialHeight, overlapInfo=overlap$dataShiftInfo[[1]])
  yData2 <- repgen:::getLaneYData(laneData2, laneHeight, initialHeight)

  expect_is(yData1, 'list')
  expect_is(yData2, 'list')
  expect_equal(yData1$laneYTop, c(100, 90, 100, 90))
  expect_equal(yData1$laneYBottom, c(90, 80, 90, 80))
  expect_equal(yData1$laneNameYPos, 90)
  expect_equal(yData2$laneYTop, c(100))
  expect_equal(yData2$laneYBottom, c(90))
  expect_equal(yData2$laneNameYPos, 95)
})

test_that("isTextLong properly calculates whether text can fit within a correction rectangle", {
  timezone <- "Etc/GMT+5"
  
  dateRange1 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-01T00:00:00", timezone))
  dateRange3 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-07-01T00:00:00", timezone))
  dateRange2 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2018-01-01T00:00:00", timezone))

  startDate <- repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone)
  
  endDate1 <- repgen:::flexibleTimeParse("2017-01-01T01:00:00", timezone)
  endDate2 <- repgen:::flexibleTimeParse("2017-01-02T00:00:00", timezone)
  endDate3 <- repgen:::flexibleTimeParse("2017-04-01T00:00:00", timezone)

  labelText1 <- "Test"
  labelText2 <- "Long Long Long Test"

  totalDays1 <- repgen:::calculateTotalDays(dateRange1[[1]], dateRange1[[2]])
  totalDays2 <- repgen:::calculateTotalDays(dateRange2[[1]], dateRange2[[2]])
  totalDays3 <- repgen:::calculateTotalDays(dateRange3[[1]], dateRange3[[2]])

  expect_true(repgen:::isTextLong(labelText1, dateRange1, startDate, endDate1))
  expect_true(!repgen:::isTextLong(labelText1, dateRange1, startDate, endDate2, totalDays1))
  expect_true(!repgen:::isTextLong(labelText1, dateRange1, startDate, endDate3))
  
  expect_true(repgen:::isTextLong(labelText1, dateRange2, startDate, endDate1))
  expect_true(repgen:::isTextLong(labelText1, dateRange2, startDate, endDate2))
  expect_true(!repgen:::isTextLong(labelText1, dateRange2, startDate, endDate3, totalDays2))
  
  expect_true(repgen:::isTextLong(labelText1, dateRange3, startDate, endDate1, totalDays3))
  expect_true(repgen:::isTextLong(labelText1, dateRange3, startDate, endDate2))
  expect_true(!repgen:::isTextLong(labelText1, dateRange3, startDate, endDate3))
  
  expect_true(repgen:::isTextLong(labelText2, dateRange1, startDate, endDate1))
  expect_true(repgen:::isTextLong(labelText2, dateRange1, startDate, endDate2))
  expect_true(!repgen:::isTextLong(labelText2, dateRange1, startDate, endDate3))
  
  expect_true(repgen:::isTextLong(labelText2, dateRange2, startDate, endDate1))
  expect_true(repgen:::isTextLong(labelText2, dateRange2, startDate, endDate2))
  expect_true(!repgen:::isTextLong(labelText2, dateRange2, startDate, endDate3))
  
  expect_true(repgen:::isTextLong(labelText2, dateRange3, startDate, endDate1))
  expect_true(repgen:::isTextLong(labelText2, dateRange3, startDate, endDate2))
  expect_true(!repgen:::isTextLong(labelText2, dateRange3, startDate, endDate3))
})

test_that("findTextLocations properly calculates the text locations for the plot labels", {
  timezone <- "Etc/GMT+5"
  startDate <- repgen:::flexibleTimeParse("2017-01-01T12:00:00", timezone)
  endDate <- repgen:::flexibleTimeParse("2017-03-01T12:00:00", timezone)
  dateRange <- c(startDate, endDate)
  startSeq <- repgen:::calcStartSeq(dateRange[[1]], dateRange[[2]])
  endSeq <- repgen:::calcEndSeq(startSeq, dateRange[[2]])
  dateSeq <- repgen:::labelDateSeq(startSeq, endSeq, repgen:::calculateTotalDays(dateRange[[1]], dateRange[[2]]))
  yTop <- 100
  yBottom <- 90

  pos1 <- repgen:::findTextLocations(list(startDates=startDate, endDates=endDate), yTop, yBottom)
  pos2 <- repgen:::findTextLocations(list(startSeq=startSeq, endSeq=endSeq), yTop, yBottom, isDateData=TRUE)

  expect_is(pos1, 'list')
  expect_is(pos2, 'list')
  
  expect_equal(as.numeric(pos1$x), as.numeric(repgen:::flexibleTimeParse("2017-01-30T24:00:00", timezone)))
  expect_equal(as.numeric(pos2$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-16T24:00:00", timezone),
    repgen:::flexibleTimeParse("2017-02-15T12:00:00", timezone)
  )))
  
  expect_equal(pos1$y, 95)
  expect_equal(pos2$y, c(95,95))
})

test_that("getLaneLabelData properly calculates the label positon data for each lane", {
  timezone <- "Etc/GMT+5"

  dateRange <- c(repgen:::flexibleTimeParse("2016-12-29T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-10T00:00:00", timezone))

  laneJSON1 <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-02T12:12:13",
        "endDate": "2017-01-04T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-04T12:12:13",
        "endDate": "2017-02-07T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-05T12:12:13",
        "endDate": "2017-02-06T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')

  laneJSON2 <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')

  startSeq <- repgen:::calcStartSeq(dateRange[[1]], dateRange[[2]])
  endSeq <- repgen:::calcEndSeq(startSeq, dateRange[[2]])
  dateSeq <- repgen:::labelDateSeq(startSeq, endSeq, repgen:::calculateTotalDays(dateRange[[1]], dateRange[[2]]))
  
  timeSeries1 <- fromJSON('{
    "approvals":[
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by gwilson.",
        "dateApplied": "2016-05-19T16:26:58.2093803Z",
        "startTime": "2017-01-01T12:12:13",
        "endTime": "2017-02-01T12:12:13"
      }
    ]
  }')

  laneHeight <- 10
  initialHeight <- 100
  laneData1 <- repgen:::parseCorrNotes(laneJSON1, timezone)
  laneData2 <- repgen:::parseCorrNotes(laneJSON2, timezone)
  laneData3 <- repgen:::parseCorrApprovals(timeSeries1, timezone, dateSeq)
  laneData3 <- c(list(startSeq=startSeq, endSeq=endSeq), laneData3)
  overlap <- repgen:::findOverlap(list(laneData1=laneData1, laneData2=laneData2))
  yData1 <- repgen:::getLaneYData(laneData1, laneHeight, initialHeight, overlapInfo=overlap$dataShiftInfo[[1]])
  yData2 <- repgen:::getLaneYData(laneData2, laneHeight, initialHeight)
  yData3 <- repgen:::getLaneYData(laneData3, laneHeight, initialHeight)
  labels1 <- repgen:::getLaneLabelData(laneData1, yData1[['laneYTop']], yData1[['laneYBottom']], dateRange)
  labels2 <- repgen:::getLaneLabelData(laneData2, yData2[['laneYTop']], yData2[['laneYBottom']], dateRange)
  labels3 <- repgen:::getLaneLabelData(laneData3, yData3[['laneYTop']], yData3[['laneYBottom']], dateRange, isDateData=TRUE)

  expect_is(labels1, 'data.frame')
  expect_is(labels2, 'data.frame')
  expect_is(labels3, 'data.frame')

  expect_equal(nrow(labels1), 4)
  expect_equal(nrow(labels2), 1)
  expect_equal(nrow(labels3), 4)

  if(nrow(labels3) < 4){
    print(labels3)
    print(laneData3)
    print(yData3[['laneYTop']])
    print(yData3[['laneYBottom']])
  }

  expect_equal(labels1$text, c("ADAPS Source Flag: *", "ADAPS Source Flag: *", "ADAPS Source Flag: *", "ADAPS Source Flag: *"))
  expect_equal(labels2$text, c("ADAPS Source Flag: *"))
  expect_equal(labels3$text, c("12/2016", "01/2017", "02/2017", "03/2017"))

  expect_equal(as.numeric(labels1$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone), 
    repgen:::flexibleTimeParse("2017-01-03T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-01-21T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-01-21T12:12:13", timezone)
  )))

  expect_equal(as.numeric(labels2$x), as.numeric(repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone)))

  expect_equal(as.numeric(labels3$x), as.numeric(c(
    repgen:::flexibleTimeParse("2016-12-30T12:00:00", timezone), 
    repgen:::flexibleTimeParse("2017-01-16T12:00:00", timezone),
    repgen:::flexibleTimeParse("2017-02-15T00:00:00", timezone),
    repgen:::flexibleTimeParse("2017-03-05T12:00:00", timezone)
  )))

  expect_equal(labels1$y, c(95,85,95,85))
  expect_equal(labels2$y, c(95))
  expect_equal(labels3$y, c(95,95,95,95))

  expect_equal(labels1$shift, c(TRUE,TRUE,FALSE,FALSE))
  expect_equal(labels2$shift, c(TRUE))
  expect_equal(labels3$shift, c(FALSE,FALSE,FALSE,FALSE))
})

test_that("boundLaneDates properly creates bounds around lane data to prevent SVG rendering issues", {
  timezone <- "Etc/GMT+5"
  dateRange <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  start1 <- c(repgen:::flexibleTimeParse("0000-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  start2 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("9999-03-09T00:00:00", timezone))
  start3 <- c(repgen:::flexibleTimeParse("0000-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("9999-03-09T00:00:00", timezone))
  end1 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  end2 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  end3 <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))

  fixed1 <- repgen:::boundLaneDates(start1, end1, dateRange)
  fixed2 <- repgen:::boundLaneDates(start2, end2, dateRange)
  fixed3 <- repgen:::boundLaneDates(start3, end3, dateRange)

  expect_equal(fixed1$startDates, c(dateRange[[1]]-days(1), dateRange[[2]]))
  expect_equal(fixed2$startDates, c(dateRange[[1]], dateRange[[2]]+days(1)))
  expect_equal(fixed3$startDates, c(dateRange[[1]]-days(1), dateRange[[2]]+days(1)))

  expect_equal(fixed1$endDates, end1)
  expect_equal(fixed2$endDates, end2)
  expect_equal(fixed3$endDates, end3)
})

test_that("splitShiftedLabels properly moves shifted labels out of lane data and into a table", {
  timezone <- "Etc/GMT+5"
  inputLabels <- data.frame(
    text = c("Test1", "Test2", "Test3"),
    x = c(
      repgen:::flexibleTimeParse("2017-01-01T12:00:00", timezone), 
      repgen:::flexibleTimeParse("2017-01-17T12:00:00", timezone),
      repgen:::flexibleTimeParse("2017-02-15T24:00:00", timezone)
    ),
    y = c(90, 80, 80),
    shift = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  shiftedLabels <- repgen:::splitShiftedLabels(inputLabels, 0)

  expect_is(shiftedLabels, 'list')
  expect_equal(length(shiftedLabels), 3)
  expect_equal(shiftedLabels$tableLabels, c("Test1", "Test3"))
  expect_equal(shiftedLabels$labels$text, c("1", "Test2", "2"))
  expect_equal(shiftedLabels$labels$x, inputLabels$x)
  expect_equal(shiftedLabels$labels$shift, c(TRUE, FALSE, TRUE))
  expect_equal(shiftedLabels$endLabelIndex, 2)
})

test_that("createLane properly creates a plot lane from the provided data", {
  timezone <- "Etc/GMT+5"
  dateRange <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  laneJSON <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "0000-01-01T12:12:13",
        "endDate": "9999-01-03T12:12:13",
        "note": "FOR EVER AND EVER"
      }
    ]
  }')
  noteData <- repgen:::parseCorrNotes(laneJSON, timezone)
  height <- 10
  initialHeight <- 100
  bgColor <- "white"

  laneData <- repgen:::createLane(noteData, height, initialHeight, dateRange, bgColor, laneName="test")

  expect_is(laneData, 'list')
  expect_equal(length(laneData), 9)
  expect_equal(as.numeric(laneData$startDates), as.numeric(c(repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone), dateRange[[1]]-days(1))))
  expect_equal(as.numeric(laneData$endDates), as.numeric(c(repgen:::flexibleTimeParse("2017-01-03T12:12:13", timezone), dateRange[[2]]+days(1))))
  expect_equal(laneData$metaLabel, c("ADAPS Source Flag: *", "FOR EVER AND EVER"))
  expect_equal(laneData$laneYTop, c(100,100))
  expect_equal(laneData$laneYBottom, c(90,90))
  expect_equal(laneData$laneNameYPos, 95)
  expect_equal(laneData$laneName, "test")
  expect_equal(laneData$bgColor, "white")
  expect_equal(laneData$labels$text, c("ADAPS Source Flag: *", "FOR EVER AND EVER"))
  expect_equal(as.numeric(laneData$labels$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-02-03T12:00:00", timezone)
  )))
  expect_equal(laneData$labels$y, c(95,95))
  expect_equal(laneData$labels$shift, c(TRUE,FALSE))
})

test_that("createApprovalLane properly creates the approval plot lane", {
  timezone <- "Etc/GMT+5"
  dateRange <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  startSeq <- repgen:::calcStartSeq(dateRange[[1]], dateRange[[2]])
  endSeq <- repgen:::calcEndSeq(startSeq, dateRange[[2]])
  dateSeq <- repgen:::labelDateSeq(startSeq, endSeq, repgen:::calculateTotalDays(dateRange[[1]], dateRange[[2]]))
  timeSeries1 <- fromJSON('{
    "approvals":[
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by gwilson.",
        "dateApplied": "2016-05-19T16:26:58.2093803Z",
        "startTime": "2017-01-01T12:12:13",
        "endTime": "2017-02-01T12:12:13"
      }
    ]
  }')
  approvalData <- repgen:::parseCorrApprovals(timeSeries1, timezone, dateSeq)
  height <- 10
  initialHeight <- 100
  laneData <- repgen:::createApprovalLane(approvalData, height, initialHeight, dateRange, startSeq, endSeq)

  expect_is(laneData, 'list')
  expect_equal(length(laneData), 8)
  expect_equal(as.numeric(laneData$startDates), as.numeric(repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)))
  expect_equal(as.numeric(laneData$endDates), as.numeric(repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone)))
  expect_equal(laneData$type, "Approval: Approved")
  expect_equal(laneData$colors, "#228B22")
  expect_equal(laneData$approvalLabel, c("01/2017", "02/2017", "03/2017"))
  expect_equal(laneData$laneYTop, 100)
  expect_equal(laneData$laneYBottom, 90)
  expect_equal(laneData$labels$text, c("01/2017", "02/2017", "03/2017"))
  expect_equal(as.numeric(laneData$labels$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-16T12:00:00", timezone), 
    repgen:::flexibleTimeParse("2017-02-15T00:00:00", timezone),
    repgen:::flexibleTimeParse("2017-03-05T00:00:00", timezone)
  )))
  expect_equal(laneData$labels$y, c(95,95,95))
  expect_equal(laneData$labels$shift, c(FALSE,FALSE,FALSE))
})

test_that("createPlotLanes properly creates plot lanes for all of the provided data", {
  library(lubridate)
  timezone <- "Etc/GMT+5"
  dateRange <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  startSeq <- repgen:::calcStartSeq(dateRange[[1]], dateRange[[2]])
  endSeq <- repgen:::calcEndSeq(startSeq, dateRange[[2]])
  dateSeq <- repgen:::labelDateSeq(startSeq, endSeq, repgen:::calculateTotalDays(dateRange[[1]], dateRange[[2]]))
  timeSeries1 <- fromJSON('{
    "approvals":[
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by gwilson.",
        "dateApplied": "2016-05-19T16:26:58.2093803Z",
        "startTime": "2017-01-01T12:12:13",
        "endTime": "2017-02-01T12:12:13"
      }
    ]
  }')
  approvalData <- repgen:::parseCorrApprovals(timeSeries1, timezone, dateSeq)
  noteJSON <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "0000-01-01T12:12:13",
        "endDate": "9999-01-03T12:12:13",
        "note": "FOR EVER AND EVER"
      }
    ]
  }')
  noteData <- repgen:::parseCorrNotes(noteJSON, timezone)
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
      ]
    }
  }')
  corrData <- repgen:::parseCorrProcessingCorrections(corrJSON, 'normal', timezone)

  requiredData <- list(normalData=corrData)
  requiredNames <- list(normalData="Normal")
  optionalData <- list(noteData=noteData)
  optionalNames <- list(noteData="Notes")

  laneData <- repgen:::createPlotLanes(approvalData, requiredData, requiredNames, optionalData, optionalNames, dateRange, startSeq, endSeq)

  expect_is(laneData, 'list')
  expect_equal(length(laneData), 4)
  expect_equal(laneData$rectHeight, 25)
  expect_equal(laneData$tableLabels, c("USGS_MULTI_POINT", "ADAPS Source Flag: *"))

  approvalLane <- laneData$approvalLane
  expect_equal(length(approvalLane), 8)
  expect_equal(as.numeric(approvalLane$startDates), as.numeric(repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone)))
  expect_equal(as.numeric(approvalLane$endDates), as.numeric(repgen:::flexibleTimeParse("2017-02-01T12:12:13", timezone)))
  expect_equal(approvalLane$type, "Approval: Approved")
  expect_equal(approvalLane$colors, "#228B22")
  expect_equal(approvalLane$approvalLabel, c("01/2017", "02/2017", "03/2017"))
  expect_equal(approvalLane$laneYTop, 100)
  expect_equal(approvalLane$laneYBottom, 75)
  expect_equal(approvalLane$labels$text, c("01/2017", "02/2017", "03/2017"))
  expect_equal(as.numeric(approvalLane$labels$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-16T12:00:00", timezone), 
    repgen:::flexibleTimeParse("2017-02-15T00:00:00", timezone),
    repgen:::flexibleTimeParse("2017-03-05T00:00:00", timezone)
  )))
  labelsYPos <- (100 + 75)/2
  expect_equal(approvalLane$labels$y, c(labelsYPos,labelsYPos,labelsYPos))
  expect_equal(approvalLane$labels$shift, c(FALSE,FALSE,FALSE))

  normLane <- laneData$dataLanes$normalData
  expect_is(normLane, 'list')
  expect_equal(length(normLane), 10)
  expect_equal(as.numeric(normLane$startDates), as.numeric(c(repgen:::flexibleTimeParse("2016-12-30T24:00:00-05:00", timezone))))
  expect_equal(as.numeric(normLane$endDates), as.numeric(c(repgen:::flexibleTimeParse("2016-12-30T24:00:00-05:00", timezone))))
  expect_equal(normLane$corrLabel, c("USGS_MULTI_POINT"))
  expect_equal(normLane$laneYTop, c(62))
  expect_equal(normLane$laneYBottom, normLane$laneYTop-laneData$rectHeight)
  expect_equal(normLane$laneNameYPos, (max(normLane$laneYTop) + min(normLane$laneYBottom))/2)
  expect_equal(normLane$laneName, "Normal")
  expect_equal(normLane$bgColor, "white")
  expect_equal(normLane$labels$text, c("1"))
  expect_equal(as.numeric(normLane$labels$x), as.numeric(c(
    repgen:::flexibleTimeParse("2016-12-30T24:00:00-05:00", timezone)
  )))
  expect_equal(normLane$labels$y, (normLane$laneYTop + normLane$laneYBottom)/2)
  expect_equal(normLane$labels$shift, c(TRUE))

  noteLane <- laneData$dataLanes$noteData
  expect_is(noteLane, 'list')
  expect_equal(length(noteLane), 9)
  expect_equal(as.numeric(noteLane$startDates), as.numeric(c(repgen:::flexibleTimeParse("2017-01-01T12:12:13", timezone), dateRange[[1]]-days(1))))
  expect_equal(as.numeric(noteLane$endDates), as.numeric(c(repgen:::flexibleTimeParse("2017-01-03T12:12:13", timezone), dateRange[[2]]+days(1))))
  expect_equal(noteLane$metaLabel, c("ADAPS Source Flag: *", "FOR EVER AND EVER"))
  laneYTop <- normLane$laneYBottom - laneData$rectHeight
  expect_equal(noteLane$laneYTop, c(laneYTop, laneYTop-laneData$rectHeight))
  expect_equal(noteLane$laneYBottom, noteLane$laneYTop-laneData$rectHeight)
  expect_equal(noteLane$laneNameYPos, (max(noteLane$laneYTop) + min(noteLane$laneYBottom))/2)
  expect_equal(noteLane$laneName, "Notes")
  expect_equal(noteLane$bgColor, "#CCCCCC")
  expect_equal(noteLane$labels$text, c("2", "FOR EVER AND EVER"))
  expect_equal(as.numeric(noteLane$labels$x), as.numeric(c(
    repgen:::flexibleTimeParse("2017-01-02T12:12:13", timezone),
    repgen:::flexibleTimeParse("2017-02-03T12:00:00", timezone)
  )))
  expect_equal(noteLane$labels$y, (noteLane$laneYTop + noteLane$laneYBottom)/2)
  expect_equal(noteLane$labels$shift, c(TRUE,FALSE))
})

test_that("createLabelTable properly generates a table of labels that have been removed from the plot", {
  tableLabels <- c("Test1", "Test3")
  tableData <- repgen:::createLabelTable(tableLabels)

  expect_is(tableData, 'data.frame')
  expect_equal(nrow(tableData), 2)
  expect_equal(tableData[[1]], c(1,2))
  expect_equal(tableData[[2]], tableLabels)
})

#Rendering Functions
test_that("doAddToPlot properly adds the lane to the plot", {
  testData1 <- list()
  testData2 <- list(
    startDates = list()
  )
  testData3 <- list(
    endDates = c("date")
  )
  testData4 <- list(
    startDates = c("date1", "date2")
  )

  expect_true(!repgen:::doAddToPlot(testData1))
  expect_true(!repgen:::doAddToPlot(testData2))
  expect_true(!repgen:::doAddToPlot(testData3))
  expect_true(repgen:::doAddToPlot(testData4))
})

test_that("plotLanes properly adds all of the calculated lane data to the plot", {
  timezone <- "Etc/GMT+5"
  dateRange <- c(repgen:::flexibleTimeParse("2017-01-01T00:00:00", timezone), repgen:::flexibleTimeParse("2017-03-09T00:00:00", timezone))
  startSeq <- repgen:::calcStartSeq(dateRange[[1]], dateRange[[2]])
  endSeq <- repgen:::calcEndSeq(startSeq, dateRange[[2]])
  dateSeq <- repgen:::labelDateSeq(startSeq, endSeq, repgen:::calculateTotalDays(dateRange[[1]], dateRange[[2]]))
  timeSeries1 <- fromJSON('{
    "approvals":[
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by gwilson.",
        "dateApplied": "2016-05-19T16:26:58.2093803Z",
        "startTime": "2017-01-01T12:12:13",
        "endTime": "2017-02-01T12:12:13"
      }
    ]
  }')
  approvalData <- repgen:::parseCorrApprovals(timeSeries1, timezone, dateSeq)
  noteJSON <- fromJSON('{
    "notes": [
      {
        "startDate": "2017-01-01T12:12:13",
        "endDate": "2017-01-03T12:12:13",
        "note": "ADAPS Source Flag: *"
      },
      {
        "startDate": "2017-01-02T12:12:13",
        "endDate": "2017-01-04T12:12:13",
        "note": "ADAPS Source Flag: *"
      }
    ]
  }')
  noteData <- repgen:::parseCorrNotes(noteJSON, timezone)
  corrJSON <- fromJSON('{
    "corrections": {
      "normal": [
        {
          "appliedTimeUtc": "2015-12-08T15:32:33Z",
          "comment": "Sensor calibrated.",
          "startTime": "0000-01-09T14:15:00-06:00",
          "endTime": "2017-01-01T02:20:00-06:00",
          "type": "USGS_MULTI_POINT_LONG_LONG_LONG_LABEL",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2015-12-08T15:32:33Z",
          "comment": "Sensor calibrated.",
          "startTime": "2017-01-09T14:15:00-06:00",
          "endTime": "2017-01-11T14:20:00-06:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "NORMAL"
        },
        {
          "appliedTimeUtc": "2015-12-08T15:32:33Z",
          "comment": "Sensor calibrated.",
          "startTime": "2017-03-08T23:00:00-06:00",
          "endTime": "9999-01-11T14:20:00-06:00",
          "type": "USGS_MULTI_POINT_LONG_LONG_LONG_LABEL",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "NORMAL"
        }
      ]
    }
  }')
  corrData <- repgen:::parseCorrProcessingCorrections(corrJSON, 'normal', timezone)

  requiredData <- list(normalData=corrData)
  requiredNames <- list(normalData="Normal")
  optionalData <- list(noteData=noteData)
  optionalNames <- list(noteData="Notes")

  laneData <- repgen:::createPlotLanes(approvalData, requiredData, requiredNames, optionalData, optionalNames, dateRange, startSeq, endSeq)

  basePlot <- gsplot() %>% 
    axis(side=1, labels=FALSE, tick=FALSE) %>%
    axis(side=2, labels=FALSE, tick=FALSE, col="white") %>% 
    axis(side=3, labels=FALSE, tick=FALSE) %>% 
    points(x = as.POSIXct(NA), y = NA, ylim=c(0,100), xlim=dateRange) %>% 
    legend(x = as.numeric(median(startSeq)), 
            y = 115, bty = 'n')

  plot1 <- repgen:::plotLanes(basePlot, laneData$dataLanes$noteData, "noteData", dateRange, laneData$rectHeight)
  plot2 <- repgen:::plotLanes(basePlot, laneData$dataLanes$normalData, "normalData", dateRange, laneData$rectHeight)

  expect_equal(length(plot1$view.1.2), 8)
  texts1 <- gsplot:::views(plot1)[[1]][which(grepl("text", names(gsplot:::views(plot1)[[1]])))]
  rects1 <- gsplot:::views(plot1)[[1]][which(grepl("rect", names(gsplot:::views(plot1)[[1]])))]
  mtext1 <- gsplot:::views(plot1)[[1]][which(grepl("mtext", names(gsplot:::views(plot1)[[1]])))]
  ablines1 <- gsplot:::views(plot1)[[1]][which(grepl("abline", names(gsplot:::views(plot1)[[1]])))]
  points1 <- gsplot:::views(plot1)[[1]][which(grepl("points", names(gsplot:::views(plot1)[[1]])))]
  expect_equal(length(texts1), 2)
  expect_equal(length(rects1), 2)
  expect_equal(length(mtext1), 1)
  expect_equal(length(ablines1), 1)
  expect_equal(length(points1), 2)

  expect_equal(length(plot2$view.1.2), 8)
  texts2 <- gsplot:::views(plot2)[[1]][which(grepl("text", names(gsplot:::views(plot2)[[1]])))]
  rects2 <- gsplot:::views(plot2)[[1]][which(grepl("rect", names(gsplot:::views(plot2)[[1]])))]
  mtext2 <- gsplot:::views(plot2)[[1]][which(grepl("mtext", names(gsplot:::views(plot2)[[1]])))]
  ablines2 <- gsplot:::views(plot2)[[1]][which(grepl("abline", names(gsplot:::views(plot2)[[1]])))]
  points2 <- gsplot:::views(plot2)[[1]][which(grepl("points", names(gsplot:::views(plot2)[[1]])))]
  expect_equal(length(texts2), 2)
  expect_equal(length(rects2), 2)
  expect_equal(length(mtext2), 1)
  expect_equal(length(ablines2), 1)
  expect_equal(length(points2), 2)
})

test_that("correctionsataglanceReport properly constructs a full CORR", {
  library(gsplot)
  reportObject1 <- fromJSON('{
    "thresholds": [],
    "primarySeries":{
      "approvals":[
        {
          "level": 2,
          "description": "Approved",
          "comment": "Approval changed to Approved by gwilson.",
          "dateApplied": "2016-05-19T16:26:58.2093803Z",
          "startTime": "2017-01-01T12:12:13",
          "endTime": "2017-02-01T12:12:13"
        }
      ],
      "notes": [
        {
          "startDate": "2017-01-01T12:12:13",
          "endDate": "2017-01-03T12:12:13",
          "note": "ADAPS Source Flag: *"
        },
        {
          "startDate": "0000-01-01T12:12:13",
          "endDate": "9999-01-03T12:12:13",
          "note": "FOR EVER AND EVER"
        }
      ],
      "grades":[],
      "qualifiers": []
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
      ]
    },
    "reportMetadata":{
      "timezone": "Etc/GMT+5",
      "startDate": "2016-12-30T12:12:12",
      "endDate": "2017-03-09T12:12:12"
    }
  }')

  reportObject2 <- fromJSON('{
    "thresholds": [],
    "primarySeries":{
      "approvals":[],
      "notes": [],
      "grades":[],
      "qualifiers": []
    },
    "corrections": {
      "normal": []
    },
    "reportMetadata":{
      "timezone": "Etc/GMT+5",
      "startDate": "2016-12-30T12:12:12",
      "endDate": "2017-11-09T12:12:12"
    }
  }')

  reportObject3 <- fromJSON('{
    "thresholds": [],
    "primarySeries":{
      "approvals":[],
      "notes": [],
      "grades":[],
      "qualifiers": []
    },
    "corrections": {
      "normal": [
        {
          "appliedTimeUtc": "2015-12-08T15:32:33Z",
          "comment": "Sensor calibrated.",
          "startTime": "2017-01-02T14:15:00-06:00",
          "endTime": "2017-03-09T14:20:00-06:00",
          "type": "USGS_MULTI_POINT",
          "parameters": "{}",
          "user": "admin",
          "processingOrder": "NORMAL"
        }
      ]
    },
    "reportMetadata":{
      "timezone": "Etc/GMT+5",
      "startDate": "2016-12-30T12:12:12",
      "endDate": "2017-11-09T12:12:12"
    }
  }')

  reportObject4 <- fromJSON('{
    "reportMetadata":{
      "timezone": "Etc/GMT+5",
      "startDate": "2016-12-30T12:12:12",
      "endDate": "2017-03-09T12:12:12"
    }
  }')

  corrData1 <- repgen:::correctionsataglanceReport(reportObject1)
  corrData2 <- repgen:::correctionsataglanceReport(reportObject2)
  corrData3 <- repgen:::correctionsataglanceReport(reportObject3)
  expect_error(repgen:::correctionsataglanceReport(reportObject4), "Data for: ' primarySeries ' was not found in report JSON.")

  expect_is(corrData1, 'list')
  expect_is(corrData2, 'character')
  expect_equal(corrData2, 'The requested dataest is empty or blank.')
  expect_is(corrData3, 'list')

  expect_equal(corrData3$tableOfLabels, NULL)
  expect_equal(corrData1$tableOfLabels[[1]], c(1, 2))
  expect_equal(corrData1$tableOfLabels[[2]], c("USGS_MULTI_POINT", "ADAPS Source Flag: *"))
  expect_is(corrData1$timeline, 'gsplot')
  plot1 <- corrData1$timeline

  expect_equal(length(plot1$view.1.2), 27)
  texts <- gsplot:::views(plot1)[[1]][which(grepl("text", names(gsplot:::views(plot1)[[1]])))]
  rects <- gsplot:::views(plot1)[[1]][which(grepl("rect", names(gsplot:::views(plot1)[[1]])))]
  mtext <- gsplot:::views(plot1)[[1]][which(grepl("mtext", names(gsplot:::views(plot1)[[1]])))]
  ablines <- gsplot:::views(plot1)[[1]][which(grepl("abline", names(gsplot:::views(plot1)[[1]])))]
  points <- gsplot:::views(plot1)[[1]][which(grepl("points", names(gsplot:::views(plot1)[[1]])))]

  expect_equal(length(texts), 10)
  expect_equal(length(rects), 9)
  expect_equal(length(mtext), 6)
  expect_equal(length(ablines), 4)
  expect_equal(length(points), 3)

})

test_that("parseCorrFieldVisits properly reads and formats the field visit points", {
  fieldVisits <- fromJSON('{
    "reportMetadata": {
      "timezone": "Etc/GMT+5"
    },
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
  timezone <- "Etc/GMT+5"

  fieldVisitData <- repgen:::parseCorrFieldVisits(fieldVisits, timezone)

  expect_is(fieldVisitData, 'list')
  expect_equal(fieldVisitData[['startDates']], repgen:::flexibleTimeParse("2015-01-06T15:00:00-06:00", timezone))
})

Sys.setenv(TZ=currentTZ)
setwd(dir = wd)
