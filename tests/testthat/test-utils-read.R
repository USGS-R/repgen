context("utils-read tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('getEstimatedDates data returns as expected', {
  #TODO fabricate test data and use to call getEstimatedDates
  #TODO don't know what third param is
})

test_that('readApprovalPoints data returns as expected', {
      library("jsonlite")
      points <- fromJSON('[
              {
              "time": "2015-09-30",
              "value": 304
              },
              {
              "time": "2015-10-01",
              "value": 89.7
              },
              {
              "time": "2015-10-02",
              "value": 39.1
              },
              {
              "time": "2015-10-03",
              "value": 26.0
              },
              {
              "time": "2015-10-04",
              "value": 20.7
              },
              {
              "time": "2015-10-05",
              "value": 17.8
              },
              {
              "time": "2015-10-06",
              "value": 15.9
              },
              {
              "time": "2015-10-07",
              "value": 14.2
              },
              {
              "time": "2015-10-08",
              "value": 12.8
              },
              {
              "time": "2015-10-09",
              "value": 30.0
              },
              {
              "time": "2015-10-10",
              "value": 31.5
              },
              {
              "time": "2015-10-11",
              "value": 18.9
              },
              {
              "time": "2015-10-12",
              "value": 15.7
              },
              {
              "time": "2015-10-13",
              "value": 14.2
              },
              {
              "time": "2015-10-14",
              "value": 15.1
              },
              {
              "time": "2015-10-15",
              "value": 13.6
              }
              ]')
  #mimic what happens when we use read function
  points[['time']] <- flexibleTimeParse(points[['time']], "Etc/GMT+8", FALSE) 
          
  approvals <- fromJSON('[
              {
              "level": 0,
              "description": "Working",
              "comment": "Approval changed to Working by lflight.",
              "dateApplied": "2016-07-09T15:47:11.9573231Z",
              "startTime": "2015-07-09T00:00:00-05:00",
              "endTime": "2015-10-06T16:03:00-05:00"
              },
              {
              "level": 0,
              "description": "Working",
              "comment": "Approval changed to Working by lflight.",
              "dateApplied": "2016-07-09T15:41:56.4029605Z",
              "startTime": "2015-10-06T16:03:00-05:00",
              "endTime": "2015-10-07T16:03:00-05:00"
              },
              {
              "level": 1,
              "description": "In Review",
              "comment": "Approval changed to In Review by lflight.",
              "dateApplied": "2016-07-09T15:42:42.6884572Z",
              "startTime": "2015-10-07T16:03:00-05:00",
              "endTime": "2015-10-10T09:25:00-05:00"
              },
              {
              "level": 2,
              "description": "Approved",
              "comment": "Approval changed to Approved by lflight.",
              "dateApplied": "2016-07-09T15:43:17.4213121Z",
              "startTime": "2015-10-10T09:25:00-05:00",
              "endTime": "2016-01-06T09:25:00-05:00"
              }
              ]')
      
  pointsOrganizedByApproval <- repgen:::readApprovalPoints(approvals, points, "Etc/GMT+5", "TEST LEGEND LABEL", 
                                                appr_var_all=c("appr_approved_dv", "appr_inreview_dv", "appr_working_dv"), point_type=21);
  expect_equal(length(pointsOrganizedByApproval), 16) 
  
  expect_equal(as.character(pointsOrganizedByApproval[[1]]$time), "2015-10-11")
  expect_equal(pointsOrganizedByApproval[[1]]$value, 18.9)
  expect_equal(pointsOrganizedByApproval[[1]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[1]]$legend.name, "Approved TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[2]]$time), "2015-10-12")
  expect_equal(pointsOrganizedByApproval[[2]]$value, 15.7)
  expect_equal(pointsOrganizedByApproval[[2]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[2]]$legend.name, "Approved TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[3]]$time), "2015-10-13")
  expect_equal(pointsOrganizedByApproval[[3]]$value, 14.2)
  expect_equal(pointsOrganizedByApproval[[3]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[3]]$legend.name, "Approved TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[4]]$time), "2015-10-14")
  expect_equal(pointsOrganizedByApproval[[4]]$value, 15.1)
  expect_equal(pointsOrganizedByApproval[[4]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[4]]$legend.name, "Approved TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[5]]$time), "2015-10-15")
  expect_equal(pointsOrganizedByApproval[[5]]$value, 13.6)
  expect_equal(pointsOrganizedByApproval[[5]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[5]]$legend.name, "Approved TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[6]]$time), "2015-10-08")
  expect_equal(pointsOrganizedByApproval[[6]]$value, 12.8)
  expect_equal(pointsOrganizedByApproval[[6]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[6]]$legend.name, "In Review TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[7]]$time), "2015-10-09")
  expect_equal(pointsOrganizedByApproval[[7]]$value, 30)
  expect_equal(pointsOrganizedByApproval[[7]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[7]]$legend.name, "In Review TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[8]]$time), "2015-10-10")
  expect_equal(pointsOrganizedByApproval[[8]]$value, 31.5)
  expect_equal(pointsOrganizedByApproval[[8]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[8]]$legend.name, "In Review TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[9]]$time), "2015-09-30")
  expect_equal(pointsOrganizedByApproval[[9]]$value, 304)
  expect_equal(pointsOrganizedByApproval[[9]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[9]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[10]]$time), "2015-10-01")
  expect_equal(pointsOrganizedByApproval[[10]]$value, 89.7)
  expect_equal(pointsOrganizedByApproval[[10]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[10]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[11]]$time), "2015-10-02")
  expect_equal(pointsOrganizedByApproval[[11]]$value, 39.1)
  expect_equal(pointsOrganizedByApproval[[11]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[11]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[12]]$time), "2015-10-03")
  expect_equal(pointsOrganizedByApproval[[12]]$value, 26)
  expect_equal(pointsOrganizedByApproval[[12]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[12]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[13]]$time), "2015-10-04")
  expect_equal(pointsOrganizedByApproval[[13]]$value, 20.7)
  expect_equal(pointsOrganizedByApproval[[13]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[13]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[14]]$time), "2015-10-05")
  expect_equal(pointsOrganizedByApproval[[14]]$value, 17.8)
  expect_equal(pointsOrganizedByApproval[[14]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[14]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[15]]$time), "2015-10-06")
  expect_equal(pointsOrganizedByApproval[[15]]$value, 15.9)
  expect_equal(pointsOrganizedByApproval[[15]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[15]]$legend.name, "Working TEST LEGEND LABEL")
  
  expect_equal(as.character(pointsOrganizedByApproval[[16]]$time), "2015-10-07")
  expect_equal(pointsOrganizedByApproval[[16]]$value, 14.2)
  expect_equal(pointsOrganizedByApproval[[16]]$point_type, 21)
  expect_equal(pointsOrganizedByApproval[[16]]$legend.name, "Working TEST LEGEND LABEL")
})

test_that('readApprovalBar data returns as expected', {
  library("jsonlite")
  ts <- fromJSON('{
    "points": [
      {
        "time": "2015-09-30",
        "value": 304
      },
      {
        "time": "2015-10-01",
        "value": 89.7
      },
      {
        "time": "2015-10-02",
        "value": 39.1
      },
      {
        "time": "2015-10-03",
        "value": 26.0
      },
      {
        "time": "2015-10-04",
        "value": 20.7
      },
      {
        "time": "2015-10-05",
        "value": 17.8
      },
      {
        "time": "2015-10-06",
        "value": 15.9
      },
      {
        "time": "2015-10-07",
        "value": 14.2
      },
      {
        "time": "2015-10-08",
        "value": 12.8
      },
      {
        "time": "2015-10-09",
        "value": 30.0
      },
      {
        "time": "2015-10-10",
        "value": 31.5
      },
      {
        "time": "2015-10-11",
        "value": 18.9
      },
      {
        "time": "2015-10-12",
        "value": 15.7
      },
      {
        "time": "2015-10-13",
        "value": 14.2
      },
      {
        "time": "2015-10-14",
        "value": 15.1
      },
      {
        "time": "2015-10-15",
        "value": 13.6
      }
    ],
    "approvals": [
      {
        "level": 0,
        "description": "Working",
        "comment": "Approval changed to Working by lflight.",
        "dateApplied": "2016-07-09T15:47:11.9573231Z",
        "startTime": "2015-07-09T00:00:00-05:00",
        "endTime": "2015-10-06T16:03:00-05:00"
      },
      {
        "level": 0,
        "description": "Working",
        "comment": "Approval changed to Working by lflight.",
        "dateApplied": "2016-07-09T15:41:56.4029605Z",
        "startTime": "2015-10-06T16:03:00-05:00",
        "endTime": "2015-10-07T16:03:00-05:00"
      },
      {
        "level": 1,
        "description": "In Review",
        "comment": "Approval changed to In Review by lflight.",
        "dateApplied": "2016-07-09T15:42:42.6884572Z",
        "startTime": "2015-10-07T16:03:00-05:00",
        "endTime": "2015-10-10T09:25:00-05:00"
      },
      {
        "level": 2,
        "description": "Approved",
        "comment": "Approval changed to Approved by lflight.",
        "dateApplied": "2016-07-09T15:43:17.4213121Z",
        "startTime": "2015-10-10T09:25:00-05:00",
        "endTime": "2016-01-06T09:25:00-05:00"
      }
    ],
    "startTime": "2015-09-30",
    "endTime": "2015-10-15"
  }')

  approvalBarsNotAtBoundaries <- repgen:::readApprovalBar(ts, "Etc/GMT+5", "TEST LEGEND LABEL", snapToDayBoundaries=FALSE);
  expect_equal(length(approvalBarsNotAtBoundaries), 4)
  expect_true(approvalBarsNotAtBoundaries[[1]]$legend.name == "Working TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsNotAtBoundaries[[1]]$x0) == "2015-09-30 12:00:00")
  expect_true(as.character(approvalBarsNotAtBoundaries[[1]]$x1) == "2015-10-06 16:03:00")
  expect_true(approvalBarsNotAtBoundaries[[2]]$legend.name == "Working TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsNotAtBoundaries[[2]]$x0) == "2015-10-06 16:03:00")
  expect_true(as.character(approvalBarsNotAtBoundaries[[2]]$x1) == "2015-10-07 16:03:00")
  expect_true(approvalBarsNotAtBoundaries[[3]]$legend.name == "In Review TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsNotAtBoundaries[[3]]$x0) == "2015-10-07 16:03:00")
  expect_true(as.character(approvalBarsNotAtBoundaries[[3]]$x1) == "2015-10-10 09:25:00")
  expect_true(approvalBarsNotAtBoundaries[[4]]$legend.name == "Approved TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsNotAtBoundaries[[4]]$x0) == "2015-10-10 09:25:00")
  expect_true(as.character(approvalBarsNotAtBoundaries[[4]]$x1) == "2015-10-15 12:00:00")
  
  approvalBarsAtBoundaries <- repgen:::readApprovalBar(ts, "Etc/GMT+5", "TEST LEGEND LABEL", snapToDayBoundaries=TRUE);
  expect_equal(length(approvalBarsAtBoundaries), 4)
  expect_true(approvalBarsAtBoundaries[[1]]$legend.name == "Working TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsAtBoundaries[[1]]$x0) == "2015-09-30")
  expect_true(as.character(approvalBarsAtBoundaries[[1]]$x1) == "2015-10-06 23:59:00")
  expect_true(approvalBarsAtBoundaries[[2]]$legend.name == "Working TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsAtBoundaries[[2]]$x0) == "2015-10-06")
  expect_true(as.character(approvalBarsAtBoundaries[[2]]$x1) == "2015-10-07 23:59:00")
  expect_true(approvalBarsAtBoundaries[[3]]$legend.name == "In Review TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsAtBoundaries[[3]]$x0) == "2015-10-07 23:59:00")
  expect_true(as.character(approvalBarsAtBoundaries[[3]]$x1) == "2015-10-10 23:59:00")
  expect_true(approvalBarsAtBoundaries[[4]]$legend.name == "Approved TEST LEGEND LABEL")
  expect_true(as.character(approvalBarsAtBoundaries[[4]]$x0) == "2015-10-10 23:59:00")
  expect_true(as.character(approvalBarsAtBoundaries[[4]]$x1) == "2015-10-15")
})

test_that('readApprovalIndex return correct data', {
  library("jsonlite")
  
  points <- fromJSON('[{
        "time": "2016-04-26T03:00:00-08:00",
        "value": 2770
      },
      {
        "time": "2016-05-22T04:30:00-08:00",
        "value": 1050
      },
      {
        "time": "2016-09-27T17:00:00-08:00",
        "value": 215
      },
      {
        "time": "2016-10-25T15:45:00-08:00",
        "value": 5350
      },
      {
        "time": "2016-11-01T07:45:00-08:00",
        "value": 11600
      }
    ]')

  #mimic what happens when we use read function
  points[['time']] <- flexibleTimeParse(points[['time']], "Etc/GMT+8", FALSE)

  approvals <- fromJSON('[
      {
        "level": 0,
        "description": "Working",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-02-16T00:00:00-08:00",
        "endTime": "2016-04-29T03:00:00-08:00"
      },
      {
        "level": 2,
        "description": "Approved",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-04-29T03:00:00-08:00",
        "endTime": "2016-09-29T03:00:00-08:00"
      },
      {
        "level": 1,
        "description": "In Review",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-09-29T03:00:00-08:00",
        "endTime": "9999-12-31T23:59:59.9999999Z"
      }
    ]')

  working_index <- repgen:::readApprovalIndex(points, approvals, "Working", "Etc/GMT+8");
  review_index <- repgen:::readApprovalIndex(points, approvals, "In Review", "Etc/GMT+8");
  approved_index <- repgen:::readApprovalIndex(points, approvals, "Approved", "Etc/GMT+8");
  
  expect_equal(working_index[1], 1) #first point is in working list
  expect_equal(approved_index[1], 2) #second point is first index found in approved list 
  expect_equal(approved_index[2], 3) #third point is second index found in approved list 
  expect_equal(review_index[1], 4) #fourth point is first index found in review list 
  expect_equal(review_index[2], 5) #fifth point is second index found in review list
})

test_that('readApprovalRanges return correct data', {
  library("jsonlite")
  approvals <- fromJSON('[
      {
        "level": 0,
        "description": "Working",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-02-16T00:00:00-08:00",
        "endTime": "2016-03-16T05:00:00-08:00"
      },{
        "level": 0,
        "description": "Working",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-03-16T05:00:00-08:00",
        "endTime": "2016-04-16T00:00:00-08:00"
      },{
        "level": 1,
        "description": "In Review",
        "comment": "",
        "dateApplied": "2016-10-04T21:58:09.9133567Z",
        "startTime": "2016-04-16T00:00:00-08:00",
        "endTime": "2016-05-16T00:00:00-08:00"
      },{
        "level": 1,
        "description": "In Review",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-05-16T00:00:00-08:00",
        "endTime": "2016-06-16T00:00:00-08:00"
      },{
        "level": 2,
        "description": "Approved",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-06-16T00:00:00-08:00",
        "endTime": "2016-07-16T00:00:00-08:00"
      },{
        "level": 2,
        "description": "Approved",
        "comment": "",
        "dateApplied": "2016-09-04T21:58:09.9133567Z",
        "startTime": "2016-07-16T00:00:00-08:00",
        "endTime": "9999-12-31T23:59:59.9999999Z"
      }
    ]
  ')

  Sys.setenv(TZ = "UTC")

  timezone <- "Etc/GMT+8"
  
  workingApprovals <- repgen:::readApprovalRanges(approvals, "Working", timezone)
  expect_equal(nrow(workingApprovals), 2)
  expect_equal(as.character(workingApprovals[1,]$startTime), "2016-02-16")
  expect_equal(as.character(workingApprovals[1,]$endTime), "2016-03-16 05:00:00")
  expect_equal(as.character(workingApprovals[2,]$startTime), "2016-03-16 05:00:00")
  expect_equal(as.character(workingApprovals[2,]$endTime), "2016-04-16")
  
  inReviewApprovals <- repgen:::readApprovalRanges(approvals, "In Review", timezone)
  expect_equal(nrow(inReviewApprovals), 2)
  expect_equal(as.character(inReviewApprovals[1,]$startTime), "2016-04-16")
  expect_equal(as.character(inReviewApprovals[1,]$endTime), "2016-05-16")
  expect_equal(as.character(inReviewApprovals[2,]$startTime), "2016-05-16")
  expect_equal(as.character(inReviewApprovals[2,]$endTime), "2016-06-16")
  
  approvedReviewApprovals <- repgen:::readApprovalRanges(approvals, "Approved", timezone)
  expect_equal(nrow(approvedReviewApprovals), 2)
  expect_equal(as.character(approvedReviewApprovals[1,]$startTime), "2016-06-16")
  expect_equal(as.character(approvedReviewApprovals[1,]$endTime), "2016-07-16")
  expect_equal(as.character(approvedReviewApprovals[2,]$startTime), "2016-07-16")
  expect_equal(as.character(approvedReviewApprovals[2,]$endTime), "9999-12-31 16:00:00")
})

test_that("sizeOf function works", {
  expect_error(repgen:::sizeOf(NULL), "data frame is null, cannot determine size") 
  
  emptyFrame <- data.frame( 
      randoField=character(), 
      stringsAsFactors=FALSE) 
  expect_equal(repgen:::sizeOf(emptyFrame), 0) 
  
  #using fromJSON out of laziness
  library("jsonlite")
  listOf2 <- fromJSON('[{ "value" : 1 }, { "value" : 2 } ]') 
  expect_equal(repgen:::sizeOf(listOf2), 2) 
})

test_that('readTimeSeries returns valid data for a valid time series', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  series <- repgen:::readTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"))
  
  expect_is(series$startTime, 'POSIXct')
  expect_is(series$endTime, 'POSIXct')
  expect_is(series$points, 'data.frame')
  expect_is(series$approvals, 'data.frame')
  expect_equal(nrow(series$points), 4)
  expect_equal(series$estimated, FALSE)
  expect_equal(series$isDV, FALSE)
  expect_equal(series$startTime, repgen:::flexibleTimeParse('2014-11-19', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$endTime, repgen:::flexibleTimeParse('2015-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$points$value[[1]], 4510)
  expect_equal(series$points$time[[1]], repgen:::flexibleTimeParse('2014-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
})

test_that('readTimeSeries returns valid data for a DV series', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  series <- repgen:::readTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"), isDV=TRUE)

  expect_equal(series$isDV, TRUE)
})

test_that('readTimeSeries throws errors for invalid time series data', {
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))
  
  expect_error(repgen:::readTimeSeries(reportObject, "testSeries2", repgen:::fetchReportMetadataField(reportObject, "timezone")), "*is missing required fields*")
  expect_error(repgen:::readTimeSeries(reportObject, "emptySeries", repgen:::fetchReportMetadataField(reportObject, "timezone")), "*is empty.")
  expect_error(repgen:::readTimeSeries(reportObject, "missingSeries", repgen:::fetchReportMetadataField(reportObject, "timezone")), "*not found in report JSON.")
})

test_that('readEstimatedTimeSeries returns only estimated data for given time series',{
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))

  series <- repgen:::readEstimatedTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"))

  expect_equal(nrow(series$points), 2)
  expect_equal(series$estimated, TRUE)
  expect_equal(series$isDV, FALSE)
  expect_equal(series$points$value[[1]], 4510)
  expect_equal(series$points$time[[1]], repgen:::flexibleTimeParse('2014-11-20', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$points$value[[length(series$points$value)]], 3960)
  expect_equal(series$points$time[[length(series$points$time)]], repgen:::flexibleTimeParse('2014-11-21', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
})

test_that('readNonEstimatedTimeSeries returns only non-estimated data for given time series',{
  library(jsonlite)

  reportObject <- fromJSON(system.file('extdata','testsnippets','test-timeSeries.json', package = 'repgen'))

  series <- repgen:::readNonEstimatedTimeSeries(reportObject, "testSeries1", repgen:::fetchReportMetadataField(reportObject, "timezone"))

  expect_equal(nrow(series$points), 2)
  expect_equal(series$estimated, FALSE)
  expect_equal(series$isDV, FALSE)
  expect_equal(series$points$value[[1]], 3961)
  expect_equal(series$points$time[[1]], repgen:::flexibleTimeParse('2014-11-23', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
  expect_equal(series$points$value[[length(series$points$value)]], 3962)
  expect_equal(series$points$time[[length(series$points$time)]], repgen:::flexibleTimeParse('2014-11-24', repgen:::fetchReportMetadataField(reportObject, "timezone"), shiftTimeToNoon=FALSE))
})

test_that('readGroundWaterLevels returns valid and properly formatted data when given valid JSON', {
  library(jsonlite)

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

  gwData <- repgen:::readGroundWaterLevels(reportObject1)
  blankData <- repgen:::readGroundWaterLevels(reportObject2)

  expect_is(gwData, 'data.frame')
  expect_is(gwData$value, 'numeric')
  expect_is(gwData$time, 'POSIXct')
  expect_is(gwData$month, 'character')

  expect_equal(gwData$value[[1]], 2)
  expect_equal(gwData$time[[2]], as.POSIXct(strptime("2015-07-16T02:00:00-06:00", "%FT%T")))

  expect_is(blankData, 'data.frame')

  expect_equal(nrow(blankData), 0)
})

test_that('readGroundWaterLevels errors when given invalid JSON', {
  library(jsonlite)

  reportObject1 <- fromJSON('{
      "gwlevel": [
        {
          "siteNumber": "12345",
          "recordDateTime": "2015-07-16T01:00:00-06:00"
        },
        {
          "siteNumber": "12345",
          "groundWaterLevel": 3,
          "recordDateTime": "2015-07-16T02:00:00-06:00",
          "timeZone": "EDT"
        }
      ]
  }')

  reportObject2 <- fromJSON('{ }')

  expect_error(repgen:::readGroundWaterLevels(reportObject1), "*missing required fields*")
  expect_error(repgen:::readGroundWaterLevels(reportObject2), "*not found in report JSON.")
})

test_that('readWaterQualityMeasurements returns valid and properly formatted data when given valid JSON', {
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

  wqData <- repgen:::readWaterQualityMeasurements(reportObject)
  expect_is(wqData, 'data.frame')
  expect_is(wqData$value, 'numeric')
  expect_is(wqData$time, 'POSIXct')
  expect_is(wqData$month, 'character')
  expect_equal(wqData$value[[1]], 5.3)
  expect_equal(wqData$time[[2]], as.POSIXct(strptime("2015-07-29T13:30:00-06:00", "%FT%T")))
})

test_that('readWaterQualityMeasurements errors when given invalid JSON', {
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

  expect_error(repgen:::readWaterQualityMeasurements(reportObject1), "*missing required fields*")
  expect_error(repgen:::readWaterQualityMeasurements(reportObject2), "*not found in report JSON.")
})

test_that('readFieldVisitMeasurementsQPoints returns valid field visit measurement discharge point data when given valid JSON', {
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

  fvData <- repgen:::readFieldVisitMeasurementsQPoints(reportObject)

  expect_is(fvData, 'data.frame')
  expect_is(fvData$minQ[[1]], 'numeric')
  expect_is(fvData$value[[1]], 'integer')
  expect_is(fvData$time[[length(fvData$time)]], 'POSIXct')

  expect_equal(nrow(fvData), 1)
  expect_equal(fvData$time[[length(fvData$time)]],  as.POSIXct(strptime('2015-07-07T15:35:59-05:00', "%FT%T")))
  expect_equal(fvData$value[[1]], 4600)
  expect_equal(fvData$maxQ[[1]], 5060)
})

test_that('readFieldVisitReadings returns multiple readings', {
  reportObject <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  expect_equal(fvData$party[[1]],"CR")
  expect_equal(fvData$time[[2]],"2015-01-06T08:46:00.000-06:00")
})

test_that('readAllFieldVisitQualifiers returns all qualifiers from all readings', {
  reportObject <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  allQuals <- repgen:::readAllFieldVisitQualifiers(fvData)
  expect_equal(nrow(allQuals), 3)
  expect_equal(allQuals$qualifiers.code[[1]], 'TQL')
  expect_equal(allQuals$qualifiers.code[[2]], 'EQP')
  expect_equal(allQuals$qualifiers.code[[3]], 'EQP')
})

test_that('readFieldVisitReadings handles full data set with empty qualifier data frame.', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
    "readings": [
    {
      "time": "2015-08-07T09:26:00.000-05:00",
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
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData$qualifiers[[1]])==0)
})

test_that('readFieldVisitReadings handles full data set with populated qualifier data frame.', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
                           "readings": [
                            {
                              "time": "2015-08-07T09:26:00.000-05:00",
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
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_is(fvData[['qualifiers']][[1]], 'data.frame')
  expect_equal(fvData[['qualifiers']][[1]]$code[[1]],"TQ")
  expect_equal(fvData[['qualifiers']][[1]]$identifier[[1]],"TESTQUAL")
  expect_equal(fvData[['qualifiers']][[1]]$description[[1]],"Test Qualifier")
  expect_equal(fvData[['qualifiers']][[1]]$code[[2]], "EQP")
  expect_equal(fvData[['qualifiers']][[1]]$identifier[[2]],"EQUIP")
  expect_equal(fvData[['qualifiers']][[1]]$description[[2]],"Equpment Malfunction")
  })

test_that('readFieldVisitReadings handles empty comments', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
                    "readings": [
                          {
                           "time": "2015-04-03T09:41:00.000-05:00",
                           "fieldVisitIdentifier": "1BAA4F773B76928FE05322EB3D98DF04",
                           "comments" : [],
                           "visitStatus": "TODO",
                           "party": "CR",
                           "monitoringMethod": "Max-min indicator",
                           "value": "9.20",
                           "parameter": "Gage height",
                           "type": "2015-01-06T08:46:00.000-06:00",
                           "startTime": "2015-01-06T08:46:00.000-06:00",
                           "associatedIvTime": "2015-03-27T21:30:00.000-05:00",
                           "associatedIvValue": "9.18",
                           "minTime": "2015-02-20T14:30:00.000-06:00",
                           "minValue": "1.93",
                           "associatedIvQualifiers": []
                            }
                          ]
                    }')
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_true(is.null(fvData$qualifiers[[1]]))
})

test_that('readFieldVisitReadings handles null qualifiers', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
                    "readings": [
                          {
                           "time": "2015-04-03T09:41:00.000-05:00",
                           "fieldVisitIdentifier": "1BAA4F773B76928FE05322EB3D98DF04",
                           "comments" : ["comment"],
                           "visitStatus": "TODO",
                           "party": "CR",
                           "monitoringMethod": "Max-min indicator",
                           "value": "9.20",
                           "parameter": "Gage height",
                           "type": "2015-01-06T08:46:00.000-06:00",
                           "startTime": "2015-01-06T08:46:00.000-06:00",
                           "associatedIvTime": "2015-03-27T21:30:00.000-05:00",
                           "associatedIvValue": "9.18",
                           "minTime": "2015-02-20T14:30:00.000-06:00",
                           "minValue": "1.93",
                           "associatedIvQualifiers": []
                            }
                          ]
                    }')
  fvData <- repgen:::readFieldVisitReadings(reportObject)
  expect_is(fvData, 'data.frame')
  expect_true(is.null(fvData$qualifiers[[1]]))
})

test_that('readQualifiers handles null time parameter passed into function', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                            {
                              "startDate": "2015-08-26T05:00:00.000-05:00",
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
                          }')
  fvData <- repgen:::readQualifiers(inQualifiers,NULL)
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==2)
})

test_that('readQualifiers handles no time parameter passed into function', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startDate": "2015-08-26T05:00:00.000-05:00",
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
}')
  fvData <- repgen:::readQualifiers(inQualifiers)
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==2)
  })

test_that('readQualifiers handles null qualifiers', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
      "time": "2015-04-03T09:41:00.000-05:00",
      "fieldVisitIdentifier": "1BAA4F773B76928FE05322EB3D98DF04",
      "visitStatus": "TODO",
      "party": "CR",
      "monitoringMethod": "Max-min indicator",
      "value": "9.20",
      "parameter": "Gage height",
      "type": "2015-01-06T08:46:00.000-06:00",
      "startTime": "2015-01-06T08:46:00.000-06:00",
      "associatedIvTime": "2015-03-27T21:30:00.000-05:00",
      "associatedIvValue": "9.18",
      "minTime": "2015-02-20T14:30:00.000-06:00",
      "minValue": "1.93",
      "associatedIvQualifiers": []
      }')
  fvData <- repgen:::readQualifiers(inQualifiers[['associatedIvQualifiers']], inQualifiers[['associatedIvTime']])
  expect_equal(fvData,NULL)
})

test_that('readQualifiers handles empty qualifier data frame.', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                            {
                              "startDate": "2015-08-26T05:00:00.000-05:00",
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
                          }')
  fvData <- repgen:::readQualifiers(inQualifiers, "2015-08-07T09:26:00.000-05:00")
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==0)
})

test_that('readQualifiers handles populated qualifier data frame with one row.', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startDate": "2015-08-26T05:00:00.000-05:00",
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
                           "identifier": "TEST",
                           "code": "BLAH",
                           "appliedBy": "gwilson",
                           "displayName": "Test Qualifier",
                           "dateApplied": "2015-09-15T12:57:22.423-05:00"
                           }
                           ]
}')
  fvData <- repgen:::readQualifiers(inQualifiers, "2015-08-26T09:26:00.000-05:00")
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==1)
  expect_equal(fvData$code[[1]],"EQP")
  expect_equal(fvData$identifier[[1]],"EQUIP")
  expect_equal(fvData$description[[1]],"Equpment Malfunction")
  })

test_that('readQualifiers handles populated qualifier data frame with more than one row.', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
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
                           "startDate": "2015-07-05T09:30:00.000-05:00",
                           "endDate": "2015-07-06T15:30:00.000-05:00",
                           "identifier": "EQUIP",
                           "code": "EQP",
                           "appliedBy": "gwilson",
                           "displayName": "Equpment Malfunction",
                           "dateApplied": "2015-09-15T12:57:22.423-05:00"
                           }
                           ]
}')
  fvData <- repgen:::readQualifiers(inQualifiers, "2015-07-05T11:26:00.000-05:00")
  expect_is(fvData, 'data.frame')
  expect_true(nrow(fvData)==2)
  expect_equal(fvData$code[[1]],"TQ")
  expect_equal(fvData$identifier[[1]],"TESTQUAL")
  expect_equal(fvData$description[[1]],"Test Qualifier")
  expect_equal(fvData$code[[2]],"EQP")
  expect_equal(fvData$identifier[[2]],"EQUIP")
  expect_equal(fvData$description[[2]],"Equpment Malfunction")
  })

test_that('readFieldVisitMeasurementsShifts returns valid field visit measurement shift data when given valid JSON', {
  library(jsonlite)

  reportObject <- fromJSON('{
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

  fvData <- repgen:::readFieldVisitMeasurementsShifts(reportObject)

  expect_is(fvData, 'data.frame')
  expect_is(fvData$minShift[[1]], 'numeric')
  expect_is(fvData$value[[1]], 'numeric')
  expect_is(fvData$time[[length(fvData$time)]], 'POSIXct')

  expect_equal(nrow(fvData), 1)
  expect_equal(fvData$time[[length(fvData$time)]],  as.POSIXct(strptime('2016-04-08T09:02:42-08:00', "%FT%T")))
  expect_equal(fvData$value[[1]], 0.05744611933222)
  expect_equal(fvData$maxShift[[1]], 0.21698855520226)
})

test_that('readCorrections returns the full set of corrections data for the specified time series', {
  library(jsonlite)

  reportObject <- fromJSON('{
      "primarySeriesCorrections": [
        {
          "appliedTimeUtc": "2012-02-29T19:18:25Z",
          "startTime": "2011-01-29T10:17:00-05:00",
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

  corrData <- repgen:::readCorrections(reportObject, "primarySeriesCorrections")

  expect_is(corrData, 'data.frame')
  expect_is(corrData$time[[1]], 'POSIXct')
  expect_is(corrData$time[[2]], 'POSIXct')
  expect_is(corrData$comment[[1]], 'character')
  expect_is(corrData$comment[[2]], 'character')

  expect_equal(nrow(corrData), 4)
  expect_equal(corrData$comment[[1]], 'Start : NA')
  expect_equal(corrData$comment[[2]], 'Start : test comment')
  expect_equal(corrData$comment[[3]], 'End : NA')
  expect_equal(corrData$time[[1]], as.POSIXct(strptime('2011-01-29T10:17:00-05:00', "%FT%T")))
})

test_that('readMeanGageHeights returns data correctly', {
  library(jsonlite)
  
  reportObject <- fromJSON('{ "fieldVisitMeasurements": [
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
  ]}')
      
  gageHeights <- repgen:::readMeanGageHeights(reportObject)
  expect_equal(nrow(gageHeights), 1)
  expect_equal(gageHeights[1,]$n, "943") 
  expect_equal(gageHeights[1,]$month, "1604")
  expect_equal(as.character(gageHeights[1,]$time), "2016-04-08 09:02:42")
  expect_equal(gageHeights[1,]$value, 7.71)
})

test_that('readReadings returns data correctly', {
  library(jsonlite)
  
  reportObject <- fromJSON('{
  "readings": [
    {
      "estimatedTime": "2014-08-12T11:00:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Non-subm pressure  transducer",
      "type": "Routine",
      "value": "1.20",
      "party": "LEF/BMG"
    },{
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Crest stage",
      "type": "ExtremeMax",
      "uncertainty": "0.01",
      "value": "1.17",
      "party": "LEF/BMG"
    },
    {
      "estimatedTime": "2014-08-12T16:30:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Reference Point",
      "type": "ReferencePrimary",
      "uncertainty": "0.01",
      "value": "1.17",
      "party": "LEF/BMG"
    },
    {
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Reference Point",
      "type": "Unknown",
      "uncertainty": "0.02",
      "value": "1.16",
      "party": "LEF/BMG"
    },{
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:55:00-05:00",
      "monitoringMethod": "Crest stage",
      "type": "ExtremeMax",
      "uncertainty": "0.01",
      "value": "1.18",
      "party": "LEF/BMG"
    },{
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:55:00-05:00",
      "monitoringMethod": "something else",
      "type": "ExtremeMax",
      "uncertainty": "0.01",
      "value": "1.19",
      "party": "LEF/BMG"
    }
  ]
  }')
  
  allReadings <- repgen:::readReadings(reportObject)
  expect_equal(nrow(allReadings), 6)
  expect_equal(allReadings[1,]$uncertainty, 0) #auto filled NA uncertainty to 0
  expect_equal(allReadings[1,]$month, "1408")
  expect_equal(as.character(allReadings[1,]$time), "2014-08-12 10:53:00")
  expect_equal(allReadings[1,]$value, 1.20)
  
  expect_equal(allReadings[2,]$uncertainty, 0.01) 
  expect_equal(allReadings[2,]$month, "1408")
  expect_equal(as.character(allReadings[2,]$time), "2014-08-12 10:53:00")
  expect_equal(allReadings[2,]$value, 1.17)
  
  expect_equal(allReadings[3,]$uncertainty, 0.01) 
  expect_equal(allReadings[3,]$month, "1408")
  expect_equal(as.character(allReadings[3,]$time), "2014-08-12 10:53:00")
  expect_equal(allReadings[3,]$value, 1.17)
  
  expect_equal(allReadings[3,]$uncertainty, 0.01) 
  expect_equal(allReadings[3,]$month, "1408")
  expect_equal(as.character(allReadings[3,]$time), "2014-08-12 10:53:00")
  expect_equal(allReadings[3,]$value, 1.17)
  
  expect_equal(allReadings[4,]$uncertainty, 0.02) 
  expect_equal(allReadings[4,]$month, "1408")
  expect_equal(as.character(allReadings[4,]$time), "2014-08-12 10:53:00")
  expect_equal(allReadings[4,]$value, 1.16)
  
  expect_equal(allReadings[5,]$uncertainty, 0.01) 
  expect_equal(allReadings[5,]$month, "1408")
  expect_equal(as.character(allReadings[5,]$time), "2014-08-12 10:55:00")
  expect_equal(allReadings[5,]$value, 1.18)
  
  expect_equal(allReadings[6,]$uncertainty, 0.01) 
  expect_equal(allReadings[6,]$month, "1408")
  expect_equal(as.character(allReadings[6,]$time), "2014-08-12 10:55:00")
  expect_equal(allReadings[6,]$value, 1.19)
  
  referenceReadings <- repgen:::readReadings(reportObject, "reference")
  expect_equal(nrow(referenceReadings), 1)
  expect_equal(referenceReadings[1,]$uncertainty, 0.01) 
  expect_equal(referenceReadings[1,]$month, "1408")
  expect_equal(as.character(referenceReadings[1,]$time), "2014-08-12 10:53:00")
  expect_equal(referenceReadings[1,]$value, 1.17)
  
  crestStageReadings <- repgen:::readReadings(reportObject, "crestStage")
  expect_equal(nrow(crestStageReadings), 2)
  expect_equal(crestStageReadings[1,]$uncertainty, 0.01) 
  expect_equal(crestStageReadings[1,]$month, "1408")
  expect_equal(as.character(crestStageReadings[1,]$time), "2014-08-12 10:53:00")
  expect_equal(crestStageReadings[1,]$value, 1.17)
  expect_equal(crestStageReadings[2,]$uncertainty, 0.01) 
  expect_equal(crestStageReadings[2,]$month, "1408")
  expect_equal(as.character(crestStageReadings[2,]$time), "2014-08-12 10:55:00")
  expect_equal(crestStageReadings[2,]$value, 1.18)
  
  # not yet implemented waterMarkReadings <- repgen:::readReadings(reportObject, "waterMark")

  #another test for crest stage detection
  reportObject2 <- fromJSON('{
  "readings": [
    {
      "estimatedTime": "2014-08-12T11:00:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Crest stage", 
      "type": "Routine",
      "value": "1.20",
      "party": "LEF/BMG"
    },{
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Crest stage",
      "type": "ExtremeMax",
      "uncertainty": "0.01",
      "value": "1.17",
      "party": "LEF/BMG"
    },
    {
      "estimatedTime": "2014-08-12T16:30:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:53:00-05:00",
      "monitoringMethod": "Reference Point",
      "type": "ReferencePrimary",
      "uncertainty": "0.01",
      "value": "1.17",
      "party": "LEF/BMG"
    },{
      "estimatedTime": "2014-08-12T12:15:00-05:00",
      "comments": [""],
      "visitStatus": "TODO",
      "parameter": "Gage height",
      "fieldVisitIdentifier": "3BBE3D100D6003DBE0530100007F1EB1",
      "time": "2014-08-12T10:55:00-05:00",
      "monitoringMethod": "something else",
      "type": "ExtremeMax",
      "uncertainty": "0.01",
      "value": "1.19",
      "party": "LEF/BMG"
    }
  ]
  }')

  crestStageReadings2 <- repgen:::readReadings(reportObject2, "crestStage")
  expect_equal(nrow(crestStageReadings2), 1) #Note that this ensures first monitoringMethod="Crest stage" is NOT detected as it is not of type ExtremeMax
  expect_equal(crestStageReadings2[1,]$uncertainty, 0.01) 
  expect_equal(crestStageReadings2[1,]$month, "1408")
  expect_equal(as.character(crestStageReadings2[1,]$time), "2014-08-12 10:53:00")
  expect_equal(crestStageReadings2[1,]$value, 1.17)
})

test_that("readMinMaxIVs properly retrieves the min/max IV values", {
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
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "firstDownChain": "24eca840ec914810a88f00a96a70fc88",
      "isInverted": false,
      "stationId": "01054200",
      "downChainDescriptions1": "Discharge.ft^3/s.Mean@01054200"
    }
  }')

  max_iv <- repgen:::readMinMaxIVs(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), FALSE)
  min_iv <- repgen:::readMinMaxIVs(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), FALSE)
  max_iv_inv <- repgen:::readMinMaxIVs(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), TRUE)
  min_iv_inv <- repgen:::readMinMaxIVs(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), TRUE)

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv$value, 892)
  expect_equal(min_iv$value, 60.5)
  expect_equal(max_iv_inv$value, 892)
  expect_equal(min_iv_inv$value, 60.5)

  expect_equal(max_iv$time, repgen:::flexibleTimeParse("2013-11-18T12:00:00-05:00", repgen:::fetchReportMetadataField(IVs, 'timezone')))
  expect_equal(min_iv$time, repgen:::flexibleTimeParse("2013-11-12T22:45:00-05:00", repgen:::fetchReportMetadataField(IVs, 'timezone')))
  expect_equal(max_iv_inv$time, repgen:::flexibleTimeParse("2013-11-18T12:00:00-05:00", repgen:::fetchReportMetadataField(IVs, 'timezone')))
  expect_equal(min_iv_inv$time, repgen:::flexibleTimeParse("2013-11-12T22:45:00-05:00", repgen:::fetchReportMetadataField(IVs, 'timezone')))

  expect_equal(max_iv$label, "Max. Instantaneous")
  expect_equal(min_iv$label, "Min. Instantaneous")
  expect_equal(max_iv_inv$label, "Min. Instantaneous")
  expect_equal(min_iv_inv$label, "Max. Instantaneous")
})

setwd(dir = wd)