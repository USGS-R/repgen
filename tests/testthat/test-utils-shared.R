context("utils-shared tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('isLogged properly detects if a TS should be plotted with logarithmic axes', {
  logged <- fromJSON('{
    "tsField": {
      "notes": [],
      "isVolumetricFlow": true,
      "description": "From Aquarius",
      "qualifiers": [],
      "units": "ft^3/s",
      "grades": [],
      "type": "Discharge",
      "points": [
        {
        "time": "2013-10-02T00:00:00.000-06:00",
        "value": 5
        }],
      "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
      "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
      "approvals": [],
      "name": "d10cfa498ed248de983cbeaf0a75c14b",
      "startTime": "2013-10-01T00:00:00.000-06:00",
      "endTime": "2013-10-30T00:00:00.000-06:00",
      "inverted": false
    }}')   
  
  zeroValues <- fromJSON('{
      "tsField": {
        "notes": [],
        "isVolumetricFlow": true,
        "description": "From Aquarius",
        "qualifiers": [],
        "units": "ft^3/s",
        "grades": [],
        "type": "Discharge",
        "points": [
          {
            "time": "2013-10-01T00:00:00.000-06:00",
            "value": 0
          }],
        "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
        "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
        "approvals": [],
        "name": "d10cfa498ed248de983cbeaf0a75c14b",
        "startTime": "2013-10-01T00:00:00.000-06:00",
        "endTime": "2013-10-30T00:00:00.000-06:00",
        "inverted": false
      }}')

  negativeValues <- fromJSON('{
      "tsField": {
        "notes": [],
        "isVolumetricFlow": true,
        "description": "From Aquarius",
        "qualifiers": [],
        "units": "ft^3/s",
        "grades": [],
        "type": "Discharge",
        "points": [
        {
        "time": "2013-10-01T00:00:00.000-06:00",
        "value": -5
        }],
        "requestedStartTime": "2013-10-01T00:00:00.000-05:00",
        "requestedEndTime": "2013-10-31T00:00:00.000-05:00",
        "approvals": [],
        "name": "d10cfa498ed248de983cbeaf0a75c14b",
        "startTime": "2013-10-01T00:00:00.000-06:00",
        "endTime": "2013-10-30T00:00:00.000-06:00",
        "inverted": false
      }}')
})

test_that('isEmpty returns true if value input is both null and na (test with null)',{
  val1 = NULL
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na)',{
  val1 = NA
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na and null)',{
  val1 <- c(NULL,NA)
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with non-null and non-na)',{
  val1 <- c(1:3,6:4)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with null)',{
  val1 = NULL
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is  null and empty string and na (test with empty string)',{
  val1 = ""
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with na)',{
  val1 = NA
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and null)',{
  val1 <- c(NULL,"")
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and na)',{
  val1 <- c(NA,"")
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with NULL and na)',{
  val1 <- c(NA,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with empty string)',{
  val1 <- c(1:3,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and null)',{
  val1 <- c(1:3,NA,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and empty string)',{
  val1 <- c(1:3,NA,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null and empty string)',{
  val1 <- c(1:3,NULL,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with non-null and non-empty string and non-na)',{
  val1 <- c(1:3,6:4,8:2)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value exists in the environment or false if it does not',{
  a <- 1
  c <- ""
  d <- NULL
  e <- NA
  expect_false(isEmptyOrBlank(listObjects = ls(), objectName = "a")) # should be FALSE
  expect_true(isEmptyOrBlank(listObjects = ls(), objectName = "b")) # should be TRUE
  expect_true(isEmptyOrBlank(val = c)) # should be TRUE
  expect_true(isEmptyOrBlank(val = d)) # should be TRUE
  expect_true(isEmptyOrBlank(val = e)) # should be TRUE
  expect_false(isEmptyOrBlank(val = a)) # should be FALSE
})

test_that('splitDataGaps and applyDataGaps work with data.frames (uvhydro)', {
  data <- fromJSON('{
            "downsampledPrimarySeries": {
               "gaps": [
                 {
                   "startTime": "2016-06-14T11:00:00-05:00",
                   "endTime": "2016-06-15T08:15:00-05:00"
                 }
               ],
               "points": [
                 {
                   "time": "2016-06-14T14:45:00.000Z",
                   "value": 2730.0
                 },
                 {
                   "time": "2016-06-14T16:00:00.000Z",
                   "value": 2770.0
                 },
                 {
                   "time": "2016-06-15T13:30:00.000Z",
                   "value": 2590.0
                 }
               ]
            },
            "reportMetadata": {
              "timezone": "Etc/GMT+5"
            }
          }')
  
  ts <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeries"), "1606")
  gapData <- splitDataGaps(data, ts, isDV=FALSE)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 2)
  
  relevantData <- list(corr_UV = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 2)
  expect_true("corr_UV" %in% names(allVars))
  
})

test_that('splitDataGaps and applyDataGaps work with lists (dvhydro)', {
  data <- fromJSON('{
            "firstDownChain": {
              "gaps": [
                {
                  "startTime": "2016-02-12",
                  "endTime": "2016-04-15"
                },
                {
                  "startTime": "2016-06-13",
                  "endTime": "2016-06-16"
                }
              ],
              "points": [
                {
                  "time": "2016-02-12",
                  "value": 22800
                },
                {
                  "time": "2016-04-15",
                  "value": 31000
                },
                {
                  "time": "2016-06-13",
                  "value": 12500
                },
                {
                  "time": "2016-06-16",
                  "value": 13000
                },
                {
                  "time": "2016-06-17",
                  "value": 10800
                }
              ]
            },
            "reportMetadata": {
            "timezone": "Etc/GMT+5",
            "downChainDescriptions1": "Discharge.ft^3/s.Mean@01014000"
            }
          }')
  
  ts <- getStatDerived(data, "firstDownChain", "downChainDescriptions1", estimated = FALSE)
  gapData <- splitDataGaps(data, ts, isDV=TRUE)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 3)
  
  relevantData <- list(stat1 = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 3)
  expect_true("stat1" %in% names(allVars))
  
})

test_that('splitDataGaps and applyDataGaps work when there are no gaps specified', {
  data <- fromJSON('{
            "downsampledPrimarySeries": {
               "gaps": [],
               "points": [
                 {
                   "time": "2016-06-14T14:45:00.000Z",
                   "value": 2730.0
                 },
                 {
                   "time": "2016-06-14T16:00:00.000Z",
                   "value": 2770.0
                 },
                 {
                   "time": "2016-06-15T13:30:00.000Z",
                   "value": 2590.0
                 }
                ]
               },
              "reportMetadata": {
                "timezone": "Etc/GMT+5"
              }
            }')
  
  ts <- subsetByMonth(getTimeSeries(data, "downsampledPrimarySeries"), "1606")
  gapData <- splitDataGaps(data, ts)
  
  expect_is(gapData, "list")
  expect_null(names(gapData))
  expect_true(length(gapData) == 1)
  
  relevantData <- list(corr_UV = ts)
  allVars <- applyDataGaps(data, relevantData)
  
  expect_true(length(allVars) == 1)
  expect_true("corr_UV" %in% names(allVars))
  
})

setwd(dir = wd)
