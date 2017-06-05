context("utils-filter tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('do negative or zero values get removed from the data frame?', {
  df <- data.frame(value=c(-1, 0, 1, 2, 3, 5))
  df <- repgen:::removeZeroNegative(df)
  expect_false(df[1,1]<0)
  expect_false(df[2,1]==0)
})

test_that('does it subset by the month I ask it to', {
  pts <- data.frame(
      time=c(as.POSIXct("2016-04-23 17:45:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 10, 20),
      month=c("1604", "1605", "1605"),
      stringsAsFactors=FALSE)
  ptsSubset <- repgen:::subsetByMonth(pts, 1604)
  expect_equal(nrow(ptsSubset),1)
})

test_that('expect no data for month requested',{
  pts <- data.frame(
      time=c(as.POSIXct("2016-04-23 17:45:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 10, 20),
      month=c("1604", "1605", "1605"),
      stringsAsFactors=FALSE)
  ptsSubset <- repgen:::subsetByMonth(pts, 1606)
  expect_false(isTRUE(nrow(ptsSubset)==1))
})

test_that('mergeLists properly merges two lists of the same structure into a single list', {
  list1 <- list(a=1, b=2)
  list2 <- list(a=c(2,3), b=c(1,3))
  merged1 <- repgen:::mergeLists(list1, list2)
  merged2 <- repgen:::mergeLists(list1, list())
  merged3 <- repgen:::mergeLists(list(), list2)
  
  expect_equal(merged1, list(a=c(1,2,3), b=c(2,1,3)))
  expect_equal(merged2, list1)
  expect_equal(merged3, list2)
})

test_that('attachFullDataToSubFrame properly creates a new data frame from the original frame', {
  testData <- fromJSON('{
    "testData":[
       {
          "a": 1,
          "b": 2,
          "c": [
             {
                "x": 3,
                "y": 4
             },
             {
                "x": 5,
                "y": 6
             }
          ]
       },
       {
          "a": 3,
          "b": 4,
          "c": [
             {
                "x": 7,
                "y": 8
             },
             {
                "x": 9,
                "y": 10
             }
          ]
       }
    ]
  }')[['testData']]
  
  fullData <- attachFullDataToSubFrame(testData, "c")
  expect_equal(nrow(fullData), 4)
  datFrame1 <- fullData[1,]
  datFrame2 <- fullData[2,]
  datFrame3 <- fullData[3,]
  datFrame4 <- fullData[4,]
  row.names(datFrame1) <- "compare"
  row.names(datFrame2) <- "compare"
  row.names(datFrame3) <- "compare"
  row.names(datFrame4) <- "compare"
  compareFrame1 <- data.frame(x=9, y=10, a=3, b=4)
  compareFrame2 <- data.frame(x=7, y=8, a=3, b=4)
  compareFrame3 <- data.frame(x=5, y=6, a=1, b=2)
  compareFrame4 <- data.frame(x=3, y=4, a=1, b=2)
  row.names(compareFrame1) <- "compare"
  row.names(compareFrame2) <- "compare"
  row.names(compareFrame3) <- "compare"
  row.names(compareFrame4) <- "compare"
  expect_equal(datFrame1, compareFrame1)
  expect_equal(datFrame2, compareFrame2)
  expect_equal(datFrame3, compareFrame3)
  expect_equal(datFrame4, compareFrame4)
})

setwd(dir = wd)
