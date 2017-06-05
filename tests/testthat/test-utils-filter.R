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

setwd(dir = wd)
