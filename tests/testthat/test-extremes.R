context("extremes tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing extremes json parsing")
test_that("bad list for extremes", {
    data <- list('data'=c(0,0,0))
    expect_error(extremes(data))
})

context("testing extremes when some fields are missing and are complete")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-example.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing elimination of repeat inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no point data")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-points-example.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no qualifiers")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-qualifiers-example.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no upchain")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-dv.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no upchain or dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain-no-dv.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of multiple min/max on the same date")
test_that("proper number of rows are created based on data",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-multiple-min-max-test.json',package = 'repgen'))
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("upchain"))]], "max", "Max", TRUE)[[1]]) == 2)
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("primary"))]], "max", "Max", FALSE)[[1]]) == 2)
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("upchain"))]], "min", "min", TRUE)[[1]]) == 1)
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("primary"))]], "min", "min", FALSE)[[1]]) == 2)
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("dv"))]], "max", "Max", FALSE)[[1]]) == 1)
  expect_true(NROW(createDataRows(data[[which(names(data) %in% c("dv"))]], "min", "min", FALSE)[[1]]) == 1)
})

context("testing filterAndMarkDuplicates")
test_that("filterAndMarkDuplicates does removes duplicate rows and applies the given note to the date field, first of duplicates found kept",{
  data <- data.frame(
    name=c("A name repeated", "A name repeated", "A name repeated"),
    date=c("08-20-2015", "08-20-2015", "08-19-2015"),
    time=c("15:15:00 (UTC -05:00)", "15:00:00 (UTC -05:00)", "16:00:00 (UTC -05:00)"),
	primary=c(" 28.2", " 28.2", " 28.2"),
	related=c(" 28.2", " 28.2", " 28.2"),
    stringsAsFactors = FALSE)

  dateFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "date")
  expect_equal(nrow(dateFilteredData), 2)
  expect_equal(dateFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(dateFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(dateFilteredData[1,]$related, " 28.2") #related field included
  
  expect_equal(dateFilteredData[2,]$date, "08-19-2015") #not a dupe
  expect_equal(dateFilteredData[2,]$time, "16:00:00 (UTC -05:00)")
  expect_equal(dateFilteredData[2,]$related, " 28.2") #related field included
  
  primaryFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "primary")
  expect_equal(nrow(primaryFilteredData), 1)
  expect_equal(primaryFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(primaryFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(primaryFilteredData[1,]$related, " 28.2") #related field included
  
  noRelatedFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", FALSE, "primary")
  expect_equal(nrow(noRelatedFilteredData), 1)
  expect_equal(noRelatedFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(noRelatedFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(noRelatedFilteredData[1,]$related, NULL) #related field NOT included
})

context("testing example of point vs. interval comparisons")
test_that("extremes report qualifiers are associated correctly",{
  library(jsonlite)
  library(dplyr)
  
  qualifiers <-
    data.frame(
      startDate = "2015-11-01", endDate = "2016-11-16",
      identifier = "ESTIMATED", code = "E", displayName = "Estimated",
      stringsAsFactors = FALSE
    )
  
  points1 <- data.frame(
    time = c("2016-11-15"), value = c(4.05), stringsAsFactors = FALSE
  )
  points2 <- data.frame(
    time = c("2016-11-16"), value = c(5.7), stringsAsFactors = FALSE
  )
  
  q1 <- repgen:::applyQualifiersToValues(points1, qualifiers)
  expect_true(grepl("E", q1$value))
  
  q2 <- repgen:::applyQualifiersToValues(points2, qualifiers)
  expect_false(grepl("E", q2$value))
})

setwd(dir = wd)
