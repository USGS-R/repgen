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
