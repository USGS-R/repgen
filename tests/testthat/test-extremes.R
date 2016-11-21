context("extremes tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing extremes json parsing")
test_that("bad list for extremes", {
    data <- list('data'=c(0,0,0))
    expect_error(extremes(data, 'html'))
})

context("testing extremes when some fields are missing and are complete")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing elimination of repeat inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no point data")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-points-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no qualifiers")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-qualifiers-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no upchain")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-dv.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no upchain or dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain-no-dv.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
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

setwd(dir = wd)
