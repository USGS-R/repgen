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
  data <- fromJSON(system.file('extdata','extremes','extremes-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing elimination of repeat inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no point data")
test_that("extremes examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-points-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no qualifiers")
test_that("extremes examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-qualifiers-example.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no upchain")
test_that("extremes examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no dv")
test_that("extremes examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-dv.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of no upchain or dv")
test_that("extremes examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain-no-dv.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

context("testing example of multiple min/max on the same date")
test_that("extremes report has 10 rows",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-multiple-min-max.json',package = 'repgen'))
  plot <- extremesTable(data)
  expect_is(NROW(plot), 10)
})

setwd(dir = wd)
