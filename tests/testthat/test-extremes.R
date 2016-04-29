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

context("testing elimination of repeats of inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
})

setwd(dir = wd)
