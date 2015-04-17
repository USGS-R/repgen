
context("setting wd to temp folder")
wd <- getwd()
setwd(dir = tempdir())
context("testing extremes json parsing")

test_that("bad list for extremes", {
    data <- list('data'=c(0,0,0))
    expect_error(extremes(data, 'html'))
})

context("testing limits fail w/ NAs")

test_that('missing data returns as expected', {
  data <- list('data'=c(0,0,0))
  expect_equal(getRatingShifts(data, 'shiftPoints'), " ")
  expect_error(getRatingShifts(data, 'shiftPoints', required = TRUE))
  expect_is(getRatingShifts(data, 'shiftPoints', as.numeric = T), 'numeric')
  expect_is(getRatingShifts(data, 'shiftPoints', as.numeric = F), 'character')
  expect_error(repgen:::getLims(NA,NA,NA,NA)) 
})

context("testing extremes when some fields are missing and are complete")
test_that("example data extremes", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata',"extremes-bad.json",package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
  expect_is(extremes(data, 'pdf'), 'character')
  data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
  expect_is(extremes(data, 'pdf'), 'character')
  
})

context("testing vdiagram when some fields are missing and are complete")
test_that("example data vdiagram", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata',"vdiagram-v6.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
  expect_is(vdiagram(data, 'pdf'), 'character')
  
})
setwd(dir = wd)
