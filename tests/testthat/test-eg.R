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
