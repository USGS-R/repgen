context("utils-time tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('flexibleTimeParse can parse dates with UTC', {
  time_str <- "2013-10-02T08:00:00.000Z"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(hour(parsed_time) == 2)
})

test_that('flexibleTimeParse can parse dates with offsets', {
  time_str <- "2013-10-02T08:00:00.000-06:00"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(hour(parsed_time) == 8)
})

test_that('flexibleTimeParse can parse DV values', {
  time_str <- "2013-10-02"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(hour(parsed_time) == 12)
})

test_that('toStartOfDay works', {
  time_str <- "2013-10-02T21:00:00.000-06:00"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  parsed_time <- toStartOfDay(parsed_time)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(hour(parsed_time) == 0)
})

test_that('toEndOfDay works', {
  time_str <- "2013-10-02T09:00:00.000-06:00"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  parsed_time <- toEndOfDay(parsed_time)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(hour(parsed_time) == 23)
  expect_true(minute(parsed_time) == 59)
})

test_that('toStartOfMonth works', {
  time_str <- "2013-10-02T21:00:00.000-06:00"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  parsed_time <- toStartOfMonth(parsed_time)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(day(parsed_time) == 1)
})

test_that('toEndOfMonth works', {
  time_str <- "2013-10-25T21:00:00.000-06:00"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  parsed_time <- toEndOfMonth(parsed_time)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(day(parsed_time) == 30)
})

test_that('toEndOfTime works', {
  time_str <- "9999-12-31T23:59:59.9999999Z"
  timezone <- "Etc/GMT+6"
  parsed_time <- flexibleTimeParse(time_str, timezone)
  parsed_time <- toEndOfTime(parsed_time)
  
  expect_is(parsed_time, "POSIXct")
  expect_true(year(parsed_time) == 2100)
})
