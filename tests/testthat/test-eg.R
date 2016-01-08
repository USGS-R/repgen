
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
  data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
  
})

context("testing vdiagram when some fields are missing and are complete")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata',"vdiagram-example.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
  
})

context("testing uvhydrograph")
test_that("uvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','uvhydro-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data,'html', 'Author Name'), 'character')
})

context("testing dvhydrograph")
test_that("dvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','dvhydro-example.json', package = 'repgen'))
  expect_is(dvhydrograph(data,'html', 'Author Name'), 'character')
})

context("testing fiveyeargwsum")
test_that("fiveyeargwsum examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','fiveyeargwsum-example.json', package = 'repgen'))
  expect_is(fiveyeargwsum(data,'html', 'Author Name'), 'character')
})

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(sensorreadingsummary(data,'html', 'Author Name'), 'character')
})

context("testing sitevisitpeak")
test_that("sitevisitpeak examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','sitevisitpeak-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data,'html', 'Author Name'), 'character')
})

context("testing correctionsataglance")
test_that("correctionsataglance examples work",{
  library(jsonlite)
  library(gsplot)
  
  data <- fromJSON(system.file('extdata','correctionsataglance-example.json', package = 'repgen'))
  expect_is(correctionsataglance(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','correctionsataglance-example2.json', package = 'repgen'))
  expect_is(correctionsataglance(data2,'html', 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','correctionsataglance-example3.json', package = 'repgen'))
  expect_is(correctionsataglance(data3,'html', 'Author Name'), 'character')
  
})

setwd(dir = wd)
