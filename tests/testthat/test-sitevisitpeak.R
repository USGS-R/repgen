context("sitevisitpeak tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sitevisitpeak")
test_that("sitevisitpeak examples work",{
  library(jsonlite)
  
  data <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-empty-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data2,'html', 'Author Name'), 'character')
})

setwd(dir = wd)
