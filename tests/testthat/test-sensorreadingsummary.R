context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  reportData <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportData, 'Author Name'), 'character')
  
  reportData2 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example-exc-comm.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportData, 'Author Name'), 'character')
})


setwd(dir = wd)
