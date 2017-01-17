context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  reportObject <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject, 'Author Name'), 'character')
  
  reportObject2 <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example-exc-comm.json', package = 'repgen'))
  expect_is(repgen:::sensorreadingsummary(reportObject2, 'Author Name'), 'character')
})


setwd(dir = wd)
