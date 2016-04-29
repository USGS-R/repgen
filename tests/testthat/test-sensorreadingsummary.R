context("sensorreadingsummary tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','sensorreadingsummary','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(sensorreadingsummary(data,'html', 'Author Name'), 'character')
})


setwd(dir = wd)
