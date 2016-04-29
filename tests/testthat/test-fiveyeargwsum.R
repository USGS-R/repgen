context("fiveyeargwsum tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing fiveyeargwsum")
test_that("fiveyeargwsum examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  
  data <- fromJSON(system.file('extdata','fiveyeargwsum','fiveyeargwsum-example.json', package = 'repgen'))
  expect_is(fiveyeargwsum(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','fiveyeargwsum','fiveyeargwsum-with-discrete-example.json', package = 'repgen'))
  expect_is(fiveyeargwsum(data2,'html', 'Author Name'), 'character')
})

setwd(dir = wd)
