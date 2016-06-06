context("uvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing uvhydrograph")
test_that("uvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-groundwater.json', package = 'repgen'))
  expect_is(uvhydrograph(data2,'html', 'Author Name'), 'character')
  
  data4 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-hawaii.json', package = 'repgen'))
  expect_is(uvhydrograph(data4,'html', 'Author Name'), 'character')
  
  data5 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-pH-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data5,'html', 'Author Name'), 'character')
  
  data6 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-missingmonth.json', package = 'repgen'))
  expect_is(uvhydrograph(data6,'html', 'Author Name'), 'character')
})

setwd(dir = wd)
