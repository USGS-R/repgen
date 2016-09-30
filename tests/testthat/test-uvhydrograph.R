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

  data5 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-wq-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data5,'html', 'Author Name'), 'character')
  
  data6 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-missingmonth.json', package = 'repgen'))
  expect_is(uvhydrograph(data6,'html', 'Author Name'), 'character')
  
  data7 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-allapprovals.json', package = 'repgen'))
  expect_is(uvhydrograph(data7,'html', 'Author Name'), 'character')
  
  data8 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-3-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data8,'html', 'Author Name'), 'character')
  
  data9 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-3-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data9,'html', 'Author Name'), 'character')
  
  data10 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-comp-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data10,'html', 'Author Name'), 'character')
  
  data11 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-ref-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data11,'html', 'Author Name'), 'character')
  
  data12 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-ref-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data12,'html', 'Author Name'), 'character')
  
  data13 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-ref-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data13,'html', 'Author Name'), 'character')
  
  data14 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-ref-comp-same-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data14,'html', 'Author Name'), 'character')
  
  data15 <- fromJSON(system.file('extdata','uvhydrograph','uvhydro-prim-comp-diff-axis.json', package = 'repgen'))
  expect_is(uvhydrograph(data15,'html', 'Author Name'), 'character')
})

setwd(dir = wd)
