wd <- getwd()
setwd(dir = tempdir())

context("testing fiveyeargwsum")

library(jsonlite)
library(gsplot)
library(lubridate)

fiveYrTestJSON <- fromJSON(system.file('extdata','testsnippets','test-fiveyeargwsum.json', package = 'repgen'))

test_that("createfiveyeargwsumPlot properly constructs a gsplot object for the provided report JSON", {
  reportObject1 <- fiveYrTestJSON[['fiveYr']]
  reportObjectInvalid1 <- fiveYrTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  plotNull <- repgen:::createfiveyeargwsumPlot(reportObjectInvalid1)
  expect_is(plotNull, 'NULL')
  expect_equal(plotNull, NULL)

  expect_error(repgen:::createfiveyeargwsumPlot(reportObjectInvalid2))

  fiveYrPlot1 <- repgen:::createfiveyeargwsumPlot(reportObject1)

  expect_is(fiveYrPlot1, 'gsplot')

  #Plot 1-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(fiveYrPlot1)), 2)
  expect_equal(length(gsplot:::views(fiveYrPlot1)), 1)

  #Check Points
  points <- gsplot:::views(fiveYrPlot1)[[1]][which(grepl("points", names(gsplot:::views(fiveYrPlot1)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 3)
  expect_equal(length(points[[1]][['x']]), 2)
  expect_equal(length(points[[1]][['y']]), 2)
  expect_equal(length(points[[2]][['x']]), 1)
  expect_equal(length(points[[2]][['y']]), 1)
  expect_equal(length(points[[3]][['x']]), 1)
  expect_equal(length(points[[3]][['y']]), 1)
  expect_equal(points[[1]][['col']], "orange")
  expect_equal(points[[2]][['col']], "blue")
  expect_equal(points[[3]][['col']], "red")

  #Check Lines
  lines <- gsplot:::views(fiveYrPlot1)[[1]][which(grepl("lines", names(gsplot:::views(fiveYrPlot1)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 2)
  expect_equal(length(lines[[1]][['x']]), 3)
  expect_equal(length(lines[[1]][['y']]), 3)
  expect_equal(length(lines[[2]][['x']]), 2)
  expect_equal(length(lines[[2]][['y']]), 2)
  expect_equal(lines[[1]][['col']], "black")
  expect_equal(lines[[2]][['col']], "black")

  #Check Legend 
  legend <- fiveYrPlot1[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 5)
})

test_that("full five year gw report rendering functions properly", {
  reportObject <- fiveYrTestJSON[['fiveYr']]
  expect_is(fiveyeargwsum(reportObject, 'author'), 'character')
})

setwd(dir = wd)
