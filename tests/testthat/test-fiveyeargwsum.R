wd <- getwd()
setwd(dir = tempdir())

context("testing fiveyeargwsum")

library(jsonlite)
library(gsplot)
library(lubridate)

fiveYrTestJSON <- fromJSON(system.file('extdata','testsnippets','test-fiveyeargwsum.json', package = 'repgen'))
fiveYrMultiple <- fromJSON(system.file('extdata','testsnippets','test-fiveyeargwsum-multiple-axis.json', package = 'repgen'))


test_that("createfiveyeargwsumPlot properly constructs a gsplot object for the provided report JSON", {
  reportObject1 <- fiveYrTestJSON[['fiveYr']]
  reportObject2 <- fiveYrTestJSON[['fiveYrNoMinMax']]
  reportObjectInvalid1 <- fiveYrTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  plotNull <- repgen:::createfiveyeargwsumPlot(reportObjectInvalid1)
  expect_is(plotNull, 'NULL')
  expect_equal(plotNull, NULL)

  expect_error(repgen:::createfiveyeargwsumPlot(reportObjectInvalid2))

  fiveYrPlot1 <- repgen:::createfiveyeargwsumPlot(reportObject1)
  fiveYrPlot2 <- repgen:::createfiveyeargwsumPlot(reportObject2)

  expect_is(fiveYrPlot1, 'gsplot')
  expect_is(fiveYrPlot2, 'gsplot')

  #Plot 1-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(fiveYrPlot1)), 3)
  expect_equal(length(gsplot:::views(fiveYrPlot1)), 2)

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
  expect_equal(length(lines[[1]][['x']]), 4)
  expect_equal(length(lines[[1]][['y']]), 4)
  expect_equal(length(lines[[2]][['x']]), 3)
  expect_equal(length(lines[[2]][['y']]), 3)
  expect_equal(lines[[1]][['col']], "blue")
  expect_equal(lines[[2]][['col']], "blue")

  #Check Legend 
  legend <- fiveYrPlot1[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 5)

  #Plot 2-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(fiveYrPlot2)), 3)
  expect_equal(length(gsplot:::views(fiveYrPlot2)), 2)

  #Check Points
  points <- gsplot:::views(fiveYrPlot2)[[1]][which(grepl("points", names(gsplot:::views(fiveYrPlot2)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 0)

  #Check Lines
  lines <- gsplot:::views(fiveYrPlot2)[[1]][which(grepl("lines", names(gsplot:::views(fiveYrPlot2)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 1)
  expect_equal(length(lines[[1]][['x']]), 3)
  expect_equal(length(lines[[1]][['y']]), 3)
  expect_equal(lines[[1]][['col']], "blue")

  #Check Legend 
  legend <- fiveYrPlot2[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 1)
})

test_that("full five year gw report rendering functions properly", {
  reportObject <- fiveYrTestJSON[['fiveYrNoMinMax']]
  expect_is(fiveyeargwsum(reportObject, 'author'), 'character')
})

test_that("multiple axis sorts data by types correctly to different sides", { 
  reportObject <- fiveYrMultiple
  timezone <- "Etc/GMT+5"
  stat1TimeSeries <- repgen:::parseTimeSeries(reportObject, 'firstStatDerived', 'firstStatDerivedLabel', timezone, isDV=TRUE)
  stat2TimeSeries <- repgen:::parseTimeSeries(reportObject, 'secondStatDerived', 'secondStatDerivedLabel', timezone, isDV=TRUE)
  stat3TimeSeries <- repgen:::parseTimeSeries(reportObject, 'thirdStatDerived', 'thirdStatDerivedLabel', timezone, isDV=TRUE)
  stat4TimeSeries <- repgen:::parseTimeSeries(reportObject, 'fourthStatDerived', 'fourthStatDerivedLabel', timezone, isDV=TRUE)
  sides <- repgen:::getSides(stat1TimeSeries, stat2TimeSeries, stat3TimeSeries, stat4TimeSeries)
  expect_equal(length(sides[['sideList']]),2)
  expect_equal(sides[['sideList']][[1]][['types']],"Elevation, GW, NAVD88")
  expect_equal(sides[['sideList']][[1]][['side']],"2")
  expect_equal(sides[['sideList']][[1]][['timeseries']],"stat1TimeSeries")
  expect_equal(sides[['sideList']][[2]][['types']],"Temperature, water")
  expect_equal(sides[['sideList']][[2]][['side']],"4")
  expect_equal(sides[['sideList']][[2]][['timeseries']],"stat2TimeSeries")
  expect_equal(sides[['seriesList']][['stat1TimeSeries']][['side']],"2")
  expect_equal(sides[['typeLims']][[stat1TimeSeries[['type']]]][['ylim']][1], -1.11)
  expect_equal(sides[['typeLims']][[stat1TimeSeries[['type']]]][['ylim']][2], 1.88)
  expect_equal(sides[['typeLims']][[stat2TimeSeries[['type']]]][['ylim']][1], -1.11)
  expect_equal(sides[['typeLims']][[stat2TimeSeries[['type']]]][['ylim']][2], 27.70)
})

setwd(dir = wd)
