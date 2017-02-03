wd <- getwd()
setwd(dir = tempdir())

context("testing fiveyeargwsum")

library(jsonlite)
library(gsplot)
library(lubridate)

fiveYrTestJSON <- fromJSON(system.file('extdata','testsnippets','test-fiveyeargwsum.json', package = 'repgen'))

test_that("getPriorityStat properly finds the priority stat time series from the report JSON", {
  reportObjectMean <- fiveYrTestJSON[['priorityStatMean']]
  reportObjectMax <- fiveYrTestJSON[['priorityStatMax']]
  reportObjectMin <- fiveYrTestJSON[['priorityStatMin']]  
  reportObjectInvalid1 <- fiveYrTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  meanStat <- repgen:::getPriorityStat(reportObjectMean)
  maxStat <- repgen:::getPriorityStat(reportObjectMax)
  minStat <- repgen:::getPriorityStat(reportObjectMin)
  invalidStat1 <- repgen:::getPriorityStat(reportObjectInvalid1)
  invalidStat2 <- repgen:::getPriorityStat(reportObjectInvalid2)

  expect_is(meanStat, 'list')
  expect_is(minStat, 'list')
  expect_is(maxStat, 'list')
  expect_is(invalidStat1, 'list')
  expect_is(invalidStat1, 'list')

  expect_equal(meanStat[['data_nm']], 'secondDownChain')
  expect_equal(maxStat[['data_nm']], 'thirdDownChain')
  expect_equal(minStat[['data_nm']], 'firstDownChain')
  expect_equal(meanStat[['descr_nm']], 'downChainDescriptions2')
  expect_equal(maxStat[['descr_nm']], 'downChainDescriptions3')
  expect_equal(minStat[['descr_nm']], 'downChainDescriptions1')
})

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
  expect_equal(length(lines[[1]][['x']]), 2)
  expect_equal(length(lines[[1]][['y']]), 2)
  expect_equal(lines[[1]][['col']], "black")

  #Check Legend 
  legend <- fiveYrPlot2[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 1)
})

test_that("full five year gw report rendering functions properly", {
  reportObject <- fiveYrTestJSON[['fiveYrNoMinMax']]
  expect_is(fiveyeargwsum(reportObject, 'author'), 'character')
})

setwd(dir = wd)
