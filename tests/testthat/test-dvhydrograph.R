wd <- getwd()
setwd(dir = tempdir())

context("testing DVHydrograph")

library(jsonlite)
library(gsplot)
library(lubridate)
library(dplyr)

dvHydroTestJSON <- fromJSON(system.file('extdata','testsnippets','test-dvhydrograph.json', package = 'repgen'))

test_that("extendStep properly extends the last time step by a day", {
  preStepData <- list(
    x=c(as.POSIXct("2014-11-20 GMT+5"), as.POSIXct("2014-11-21 GMT+5")),
    y=c(4510, 4511),
    type=c("s")
  )

  postStepData <- repgen:::extendStep(preStepData)

  expect_is(postStepData, 'list')

  expect_equal(length(postStepData$x), 3)
  expect_equal(length(postStepData$y), 3)
  expect_equal(postStepData$x[1], as.POSIXct("2014-11-20 GMT+5"))
  expect_equal(postStepData$x[2], as.POSIXct("2014-11-21 0GMT+5"))
  expect_equal(postStepData$x[3], as.POSIXct("2014-11-22 0GMT+5"))
  expect_equal(postStepData$y[1], 4510)
  expect_equal(postStepData$y[2], 4511)
  expect_equal(postStepData$y[3], 4511)
})

test_that("getEstimatedEdges properly creates vertical edge lines between estimated and non-estimated time series", {
  estPoints <- list(points = data.frame(value=c(1,2,3,2), time=c(as.POSIXct("2017-01-01 Etc/GMT+5"), as.POSIXct("2017-01-03 Etc/GMT+5"), as.POSIXct("2017-01-04 Etc/GMT+5"), as.POSIXct("2017-01-07 Etc/GMT+5"))))
  statPoints <- list(points = data.frame(value=c(3,2,2,3), time=c(as.POSIXct("2017-01-02 Etc/GMT+5"), as.POSIXct("2017-01-05 Etc/GMT+5"), as.POSIXct("2017-01-06 Etc/GMT+5"), as.POSIXct("2017-01-08 Etc/GMT+5"))))
  emptyPoints <- list(points = data.frame(value=c(), time=c()))
  estEdges <- repgen:::getEstimatedEdges(statPoints, estPoints)
  estEdges2 <- repgen:::getEstimatedEdges(statPoints, emptyPoints)

  expect_is(estEdges, 'list')
  expect_is(estEdges2, 'NULL')
  
  expect_equal(length(estEdges$y0), 5)
  expect_equal(length(estEdges$y1), 5)
  expect_equal(length(estEdges$time), 5)
  expect_equal(length(estEdges$newSet), 5)
  expect_equal(estEdges$y0[1], 1)
  expect_equal(estEdges$y1[1], 3)
  expect_equal(estEdges$y0[3], 3)
  expect_equal(estEdges$y1[3], 2)
  expect_equal(estEdges$newSet[1], "stat")
  expect_equal(estEdges$newSet[4], "est")
  expect_equal(estEdges$time[1], as.POSIXct("2017-01-02 Etc/GMT+5"))
  expect_equal(estEdges$time[4], as.POSIXct("2017-01-07 Etc/GMT+5"))
  expect_equal(estEdges$time[5], as.POSIXct("2017-01-08 Etc/GMT+5"))
})

test_that("createDVHydrographPlot properly constructs a gsplot object for the provided report JSON", {
  reportObject1 <- dvHydroTestJSON[['onlyStat1']]
  reportObject2 <- dvHydroTestJSON[['allStats']]
  reportObjectInvalid1 <- dvHydroTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  plotNull <- repgen:::createDVHydrographPlot(reportObjectInvalid1)
  expect_is(plotNull, 'NULL')
  expect_equal(plotNull, NULL)

  expect_error(repgen:::createDVHydrographPlot(reportObjectInvalid2))

  dvHydroPlot1 <- repgen:::createDVHydrographPlot(reportObject1)
  dvHydroPlot2 <- repgen:::createDVHydrographPlot(reportObject2)

  expect_is(dvHydroPlot1, 'gsplot')
  expect_is(dvHydroPlot2, 'gsplot')

  #Plot 1-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot1)), 3)
  expect_equal(length(gsplot:::views(dvHydroPlot1)), 2)

  #Check Points
  points <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 1)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 3)
  expect_equal(length(lines[[1]][['x']]), 2)
  expect_equal(length(lines[[1]][['y']]), 2)
  expect_equal(length(lines[[2]][['x']]), 2)
  expect_equal(length(lines[[2]][['y']]), 2)
  expect_equal(length(lines[[3]][['x']]), 2)
  expect_equal(length(lines[[3]][['y']]), 2)
  expect_equal(lines[[1]][['col']], "blue")
  expect_equal(lines[[2]][['col']], "blue")
  expect_equal(lines[[3]][['col']], "red")

  #Check Legend 
  legend <- dvHydroPlot1[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 4)

  #Plot 2-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot2)), 3)
  expect_equal(length(gsplot:::views(dvHydroPlot2)), 2)

  #Check Points
  points <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 4)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 10)
  expect_equal(length(lines[[1]][['x']]), 6)
  expect_equal(length(lines[[1]][['y']]), 6)
  expect_equal(length(lines[[2]][['x']]), 2)
  expect_equal(length(lines[[2]][['y']]), 2)
  expect_equal(length(lines[[3]][['x']]), 4)
  expect_equal(length(lines[[3]][['y']]), 4)
  expect_equal(length(lines[[4]][['x']]), 2)
  expect_equal(length(lines[[4]][['y']]), 2)
  expect_equal(length(lines[[5]][['x']]), 4)
  expect_equal(length(lines[[5]][['y']]), 4)
  expect_equal(length(lines[[6]][['x']]), 2)
  expect_equal(length(lines[[6]][['y']]), 2)
  expect_equal(length(lines[[7]][['x']]), 2)
  expect_equal(length(lines[[7]][['y']]), 2)
  expect_equal(length(lines[[8]][['x']]), 2)
  expect_equal(length(lines[[8]][['y']]), 2)
  expect_equal(length(lines[[9]][['x']]), 4)
  expect_equal(length(lines[[9]][['y']]), 4)
  expect_equal(length(lines[[10]][['x']]), 2)
  expect_equal(length(lines[[10]][['y']]), 2)
  expect_equal(lines[[1]][['col']], 'blue')
  expect_equal(lines[[2]][['col']], 'maroon')
  expect_equal(lines[[3]][['col']], 'maroon')
  expect_equal(lines[[4]][['col']], 'orange')
  expect_equal(lines[[5]][['col']], 'orange')
  expect_equal(lines[[6]][['col']], 'red1')
  expect_equal(lines[[7]][['col']], 'red2')
  expect_equal(lines[[8]][['col']], 'green')
  expect_equal(lines[[9]][['col']], 'green')
  expect_equal(lines[[10]][['col']], 'red4')

  #Check Legend
  legend <- dvHydroPlot2[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 12)
})

test_that("createDVHydrographRefPlot properly constructs a gsplot object for the provided report JSON", {
  reportObject <- dvHydroTestJSON[['allStats']]
  reportObjectInvalid1 <- dvHydroTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  plotNull <- repgen:::createDVHydrographRefPlot(reportObjectInvalid1, "test1", "test2")
  expect_is(plotNull, 'NULL')
  expect_equal(plotNull, NULL)

  expect_error(repgen:::createDVHydrographRefPlot(reportObjectInvalid2, "test1", "test2"))

  dvHydroPlot1 <- repgen:::createDVHydrographRefPlot(reportObject, "firstReferenceTimeSeries", "firstReferenceTimeSeriesLabel")
  dvHydroPlot2 <- repgen:::createDVHydrographRefPlot(reportObject, "secondReferenceTimeSeries", "secondReferenceTimeSeriesLabel")
  dvHydroPlot3 <- repgen:::createDVHydrographRefPlot(reportObject, "thirdReferenceTimeSeries", "thirdReferenceTimeSeriesLabel")

  expect_is(dvHydroPlot1, 'gsplot')
  expect_is(dvHydroPlot2, 'gsplot')
  expect_is(dvHydroPlot3, 'gsplot')

  #Plot 1-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot1)), 2)
  expect_equal(length(gsplot:::views(dvHydroPlot1)), 1)

  #Check Points
  points <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 0)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 1)
  expect_equal(length(lines[[1]][['x']]), 6)
  expect_equal(length(lines[[1]][['y']]), 6)
  expect_equal(lines[[1]][['col']], "blue")

  #Check Legend 
  legend <- dvHydroPlot1[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 2)

  #Plot 2-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot2)), 2)
  expect_equal(length(gsplot:::views(dvHydroPlot2)), 1)

  #Check Points
  points <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 0)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 3)
  expect_equal(length(lines[[1]][['x']]), 2)
  expect_equal(length(lines[[1]][['y']]), 2)
  expect_equal(length(lines[[2]][['x']]), 4)
  expect_equal(length(lines[[2]][['y']]), 4)
  expect_equal(length(lines[[3]][['x']]), 2)
  expect_equal(length(lines[[3]][['y']]), 2)
  expect_equal(lines[[1]][['col']], "orange")
  expect_equal(lines[[2]][['col']], "orange")
  expect_equal(lines[[3]][['col']], "red2")

  #Check Legend 
  legend <- dvHydroPlot2[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 3)

  #Plot 3-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot3)), 2)
  expect_equal(length(gsplot:::views(dvHydroPlot3)), 1)

  #Check Points
  points <- gsplot:::views(dvHydroPlot3)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot3)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 0)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot3)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot3)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 1)
  expect_equal(length(lines[[1]][['x']]), 6)
  expect_equal(length(lines[[1]][['y']]), 6)
  expect_equal(lines[[1]][['col']], "purple")

  #Check Legend 
  legend <- dvHydroPlot3[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 2)
})

test_that("full DV Hydro report rendering functions properly", {
  reportObject <- dvHydroTestJSON[['allStats']]
  expect_is(dvhydrograph(reportObject, 'author'), 'character')
})

setwd(dir = wd)
