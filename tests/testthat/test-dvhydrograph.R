wd <- getwd()
setwd(dir = tempdir())

context("testing DVHydrograph")
dvHydroTestJSON <- fromJSON(system.file('extdata','testsnippets','test-dvhydrograph.json', package = 'repgen'))
test_that("parseDVTimeSeries correctly parses DV Time Series JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- dvHydroTestJSON[['onlyStat1']]

  timezone <- fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- parseDVTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone)
  stat1Est <- parseDVTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone, estimated=TRUE)

  stat2 <- parseDVTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone)
  stat2Est <- parseDVTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone, estimated=TRUE)

  expect_is(stat1, 'list')
  expect_is(stat1Est, 'list')
  expect_is(stat2, 'NULL')
  expect_is(stat2Est, 'NULL')

  expect_equal(nrow(stat1$points), 2)
  expect_equal(nrow(stat1Est$points), 1)
})

test_that("parseDVApprovals returns valid approvals for valid JSON", {
  onlyStat1 <- dvHydroTestJSON[['onlyStat1']]

  timezone <- fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- parseDVTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone)
  approvals <- parseDVApprovals(stat1, timezone)

  expect_is(approvals, 'list')

  expect_equal(length(approvals), 1)
  expect_equal(names(approvals), c('appr_approved_uv'))
})

test_that("parseDVFieldVisitMeasurements returns valid field visit measurements for valid JSON", {
  fieldVisits <- dvHydroTestJSON[['fieldVisits']]
  emptyFieldVisits <- fromJSON('{"fieldVisitMeasurements": []}')
  noFieldVisits <- fromJSON('{}')

  valid <- parseDVFieldVisitMeasurements(fieldVisits)
  empty <- parseDVFieldVisitMeasurements(emptyFieldVisits)
  invalid <- parseDVFieldVisitMeasurements(noFieldVisits)

  expect_warning(parseDVFieldVisitMeasurements(noFieldVisits), "Returning empty data frame")

  expect_is(valid, 'data.frame')
  expect_is(invalid, 'NULL')
  expect_is(empty, 'NULL')
})

test_that("parseDVGroundWaterLevels returns valid min/max IVs for valid JSON", {
  reportObject1 <- dvHydroTestJSON[['gwLevels']]
  reportObject2 <- fromJSON('{"gwlevel": []}')
  reportObject3 <- fromJSON('{}')

  gwData <- parseDVGroundWaterLevels(reportObject1)
  blankData <- parseDVGroundWaterLevels(reportObject2)
  missingData <- parseDVGroundWaterLevels(reportObject3)

  expect_warning(parseDVGroundWaterLevels(reportObject3))

  expect_is(gwData, 'data.frame')
  expect_is(blankData, 'NULL')
  expect_is(missingData, 'NULL')
})

test_that("parseDVMinMaxIVs returns valid min/max IVs for valid JSON", {
  IVs <- dvHydroTestJSON[['onlyIVs']]
  onlyMax <- dvHydroTestJSON[['onlyMaxIV']]
  noTSNoIVs <- dvHydroTestJSON[['noData']]

  timezone <- repgen:::fetchReportMetadataField(IVs, 'timezone')
  type <- "Discharge"

  invalid <- repgen:::parseDVMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseDVMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  onlyMax <- repgen:::parseDVMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseDVMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  normal <- repgen:::parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  inverted <- repgen:::parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = TRUE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  excludeMinMax <- repgen:::parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = TRUE, excludeZeroNegativeFlag = FALSE)
  excludeZeroNegative <- repgen:::parseDVMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = TRUE)

  expect_is(invalid, 'NULL')
  expect_is(normal, 'list')
  expect_is(onlyMax, 'list')
  expect_is(inverted, 'list')
  expect_is(excludeMinMax, 'list')
  expect_is(excludeZeroNegative, 'list')

  expect_equal(names(onlyMax), c('max_iv'))
  expect_equal(names(normal), c('max_iv', 'min_iv'))
  expect_equal(names(inverted), c('max_iv', 'min_iv'))
  expect_equal(names(excludeMinMax), c('max_iv_label', 'min_iv_label'))
  expect_equal(names(excludeZeroNegative), c('max_iv', 'min_iv_label'))
})

test_that("getEstimatedEdges properly creates vertical edge lines between estimated and non-estimated time series", {
  estPoints <- data.frame(value=c(1,2,3,2), time=c(as.POSIXct("2017-01-01 Etc/GMT+5"), as.POSIXct("2017-01-03 Etc/GMT+5"), as.POSIXct("2017-01-04 Etc/GMT+5"), as.POSIXct("2017-01-07 Etc/GMT+5")))
  statPoints <- data.frame(value=c(3,2,2,3), time=c(as.POSIXct("2017-01-02 Etc/GMT+5"), as.POSIXct("2017-01-05 Etc/GMT+5"), as.POSIXct("2017-01-06 Etc/GMT+5"), as.POSIXct("2017-01-08 Etc/GMT+5")))
  emptyPoints <- data.frame(value=c(), time=c())
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

test_that("getMinMaxIV properly retrieves the min/max IV values", {
  IVs <- dvHydroTestJSON[['onlyIVs']]

  max_iv <- repgen:::getMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::getMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::getMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::getMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv$legend.name, "Max. Instantaneous test : 892")
  expect_equal(min_iv$legend.name, "Min. Instantaneous test : -60.5")
  expect_equal(max_iv_inv$legend.name, "Min. Instantaneous test : 892")
  expect_equal(min_iv_inv$legend.name, "Max. Instantaneous test : -60.5")
})

test_that("getMinMaxIV returns NULL when given invalid JSON", { 
  noTSNoIVs <- dvHydroTestJSON[['noTSNoIVs']]

  max_iv <- repgen:::getMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::getMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::getMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::getMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'NULL')
  expect_is(min_iv, 'NULL')
  expect_is(max_iv_inv, 'NULL')
  expect_is(min_iv_inv, 'NULL')
})

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

test_that("createDVHydrographPlot properly constructs a gsplot object for the provided report JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

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
  expect_equal(length(gsplot:::sides(dvHydroPlot1)), 2)
  expect_equal(length(gsplot:::views(dvHydroPlot1)), 1)

  #Check Points
  points <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 0)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot1)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot1)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 2)
  expect_equal(length(lines[[1]][['x']]), 3)
  expect_equal(length(lines[[1]][['y']]), 3)
  expect_equal(length(lines[[2]][['x']]), 2)
  expect_equal(length(lines[[2]][['y']]), 2)
  expect_equal(lines[[1]][['col']], "blue")
  expect_equal(lines[[2]][['col']], "red1")

  #Check Legend 
  legend <- dvHydroPlot1[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 3)

  #Plot 2-----
  #Check Overall Plot Data
  expect_equal(length(gsplot:::sides(dvHydroPlot2)), 2)
  expect_equal(length(gsplot:::views(dvHydroPlot2)), 1)

  #Check Points
  points <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("points", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(points, 'list')
  expect_equal(length(points), 2)

  #Check Lines
  lines <- gsplot:::views(dvHydroPlot2)[[1]][which(grepl("lines", names(gsplot:::views(dvHydroPlot2)[[1]])))]
  expect_is(lines, 'list')
  expect_equal(length(lines), 7)
  expect_equal(length(lines[[1]][['x']]), 6)
  expect_equal(length(lines[[1]][['y']]), 6)
  expect_equal(length(lines[[2]][['x']]), 5)
  expect_equal(length(lines[[2]][['y']]), 5)
  expect_equal(length(lines[[3]][['x']]), 2)
  expect_equal(length(lines[[3]][['y']]), 2)
  expect_equal(length(lines[[4]][['x']]), 5)
  expect_equal(length(lines[[4]][['y']]), 5)
  expect_equal(length(lines[[5]][['x']]), 2)
  expect_equal(length(lines[[5]][['y']]), 2)
  expect_equal(length(lines[[6]][['x']]), 5)
  expect_equal(length(lines[[6]][['y']]), 5)
  expect_equal(length(lines[[7]][['x']]), 2)
  expect_equal(length(lines[[7]][['y']]), 2)
  expect_equal(lines[[1]][['col']], 'blue')
  expect_equal(lines[[2]][['col']], 'maroon')
  expect_equal(lines[[3]][['col']], 'red2')
  expect_equal(lines[[4]][['col']], 'orange')
  expect_equal(lines[[5]][['col']], 'red3')
  expect_equal(lines[[6]][['col']], 'green')
  expect_equal(lines[[7]][['col']], 'red4')

  #Check Legend
  legend <- dvHydroPlot2[['legend']][['legend.auto']][['legend']]
  expect_is(legend, 'character')
  expect_equal(length(legend), 10)
})

test_that("createDVHydrographRefPlot properly constructs a gsplot object for the provided report JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  reportObject <- dvHydroTestJSON[['allStats']]
  reportObjectInvalid1 <- dvHydroTestJSON[['noData']]
  reportObjectInvalid2 <- fromJSON('{}')

  plotNull <- repgen:::createDVHydrographPlot(reportObjectInvalid1)
  expect_is(plotNull, 'NULL')
  expect_equal(plotNull, NULL)

  expect_error(repgen:::createDVHydrographRefPlot(reportObjectInvalid2))

  dvHydroPlot1 <- repgen:::createDVHydrographRefPlot(reportObject, "secondaryReferenceTimeSeries", "inputDataDescriptions2")
  dvHydroPlot2 <- repgen:::createDVHydrographRefPlot(reportObject, "tertiaryReferenceTimeSeries", "inputDataDescriptions3")
  dvHydroPlot3 <- repgen:::createDVHydrographRefPlot(reportObject, "quaternaryReferenceTimeSeries", "inputDataDescriptions4")

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
  expect_equal(length(lines[[1]][['x']]), 5)
  expect_equal(length(lines[[1]][['y']]), 5)
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
  expect_equal(length(lines), 2)
  expect_equal(length(lines[[1]][['x']]), 4)
  expect_equal(length(lines[[1]][['y']]), 4)
  expect_equal(length(lines[[2]][['x']]), 1)
  expect_equal(length(lines[[2]][['y']]), 1)
  expect_equal(lines[[1]][['col']], "orange")
  expect_equal(lines[[2]][['col']], "red2")

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
  expect_equal(length(lines[[1]][['x']]), 5)
  expect_equal(length(lines[[1]][['y']]), 5)
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
