wd <- getwd()
setwd(dir = tempdir())

context("testing DVHydrograph")
dvHydroTestJSON <- fromJSON(system.file('extdata','testsnippets','test-dvhydrograph.json', package = 'repgen'))
test_that("parseTimeSeries correctly parses DV Time Series JSON", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)

  onlyStat1 <- dvHydroTestJSON[['onlyStat1']]

  timezone <- fetchReportMetadataField(onlyStat1, 'timezone')

  stat1 <- parseTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone, isDV=TRUE)
  stat1Est <- parseTimeSeries(onlyStat1, 'firstDownChain', 'downChainDescriptions1', timezone, estimated=TRUE, isDV=TRUE)

  stat2 <- parseTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone, isDV=TRUE)
  stat2Est <- parseTimeSeries(onlyStat1, 'secondDownChain', 'downChainDescriptions2', timezone, estimated=TRUE, isDV=TRUE)

  expect_is(stat1, 'list')
  expect_is(stat1Est, 'list')
  expect_is(stat2, 'NULL')
  expect_is(stat2Est, 'NULL')

  expect_equal(nrow(stat1$points), 2)
  expect_equal(nrow(stat1Est$points), 1)
})

test_that("parseFieldVisitMeasurements returns valid field visit measurements for valid JSON", {
  fieldVisits <- dvHydroTestJSON[['fieldVisits']]
  emptyFieldVisits <- fromJSON('{"fieldVisitMeasurements": []}')
  noFieldVisits <- fromJSON('{}')

  valid <- parseFieldVisitMeasurements(fieldVisits)
  empty <- parseFieldVisitMeasurements(emptyFieldVisits)
  invalid <- parseFieldVisitMeasurements(noFieldVisits)

  expect_warning(parseFieldVisitMeasurements(noFieldVisits), "Returning NULL as DV Hydro field visit measurements.")

  expect_is(valid, 'data.frame')
  expect_is(invalid, 'NULL')
  expect_is(empty, 'NULL')
})

test_that("parseGroundWaterLevels returns valid min/max IVs for valid JSON", {
  reportObject1 <- dvHydroTestJSON[['gwLevels']]
  reportObject2 <- fromJSON('{"gwlevel": []}')
  reportObject3 <- fromJSON('{}')

  gwData <- parseGroundWaterLevels(reportObject1)
  blankData <- parseGroundWaterLevels(reportObject2)
  missingData <- parseGroundWaterLevels(reportObject3)

  expect_warning(parseGroundWaterLevels(reportObject3), "Returning NULL as DV Hydro ground water levels.")

  expect_is(gwData, 'data.frame')
  expect_is(blankData, 'NULL')
  expect_is(missingData, 'NULL')
})

test_that("parseMinMaxIVs returns valid min/max IVs for valid JSON", {
  IVs <- dvHydroTestJSON[['onlyIVs']]
  onlyMax <- dvHydroTestJSON[['onlyMaxIV']]
  noTSNoIVs <- dvHydroTestJSON[['noData']]

  timezone <- repgen:::fetchReportMetadataField(IVs, 'timezone')
  type <- "Discharge"

  invalid <- repgen:::parseMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseMinMaxIVs(noTSNoIVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  onlyMax <- repgen:::parseMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  expect_warning(repgen:::parseMinMaxIVs(onlyMax, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE))

  normal <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  inverted <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = TRUE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = FALSE)
  excludeMinMax <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = TRUE, excludeZeroNegativeFlag = FALSE)
  excludeZeroNegative <- repgen:::parseMinMaxIVs(IVs, timezone, type, invertedFlag = FALSE, excludeMinMaxFlag = FALSE, excludeZeroNegativeFlag = TRUE)

  expect_is(invalid, 'NULL')
  expect_is(normal, 'list')
  expect_is(onlyMax, 'list')
  expect_is(inverted, 'list')
  expect_is(excludeMinMax, 'list')
  expect_is(excludeZeroNegative, 'list')

  expect_equal(names(onlyMax), c('max_iv', 'canLog'))
  expect_equal(names(normal), c('max_iv', 'min_iv', 'canLog'))
  expect_equal(names(inverted), c('max_iv', 'min_iv', 'canLog'))
  expect_equal(names(excludeMinMax), c('max_iv_label', 'min_iv_label', 'canLog'))
  expect_equal(names(excludeZeroNegative), c('max_iv', 'min_iv_label', 'canLog'))
  expect_equal(onlyMax[['canLog']], TRUE)
  expect_equal(normal[['canLog']], FALSE)
  expect_equal(inverted[['canLog']], FALSE)
  expect_equal(excludeMinMax[['canLog']], TRUE)
  expect_equal(excludeZeroNegative[['canLog']], TRUE)
})

test_that("parseMinMaxIV properly retrieves the min/max IV values", {
  IVs <- dvHydroTestJSON[['onlyIVs']]

  max_iv <- repgen:::parseMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(IVs, "MAX", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(IVs, "MIN", repgen:::fetchReportMetadataField(IVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'list')
  expect_is(min_iv, 'list')
  expect_is(max_iv_inv, 'list')
  expect_is(min_iv_inv, 'list')

  expect_equal(max_iv$legend.name, "Max. Instantaneous test : 892")
  expect_equal(min_iv$legend.name, "Min. Instantaneous test : -60.5")
  expect_equal(max_iv_inv$legend.name, "Min. Instantaneous test : 892")
  expect_equal(min_iv_inv$legend.name, "Max. Instantaneous test : -60.5")
})

test_that("parseMinMaxIV returns NULL when given invalid JSON", { 
  noTSNoIVs <- dvHydroTestJSON[['noTSNoIVs']]

  max_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  min_iv <- repgen:::parseMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", FALSE)
  max_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "MAX", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)
  min_iv_inv <- repgen:::parseMinMaxIV(noTSNoIVs, "MIN", repgen:::fetchReportMetadataField(noTSNoIVs, 'timezone'), "test", TRUE)

  expect_is(max_iv, 'NULL')
  expect_is(min_iv, 'NULL')
  expect_is(max_iv_inv, 'NULL')
  expect_is(min_iv_inv, 'NULL')
})

setwd(dir = wd)