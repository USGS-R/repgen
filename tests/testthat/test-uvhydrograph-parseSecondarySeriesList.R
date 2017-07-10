context("uvhydrograph parseSecondarySeriesList tests")

wd <- getwd()
setwd(dir = tempdir())


test_that("useEstimated is true if corrected data exists only as estimated data and non-estimated data",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-all-estimated-secondary-series.json', package = 'repgen'))
  
  expect_true(repgen:::parseSecondarySeriesList(reportObject, "1701", "Etc/GMT+5")[['useEstimated']])
})

test_that("useEstimated is false if no corrected data exists",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-no-secondary-pts.json', package = 'repgen'))
                           
  expect_false(repgen:::parseSecondarySeriesList(reportObject, "1701", "Etc/GMT+5")[['useEstimated']])
})
  
test_that("useEstimated is false if corrected estimated and non-estimated data exists",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-estimated-and-nonestimated-periods.json', package = 'repgen'))

  expect_false(repgen:::parseSecondarySeriesList(reportObject, "1611", "Etc/GMT+5")[['useEstimated']])
})

setwd(dir = wd)