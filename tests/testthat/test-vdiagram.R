context("vdiagram tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing vdiagram when some fields are missing and are complete")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-example.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
})

# This test triggers the "maxStage" error found in AQCU-680
context("testing vdiagram 5yr data")
test_that("vdiagram examples work", {
 library(jsonlite)
 library(gsplot)
 data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-5yr-example.json",package = 'repgen'))
 expect_is(vdiagram(data, 'html'), 'character')
})

context("testing vdiagram when gage heights are blank")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-no_gage_heights.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
})

context("testing vdiagram when gage heights are blank for historic years")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-no_gage_heights_historic.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
})

setwd(dir = wd)
