context("derivationchain tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing derivationchain report")
test_that("derivationchain work", {
  library(jsonlite)
  library(readr)
  
  report <- renderReport(fromJSON(system.file('extdata',"derivationchain", "derivationchain-example.json",package = 'repgen')), "derivationchain", "author");
  renderedHtml <- read_file(report)
  
  expect_is(report, 'character')
  
  #html title exists in head element present
  expect_equal(grep("<title>Derivation Chain - 06933500</title>", renderedHtml), 1)
  
  #Contains cytoscape div
  expect_equal(grep('<div id="cy"></div>', renderedHtml), 1)
  
})

setwd(dir = wd)

