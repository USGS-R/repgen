context("reportRender tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing generic example report")
test_that("generic examples work", {
  library(jsonlite)
  library(readr)
  
  report <- renderReport(fromJSON(system.file('extdata',"example-report.json",package = 'repgen')), "examplereport", "author");
  renderedHtml <- read_file(report)
  
  expect_is(report, 'character')
  
  #html title exists in head element present
  expect_equal(grep("<title>Example Report - 12345</title>", renderedHtml), 1)
  
  #Contains css links
  expect_equal(grep('<link rel="stylesheet" href="data:text/css;base64', renderedHtml), 1)
  
  #Contains js script tags
  expect_equal(grep('<script type="text/javascript" src="data:application/x-javascript;base64,', renderedHtml), 1)
  
  #Contains text from header
  expect_equal(grep("<h2>Example Report</h2>", renderedHtml), 1)
  
  #Contains image for logo
  expect_equal(grep('<img src="data:image/jpeg;base64,', renderedHtml), 1)
  
  #Contains text from body.mustache
  expect_equal(grep("THIS TEXT FROM REPORT'S BODY TEMPLATE. ", renderedHtml), 1)
  
  #Contains station string from header
  expect_equal(grep('12345 - Station Name', renderedHtml), 1)
  
  #custom fragments rendered
  expect_equal(grep("HTML from code  data value 1", renderedHtml), 1)
  expect_equal(grep("HTML from code  data value 2", renderedHtml), 1)
  
  #data from template data rendered
  expect_equal(grep("From data data value 1", renderedHtml), 1)
  expect_equal(grep("From data data value 2", renderedHtml), 1)
  
  #Html from first partial
  expect_equal(grep("HTML FROM FIRST PARTIAL", renderedHtml), 1)
  
  #Html from second partial
  expect_equal(grep("HTML FROM SECOND PARTIAL", renderedHtml), 1)
})

setwd(dir = wd)

