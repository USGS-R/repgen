context("reportRender tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing generic example report")
test_that("generic examples work", {
  library(jsonlite)
  
  report <- renderReport(fromJSON(system.file('extdata',"example-report.json",package = 'repgen')), "examplereport", "author");
  expect_is(report, 'character')
})

setwd(dir = wd)

