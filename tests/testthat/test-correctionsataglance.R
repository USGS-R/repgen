context("correctionsataglance tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing correctionsataglance")
test_that("correctionsataglance examples work",{
  library(jsonlite)
  library(lubridate)
  library(dplyr)
  library(gsplot)
  
  data <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example.json', package = 'repgen'))
  expect_is(correctionsataglance(data, 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example2.json', package = 'repgen'))
  expect_is(correctionsataglance(data2, 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example3.json', package = 'repgen'))
  expect_is(correctionsataglance(data3, 'Author Name'), 'character')
  
  data4 <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example4.json', package = 'repgen'))
  expect_is(correctionsataglance(data4, 'Author Name'), 'character')
})

test_that("correctionsataglance duplicate legend values are removed",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','correctionsataglance','correctionsataglance-example.json', package = 'repgen'))
  corr_results <- correctionsataglanceReport(data)
  corr_plot <- corr_results$timeline
  
  i <- which(names(corr_plot$legend) == 'legend.args')
  all_legend_names <- unlist(lapply(corr_plot$legend[i], function(l) {l$legend}))
  
  expect_equal(anyDuplicated(all_legend_names), 0)
  
})

test_that("correctionsataglance qualifier appends are based on closed-open intervals", {
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  library(dplyr)
  
  # TODO:
  expect_equal(0, 0)
})

setwd(dir = wd)
