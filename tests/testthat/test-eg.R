
context("setting wd to temp folder")
wd <- getwd()
setwd(dir = tempdir())
context("testing extremes json parsing")

test_that("bad list for extremes", {
    data <- list('data'=c(0,0,0))
    expect_error(extremes(data, 'html'))
})

context("testing limits fail w/ NAs")

test_that('missing data returns as expected', {
  data <- list('data'=c(0,0,0))
  expect_equal(getRatingShifts(data, 'shiftPoints'), " ")
  expect_error(getRatingShifts(data, 'shiftPoints', required = TRUE))
  expect_is(getRatingShifts(data, 'shiftPoints', as.numeric = T), 'numeric')
  expect_is(getRatingShifts(data, 'shiftPoints', as.numeric = F), 'character')
  expect_error(repgen:::getLims(NA,NA,NA,NA)) 
})

context("testing extremes when some fields are missing and are complete")
test_that("example data extremes", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
  expect_is(extremes(data, 'html'), 'character')
  
})

context("testing vdiagram when some fields are missing and are complete")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata',"vdiagram-example.json",package = 'repgen'))
  expect_is(vdiagram(data, 'html'), 'character')
  
})

context("testing uvhydrograph")
test_that("uvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','uvhydro-example.json', package = 'repgen'))
  expect_is(uvhydrograph(data,'html', 'Author Name'), 'character')
})

context("testing dvhydrograph")
test_that("dvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','dvhydro-example.json', package = 'repgen'))
  expect_is(dvhydrograph(data,'html', 'Author Name'), 'character')
})

test_that("dvhydrograph axes flip",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','dvhydro-waterlevel-example.json', package = 'repgen'))
  expect_true(data$reportMetadata$isInverted)
  
  plot1 <- createDvhydrographPlot(data)
  ylims1 <- ylim(plot1)[[1]]
  expect_true(ylims1[1] > ylims1[2])
  
  plot2 <- createRefPlot(data, "secondary")
  ylims2 <- ylim(plot2)[[1]]
  expect_true(ylims2[1] > ylims2[2])
  
  plot3 <- createRefPlot(data, "tertiary")
  ylims3 <- ylim(plot3)[[1]]
  expect_true(ylims3[1] > ylims3[2])
  
  plot4 <- createRefPlot(data, "quaternary")
  ylims4 <- ylim(plot4)[[1]]
  expect_true(ylims4[1] > ylims4[2])
})

context("testing fiveyeargwsum")
test_that("fiveyeargwsum examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','fiveyeargwsum-example.json', package = 'repgen'))
  expect_is(fiveyeargwsum(data,'html', 'Author Name'), 'character')
})

context("testing sensorreading")
test_that("sensorreading examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','sensorReadingSummary-example.json', package = 'repgen'))
  expect_is(sensorreadingsummary(data,'html', 'Author Name'), 'character')
})

context("testing sitevisitpeak")
test_that("sitevisitpeak examples work",{
  library(jsonlite)
  data <- fromJSON(system.file('extdata','sitevisitpeak-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data,'html', 'Author Name'), 'character')
})

context("testing correctionsataglance")
test_that("correctionsataglance examples work",{
  library(jsonlite)
  library(gsplot)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','correctionsataglance-example.json', package = 'repgen'))
  expect_is(correctionsataglance(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','correctionsataglance-example2.json', package = 'repgen'))
  expect_is(correctionsataglance(data2,'html', 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','correctionsataglance-example3.json', package = 'repgen'))
  expect_is(correctionsataglance(data3,'html', 'Author Name'), 'character')
  
})

test_that("correctionsataglance duplicate legend values are removed",{
  library(jsonlite)
  library(gsplot)
  library(dplyr)
  
  data <- fromJSON(system.file('extdata','correctionsataglance-example.json', package = 'repgen'))
  corr_results <- correctionsataglanceReport(data)
  corr_plot <- corr_results$timeline
  
  i <- which(names(corr_plot$legend) == 'legend.args')
  all_legend_names <- unlist(lapply(corr_plot$legend[i], function(l) {l$legend}))

  expect_equal(anyDuplicated(all_legend_names), 0)
  
})

setwd(dir = wd)
