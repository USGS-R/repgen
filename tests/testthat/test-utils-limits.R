context("test-utils-limits")
wd <- getwd()
setwd(dir = tempdir())

test_that("getErrorBarYLims has expected output", {
  error_bar_args <- list(
    side=3,
    y = 15,
    y.low = 6,
    y.high = 2
  )
  
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(err_lims[['side']],3)
  expect_equal(min(err_lims[['comparisonLims']]),9)
  expect_equal(max(err_lims[['comparisonLims']]),17)
  
  error_bar_args <- list(
    y = 6,
    y.low = -6,
    y.high = -1
  )
  
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(err_lims[['side']],2)
  expect_equal(min(err_lims[['comparisonLims']]),5) #6 + -1
  expect_equal(max(err_lims[['comparisonLims']]),12) #6 - -6
  
  error_bar_args <- list()
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(min(err_lims[['comparisonLims']]),-Inf)
  expect_equal(max(err_lims[['comparisonLims']]),Inf)
  
  error_bar_args <- list(
    y = 5
  )
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(min(err_lims[['comparisonLims']]),5)
  expect_equal(max(err_lims[['comparisonLims']]),5)
  
  error_bar_args <- list(
    y = 5,
    y.high = 3
  )
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(min(err_lims[['comparisonLims']]),5)
  expect_equal(max(err_lims[['comparisonLims']]),8)
  
  error_bar_args <- list(
    y = 5,
    y.low = 3
  )
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(min(err_lims[['comparisonLims']]),2)
  expect_equal(max(err_lims[['comparisonLims']]),5)
  
  error_bar_args <- list(
    y.low = 5,
    y.high = -3
  )
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(min(err_lims[['comparisonLims']]),-Inf)
  expect_equal(max(err_lims[['comparisonLims']]),Inf)
  
  #for a list of error bars, should find the largest lim to cover all points
  error_bar_args <- list(
      side=c(3, 3, 3),
      y = c(15, 15, 15),
      y.low = c(1, 1, 6), #note that lowest is #3
      y.high = c(1, 6, 1) #and highest is point #2
  )
  
  err_lims <- repgen:::getErrorBarYLims(error_bar_args)
  expect_equal(err_lims[['side']],3)
  expect_equal(min(err_lims[['comparisonLims']]),9)
  expect_equal(max(err_lims[['comparisonLims']]),21)
})

test_that("lines to callouts", {
  
  g = gsplot::callouts(gsplot::gsplot(),x=3, y=2, labels=c('EVEN LONGER HOLY CATS'), angle=150)
  j = repgen:::testCallouts(g,c(2,3))
  expect_equal(g$view.1.2$callouts$angle,j$view.1.2$callouts$angle)
  
  
  g = gsplot::callouts(gsplot::gsplot(), x=3, y=2, labels=c('EVEN LONGER HOLY CATS'), angle=250)
  j = repgen:::testCallouts(g,c(2,3))
  expect_false(g$view.1.2$callouts$angle==j$view.1.2$callouts$angle)
})

test_that("RescaleYTop adds padding to the top of plot", {
  #standard
  testPlot <- gsplot() %>% points(c(1,2), c(1,2))
  testPlot <- repgen:::RescaleYTop(testPlot)
  expect_true(ylim(testPlot)[['side.2']][2] > 2)
  
  #standard logged
  testPlot <- gsplot() %>% 
    view(log='y') %>% 
    points(c(1,2), c(1,2))
  testPlot <- repgen:::RescaleYTop(testPlot)
  expect_true(ylim(testPlot)[['side.2']][2] > 2)
  
  #reversed axis
  testPlot <- gsplot() %>% 
      axis(side = 2, reverse = TRUE, las = 0) %>% 
      points(c(1,2), c(1,2))
  testPlot <- repgen:::RescaleYTop(testPlot)
  expect_true(ylim(testPlot)[['side.2']][2] < 1) #the top of the plot is now 1
  
  #reversed/logged axis
  testPlot <- gsplot() %>% 
    view(log='y') %>% 
    axis(side = 2, reverse = TRUE, las = 0) %>% 
    points(c(2,4), c(2,4))
  testPlot <- repgen:::RescaleYTop(testPlot)
  expect_true(ylim(testPlot)[['side.2']][2] < 2) 
})

setwd(dir = wd)
