context("uvhydrograph-render tests")

wd <- getwd()
setwd(dir = tempdir())

test_that("getPrimaryReportElements  correctly configured gsplot, a corrections table, and/or failure message depending on report config",{
  #TODO
})

test_that("getSecondaryReportElements correctly configured gsplot, a corrections table, and/or failure message depending on report config",{
  #TODO
})

test_that("createPrimaryPlot correctly configured gsplot",{
  #TODO
})

test_that("createSecondaryPlot correctly configured gsplot",{
  #TODO
})

test_that("calculateYLim returns y-lim which covers corrected points and most (possibly not all) of the uncorrected points ",{
  yVals1 <- c(10, 15, 16, 17, 40)
  
  #this series within 30% on both ends, will use as lims
  yVals2 <- c(5, 15, 16, 17, 45)
  
  #this series much larger range on both ends and will not be used
  yVals3 <- c(-5, 15, 16, 17, 50)
  
  #this series much larger range on only one end, will use lims on one end
  yVals4 <- c(8, 15, 16, 17, 52)
  
  #this is a smaller lims, won't use lims
  yVals5 <- c(15, 16, 17)
  
  limsSeries1 <- repgen:::calculateYLim(yVals1, yVals2)
  limsSeries2 <- repgen:::calculateYLim(yVals1, yVals3)
  limsSeries3 <- repgen:::calculateYLim(yVals1, yVals4)
  limsSeries4 <- repgen:::calculateYLim(yVals1, yVals5)
  
  #lims expanded on both ends
  expect_equal(limsSeries1[1], 5)
  expect_equal(limsSeries1[2], 45)
  
  #lims not expanded at all
  expect_equal(limsSeries2[1], 10)
  expect_equal(limsSeries2[2], 40)
  
  #lims allowed to expanded only on 1 side
  expect_equal(limsSeries3[1], 8)
  expect_equal(limsSeries3[2], 40)
  
  #lims not allowed to contract
  expect_equal(limsSeries4[1], 10)
  expect_equal(limsSeries4[2], 40)
})

test_that("getPrimaryPlotConfig correctly creates lines for 6 possible types of series for gsplot",{
  testSeries <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE)
  
  testLimits <- c(10,20)
    
  asCorrected <- repgen:::getPrimaryPlotConfig(testSeries, "corrected", "Test Series", testLimits)
  asEstimated <- repgen:::getPrimaryPlotConfig(testSeries, "estimated", "Test Series", testLimits)
  asUncorrected <- repgen:::getPrimaryPlotConfig(testSeries, "uncorrected", "Test Series", testLimits)
  
  asComparisonSharedAxis <- repgen:::getPrimaryPlotConfig(testSeries, "comparison", "Test Series", testLimits, dataSide=4)
  asComparisonIndependentAxis <- repgen:::getPrimaryPlotConfig(testSeries, "comparison", "Test Series", testLimits, dataSide=6, comparisonOnIndependentAxes=FALSE)
  
  asCorrectedReference <- repgen:::getPrimaryPlotConfig(testSeries, "corrected_reference", "Test Series", testLimits, dataSide=4)
  asEstimatedReference <- repgen:::getPrimaryPlotConfig(testSeries, "estimated_reference", "Test Series", testLimits, dataSide=4)
  
  #corrected lines
  expect_equal(length(asCorrected$lines$x), 2)
  expect_equal(length(asCorrected$lines$y), 2)
  expect_equal(asCorrected$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asCorrected$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asCorrected$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asCorrected$lines$col[1])) #only care that color was set
  expect_true(grepl("Corrected", asCorrected$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asCorrected$lines[['legend.name']]))
  
  #estimated lines
  expect_equal(length(asEstimated$lines$x), 2)
  expect_equal(length(asEstimated$lines$y), 2)
  expect_equal(asEstimated$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asEstimated$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asEstimated$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asEstimated$lines$col[1])) #only care that color was set
  expect_true(grepl("Estimated", asEstimated$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asEstimated$lines[['legend.name']]))
  
  #uncorrected lines
  expect_equal(length(asUncorrected$lines$x), 2)
  expect_equal(length(asUncorrected$lines$y), 2)
  expect_equal(asUncorrected$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asUncorrected$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asUncorrected$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asUncorrected$lines$col[1])) #only care that color was set
  expect_true(grepl("Uncorrected", asUncorrected$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asUncorrected$lines[['legend.name']]))
  
  #comparison lines
  expect_equal(length(asComparisonSharedAxis$lines$x), 2)
  expect_equal(length(asComparisonSharedAxis$lines$y), 2)
  expect_equal(asComparisonSharedAxis$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asComparisonSharedAxis$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asComparisonSharedAxis$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asComparisonSharedAxis$lines$col[1])) #only care that color was set
  expect_equal("Test Series", asComparisonSharedAxis$lines[['legend.name']])
  expect_equal("Test Series", asComparisonSharedAxis$lines[['ylab']])
  expect_false(asComparisonSharedAxis$lines[['ann']])
  expect_false(asComparisonSharedAxis$lines[['axes']])
  
  #comparison (independent) lines
  expect_equal(length(asComparisonIndependentAxis$lines$x), 2)
  expect_equal(length(asComparisonIndependentAxis$lines$y), 2)
  expect_equal(asComparisonIndependentAxis$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asComparisonIndependentAxis$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asComparisonIndependentAxis$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asComparisonIndependentAxis$lines$col[1])) #only care that color was set
  expect_equal("Test Series", asComparisonIndependentAxis$lines[['legend.name']])
  expect_equal("Test Series", asComparisonIndependentAxis$lines[['ylab']])
  expect_true(asComparisonIndependentAxis$lines[['ann']])
  expect_true(asComparisonIndependentAxis$lines[['axes']])
  
  #corrected ref lines
  expect_equal(length(asCorrectedReference$lines$x), 2)
  expect_equal(length(asCorrectedReference$lines$y), 2)
  expect_equal(asCorrectedReference$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asCorrectedReference$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asCorrectedReference$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asCorrectedReference$lines$col[1])) #only care that color was set
  expect_true(grepl("Corrected", asCorrectedReference$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asCorrectedReference$lines[['legend.name']]))
  
  #estimated ref lines
  expect_equal(length(asEstimatedReference$lines$x), 2)
  expect_equal(length(asEstimatedReference$lines$y), 2)
  expect_equal(asEstimatedReference$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asEstimatedReference$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asEstimatedReference$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asEstimatedReference$lines$col[1])) #only care that color was set
  expect_true(grepl("Estimated", asEstimatedReference$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asEstimatedReference$lines[['legend.name']]))
  
  #ensure estimated and corrected have different line type
  expect_false(asCorrected$lines$lty[1] == asEstimated$lines$lty[1])
  expect_false(asCorrectedReference$lines$lty[1] == asEstimatedReference$lines$lty[1])
  
  #ensure color is different for different series types
  expect_false(asCorrected$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asCorrected$lines$col[1] == asUncorrected$lines$col[1])
  expect_false(asEstimated$lines$col[1] == asUncorrected$lines$col[1])
  
  expect_false(asComparisonSharedAxis$lines$col[1] == asCorrected$lines$col[1])
  expect_false(asComparisonSharedAxis$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asComparisonSharedAxis$lines$col[1] == asUncorrected$lines$col[1])
  expect_false(asComparisonIndependentAxis$lines$col[1] == asCorrected$lines$col[1])
  expect_false(asComparisonIndependentAxis$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asComparisonIndependentAxis$lines$col[1] == asUncorrected$lines$col[1])
  
  expect_false(asCorrectedReference$lines$col[1] == asComparisonSharedAxis$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asCorrected$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asUncorrected$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asCorrected$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asCorrectedReference$lines$col[1] == asUncorrected$lines$col[1])
})

test_that("getSecondaryPlotConfig correctly creates lines for 3 possible types of series for gsplot",{
  testSeries <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE)
  
  asCorrected <- repgen:::getSecondaryPlotConfig(testSeries, "corrected", "Test Series")
  asEstimated <- repgen:::getSecondaryPlotConfig(testSeries, "estimated", "Test Series")
  asUncorrected <- repgen:::getSecondaryPlotConfig(testSeries, "uncorrected", "Test Series")
  
  #corrected lines
  expect_equal(length(asCorrected$lines$x), 2)
  expect_equal(length(asCorrected$lines$y), 2)
  expect_equal(asCorrected$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asCorrected$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asCorrected$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asCorrected$lines$col[1])) #only care that color was set
  expect_true(grepl("Corrected", asCorrected$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asCorrected$lines[['legend.name']]))
  
  #estimated lines
  expect_equal(length(asEstimated$lines$x), 2)
  expect_equal(length(asEstimated$lines$y), 2)
  expect_equal(asEstimated$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asEstimated$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asEstimated$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asEstimated$lines$col[1])) #only care that color was set
  expect_true(grepl("Estimated", asEstimated$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asEstimated$lines[['legend.name']]))
  
  #uncorrected lines
  expect_equal(length(asUncorrected$lines$x), 2)
  expect_equal(length(asUncorrected$lines$y), 2)
  expect_equal(asUncorrected$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asUncorrected$lines$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asUncorrected$lines$lty[1])) #only care that lty was set
  expect_false(repgen:::isEmptyOrBlank(asUncorrected$lines$col[1])) #only care that color was set
  expect_true(grepl("Uncorrected", asUncorrected$lines[['legend.name']])) #note this depends on uvhydrograph-style
  expect_true(grepl("Test Series", asUncorrected$lines[['legend.name']]))
  
  #ensure color is different for different series types
  expect_false(asCorrected$lines$col[1] == asEstimated$lines$col[1])
  expect_false(asCorrected$lines$col[1] == asUncorrected$lines$col[1])
  expect_false(asEstimated$lines$col[1] == asUncorrected$lines$col[1])
})

test_that("getWqPlotConfig correctly creates a points for gsplot",{
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  wqConfig <- repgen:::getWqPlotConfig(testData)
  
  expect_equal(length(wqConfig$points$x), 2)
  expect_equal(length(wqConfig$points$y), 2)
  
  #points correct
  expect_equal(wqConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(wqConfig$points$y[1], 10)
  expect_equal(wqConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(wqConfig$points$y[2], 20)
})

test_that("getMeasQPlotConfig correctly creates a points, error bars, and callouts calls for gsplot",{
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      minQ=c(9, 18),
      maxQ=c(12, 23),
      n=c("33", "44"),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  measuredQConfig <- repgen:::getMeasQPlotConfig(testData)
  
  expect_equal(length(measuredQConfig$points$x), 2)
  expect_equal(length(measuredQConfig$points$y), 2)
  expect_equal(length(measuredQConfig$callouts$x), 2)
  expect_equal(length(measuredQConfig$callouts$y), 2)
  expect_equal(length(measuredQConfig$callouts$labels), 2)
  expect_equal(length(measuredQConfig$points$y), 2)
  expect_equal(length(measuredQConfig$error_bar$x), 2)
  expect_equal(length(measuredQConfig$error_bar$y), 2)
  expect_equal(length(measuredQConfig$error_bar$y.low), 2)
  expect_equal(length(measuredQConfig$error_bar$y.high), 2)
  
  #points correct
  expect_equal(measuredQConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(measuredQConfig$points$y[1], 10)
  expect_equal(measuredQConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(measuredQConfig$points$y[2], 20)
  
  #bars correct
  expect_equal(measuredQConfig$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(measuredQConfig$error_bar$y[1], 10)
  expect_equal(measuredQConfig$error_bar$y.low[1], 1)
  expect_equal(measuredQConfig$error_bar$y.high[1], 2)
  expect_equal(measuredQConfig$error_bar$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(measuredQConfig$error_bar$y[2], 20)
  expect_equal(measuredQConfig$error_bar$y.low[2], 2)
  expect_equal(measuredQConfig$error_bar$y.high[2], 3)
  
  #callouts correct
  expect_equal(measuredQConfig$callouts$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(measuredQConfig$callouts$y[1], 10)
  expect_equal(measuredQConfig$callouts$labels[1], "33")
  expect_equal(measuredQConfig$callouts$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(measuredQConfig$callouts$y[2], 20)
  expect_equal(measuredQConfig$callouts$labels[2], "44")
})

test_that("getGwPlotConfig correctly creates a points call for gsplot",{
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  gwConfig <- repgen:::getGwPlotConfig(testData)
  
  expect_equal(length(gwConfig$points$x), 2)
  expect_equal(length(gwConfig$points$y), 2)
  
  #points correct
  expect_equal(gwConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(gwConfig$points$y[1], 10)
  expect_equal(gwConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(gwConfig$points$y[2], 20)
})

test_that("getReadingsPlotConfig correctly creates points and erorr bar calls for gsplot with different styles for different reading types",{
  testSeries <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      uncertainty=c(1, 3),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE)
  
  asCsg <- repgen:::getReadingsPlotConfig("csg", testSeries)
  asRef <- repgen:::getReadingsPlotConfig("ref", testSeries)
  asHwm <- repgen:::getReadingsPlotConfig("hwm", testSeries)
  
  #csg points
  expect_equal(length(asCsg$points$x), 2)
  expect_equal(length(asCsg$points$y), 2)
  expect_equal(asCsg$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asCsg$points$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asCsg$points$pch[1])) #only care that pch was set
  expect_false(repgen:::isEmptyOrBlank(asCsg$points$col[1])) #only care that color was set
  
  #csg error_bar
  expect_equal(length(asCsg$error_bar$x), 2)
  expect_equal(length(asCsg$error_bar$y), 2)
  expect_equal(asCsg$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asCsg$error_bar$y[1], 10)
  expect_equal(asCsg$error_bar$y.low[1], 1)
  expect_equal(asCsg$error_bar$y.high[1], 1)
  expect_false(repgen:::isEmptyOrBlank(asCsg$error_bar$col[1])) #only care that color was set
  
  #ref points
  expect_equal(length(asRef$points$x), 2)
  expect_equal(length(asRef$points$y), 2)
  expect_equal(asRef$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asRef$points$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asRef$points$pch[1])) #only care that pch was set
  expect_false(repgen:::isEmptyOrBlank(asRef$points$col[1])) #only care that color was set
  
  #ref error_bar
  expect_equal(length(asRef$error_bar$x), 2)
  expect_equal(length(asRef$error_bar$y), 2)
  expect_equal(asRef$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asRef$error_bar$y[1], 10)
  expect_equal(asRef$error_bar$y.low[1], 1)
  expect_equal(asRef$error_bar$y.high[1], 1)
  expect_false(repgen:::isEmptyOrBlank(asRef$error_bar$col[1])) #only care that color was set
  
  #hwm points
  expect_equal(length(asHwm$points$x), 2)
  expect_equal(length(asHwm$points$y), 2)
  expect_equal(asHwm$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asHwm$points$y[1], 10)
  expect_false(repgen:::isEmptyOrBlank(asHwm$points$pch[1])) #only care that pch was set
  expect_false(repgen:::isEmptyOrBlank(asHwm$points$col[1])) #only care that color was set
  
  #hwm error_bar
  expect_equal(length(asHwm$error_bar$x), 2)
  expect_equal(length(asHwm$error_bar$y), 2)
  expect_equal(asHwm$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(asHwm$error_bar$y[1], 10)
  expect_equal(asHwm$error_bar$y.low[1], 1)
  expect_equal(asHwm$error_bar$y.high[1], 1)
  expect_false(repgen:::isEmptyOrBlank(asHwm$error_bar$col[1])) #only care that color was set
  
  #ensure pch and color are different for different reading types
  expect_false(asCsg$points$pch[1] == asRef$points$pch[1])
  expect_false(asCsg$points$pch[1] == asHwm$points$pch[1])
  expect_false(asRef$points$pch[1] == asHwm$points$pch[1])
  expect_false(asCsg$points$col[1] == asRef$points$col[1])
  expect_false(asCsg$points$col[1] == asHwm$points$col[1])
  expect_false(asRef$points$col[1] == asHwm$points$col[1])
  expect_false(asCsg$error_bar$col[1] == asRef$error_bar$col[1])
  expect_false(asCsg$error_bar$col[1] == asHwm$error_bar$col[1])
  expect_false(asRef$error_bar$col[1] == asHwm$error_bar$col[1])
})

test_that("getDvPlotConfig correctly creates points calls for gsplot with different styles for different approval levels",{
  dvPoints <- data.frame(
      time=c(as.POSIXct("2016-05-03"), as.POSIXct("2016-05-23")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      point_type=c(21, 21),
      legend.name=c("Test DV", "Test DV"),
      stringsAsFactors=FALSE)
  
  asApproved <- repgen:::getDvPlotConfig("approved_dv", dvPoints)
  asInReview <- repgen:::getDvPlotConfig("inreview_dv", dvPoints)
  asWorking <- repgen:::getDvPlotConfig("working_dv", dvPoints)
  
  #approved points
  expect_equal(length(asApproved$points$x), 2)
  expect_equal(length(asApproved$points$y), 2)
  expect_equal(asApproved$points$x[1], as.POSIXct("2016-05-03"))
  expect_equal(asApproved$points$y[1], 10)
  expect_equal(asApproved$points$legend.name[1], "Test DV") 
  expect_equal(asApproved$points$pch[1], 21)
  expect_false(repgen:::isEmptyOrBlank(asApproved$points$bg[1])) #only care that color was set
  expect_equal(asApproved$points$legend.name[1], "Test DV")
  expect_equal(asApproved$points$x[2], as.POSIXct("2016-05-23"))
  expect_equal(asApproved$points$legend.name[2], "Test DV") 
  expect_equal(asApproved$points$y[2], 20)
  expect_equal(asApproved$points$pch[2], 21)
  
  #in-review points
  expect_equal(length(asInReview$points$x), 2)
  expect_equal(length(asInReview$points$y), 2)
  expect_equal(asInReview$points$x[1], as.POSIXct("2016-05-03"))
  expect_equal(asInReview$points$y[1], 10)
  expect_equal(asInReview$points$legend.name[1], "Test DV") 
  expect_equal(asInReview$points$pch[1], 21)
  expect_false(repgen:::isEmptyOrBlank(asInReview$points$bg[1])) #only care that bg was set
  expect_equal(asInReview$points$legend.name[1], "Test DV")
  expect_equal(asInReview$points$x[2], as.POSIXct("2016-05-23"))
  expect_equal(asInReview$points$legend.name[2], "Test DV") 
  expect_equal(asInReview$points$y[2], 20)
  expect_equal(asInReview$points$pch[2], 21)
  
  #working points
  expect_equal(length(asWorking$points$x), 2)
  expect_equal(length(asWorking$points$y), 2)
  expect_equal(asWorking$points$x[1], as.POSIXct("2016-05-03"))
  expect_equal(asWorking$points$y[1], 10)
  expect_equal(asWorking$points$legend.name[1], "Test DV") 
  expect_equal(asWorking$points$pch[1], 21)
  expect_false(repgen:::isEmptyOrBlank(asWorking$points$bg[1])) #only care that bg was set
  expect_equal(asWorking$points$legend.name[1], "Test DV")
  expect_equal(asWorking$points$x[2], as.POSIXct("2016-05-23"))
  expect_equal(asWorking$points$legend.name[2], "Test DV") 
  expect_equal(asWorking$points$y[2], 20)
  expect_equal(asWorking$points$pch[2], 21)
  
  #ensure background color are different accross levels
  expect_false(asApproved$points$bg[1] == asInReview$points$bg[1])
  expect_false(asApproved$points$bg[1] == asWorking$points$bg[1])
  expect_false(asInReview$points$bg[1] == asWorking$points$bg[1])
})

test_that("getEffectiveShiftPlotConfig correctly creates lines with correct legend name for gsplot",{
  #empty case returns empty list
  emptyConfigs <- repgen:::getEffectiveShiftPlotConfig(
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
      , "label1", "label2"
  )
  expect_equal(length(emptyConfigs$lines$x), 0)
  expect_equal(length(emptyConfigs$lines$y), 0)
  
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  effShiftConfig <- repgen:::getEffectiveShiftPlotConfig(testData, "label1", "label2")
  
  expect_equal(length(effShiftConfig$lines$x), 2)
  expect_equal(length(effShiftConfig$lines$y), 2)
  
  #points correct
  expect_equal(effShiftConfig$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effShiftConfig$lines$y[1], 10)
  expect_equal(effShiftConfig$lines$legend.name[1], "label1 label2")
  expect_equal(effShiftConfig$lines$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effShiftConfig$lines$y[2], 20)
  
  #a text entry exists to ensure axis shows, BUT this might be removed, remove from test if that happens
  expect_equal(length(effShiftConfig$text$x), 1)
  expect_equal(length(effShiftConfig$text$y), 1)
  expect_equal(effShiftConfig$text$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effShiftConfig$text$y[1], 10)
})

test_that("getGageHeightPlotConfig correctly creates points and call out labels for gsplot",{
  #empty case returns empty list
  emptyConfigs <- repgen:::getGageHeightPlotConfig(
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), n=as.character(NA), month=as.character(NA), stringsAsFactors=FALSE))
  )
  expect_equal(length(emptyConfigs$points$x), 0)
  expect_equal(length(emptyConfigs$points$y), 0)
  expect_equal(length(emptyConfigs$callouts$x), 0)
  expect_equal(length(emptyConfigs$callouts$y), 0)
  
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      n=c("1222", "22"),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  ghConfig <- repgen:::getGageHeightPlotConfig(testData)
  
  expect_equal(length(ghConfig$points$x), 2)
  expect_equal(length(ghConfig$points$y), 2)
  expect_equal(length(ghConfig$callouts$x), 2)
  expect_equal(length(ghConfig$callouts$y), 2)
  expect_equal(length(ghConfig$callouts$labels), 2)
  
  #points correct
  expect_equal(ghConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(ghConfig$points$y[1], 10)
  expect_equal(ghConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(ghConfig$points$y[2], 20)
  
  #callouts correct
  expect_equal(ghConfig$callouts$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(ghConfig$callouts$y[1], 10)
  expect_equal(ghConfig$callouts$labels[1], "1222")
  expect_equal(ghConfig$callouts$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(ghConfig$callouts$y[2], 20)
  expect_equal(ghConfig$callouts$labels[2], "22")
})

test_that("getMeasuredShiftPlotConfig correctly creates points and error bars calls for gsplot",{ 
  #empty case returns empty list
  emptyConfigs <- repgen:::getMeasuredShiftPlotConfig(
        na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE))
    )
  expect_equal(length(emptyConfigs$points$x), 0)
  expect_equal(length(emptyConfigs$points$y), 0)
  expect_equal(length(emptyConfigs$error_bar$x), 0)
  expect_equal(length(emptyConfigs$error_bar$y), 0)
          
  testData <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      minShift=c(9, 18),
      maxShift=c(12, 23),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
      )
      
  measShiftConfig <- repgen:::getMeasuredShiftPlotConfig(testData)
  
  expect_equal(length(measShiftConfig$points$x), 2)
  expect_equal(length(measShiftConfig$points$y), 2)
  expect_equal(length(measShiftConfig$error_bar$x), 2)
  expect_equal(length(measShiftConfig$error_bar$y), 2)
  expect_equal(length(measShiftConfig$error_bar$y.low), 2)
  expect_equal(length(measShiftConfig$error_bar$y.high), 2)
  
  #points correct
  expect_equal(measShiftConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(measShiftConfig$points$y[1], 10)
  expect_equal(measShiftConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(measShiftConfig$points$y[2], 20)
  
  #bars correct
  expect_equal(measShiftConfig$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(measShiftConfig$error_bar$y[1], 10)
  expect_equal(measShiftConfig$error_bar$y.low[1], 1)
  expect_equal(measShiftConfig$error_bar$y.high[1], 2)
  expect_equal(measShiftConfig$error_bar$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(measShiftConfig$error_bar$y[2], 20)
  expect_equal(measShiftConfig$error_bar$y.low[2], 2)
  expect_equal(measShiftConfig$error_bar$y.high[2], 3)
})

test_that("getCorrectionsPlotConfig correctly returns a list of gsplot calls with needed corrections elements",{
  
  #NULL case returns empty list
  expect_equal(length(repgen:::getCorrectionsPlotConfig(NULL, NULL, NULL, NULL, NULL)), 0)
  expect_equal(length(repgen:::getCorrectionsPlotConfig(list(), NULL, NULL, NULL, NULL)), 0)
  
  #empty data frame case returns empty list
  expect_equal(length(repgen:::getCorrectionsPlotConfig(
              na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE))
              , NULL, NULL, NULL, NULL)), 0)
  
  testCorrections <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(NA, NA, NA), 
      month=c("1605", "1605", "1605"), 
      comment=c("correction 1", "correction 2", "correction 3"), 
      stringsAsFactors=FALSE)
  
  starteDate <- as.POSIXct("2016-05-01 17:00:00");
  endDate <- as.POSIXct("2016-05-30 17:00:00");
  
  testLims <- list(xlim=c(as.POSIXct("2016-05-01 00:00:00"), as.POSIXct("2016-05-31 00:00:00")), ylim=c(1, 2))
  
  correctionsPlotConfigs <- repgen:::getCorrectionsPlotConfig(testCorrections, starteDate, endDate, "TEST", testLims)
  
  #lines call constructed
  expect_equal(correctionsPlotConfigs$lines$x, 0)
  expect_equal(correctionsPlotConfigs$lines$y, 0)
  expect_equal(correctionsPlotConfigs$lines$xlim[1], as.POSIXct("2016-05-01 17:00:00"))
  expect_equal(correctionsPlotConfigs$lines$xlim[2], as.POSIXct("2016-05-30 17:00:00"))
  
  #two vertical lines for corrections (of the 3, two are on the same datetime)
  expect_equal(correctionsPlotConfigs$abline$v[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(correctionsPlotConfigs$abline$v[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(grep(".*TEST.*", correctionsPlotConfigs$abline$legend.name), 1) #legend entry contains the passed in label
  
  # horizontal arrows for connecting the vertical correction lines to their boxed labels
  expect_equal(correctionsPlotConfigs$arrows$x0[1], as.POSIXct("2016-05-03 17:00:00")) #starts at correction line
  expect_true(as.integer(correctionsPlotConfigs$arrows$x1[1]) > as.integer(as.POSIXct("2016-05-03 17:00:00"))) #in millis form, shifted to the right of x0
  expect_equal(correctionsPlotConfigs$arrows$y0[1], correctionsPlotConfigs$arrows$y1[1]) #y vals are equal for horizontal line
  expect_equal(correctionsPlotConfigs$arrows$x0[2], as.POSIXct("2016-05-23 17:45:00")) #starts at correction line
  expect_true(as.integer(correctionsPlotConfigs$arrows$x1[2]) > as.integer(as.POSIXct("2016-05-23 17:45:00"))) #in millis form, shifted to the right of x0
  expect_equal(correctionsPlotConfigs$arrows$y0[2], correctionsPlotConfigs$arrows$y1[2]) #y vals are equal for horizontal line
  expect_equal(correctionsPlotConfigs$arrows$x0[3], as.POSIXct("2016-05-23 17:45:00")) #starts at correction line
  expect_true(as.integer(correctionsPlotConfigs$arrows$x1[3]) > as.integer(as.POSIXct("2016-05-23 17:45:00"))) #in millis form, shifted to the right of x0
  expect_equal(correctionsPlotConfigs$arrows$y0[3], correctionsPlotConfigs$arrows$y1[3]) #y vals are equal for horizontal line
  expect_equal(correctionsPlotConfigs$arrows$x0[2], correctionsPlotConfigs$arrows$x0[3]) #2nd and 3rd correction line are the same
  expect_true(correctionsPlotConfigs$arrows$y0[3] < correctionsPlotConfigs$arrows$y0[2]) #arrow for 3rd correction is lower than 2nd to not overlap
  
  #3 points as boxes around labels for each correction (these tests are "fuzzy" since exact distances may change depending on styling requests)
  expect_true(correctionsPlotConfigs$points$x[1] > as.integer(correctionsPlotConfigs$abline$v[1])) #x shifted to the right of correction line
  expect_true(correctionsPlotConfigs$points$x[1] - as.integer(correctionsPlotConfigs$abline$v[1]) < 50000) #but not by too much
  expect_true(correctionsPlotConfigs$points$x[2] > as.integer(correctionsPlotConfigs$abline$v[2])) #x shifted to the right of correction line
  expect_true(correctionsPlotConfigs$points$x[2] - as.integer(correctionsPlotConfigs$abline$v[2]) < 50000) #but not by too much
  expect_true(correctionsPlotConfigs$points$x[3] > as.integer(correctionsPlotConfigs$abline$v[2])) #x shifted to the right of correction line
  expect_true(correctionsPlotConfigs$points$x[3] - as.integer(correctionsPlotConfigs$abline$v[2]) < 50000) #but not by too much
  expect_equal(correctionsPlotConfigs$points$x[2], correctionsPlotConfigs$points$x[2]) #at same x for the duplicate time
  expect_equal(correctionsPlotConfigs$points$y[1], correctionsPlotConfigs$points$y[2]) #corr 1 and 2 are at same y since they are far enough apart and won't overlap
  expect_true(correctionsPlotConfigs$points$y[3] < correctionsPlotConfigs$points$y[2]) #corr 3 is lower than 2 since it is at the same x and we don't want it to overlap
  
  #4 positioning of actual labels should match points above and be numbered labels instead of full comment
  expect_equal(correctionsPlotConfigs$text$x[1], correctionsPlotConfigs$points$x[1])
  expect_equal(correctionsPlotConfigs$text$x[2], correctionsPlotConfigs$points$x[2])
  expect_equal(correctionsPlotConfigs$text$x[3], correctionsPlotConfigs$points$x[3])
  expect_equal(correctionsPlotConfigs$text$y[1], correctionsPlotConfigs$points$y[1])
  expect_equal(correctionsPlotConfigs$text$y[2], correctionsPlotConfigs$points$y[2])
  expect_equal(correctionsPlotConfigs$text$y[3], correctionsPlotConfigs$points$y[3])
  expect_equal(correctionsPlotConfigs$text$label[1], 1)
  expect_equal(correctionsPlotConfigs$text$label[2], 3) #looks like the ordering of dupes is backward on labeling, but that's ok. This could change though
  expect_equal(correctionsPlotConfigs$text$label[3], 2)
})

setwd(dir = wd)
