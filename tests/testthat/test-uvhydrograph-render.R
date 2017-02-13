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
  #TODO
})

test_that("getPrimaryPlotConfig correctly creates lines for 6 possible types of series for gsplot",{
  #TODO
})

test_that("getSecondaryPlotConfig correctly creates lines for 3 possible types of series for gsplot",{
  #TODO
})

test_that("getWqPlotConfig correctly creates a points for gsplot",{
  #TODO
})

test_that("getMeasQPlotConfig correctly creates a points, error bars, and callouts calls for gsplot",{
  #TODO
})

test_that("getGwPlotConfig correctly creates a points call for gsplot",{
  #TODO
})

test_that("getReadingsPlotConfig correctly creates points calls for gsplot with different styles for different reading types",{
  #TODO
})

test_that("getDvPlotConfig correctly creates points calls for gsplot with different styles for different approval levels",{
  #TODO DVs need to be refactored before testing
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
  
  effectiveShiftConfig <- repgen:::getEffectiveShiftPlotConfig(testData, "label1", "label2")
  
  expect_equal(length(effectiveShiftConfig$lines$x), 2)
  expect_equal(length(effectiveShiftConfig$lines$y), 2)
  
  #points correct
  expect_equal(effectiveShiftConfig$lines$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$lines$y[1], 10)
  expect_equal(effectiveShiftConfig$lines$legend.name[1], "label1 label2")
  expect_equal(effectiveShiftConfig$lines$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effectiveShiftConfig$lines$y[2], 20)
  
  #a text entry exists to ensure axis shows, BUT this might be removed, remove from test if that happens
  expect_equal(length(effectiveShiftConfig$text$x), 1)
  expect_equal(length(effectiveShiftConfig$text$y), 1)
  expect_equal(effectiveShiftConfig$text$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$text$y[1], 10)
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
  
  effectiveShiftConfig <- repgen:::getGageHeightPlotConfig(testData)
  
  expect_equal(length(effectiveShiftConfig$points$x), 2)
  expect_equal(length(effectiveShiftConfig$points$y), 2)
  expect_equal(length(effectiveShiftConfig$callouts$x), 2)
  expect_equal(length(effectiveShiftConfig$callouts$y), 2)
  expect_equal(length(effectiveShiftConfig$callouts$labels), 2)
  
  #points correct
  expect_equal(effectiveShiftConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$points$y[1], 10)
  expect_equal(effectiveShiftConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effectiveShiftConfig$points$y[2], 20)
  
  #callouts correct
  expect_equal(effectiveShiftConfig$callouts$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$callouts$y[1], 10)
  expect_equal(effectiveShiftConfig$callouts$labels[1], "1222")
  expect_equal(effectiveShiftConfig$callouts$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effectiveShiftConfig$callouts$y[2], 20)
  expect_equal(effectiveShiftConfig$callouts$labels[2], "22")
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
      
  effectiveShiftConfig <- repgen:::getMeasuredShiftPlotConfig(testData)
  
  expect_equal(length(effectiveShiftConfig$points$x), 2)
  expect_equal(length(effectiveShiftConfig$points$y), 2)
  expect_equal(length(effectiveShiftConfig$error_bar$x), 2)
  expect_equal(length(effectiveShiftConfig$error_bar$y), 2)
  expect_equal(length(effectiveShiftConfig$error_bar$y.low), 2)
  expect_equal(length(effectiveShiftConfig$error_bar$y.high), 2)
  
  #points correct
  expect_equal(effectiveShiftConfig$points$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$points$y[1], 10)
  expect_equal(effectiveShiftConfig$points$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effectiveShiftConfig$points$y[2], 20)
  
  #bars correct
  expect_equal(effectiveShiftConfig$error_bar$x[1], as.POSIXct("2016-05-03 17:00:00"))
  expect_equal(effectiveShiftConfig$error_bar$y[1], 10)
  expect_equal(effectiveShiftConfig$error_bar$y.low[1], 1)
  expect_equal(effectiveShiftConfig$error_bar$y.high[1], 2)
  expect_equal(effectiveShiftConfig$error_bar$x[2], as.POSIXct("2016-05-23 17:45:00"))
  expect_equal(effectiveShiftConfig$error_bar$y[2], 20)
  expect_equal(effectiveShiftConfig$error_bar$y.low[2], 2)
  expect_equal(effectiveShiftConfig$error_bar$y.high[2], 3)
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
