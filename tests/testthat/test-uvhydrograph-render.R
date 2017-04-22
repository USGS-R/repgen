context("uvhydrograph-render tests")

wd <- getwd()
setwd(dir = tempdir())

test_that("uvhydrographPlot correctly includes list of months with rendering items for a normal Q hydrograph",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-minimal.json', package = 'repgen'))
  
  renderList <- repgen:::uvhydrographPlot(reportObject)
  
  expect_equal(length(renderList), 1) #1 item for the month of 1510
  expect_equal(length(renderList[['1510']]), 6) #2 plots, 2 corrections tables, and 2 status messages
  expect_false(is.null(renderList[['1510']][['plot1']]))
  expect_false(is.null(renderList[['1510']][['plot2']]))
  expect_false(is.null(renderList[['1510']][['table1']]))
  expect_false(is.null(renderList[['1510']][['table2']]))
  expect_true(is.null(renderList[['1510']][['status_msg1']]))
  expect_true(is.null(renderList[['1510']][['status_msg2']]))
})

test_that("uvhydrographPlot correctly skips rendering all if no primary series exists",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-no-primary-pts.json', package = 'repgen'))
  
  renderList <- repgen:::uvhydrographPlot(reportObject)
  
  expect_equal(length(renderList), 0)
})

test_that("uvhydrographPlot correctly skips secondard plot if an upchain series is not provided for Q hydrographs",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-Q-no-upchain.json', package = 'repgen'))
  
  renderList <- repgen:::uvhydrographPlot(reportObject)
  
  expect_equal(length(renderList), 1) #1 item for the month of 1510
  expect_equal(length(renderList[['1510']]), 6) #2 plots, 2 corrections tables, and 2 status messages
  expect_false(is.null(renderList[['1510']][['plot1']]))
  expect_true(is.null(renderList[['1510']][['plot2']])) #skipped
  expect_false(is.null(renderList[['1510']][['table1']]))
  expect_true(is.null(renderList[['1510']][['table2']])) #skipped
  expect_true(is.null(renderList[['1510']][['status_msg1']])) #no error message
  expect_true(is.null(renderList[['1510']][['status_msg2']])) #no error message
})


test_that("uvhydrographPlot correctly renders secondary plot if a reference series is provided for non-Q hydrographs",{
    library('jsonlite')
    reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-with-ref.json', package = 'repgen'))
    
    renderList <- repgen:::uvhydrographPlot(reportObject)
    
    expect_equal(length(renderList), 1) #1 item for the month of 1510
    expect_equal(length(renderList[['1206']]), 6) #2 plots, 2 corrections tables, and 2 status messages
    expect_false(is.null(renderList[['1206']][['plot1']]))
    expect_false(is.null(renderList[['1206']][['plot2']]))
    expect_false(is.null(renderList[['1206']][['table1']]))
    expect_false(is.null(renderList[['1206']][['table2']])) 
    expect_true(is.null(renderList[['1206']][['status_msg1']])) #no error message
    expect_true(is.null(renderList[['1206']][['status_msg2']])) #no error message
})

test_that("uvhydrographPlot correctly skips secondary plot if a reference series is not provided for non-Q hydrographs",{
  library('jsonlite')
  reportObject <- fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-no-ref.json', package = 'repgen'))
  
  renderList <- repgen:::uvhydrographPlot(reportObject)
  
  expect_equal(length(renderList), 1) #1 item for the month of 1510
  expect_equal(length(renderList[['1206']]), 6) #2 plots, 2 corrections tables, and 2 status messages
  expect_false(is.null(renderList[['1206']][['plot1']]))
  expect_true(is.null(renderList[['1206']][['plot2']])) #skipped
  expect_false(is.null(renderList[['1206']][['table1']]))
  expect_true(is.null(renderList[['1206']][['table2']])) #skipped
  expect_true(is.null(renderList[['1206']][['status_msg1']]))
  expect_true(is.null(renderList[['1206']][['status_msg2']]))
})

test_that("useSecondaryPlot correctly flags when to use a secondary plot",{
  expect_false(repgen:::useSecondaryPlot(fromJSON(system.file('extdata','testsnippets','test-uvhydro-Q-no-upchain.json', package = 'repgen'))))
  expect_true(repgen:::useSecondaryPlot(fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-with-ref.json', package = 'repgen'))))
  expect_false(repgen:::useSecondaryPlot(fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-no-ref.json', package = 'repgen'))))
})

test_that("getPrimaryReportElements  correctly configured gsplot, a corrections table, and/or failure message depending on report config",{
  reportEls <- repgen:::getPrimaryReportElements(
      fromJSON(system.file('extdata','testsnippets','test-uvhydro-no-primary-pts.json', package = 'repgen'))
      , "1510", "Etc/GMT", TRUE)
  expect_equal(reportEls[['plot']], NULL)
  expect_equal(reportEls[['table']], NULL)
  expect_equal(reportEls[['status_msg']], "Corrected data missing for Discharge.ft^3/s@01047200")
  
  reportEls <- repgen:::getPrimaryReportElements(
      fromJSON(system.file('extdata','testsnippets','test-uvhydro-Q-no-upchain.json', package = 'repgen'))
      , "1510", "Etc/GMT", TRUE)
  expect_is(reportEls[['plot']], "gsplot")
  expect_is(reportEls[['table']], "data.frame")
  expect_equal(reportEls[['table']][1,][["Time"]], "2015-10-06")
  expect_equal(reportEls[['table']][1,][["Comments"]], "End : Approval period copy paste from Ref")
  expect_equal(reportEls[['status_msg']], NULL)
})

test_that("getPrimaryReportElements correctly configured gsplot, a corrections table, and/or failure message depending on report config",{
  reportEls <- repgen:::getPrimaryReportElements(
      fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-with-ref.json', package = 'repgen'))
      , "1510", "Etc/GMT", TRUE) #wrong month
  expect_equal(reportEls[['plot']], NULL)
  expect_equal(reportEls[['table']], NULL)
  expect_equal(reportEls[['status_msg']], "Corrected data missing for WaterLevel, BelowLSD.ft@353922083345600")
  
  reportEls <- repgen:::getPrimaryReportElements(
      fromJSON(system.file('extdata','testsnippets','test-uvhydro-gw-with-ref.json', package = 'repgen'))
      , "1206", "Etc/GMT", TRUE)
  expect_is(reportEls[['plot']], "gsplot")
  expect_is(reportEls[['table']], "data.frame")
  expect_equal(reportEls[['table']][1,][["Time"]], "2012-06-29 10:17:00")
  expect_equal(reportEls[['table']][1,][["Comments"]], "Start : Example primary series correction")
  expect_equal(reportEls[['table']][2,][["Time"]], "2012-06-30 22:59:00")
  expect_equal(reportEls[['table']][2,][["Comments"]], "End : Example primary series correction")
  expect_equal(reportEls[['status_msg']], NULL)
})

test_that("createPrimaryPlot only can handle minimal requirements (just corrected series)",{
  Sys.setenv(TZ = "UTC")
  #minimal case should plot (only corrected series)
  testSeries <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  plot_object <- repgen:::createPrimaryPlot(
      list(label="Primary Test Series", units="ft", type="Test"), 
      NULL, 
      NULL, 
      NULL,
      list(corrected=testSeries, estimated=NULL, uncorrected=NULL, corrected_reference=NULL,
          estimated_reference=NULL,
          comparison=NULL,inverted=FALSE,loggedAxis=FALSE), 
      list(), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minQ=as.numeric(NA), maxQ=as.numeric(NA), n=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))),
      list(), 
      list(),
      na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)),
      TRUE,
      "Etc/GMT", 
      FALSE)
  
  expect_is(plot_object[['side.1']], "list")
  #full month on plot
  expect_equal(xlim(plot_object)[['side.1']][1], as.POSIXct("2016-05-01 00:00:00")) 
  expect_equal(xlim(plot_object)[['side.1']][2], as.POSIXct("2016-05-31 23:45:00")) 
  
  expect_is(plot_object[['side.2']], "list")
  expect_equal(ylim(plot_object)[['side.2']][1], 10) 
  expect_equal(ylim(plot_object)[['side.2']][2], 20) 
  
  expect_is(plot_object[['legend']], "list")
  expect_equal(plot_object[['legend']][['legend.auto']][['legend']], "Corrected UV Primary Test Series")
})

test_that("createPrimaryPlot correctly configured gsplot",{
  Sys.setenv(TZ = "UTC")
  testSeries <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-02 17:00:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(-1, 10, 20),
          month=c("1605", "1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesEst <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-02 17:00:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(-1, 10, 20),
          month=c("1605", "1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesUnc <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-02 17:00:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(-1, 10, 20),
          month=c("1605", "1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesRef <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(4, 15),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesEstRef <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-24 17:15:00"), as.POSIXct("2016-05-28 17:45:00")), 
          value=c(7, 16),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesComp <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(9, 12),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  dvs <- list(
      approved_dv=data.frame(
          time=c(as.POSIXct("2016-05-03"), as.POSIXct("2016-05-04")), 
          value=c(10, 11),
          month=c("1605", "1605"),
          point_type=c(21, 21),
          legend.name=c("Test DV", "Test DV"),
          stringsAsFactors=FALSE),
      inreview_dv=data.frame(
          time=c(as.POSIXct("2016-05-05"), as.POSIXct("2016-05-06")), 
          value=c(12, 14),
          month=c("1605", "1605"),
          point_type=c(21, 21),
          legend.name=c("In Review Test DV", "In Review Test DV"),
          stringsAsFactors=FALSE),
      working_dv=data.frame(
          time=c(as.POSIXct("2016-05-20"), as.POSIXct("2016-05-22")), 
          value=c(15, 16),
          month=c("1605", "1605"),
          point_type=c(21, 21),
          legend.name=c("Working Test DV", "Working Test DV"),
          stringsAsFactors=FALSE)
      )
      
  qMeas <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(7, 8),
      minQ=c(6, 18),
      maxQ=c(12, 50),
      n=c("33", "44"),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
    
  wq <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(14, 10),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  gw <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(13, 9),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  readings <- list(
      reference=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(6, 7),
          uncertainty=c(1, 3),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE), 
      crest_stage_gage=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          uncertainty=c(8, 9),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE), 
      high_water_mark=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          uncertainty=c(4, 5),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  approvalBars <- list(
      appr_working_uv=list(x0=as.POSIXct("2016-05-01 00:00:00"), x1=as.POSIXct("2016-05-06 00:00:00"), legend.name="Working Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
      appr_inreview_uv=list(x0=as.POSIXct("2016-05-06 00:00:00"), x1=as.POSIXct("2016-05-20 00:00:00"), legend.name="In Review Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
      appr_approved_uv=list(x0=as.POSIXct("2016-05-20 00:00:00"), x1=as.POSIXct("2016-06-30 00:00:00"), legend.name="Approved Test Series", time=as.POSIXct("2016-05-01 00:00:00"))
  )
  
  testCorrections <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(NA, NA, NA), 
      month=c("1605", "1605", "1605"), 
      comment=c("correction 1", "correction 2", "correction 3"), 
      stringsAsFactors=FALSE)
  
  plot_object <- repgen:::createPrimaryPlot(
      list(label="Primary Test Series", units="ft", type="Test"), 
      list(label="Reference Test Series", units="ft", type="Test"), 
      list(label="Comparison Test Series", units="ft", type="Test"), 
      "testComparisonStationId",
      list(corrected=testSeries, estimated=testSeriesEst, uncorrected=testSeriesUnc, corrected_reference=testSeriesRef,
          estimated_reference=testSeriesEstRef,
          comparison=testSeriesComp,inverted=FALSE,loggedAxis=FALSE), 
      dvs, 
      qMeas, 
      wq, 
      gw,
      readings, 
      approvalBars,
      testCorrections,
      TRUE,
      "Etc/GMT", 
      TRUE)
  
  #full month on plot
  expect_equal(xlim(plot_object)[['side.1']][1], as.POSIXct("2016-05-01 00:00:00")) 
  expect_equal(xlim(plot_object)[['side.1']][2], as.POSIXct("2016-05-31 23:45:00")) 
  
  expect_equal(ylim(plot_object)[['side.2']][1], -1) 
  expect_equal(ylim(plot_object)[['side.2']][2], 50)  #The high matches the top of the Q error bar
  
  expect_equal(plot_object[['global']][['title']][['xlab']], "UV Series: 2016-05-02 17:00:00 through 2016-05-23 17:45:00") 
  
  expect_is(plot_object[['view.1.2']], "list")
  expect_equal(length(plot_object[['view.1.2']]), 27) #all plot calls are there 
  
  #do not exclude negatives
  plot_object <- repgen:::createPrimaryPlot(
      list(label="Primary Test Series", units="ft", type="Test"), 
      list(label="Reference Test Series", units="ft", type="Test"), 
      list(label="Comparison Test Series", units="ft", type="Test"), 
      "testComparisonStationId",
      list(corrected=testSeries, estimated=testSeriesEst, uncorrected=testSeriesUnc, corrected_reference=testSeriesRef,
          estimated_reference=testSeriesEstRef,
          comparison=testSeriesComp,inverted=FALSE,loggedAxis=FALSE), 
      dvs, 
      qMeas, 
      wq, 
      gw,
      readings, 
      approvalBars,
      testCorrections,
      TRUE,
      "Etc/GMT", 
      FALSE)
  
  #TODO need an assertion to test if zeros/negatives are excluded
  
  #full month on plot
  expect_equal(xlim(plot_object)[['side.1']][1], as.POSIXct("2016-05-01 00:00:00")) 
  expect_equal(xlim(plot_object)[['side.1']][2], as.POSIXct("2016-05-31 23:45:00")) 
  
  expect_equal(ylim(plot_object)[['side.2']][1], -1) 
  expect_equal(ylim(plot_object)[['side.2']][2], 50)  #The high matches the top of the Q error bar
  
  expect_equal(plot_object[['global']][['title']][['xlab']], "UV Series: 2016-05-02 17:00:00 through 2016-05-23 17:45:00") 
  
  expect_is(plot_object[['view.1.2']], "list")
  expect_equal(length(plot_object[['view.1.2']]), 27) #all plot calls are there 
})

test_that("createSecondaryPlot only can handle minimal requirements (just corrected series)",{
  Sys.setenv(TZ = "UTC")
  #minimal case should plot (only corrected series)
  testSeries <- list(
      points=data.frame(
        time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
        value=c(10, 20),
        month=c("1605", "1605"),
        stringsAsFactors=FALSE)
      )
  plot_object <- repgen:::createSecondaryPlot(
      list(label="Test Series", units="ft", type="Test"), 
      list(corrected=testSeries, estimated=NULL, uncorrected=NULL, inverted=FALSE), 
      list(), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), minShift=as.numeric(NA), maxShift=as.numeric(NA), month=as.character(NA), stringsAsFactors=FALSE)), 
      na.omit(data.frame(time=as.POSIXct(NA), value=as.numeric(NA), month=as.character(NA))),
      na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)),
      list(),
      "Etc/GMT", 
      FALSE, 
      tertiary_label="")
  
  #full month on plot
  expect_equal(xlim(plot_object)[['side.1']][1], as.POSIXct("2016-05-01 00:00:00")) 
  expect_equal(xlim(plot_object)[['side.1']][2], as.POSIXct("2016-05-31 23:45:00")) 
  
  expect_equal(ylim(plot_object)[['side.2']][1], 10) 
  expect_equal(ylim(plot_object)[['side.2']][2], 20) 
  
  expect_is(plot_object[['legend']], "list")
  expect_equal(plot_object[['legend']][['legend.auto']][['legend']], "Corrected UV Test Series")
})

test_that("createSecondaryPlot more tests",{
  Sys.setenv(TZ = "UTC")
  testSeries <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesEst <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:15:00"), as.POSIXct("2016-05-23 17:15:00")), 
          value=c(11, 22),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testSeriesUnc <- list(
      points=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(20, 30),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  approvalBars <- list(
      appr_working_uv=list(x0=as.POSIXct("2016-05-01 00:00:00"), x1=as.POSIXct("2016-05-06 00:00:00"), legend.name="Working Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
      appr_inreview_uv=list(x0=as.POSIXct("2016-05-06 00:00:00"), x1=as.POSIXct("2016-05-20 00:00:00"), legend.name="In Review Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
      appr_approved_uv=list(x0=as.POSIXct("2016-05-20 00:00:00"), x1=as.POSIXct("2016-06-30 00:00:00"), legend.name="Approved Test Series", time=as.POSIXct("2016-05-01 00:00:00"))
      )
  
  effShift <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(2, 3),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  measShift <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      minShift=c(9, 18),
      maxShift=c(12, 44),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  gageHeight <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      n=c("1222", "22"),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE
  )
  
  readings <- list(
      reference=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(6, 7),
          uncertainty=c(1, 3),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE), 
      crest_stage_gage=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          uncertainty=c(8, 9),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE), 
      high_water_mark=data.frame(
          time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
          value=c(10, 20),
          uncertainty=c(4, 5),
          month=c("1605", "1605"),
          stringsAsFactors=FALSE)
  )
  
  testCorrections <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(NA, NA, NA), 
      month=c("1605", "1605", "1605"), 
      comment=c("correction 1", "correction 2", "correction 3"), 
      stringsAsFactors=FALSE)
  
  plot_object <- repgen:::createSecondaryPlot(
      list(label="Test Series", units="ft", type="Test"), 
      list(corrected=testSeries, estimated=testSeriesEst, uncorrected=testSeriesUnc, inverted=FALSE), 
      approvalBars, 
      effShift, 
      measShift, 
      gageHeight,
      readings,
      testCorrections,
      "Etc/GMT", 
      FALSE, 
      tertiary_label="Tertiary Label")
  
  #full month on plot
  expect_equal(xlim(plot_object)[['side.1']][1], as.POSIXct("2016-05-01 00:00:00")) 
  expect_equal(xlim(plot_object)[['side.1']][2], as.POSIXct("2016-05-31 23:45:00")) 
  
  expect_equal(ylim(plot_object)[['side.2']][1], 2) 
  expect_equal(ylim(plot_object)[['side.2']][2], 29) 
  
  expect_equal(ylim(plot_object)[['side.4']][1], 2) # low of effective shift series
  expect_equal(ylim(plot_object)[['side.4']][2], 44) # high of top of meas shift error 
  
  expect_equal(plot_object[['global']][['title']][['ylab']], "Test Series")
  expect_equal(plot_object[['global']][['title']][['xlab']], "UV Series: 2016-05-03 17:00:00 through 2016-05-23 17:45:00")
  
  expect_is(plot_object[['view.1.2']], "list")
  expect_equal(length(plot_object[['view.1.2']]), 17) #all plot calls are there
  
  expect_is(plot_object[['view.1.4']], "list")
  expect_equal(length(plot_object[['view.1.4']]), 6) #all plot calls are there
  
  expect_is(plot_object[['view.7.2']], "list")
  expect_equal(length(plot_object[['view.7.2']]), 6) #all plot calls are there
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
  
  asCorrected <- repgen:::getSecondaryPlotConfig(testSeries, "corrected", "Test Series", c(10, 20))
  asEstimated <- repgen:::getSecondaryPlotConfig(testSeries, "estimated", "Test Series", c(10, 20))
  asUncorrected <- repgen:::getSecondaryPlotConfig(testSeries, "uncorrected", "Test Series", c(10, 20))
  
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
  testReadings <- data.frame(
      time=c(as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(10, 20),
      uncertainty=c(1, 3),
      month=c("1605", "1605"),
      stringsAsFactors=FALSE)
  
  asCsg <- repgen:::getReadingsPlotConfig("csg", testReadings)
  asRef <- repgen:::getReadingsPlotConfig("ref", testReadings)
  asHwm <- repgen:::getReadingsPlotConfig("hwm", testReadings)
  
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
