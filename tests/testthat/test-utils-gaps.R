context("utils-gaps tests")
  
context("splitDataGapsList")
  
  

context("splitDataGapsTimeSeries")
  
  timezone <- "Etc/GMT+5"
  flagZeroNeg <- FALSE
  timeSeriesName <- 'upchainTimeSeries'
  set.seed(53)
  pointsDf <- data.frame(time = seq(as.POSIXct("2015-10-01 00:00:00", tz=timezone), 
                                    as.POSIXct("2015-10-03 11:00:00", tz=timezone), by = "hour"), 
                         values = runif(60, 1, 3000))
  estimatedPeriods <- data.frame(startDate = as.POSIXct("2015-10-02 05:00:00", tz=timezone),
                                 endDate = as.POSIXct("2015-10-02 08:00:00", tz=timezone))
  gapDf <- data.frame(startTime = as.POSIXct("2015-10-03 01:00:00", tz=timezone),
                      endTime = as.POSIXct("2015-10-03 04:00:00", tz=timezone))
  
  pointsDf2 <- data.frame(time = seq(as.POSIXct("2015-10-01 00:00:00", tz=timezone), 
                                     as.POSIXct("2015-10-01 11:00:00", tz=timezone), by = "15 min"), 
                          values = runif(45, 1, 3000))
  pointsDf2[['values']][3:10] <- 0
  gapDf2 <- data.frame(startTime = as.POSIXct("2015-10-01 10:00:00", tz=timezone),
                       endTime = as.POSIXct("2015-10-03 04:00:00", tz=timezone))
  
  test_that("data is split for estimated series", {
    timeSeries <- list(points = pointsDf, estimatedPeriods = estimatedPeriods, 
                       estimated = TRUE, isVolumetricFlow = TRUE)
    estSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                 timezone, flagZeroNeg, isDV=FALSE)
    expect_equal(length(estSplit), 1)
    expect_equal(nrow(estSplit[[1]]), 3)
    expect_true(all(estSplit[[1]][['points']][['time']] < estimatedPeriods[['endDate']]))
    expect_true(estSplit[[1]][['points']][['time']][1] == estimatedPeriods[['startDate']])
  })
  
  test_that("data is split for non-estimated series", {
    timeSeries <- list(points = pointsDf, estimatedPeriods = estimatedPeriods,
                       estimated = FALSE, isVolumetricFlow = TRUE)
    nonEstSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                    timezone, flagZeroNeg, isDV=FALSE)
    expect_equal(length(nonEstSplit), 2)
    expect_equal(nrow(nonEstSplit[[1]][['points']]), 29)
    expect_equal(nrow(nonEstSplit[[2]][['points']]), 28)
    expect_true(tail(nonEstSplit[[1]][['points']][['time']],1) < estimatedPeriods[['startDate']])
    expect_equal(nonEstSplit[[2]][['points']][['time']][1], estimatedPeriods[['endDate']])
  })
  
  test_that("data is split for defined gaps", {
    # includes the values for the gaps 
    timeSeries <- list(points = pointsDf, gaps = gapDf, 
                       estimated = TRUE, isVolumetricFlow = TRUE)
    definedSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                     timezone, flagZeroNeg, isDV=FALSE)
    expect_equal(length(definedSplit), 2)
    expect_equal(nrow(definedSplit[[1]][['points']]), 50)
    expect_equal(nrow(definedSplit[[2]][['points']]), 8)
    expect_equal(tail(definedSplit[[1]][['points']][['time']], 1), gapDf[['startTime']])
    expect_equal(head(definedSplit[[2]][['points']][['time']], 1), gapDf[['endTime']])
  })
  
  test_that("data is split for negative/zero gaps", {
    # includes the values for the gaps (SHOULD IT???)
    # "gap" is 2015-10-01 00:15:00 to 2015-10-01 02:30:00 
    # but zeros only exist from 2015-10-01 00:30:00 to 2015-10-01 02:15:00
    timeSeries <- list(points = pointsDf2, estimated = TRUE, isVolumetricFlow = TRUE)
    negZeroSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                     timezone, flagZeroNeg=TRUE, isDV=FALSE)
    expect_equal(length(negZeroSplit), 2)
    expect_equal(nrow(negZeroSplit[[1]]), 2)
    expect_equal(nrow(negZeroSplit[[2]]), 35)
    expect_equal(tail(negZeroSplit[[1]][['points']][['time']], 1), 
                 as.POSIXct("2015-10-01 00:15:00", tz=timezone))
    expect_equal(head(negZeroSplit[[2]][['points']][['time']], 1), 
                 as.POSIXct("2015-10-01 02:30:00", tz=timezone))
  })
  
  test_that("data is split for defined + neg/zero gaps", {
    # includes the values for the gaps (SHOULD IT???)
    timeSeries <- list(points = pointsDf2, gaps = gapDf2, 
                       estimated = TRUE, isVolumetricFlow = TRUE)
    multiGapSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                      timezone, flagZeroNeg=TRUE, isDV=FALSE)
    expect_equal(length(multiGapSplit), 2)
    expect_equal(nrow(multiGapSplit[[1]]), 2)
    expect_equal(nrow(multiGapSplit[[2]]), 31)
    expect_equal(tail(negZeroSplit[[1]][['points']][['time']], 1), 
                 as.POSIXct("2015-10-01 00:15:00", tz=timezone))
    expect_equal(head(negZeroSplit[[2]][['points']][['time']], 1), 
                 as.POSIXct("2015-10-01 02:30:00", tz=timezone))
    expect_equal(tail(negZeroSplit[[2]][['points']][['time']], 1), gapDf2[['startTime']][1])
  })
  
  test_that("split data gets correct timeseries names", {
    timeSeries <- list(points = pointsDf,
                       gaps = gapDf,
                       estimatedPeriods = estimatedPeriods,
                       estimated = TRUE, 
                       isVolumetricFlow = TRUE)
    splitData <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                  timezone, flagZeroNeg, isDV=FALSE)
    expect_equal(all(names(splitData) == timeSeriesName))
  })
  
  test_that('splitDataGaps work when there are no gaps specified', {
    timeSeries <- list(points = pointsDf, estimated = TRUE, isVolumetricFlow = TRUE)
    noSplit <- repgen:::splitDataGapsTimeSeries(timeSeries, timeSeriesName, 
                                                timezone, flagZeroNeg, isDV=FALSE)
    expect_equal(length(estSplit), 1)
    expect_equal(nrow(estSplit[[1]]), 60)
  })
  


context("findZeroNegativeGaps")
   
  df_dates <- seq(as.POSIXct("2015-10-01 00:00:00", tz="Etc/GMT+5"), 
                  as.POSIXct("2015-10-01 15:00:00", tz="Etc/GMT+5"), by = "15 min")
  set.seed(53)
  timeValueDF <- data.frame(time = df_dates, values = runif(61, 1, 3000))
  set.seed(53)
  timeValueDF2 <- data.frame(time = df_dates, values = runif(61, -1000, 3000))
  timezone <- "Etc/GMT+5"
  
  test_that("no gap is returned if user does not want to exclude zeros and negatives", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF, timezone=timezone, 
                                                 flagZeroNeg = FALSE, isVolumetricFlow=TRUE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
  })
  
  test_that("no gap is returned if the data can't be logged", {
    # positive data, but not volumetric
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF, timezone=timezone, 
                                                 flagZeroNeg = TRUE, isVolumetricFlow=FALSE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
    
    # negative volumetric data, but user won't exclude any
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF2, timezone=timezone,
                                                 flagZeroNeg = FALSE, isVolumetricFlow=TRUE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
    
  })
  
  test_that("does not find any gaps if there is no data <= 0", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF, timezone=timezone, 
                                                 flagZeroNeg = TRUE, isVolumetricFlow = TRUE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
  })
  
  test_that("returns gaps if there is data <= 0 & user wants to exclude", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF2, timezone=timezone, 
                                                 flagZeroNeg = TRUE, isVolumetricFlow = TRUE)
    expect_equal(length(negZeroGaps[['startGaps']]), 13)
    expect_equal(length(negZeroGaps[['endGaps']]), 13)
    expect_equal(negZeroGaps[['startGaps']][1], timeValueDF2[['time']][2])
    expect_equal(negZeroGaps[['endGaps']][3], timeValueDF2[['time']][9])
    expect_equal(negZeroGaps[['startGaps']][9], negZeroGaps[['endGaps']][9])
  })
  
  test_that("returns gaps if there is data <= 0 for DV", {
    df_dates3 <- seq(as.POSIXct("2015-10-01", tz="Etc/GMT+5"), 
                     as.POSIXct("2015-10-05", tz="Etc/GMT+5"), by = "days")
    timeValueDF3 <- data.frame(time = df_dates3, values = c(43,45,15,-25,10))
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF3, timezone=timezone, flagZeroNeg = TRUE,
                                                 isVolumetricFlow = TRUE, isDV=TRUE)
    expect_equal(length(negZeroGaps[['startGaps']]), 1)
    expect_equal(length(negZeroGaps[['endGaps']]), 1)
    expect_equal(negZeroGaps[['startGaps']], negZeroGaps[['endGaps']])
  })
  
  test_that("if empty timeValueDF, return empty gaps", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF=data.frame(), timezone=timezone, 
                                                 flagZeroNeg=NULL, isVolumetricFlow=FALSE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
  })
  
  test_that("if empty flagZeroNeg, return empty gaps", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF=timeValueDF, timezone=timezone, 
                                                 flagZeroNeg=NULL, isVolumetricFlow=FALSE)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
  })
  
  test_that("if empty isVolumetricFlow, return empty gaps", {
    negZeroGaps <- repgen:::findZeroNegativeGaps(timeValueDF=timeValueDF, timezone=timezone, 
                                                 flagZeroNeg=FALSE, isVolumetricFlow=NULL)
    expect_true(is.null(negZeroGaps[['startGaps']]))
    expect_true(is.null(negZeroGaps[['endGaps']]))
  })
  
  test_that("missing or empty timezone causes error", {
    expect_error(repgen:::findZeroNegativeGaps(timeValueDF=timeValueDF, flagZeroNeg=FALSE, isVolumetricFlow=TRUE), 
                 "timezone is either missing or empty")
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeValueDF=timeValueDF, timezone="", 
                                                         flagZeroNeg=FALSE, isVolumetricFlow=TRUE), 
                 "timezone is either missing or empty")
  })
  

  
context("findDefinedGaps")

  timezone <- "Etc/GMT+5"
  starts <- as.POSIXct(c("2015-10-01 07:00:00", "2015-12-05 07:00:00"), tz=timezone)
  ends <- as.POSIXct(c("2015-10-02 04:00:00", "2015-12-05 11:00:00"), tz=timezone)
  
  test_that("get appropriate start/end times from the 'gaps' field", {
    timeSeries <- list(gaps = data.frame(startTime = starts, endTime = ends))
    gaps <- repgen:::findDefinedGaps(timeSeries, timezone)
    
    expect_equal(timeSeries[['gaps']][['startTime']], gaps[['startGaps']])
    expect_equal(timeSeries[['gaps']][['endTime']], gaps[['endGaps']])
  })
  
  test_that("get appropriate and ordered start/end times from the 'gaps' field", {
    timeSeries <- list(gaps = data.frame(startTime = rev(starts), endTime = rev(ends)))
    gaps <- repgen:::findDefinedGaps(timeSeries, timezone)
    
    expect_false(all(timeSeries[['gaps']][['startTime']] == gaps[['startGaps']]))
    expect_false(all(timeSeries[['gaps']][['endTime']] == gaps[['endGaps']]))
    expect_equal(rev(timeSeries[['gaps']][['startTime']]), gaps[['startGaps']])
    expect_equal(rev(timeSeries[['gaps']][['endTime']]), gaps[['endGaps']])
  })
  
  test_that("time series 'gaps' is empty, but doesn't cause an error", {
    timeSeries <- list(gaps = data.frame(startTime = c(), endTime = c()))
    gaps <- repgen:::findDefinedGaps(timeSeries, timezone)
    expect_true(is.null(gaps[['startGaps']]))
    expect_true(is.null(gaps[['endGaps']]))
    
    timeSeries2 <- list(gaps = data.frame())
    gaps2 <- repgen:::findDefinedGaps(timeSeries, timezone)
    expect_true(is.null(gaps2[['startGaps']]))
    expect_true(is.null(gaps2[['endGaps']]))
    
  })
  
  test_that("empty timeSeries returns no data", {
    gaps <- repgen:::findDefinedGaps(timeSeries = list())
    expect_true(is.null(gaps$startGaps))
    expect_true(is.null(gaps$endGaps))
  })
  
  test_that("missing or empty timezone throws an error only if gaps are defined", {
    
    # gaps are defined
    timeSeries <- list(gaps = data.frame(startTime = starts, endTime = ends))
    expect_error(repgen:::findDefinedGaps(timeSeries = timeSeries), 
                 "timezone is either missing or empty")
    expect_error(repgen:::findDefinedGaps(timeSeries = timeSeries, timezone = ""), 
                 "timezone is either missing or empty")
    
    # gaps are not defined
    timeSeries <- list(gaps = data.frame(startTime = c(), endTime = c()))
    gaps <- repgen:::findDefinedGaps(timeSeries = timeSeries, timezone = "")
    expect_true(is.null(gaps$startGaps))
    expect_true(is.null(gaps$endGaps))
  })
  
  test_that("time series 'gaps' has incorrectly named columns and throws an error", {
    
    # both named incorrectly
    timeSeries <- list(gaps = data.frame(startGap = starts, endGap = ends))
    expect_error(repgen:::findDefinedGaps(timeSeries, timezone), "unexpected colnames for gaps")
    
    # only one named incorrectly
    timeSeries <- list(gaps = data.frame(startTime = starts, endGap = ends))
    expect_error(repgen:::findDefinedGaps(timeSeries, timezone), "unexpected colnames for gaps")
    
    # extra unnecessary column lets it continue
    timeSeries <- list(gaps = data.frame(startTime = starts, endTime = ends, randomCol = starts))
    gaps <- repgen:::findDefinedGaps(timeSeries, timezone)
    expect_equal(timeSeries[['gaps']][['startTime']], gaps[['startGaps']])
    expect_equal(timeSeries[['gaps']][['endTime']], gaps[['endGaps']])
    
  })
  


context("createGapsFromEstimatedPeriods")
  
  timezone <- "Etc/GMT+5"
  set.seed(53)
  df_dates <- seq(as.POSIXct("2015-10-01 00:00:00", tz=timezone), as.POSIXct("2015-10-03 11:00:00", tz=timezone), by = "hour")
  startEst <- as.POSIXct("2015-10-02 05:00:00", tz=timezone)
  endEst <- as.POSIXct("2015-10-02 08:00:00", tz=timezone)
  timeSeries <- list(points = data.frame(time = df_dates, values = runif(60, 0, 3000)),
                     estimatedPeriods = data.frame(startDate = startEst, endDate = endEst))
  
  test_that("estimated periods are treated as gaps", {
    # single estimated period
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries, timezone, inverted = FALSE)
    expect_true(timeSeries[['estimatedPeriods']][['startDate']] > estimatedGaps[['startGaps']])
    expect_true(timeSeries[['estimatedPeriods']][['endDate']] == estimatedGaps[['endGaps']])
    
    # multiple estimated periods
    timeSeries2 <- timeSeries
    estGaps_mult <- data.frame(startDate = as.POSIXct(c("2015-10-02 05:00:00", "2015-10-03 02:00:00"), tz=timezone),
                               endDate = as.POSIXct(c("2015-10-02 08:00:00", "2015-10-03 07:00:00"), tz=timezone))
    timeSeries2[['estimatedPeriods']] <- estGaps_mult
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries2, timezone, inverted = FALSE)
    expect_true(all(timeSeries2[['estimatedPeriods']][['startDate']] > estimatedGaps[['startGaps']]))
    expect_true(all(timeSeries2[['estimatedPeriods']][['endDate']] == estimatedGaps[['endGaps']]))
  })
  
  test_that("non-estimated periods are treated as gaps", {
    # single estimated period
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries, timezone, inverted = TRUE)
    expect_false(timeSeries[['estimatedPeriods']][['startDate']] %in% estimatedGaps[['startGaps']])
    expect_false(timeSeries[['estimatedPeriods']][['endDate']] %in% estimatedGaps[['endGaps']])
    expect_true(head(timeSeries[['points']][['time']],1) > estimatedGaps[['startGaps']][1])
    expect_true(head(timeSeries[['points']][['time']],1) < estimatedGaps[['startGaps']][2])
    expect_true(tail(timeSeries[['points']][['time']],1) < estimatedGaps[['endGaps']])
    expect_true(estimatedGaps[['startGaps']][2] < timeSeries[['estimatedPeriods']][['endDate']])
    expect_true(estimatedGaps[['endGaps']][1] == timeSeries[['estimatedPeriods']][['startDate']])
    
    # multiple estimated periods
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries2, timezone, inverted = TRUE)
    expect_false(all(timeSeries2[['estimatedPeriods']][['startDate']] %in% estimatedGaps[['startGaps']]))
    expect_false(all(timeSeries2[['estimatedPeriods']][['endDate']] %in% estimatedGaps[['endGaps']]))
    expect_true(head(timeSeries[['points']][['time']],1) > estimatedGaps[['startGaps']][1])
    expect_true(all(head(timeSeries[['points']][['time']],1) < estimatedGaps[['startGaps']][2:3]))
    expect_true(all(estimatedGaps[['startGaps']][2:3] < timeSeries2[['estimatedPeriods']][['endDate']]))
    expect_true(all(estimatedGaps[['endGaps']][1:2] == timeSeries2[['estimatedPeriods']][['startDate']]))
  })
  
  test_that("estimated periods are treated as gaps for DV", {
    # single estimated period
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries, timezone, isDV=TRUE, inverted = FALSE)
    expect_true(timeSeries[['estimatedPeriods']][['startDate']] > estimatedGaps[['startGaps']])
    expect_false(lubridate::day(timeSeries[['estimatedPeriods']][['startDate']]) == 
                 lubridate::day(estimatedGaps[['startGaps']]))
    expect_equal(lubridate::hour(estimatedGaps[['startGaps']]), 12)
    expect_false(timeSeries[['estimatedPeriods']][['endDate']] == estimatedGaps[['endGaps']])
    expect_equal(lubridate::day(timeSeries[['estimatedPeriods']][['endDate']]), 
                 lubridate::day(estimatedGaps[['endGaps']]))
    expect_equal(lubridate::hour(estimatedGaps[['endGaps']]), 12)
    
  })
  
  test_that("missing or empty estimated periods returns empty data", {
    # empty estimatedPeriods data.frame
    timeSeries3 <- timeSeries
    timeSeries3[['estimatedPeriods']] <- data.frame()
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries3, timezone)
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['startGaps']]))
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['endGaps']]))
    
    # missing estimatedPeriods
    timeSeries3[['estimatedPeriods']] <- NULL
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries3, timezone)
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['startGaps']]))
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['endGaps']]))
  })
  
  test_that("empty timeSeries returns empty data", {
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries = list(), timezone)
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['startGaps']]))
    expect_true(repgen:::isEmptyOrBlank(estimatedGaps[['endGaps']]))
  })
  
  test_that("missing, empty, or incorrectly named points data.frame causes error when inverted=TRUE", {
    timeSeries4 <- timeSeries
    
    timeSeries4[['points']] <- data.frame(fakeName = c(1:2))
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeSeries4, timezone, inverted=TRUE), 
                 "unexpected colnames for points data.frame")
    
    timeSeries4[['points']] <- data.frame()
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeSeries4, timezone, inverted=TRUE), 
                 "points data.frame is empty")
    
    timeSeries4[['points']] <- NULL
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeSeries4, timezone, inverted=TRUE), 
                 "points data.frame is empty")
  })
  
  test_that("missing or empty timezone causes error", {
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeSeries = timeSeries), 
                 "timezone is either missing or empty")
    expect_error(repgen:::createGapsFromEstimatedPeriods(timeSeries = timeSeries, timezone = ""), 
                 "timezone is either missing or empty")
  })



context('applyDataGaps')

  timezone <- "Etc/GMT+5"
  isDV <- FALSE
  set.seed(53)
  df_dates <- seq(as.POSIXct("2015-10-01 00:00:00", tz=timezone), 
                  as.POSIXct("2015-10-03 11:00:00", tz=timezone), by = "hour")
  timeValueDF <- data.frame(time = df_dates,
                            values = runif(60, 0, 3000))
  
  test_that('gaps split for single gap', {
    
    startGaps <- as.POSIXct("2015-10-01 07:00:00", tz=timezone)
    endGaps <- as.POSIXct("2015-10-02 04:00:00", tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    # list of data frames correct
    expect_equal(length(splitData), 2)
    expect_equal(nrow(splitData[[1]]), 8)
    expect_equal(nrow(splitData[[2]]), 32)
    
    # times are exclusive (not removed as part of gap)
    expect_equal(tail(splitData[[1]][['time']], 1), startGaps)
    expect_equal(head(splitData[[2]][['time']], 1), endGaps)
    
  })
  
  test_that('gaps split for multiple gaps', {
    
    startGaps <- as.POSIXct(c("2015-10-01 07:00:00", "2015-10-02 09:00:00", 
                              "2015-10-03 03:00:00"), tz=timezone)
    endGaps <- as.POSIXct(c("2015-10-02 04:00:00", "2015-10-02 14:00:00", 
                            "2015-10-03 07:00:00"), tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 4)
    expect_equal(nrow(splitData[[1]]), 8)
    expect_equal(nrow(splitData[[2]]), 6)
    expect_equal(nrow(splitData[[3]]), 14)
    expect_equal(nrow(splitData[[4]]), 5)
    
  })
  
  test_that('gaps split for when gap times are not exactly times in data', {
    
    startGaps <- as.POSIXct("2015-10-02 09:30:00", tz=timezone)
    endGaps <- as.POSIXct("2015-10-02 10:30:00", tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 2)
    expect_equal(nrow(splitData[[1]]), 34)
    expect_equal(nrow(splitData[[2]]), 25)
    expect_false(tail(splitData[[1]][['time']], 1) == startGaps)
    expect_false(head(splitData[[2]][['time']], 1) == endGaps)
    expect_true(tail(splitData[[1]][['time']], 1) < startGaps)
    expect_true(head(splitData[[2]][['time']], 1) > endGaps)
    
  })
  
  test_that('no data is returned when start/end of gap is outside date range', {
    
    # both are outside the range, so no data should be returned
    startGaps <- as.POSIXct("2015-09-30 09:00:00", tz=timezone)
    endGaps <- as.POSIXct("2015-11-02 10:00:00", tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 0)
    expect_true(repgen:::isEmptyOrBlank(splitData))
  })
  
  test_that('partial data is returned when start or end of gap is outside date range', {
    
    # startGaps is before, endGaps is in range
    startGaps <- as.POSIXct("2015-09-30 09:00:00", tz=timezone)
    endGaps <- as.POSIXct("2015-10-02 10:00:00", tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 1)
    expect_equal(nrow(splitData[[1]]), 26)
    expect_equal(head(splitData[[1]][['time']], 1), endGaps)
    
    # startGaps is in range, endGaps is not in range 
    startGaps <- as.POSIXct("2015-10-02 10:00:00", tz=timezone)
    endGaps <- as.POSIXct("2015-11-02 10:00:00", tz=timezone)
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 1)
    expect_equal(nrow(splitData[[1]]), 35)
    expect_equal(tail(splitData[[1]][['time']], 1), startGaps)
    
  })
  
  test_that('original data is returned when start or end gaps are empty', {
    
    startGaps <- c()
    endGaps <- c()
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 1)
    expect_equal(nrow(splitData[[1]]), 60)
    
  })
  
  test_that('gaps split at correct dates and times for DV', {
    
    startGaps <- as.POSIXct("2015-10-01", tz=timezone)
    endGaps <- as.POSIXct("2015-10-02", tz=timezone)
    isDV <- TRUE
    splitData <- repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV)
    
    expect_equal(length(splitData), 2)
    expect_equal(nrow(splitData[[1]]), 1)
    expect_equal(nrow(splitData[[2]]), 36)
    
  })
  
  test_that('error when any argument is not provided', {
    
    expect_error(repgen:::applyDataGaps(), "timeValueDF is either missing or empty")
    expect_error(repgen:::applyDataGaps(timeValueDF), "start or end gaps are missing")
    expect_error(repgen:::applyDataGaps(timeValueDF, startGaps, endGaps), "timezone is either missing or empty")
    
  })
  
  test_that('error when timeValueDF, timezone, or isDV are empty', {
    
    expect_error(repgen:::applyDataGaps(timeValueDF = data.frame(), startGaps, endGaps, timezone, isDV), 
                 "timeValueDF is either missing or empty")
    expect_error(repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone = "", isDV),
                 "timezone is either missing or empty")
    expect_error(repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone = NULL, isDV),
                 "timezone is either missing or empty")
    
  })
  
  test_that('error when start and end gaps are not the same length', {
    
    startGaps <- as.POSIXct("2015-10-02 10:00:00", tz=timezone)
    endGaps <- as.POSIXct(c("2015-10-02 10:00:00", "2015-11-02 10:00:00"), tz=timezone)
    expect_error(repgen:::applyDataGaps(timeValueDF, startGaps, endGaps, timezone, isDV),
                 "start and end gaps are different lengths")
    
  })
