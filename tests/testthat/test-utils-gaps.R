context("utils-gaps tests")
  
  

context("splitDataGaps")
  
  

context("splitDataGapsTimeSeries")
  
  test_that('splitDataGaps work when there are no gaps specified', {
    
  })


context("findZeroNegativeGaps")
   
  

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
  
  test_that("missing time series throws an error, but empty timeSeries continues", {
    expect_error(repgen:::findDefinedGaps(timezone = timezone), 'timeSeries is missing')
    
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
    expect_equal(timeSeries[['estimatedPeriods']][['startDate']], estimatedGaps[['startGaps']])
    expect_equal(timeSeries[['estimatedPeriods']][['endDate']], estimatedGaps[['endGaps']])
    
    # multiple estimated periods
    timeSeries2 <- timeSeries
    estGaps_mult <- data.frame(startDate = as.POSIXct(c("2015-10-02 05:00:00", "2015-10-03 02:00:00"), tz=timezone),
                               endDate = as.POSIXct(c("2015-10-02 08:00:00", "2015-10-03 07:00:00"), tz=timezone))
    timeSeries2[['estimatedPeriods']] <- estGaps_mult
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries2, timezone, inverted = FALSE)
    expect_equal(timeSeries2[['estimatedPeriods']][['startDate']], estimatedGaps[['startGaps']])
    expect_equal(timeSeries2[['estimatedPeriods']][['endDate']], estimatedGaps[['endGaps']])
  })
  
  test_that("non-estimated periods are treated as gaps", {
    # single estimated period
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries, timezone, inverted = TRUE)
    expect_false(timeSeries[['estimatedPeriods']][['startDate']] %in% estimatedGaps[['startGaps']])
    expect_false(timeSeries[['estimatedPeriods']][['endDate']] %in% estimatedGaps[['endGaps']])
    expect_true(head(timeSeries[['points']][['time']],1) %in% estimatedGaps[['startGaps']])
    expect_true(estimatedGaps[['startGaps']][2] > timeSeries[['estimatedPeriods']][['endDate']])
    expect_true(estimatedGaps[['endGaps']][1] < timeSeries[['estimatedPeriods']][['startDate']])
    
    # multiple estimated periods
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries2, timezone, inverted = TRUE)
    expect_false(all(timeSeries2[['estimatedPeriods']][['startDate']] %in% estimatedGaps[['startGaps']]))
    expect_false(all(timeSeries2[['estimatedPeriods']][['endDate']] %in% estimatedGaps[['endGaps']]))
    expect_true(head(timeSeries[['points']][['time']],1) %in% estimatedGaps[['startGaps']])
    expect_true(all(estimatedGaps[['startGaps']][2:3] > timeSeries2[['estimatedPeriods']][['endDate']]))
    expect_true(all(estimatedGaps[['endGaps']][1:2] < timeSeries2[['estimatedPeriods']][['startDate']]))
  })
  
  test_that("estimated periods are treated as gaps for DV", {
    # single estimated period
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries, timezone, isDV=TRUE, inverted = FALSE)
    expect_false(timeSeries[['estimatedPeriods']][['startDate']] == estimatedGaps[['startGaps']])
    expect_equal(lubridate::day(timeSeries[['estimatedPeriods']][['startDate']]), 
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
    expect_true(is.null(estimatedGaps[['startGaps']]))
    expect_true(is.null(estimatedGaps[['endGaps']]))
    
    # missing estimatedPeriods
    timeSeries3[['estimatedPeriods']] <- NULL
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries3, timezone)
    expect_true(is.null(estimatedGaps[['startGaps']]))
    expect_true(is.null(estimatedGaps[['endGaps']]))
  })
  
  test_that("empty timeSeries returns empty data", {
    estimatedGaps <- repgen:::createGapsFromEstimatedPeriods(timeSeries = list(), timezone)
    expect_true(is.null(estimatedGaps[['startGaps']]))
    expect_true(is.null(estimatedGaps[['endGaps']]))
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
