context("utils-gaps tests")
  
  

context("splitDataGaps")
  
  

context("splitDataGapsTimeSeries")
  
  test_that('splitDataGaps work when there are no gaps specified', {
    
  })


context("findZeroNegativeGaps")
   
  

context("findDefinedGaps")
  


context("createGapsFromEstimatedPeriods")
  
  

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
