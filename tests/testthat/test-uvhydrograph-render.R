context("uvhydrograph-render tests")

wd <- getwd()
setwd(dir = tempdir())

test_that("getCorrectionArrows works",{
    #this test data mimics how correction labels should be. xorigin is a datatime, x is the millis version of a datetime to the right or left of xorigin
    xorigin1 <- as.POSIXct("2016-05-01 18:00:00")
    xorigin2 <- as.POSIXct("2016-05-01 18:45:00")
    xorigin3 <- as.POSIXct("2016-05-23 17:00:00")
    xorigin4 <- as.POSIXct("2016-05-23 17:45:00")
    
    x1 <- as.integer(xorigin1) + 10000 #to the right
    x2 <- as.integer(xorigin2) + 10000 #to the right
    x3 <- as.integer(xorigin3) - 10000 #to the left
    x4 <- as.integer(xorigin4) - 10000 #to the left
    
    testCorrectionLabels <- data.frame(
        x=c(x1, x2, x3, x4),
        xorigin=c(xorigin1, xorigin2, xorigin3, xorigin4), 
        y=c(10, 10, 10, 10), 
        r=c(1, 1, 1, 1), 
        label=c(1, 2, 3, 4), 
        stringsAsFactors=FALSE)
    
    arrows <- repgen:::getCorrectionArrows(testCorrectionLabels)
    
    #all y positions stay the same
    expect_equal(arrows[['y']][1], 10)
    expect_equal(arrows[['y']][2], 10)
    expect_equal(arrows[['y']][3], 10)
    expect_equal(arrows[['y']][4], 10)
    
    #all xorigin positions stay the same
    expect_equal(arrows[['xorigin']][1], xorigin1)
    expect_equal(arrows[['xorigin']][2], xorigin2)
    expect_equal(arrows[['xorigin']][3], xorigin3)
    expect_equal(arrows[['xorigin']][4], xorigin4)
    
    #all x's got shifted so that x the arrow (x-xorigin) is shorter
    expect_equal(arrows[['x']][1] < x1, TRUE)
    expect_equal(arrows[['x']][2] < x2, TRUE)
    expect_equal(arrows[['x']][3] > x3, TRUE)
    expect_equal(arrows[['x']][4] > x4, TRUE)
})

test_that("getCorrectionPositions works",{
  testCorrections <- data.frame(
      time=c(as.POSIXct("2016-05-23 17:00:00"), as.POSIXct("2016-05-23 17:45:00"), as.POSIXct("2016-05-23 17:45:00")), 
      value=c(NA, NA, NA), 
      month=c("1605", "1605", "1605"), 
      comment=c("correction 1", "correction 2", "correction 3"), 
      stringsAsFactors=FALSE)
  
  abLines <- repgen:::getCorrectionPositions(testCorrections)
  expect_equal(length(abLines), 2) #dupe position removed
  expect_equal(abLines[[1]], as.POSIXct("2016-05-23 17:00:00"))
  expect_equal(abLines[[2]], as.POSIXct("2016-05-23 17:45:00")) 
  
  #null supported
  expect_equal(length(repgen:::getCorrectionPositions(NULL)), 0)
  
  #empty frame supported
  expect_equal(length(repgen:::getCorrectionPositions(
              na.omit(data.frame(time=as.POSIXct(NA), value=NA, month=as.character(NA), comment=as.character(NA), stringsAsFactors=FALSE)))
    ), 0)
})

setwd(dir = wd)
