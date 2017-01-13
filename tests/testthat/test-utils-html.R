context("utils-html tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('does it replace the escaped characters with real html breaks?', {
  library(jsonlite)
  reportObject <- fromJSON('
                       { "readings": [
                        {
                       "displayTime": "2016-01-06T10:21:00-05:00",
                       "recorderComments": [
                       "Comment \u003d Sensor readings were from the H522, which agreed with Sutron combo unit. no AF readings were taken during this SV.\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                       ],
                       "visitStatus": "TODO",
                       "recorderMethod": "Non-subm pressure  transducer",
                       "recorderValue": "5.02",
                       "recorderType": "Routine",
                       "party": "ARC/SGS",
                       "nearestcorrectedValue": "5.02",
                       "qualifiers": [],
                       "nearestcorrectedTime": "2016-01-06T10:15:00-05:00",
                       "nearestrawTime": "2016-01-06T10:15:00-05:00",
                       "nearestrawValue": "5.02"
},
                       {
                       "displayTime": "2016-02-29T10:57:00-05:00",
                       "recorderComments": [
                       "Comment \\u003d AF/AL offset 1.570\\r\\nOrificeServicedCode \\u003d NTSV\\r\\nDesiccantConditionCode \\u003d DESG\\r\\nDesiccantChangedIndicator \\u003d false\\r\\nGasSystemTypeCode \\u003d BAIR\\r\\nGasTankChangedIndicator \\u003d false"
                       ],
                       "visitStatus": "TODO",
                       "recorderMethod": "Non-subm pressure  transducer",
                       "recorderValue": "5.24",
                       "recorderType": "Routine",
                       "party": "ARC",
                       "nearestcorrectedValue": "5.24",
                       "qualifiers": [],
                       "nearestcorrectedTime": "2016-02-29T11:00:00-05:00",
                       "nearestrawTime": "2016-02-29T11:00:00-05:00",
                       "nearestrawValue": "5.24"
                       }
                        ] }
                       ')
  refComm <- repgen:::getComments(reportObject[["readings"]][["recorderComments"]])
  refComm <- repgen:::formatComments(refComm)
  expect_false(grepl("\\r\\n", refComm))
  expect_true(grepl("<br/>", refComm))
  
  
})  

test_that('timeFormatting properly breaks apart a date-time string into date and time parts', {
  testDateTime <- "2016-05-09T09:01:35.000-06:00"
  testFormatMask <- "%m/%d/%Y"
  testDateFormat <- "05/09/2016"
  testTimeFormat <- "09:01:35  (UTC -06:00)"
  testFormattedList <-  list(date = testDateFormat, time = testTimeFormat)
  testTimeFormatting <- repgen:::timeFormatting(testDateTime, testFormatMask)
  expect_equal(testFormattedList, testTimeFormatting)
  expect_equal(testDateFormat, testTimeFormatting[[1]])
  expect_equal(testTimeFormat, testTimeFormatting[[2]])
})

setwd(dir = wd)
