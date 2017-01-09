context("utils-fetch tests")

test_that('fetchRatingShifts data returns as expected', {
  library(jsonlite)
  
  rawReportJson <- fromJSON('{
    "ratingShifts" : [
        {
          "curveNumber": "9",
          "shiftPoints": [
            0,
            0
          ],
          "stagePoints": [
            3.5,
            5
          ],
          "applicableStartDateTime": "2014-10-09T10:50:00.000-05:00",
          "shiftNumber": 1
        }
      ]
  }')

  ratingShifts <- repgen:::fetchRatingShifts(reportObject)
  expect_equal(ratingShifts$shiftPoints, 1)
})

test_that('fetchReportMetadataField return values and empty string if not found', {
  library(jsonlite)
  
  data <- fromJSON('{ "reportMetadata" : { "field1" : "value1", "field2": "value2" } }')
  
  val1 <- fetchReportMetadataField(data, "field1")
  val2 <- fetchReportMetadataField(data, "field2")
  val3 <- fetchReportMetadataField(data, "field3")
  
  expect_is(val1, 'character')
  expect_is(val2, 'character')
  expect_is(val3, 'NULL')
  
  expect_equal(val1, "value1")
  expect_equal(val2, "value2")
  expect_equal(val3, NULL)
})