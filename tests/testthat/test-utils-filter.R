context("utils-filter tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('do negative or zero values get removed from the data frame?', {
  df <- data.frame(value=c(-1, 0, 1, 2, 3, 5))
  df <- repgen:::removeZeroNegative(df)
  expect_false(df[1,1]<0)
  expect_false(df[2,1]==0)
})

test_that('does it subset by the month I ask it to', {
  library(jsonlite)
  library(lubridate)
  pts <- fromJSON('{
                     "reportMetadata": {
                      "timezone": "Etc/GMT+8"
                     },
                      "aTimeseriesLabel": {
                    
                        "points" : [  
                        {
                        "time": "2016-01-01T00:00:00-08:00",
                        "value": 8.54
                        },
                        {
                        "time": "2016-02-01T00:15:00-08:00",
                        "value": 8.53
                        },
                        {
                        "time": "2016-03-01T00:30:00-08:00",
                        "value": 8.53
                        },
                        {
                        "time": "2016-04-01T00:45:00-08:00",
                        "value": 8.54
                        },
                        {
                        "time": "2016-05-01T01:00:00-08:00",
                        "value": 8.54
                        }
                    ]
                      }
                }')
  pts <- repgen:::getTimeSeries(pts,"aTimeseriesLabel")
  ptsSubset <- repgen:::subsetByMonth(pts, 1604)
  expect_equal(nrow(ptsSubset),1)

})

test_that('expect no data for month requested',{
  library(jsonlite)
  library(lubridate)
  pts <- fromJSON('{
                     "reportMetadata": {
                  "timezone": "Etc/GMT+8"
                    },
                  "aTimeseriesLabel": {
                  
                  "points" : [  
                  {
                  "time": "2016-01-01T00:00:00-08:00",
                  "value": 8.54
                  },
                  {
                  "time": "2016-02-01T00:15:00-08:00",
                  "value": 8.53
                  },
                  {
                  "time": "2016-03-01T00:30:00-08:00",
                  "value": 8.53
                  },
                  {
                  "time": "2016-04-01T00:45:00-08:00",
                  "value": 8.54
                  },
                  {
                  "time": "2016-05-01T01:00:00-08:00",
                  "value": 8.54
                  }
                  ]
                  }
                  }')
  pts <- repgen:::getTimeSeries(pts,"aTimeseriesLabel")
  ptsSubset <- repgen:::subsetByMonth(pts, 1606)
  expect_false(isTRUE(nrow(ptsSubset)==1))

})


setwd(dir = wd)
