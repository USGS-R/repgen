context("extremes tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing extremes json parsing")
test_that("bad list for extremes", {
    data <- list('data'=c(0,0,0))
    expect_error(extremes(data))
})

context("testing extremes when some fields are missing and are complete")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-example.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing elimination of repeat inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing example of no point data")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-points-example.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing example of no qualifiers")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-qualifiers-example.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing example of no upchain")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing example of no dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-dv.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing example of no upchain or dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain-no-dv.json',package = 'repgen'))
  expect_is(repgen:::extremes(data, 'Author Name'), 'character')
})

context("testing converting extremes reportObject into an extremes data table")
test_that("extremesQualifiersTable finds all qualifiers", {
  library(jsonlite)
  reportObject <- fromJSON('{
    "dv": {
      "min": {
        "points": [
          {
            "time": "2015-09-24",
            "value": 669
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-22",
          "endTime": "2015-04-22",
          "identifier": "ESTIMATED"
        }
      ]
    },
    "reportMetadata": {
      "endDate": "2015-12-16T06:00:00.0000000Z",
      "upchainParameter": "Gage height",
      "upchainLabel": "Gage height.ft@06933500",
      "primaryParameter": "Discharge",
      "upchainId": "bd63dd6b916040d2b277204851d6ef22",
      "upchainUnit": "ft",
      "primaryLabel": "Discharge.ft^3/s@06933500",
      "dvParameter": "Discharge",
      "dvLabel": "Discharge.ft^3/s.Mean@06933500",
      "primaryUnit": "ft^3/s",
      "dvComputation": "Mean",
      "isInverted": false,
      "timezone": "Etc/GMT+5",
      "qualifierMetadata": {
        "ESTIMATED": {
           "identifier": "ESTIMATED",
           "code": "E",
           "displayName": "Estimated"
        },
	       "ICE": {
            "identifier": "ICE",
            "code": "I",
            "displayName": "Flow affected by Ice"
          }
      }
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ]
      },
    "multipleMinFlag": false,
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-17T03:46:01.0000000Z",
          "endTime": "2015-10-17T04:55:36.0000000Z",
          "identifier": "ICE"
        },
        {
          "startTime": "2015-04-22T08:29:17.0000000Z",
          "endTime": "2015-10-23T01:58:31.0000000Z",
          "identifier": "ESTIMATED"
        }
      ]
    }
  }')
  extremesTable <- repgen:::extremesTable(reportObject)$toRet
  expect_equal(nrow(extremesTable), 6)
  
  expect_equal(extremesTable[1,1], "Max Inst Gage height and corresponding Discharge")
  expect_equal(extremesTable[1,2], "2015-06-22")
  expect_equal(extremesTable[1,3], "00:00:00  (UTC -05:00)")
  expect_equal(extremesTable[1,4], "I,E 56900")
  expect_equal(extremesTable[1,5], "21.75")
  
  expect_equal(extremesTable[2,1], "Max Inst Discharge and corresponding Gage height")
  expect_equal(extremesTable[2,2], "2015-06-22")
  expect_equal(extremesTable[2,3], "00:00:00  (UTC -05:00)")
  expect_equal(extremesTable[2,4], "I,E 56900")
  expect_equal(extremesTable[2,5], "21.75")
  
  expect_equal(extremesTable[3,1], "Max Daily Mean   Discharge")
  expect_equal(extremesTable[3,2], "2015-06-22")
  expect_equal(extremesTable[3,3], "")
  expect_equal(extremesTable[3,4], "46100")
  expect_equal(extremesTable[3,5], "N/A")
  
  expect_equal(extremesTable[4,1], "Min Inst Gage height and corresponding Discharge")
  expect_equal(extremesTable[4,2], "2015-09-24")
  expect_equal(extremesTable[4,3], "03:45:00  (UTC -05:00)")
  expect_equal(extremesTable[4,4], "I,E 659")
  expect_equal(extremesTable[4,5], "1.62")
  
  expect_equal(extremesTable[5,1], "Min Inst Discharge and corresponding Gage height")
  expect_equal(extremesTable[5,2], "2015-09-24")
  expect_equal(extremesTable[5,3], "03:45:00  (UTC -05:00)")
  expect_equal(extremesTable[5,4], "I,E 659")
  expect_equal(extremesTable[5,5], "1.62")
  
  expect_equal(extremesTable[6,1], "Min Daily Mean   Discharge")
  expect_equal(extremesTable[6,2], "2015-09-24")
  expect_equal(extremesTable[6,3], "")
  expect_equal(extremesTable[6,4], "669")
  expect_equal(extremesTable[6,5], "N/A")
})

context("testing qualifiers list resolution from data and processed extremes table")
test_that("extremesQualifiersTable finds all qualifiers", {
  reportObject <- fromJSON('{
    "dv": {
      "min": {
        "points": [
          {
            "time": "2015-09-24",
            "value": 669
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-21",
          "endTime": "2015-04-21",
          "identifier": "ESTIMATED"
        }
      ]
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
      "qualifierMetadata": {
        "ESTIMATED": {
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated"
        },
       "ICE": {
         "identifier": "ICE",
         "code": "I",
         "displayName": "Flow affected by Ice"
        }
      }
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T03:45:00.0000000Z",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.0000000Z",
            "value": 1.62
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T00:00:00.0000000Z",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.0000000Z",
            "value": 21.75
          }
        ]
      },
        "multipleMaxFlag": false,
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T03:45:00.0000000Z",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.0000000Z",
            "value": 659
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T00:00:00.0000000Z",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.0000000Z",
            "value": 56900
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-16T22:46:01.0000000Z",
          "endTime": "2015-10-16T23:55:36.0000000Z",
          "identifier": "ICE"
        },
        {
          "startTime": "2015-04-22T03:29:17.0000000Z",
          "endTime": "2015-10-22T20:58:31.0000000Z",
          "identifier": "ESTIMATED"
        }
      ]
    }
  }')

  extremesTable <- list(
      c("max GH and corr Q", "max q and corr GH", "max dv Q", "min GH and corr Q", "min Q and corr GH", "min dv Q"), #short handed titles
      "Date"=c("2015-06-22", "2015-06-22", "2015-06-22", "2015-09-24 *", "2015-09-24", "2015-09-24"), 
      "Time"=c("00:00:00 (UTC -05:00)", "00:00:00 (UTC -05:00)", "", "03:45:00 (UTC -05:00)", "03:45:00 (UTC -05:00)", ""), 
      "Prim"=c("I,E 56900", "I,E 56900", "46100", "I,E 659", "I,E 659", "669"),
      "Up"=c("21.75", "21.75", "N/A", "1.62", "1.62", "N/A")
  )
  qualifiersFound <- repgen:::extremesQualifiersTable(reportObject, extremesTable, "Prim", "Up")
  expect_equal(nrow(qualifiersFound), 2)
  expect_equal(qualifiersFound[1,]$Code, "E")
  expect_equal(qualifiersFound[1,]$Identifier, "ESTIMATED")
  expect_equal(qualifiersFound[1,]$Description, "Estimated")
  expect_equal(qualifiersFound[2,]$Code, "I")
  expect_equal(qualifiersFound[2,]$Identifier, "ICE")
  expect_equal(qualifiersFound[2,]$Description, "Flow affected by Ice")
})

context("testing qualifier detection in processed extremes table")
test_that("getExtremesTableQualifiers finds all qualifiers",{
  extremesTable <- list(
      c("max GH and corr Q", "max q and corr GH", "max dv Q", "min GH and corr Q", "min Q and corr GH", "min dv Q"), #short handed titles
      "Date"=c("2015-06-22", "2015-06-22", "2015-06-22", "2015-09-24 *", "2015-09-24", "2015-09-24"),
      "Time"=c("00:00:00 (UTC -05:00)", "00:00:00 (UTC -05:00)", "", "03:45:00 (UTC -05:00)", "03:45:00 (UTC -05:00)", ""), 
      "Prim Col"=c("I,E 56900", "I,E 56900", "46100", "I,E 659", "I,E 659", "669"),
      "Up Col"=c("L,O,L 21.75", "21.75", "N/A", "BRB 1.62", "1.62", "N/A")
    )
  qualifiersFound <- repgen:::getExtremesTableQualifiers(extremesTable, "Prim", "Up")
  expect_equal(length(qualifiersFound), 5)
  expect_equal(qualifiersFound[1], "I")
  expect_equal(qualifiersFound[2], "E")
  expect_equal(qualifiersFound[3], "L")
  expect_equal(qualifiersFound[4], "O")
  expect_equal(qualifiersFound[5], "BRB")
})

context("testing example of multiple min/max on the same date")
test_that("proper number of rows are created based on data",{
  library(jsonlite)
  library(dplyr)
  reportObject <- fromJSON(system.file('extdata','extremes','extremes-multiple-min-max-test.json',package = 'repgen'))
  timezone <- "Etc/GMT+5"
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("upchain"))]], "max", "Max", TRUE, timezone=timezone)[[1]]) == 2)
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("primary"))]], "max", "Max", FALSE, timezone=timezone)[[1]]) == 2)
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("upchain"))]], "min", "min", TRUE, timezone=timezone)[[1]]) == 1)
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("primary"))]], "min", "min", FALSE, timezone=timezone)[[1]]) == 2)
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("dv"))]], "max", "Max", FALSE, timezone=timezone)[[1]]) == 1)
  expect_true(NROW(repgen:::createDataRows(reportObject[[which(names(reportObject) %in% c("dv"))]], "min", "min", FALSE, timezone=timezone)[[1]]) == 1)
})

context("testing filterAndMarkDuplicates")
test_that("filterAndMarkDuplicates does removes duplicate rows and applies the given note to the date field, first of duplicates found kept",{
  data <- data.frame(
    name=c("A name repeated", "A name repeated", "A name repeated"),
    date=c("2015-08-20", "2015-08-20", "2015-08-19"),
    time=c("15:15:00 (UTC -05:00)", "15:00:00 (UTC -05:00)", "16:00:00 (UTC -05:00)"),
	primary=c(" 28.2", " 28.2", " 28.2"),
	related=c(" 28.2", " 28.2", " 28.2"),
	footnote=c("", "", ""),
    stringsAsFactors = FALSE)

  dateFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "date")
  expect_equal(nrow(dateFilteredData), 2)
  expect_equal(dateFilteredData[1,]$date, "2015-08-20 *")
  expect_equal(dateFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(dateFilteredData[1,]$related, " 28.2") #related field included
  
  expect_equal(dateFilteredData[2,]$date, "2015-08-19") #not a dupe
  expect_equal(dateFilteredData[2,]$time, "16:00:00 (UTC -05:00)")
  expect_equal(dateFilteredData[2,]$related, " 28.2") #related field included
  
  primaryFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "primary")
  expect_equal(nrow(primaryFilteredData), 1)
  expect_equal(primaryFilteredData[1,]$date, "2015-08-20 *")
  expect_equal(primaryFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(primaryFilteredData[1,]$related, " 28.2") #related field included
  
  noRelatedFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", FALSE, "primary")
  expect_equal(nrow(noRelatedFilteredData), 1)
  expect_equal(noRelatedFilteredData[1,]$date, "2015-08-20 *")
  expect_equal(noRelatedFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(noRelatedFilteredData[1,]$related, NULL) #related field 
  
  dvFilteredData <- repgen:::filterAndMarkDuplicates(data, "**", TRUE, "primary", TRUE)
  expect_equal(nrow(dvFilteredData), 1)
  expect_equal(dvFilteredData[1,]$date, "2015-08-20 **")
  expect_equal(dvFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(dvFilteredData[1,]$related, " 28.2") #related field included
})

test_that("extremes report qualifiers are associated correctly (applyQualifiers)",{
  library("jsonlite")
  library("dplyr")
  reportObject <- fromJSON('{
    "dv": {
      "min": {
        "points": [
          {
            "time": "2015-09-24",
            "value": 669
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-22",
          "endTime": "2015-04-22",
          "identifier": "ESTIMATED"
        }
      ]
    },
    "reportMetadata": {
      "timezone": "Etc/GMT+5",
                           "qualifierMetadata": {
                           "ESTIMATED": {
                           "identifier": "ESTIMATED",
                           "code": "E",
                           "displayName": "Estimated"
                           },
                           "ICE": {
                           "identifier": "ICE",
                           "code": "I",
                           "displayName": "Flow affected by Ice"
                           }
                           }
},
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-17T03:46:01.0000000Z",
          "endTime": "2015-10-17T04:55:36.0000000Z",
          "identifier": "ICE"
        },
        {
          "startTime": "2015-04-22T08:29:17.0000000Z",
          "endTime": "2015-10-23T01:58:31.0000000Z",
          "identifier": "ESTIMATED"
        }
      ]
    }
  }')
  
  consolidated <- repgen:::completeQualifiers(reportObject)
  reportObject$primary <- consolidated$primary
  reportObject$upchain <- consolidated$upchain
  reportObject$dv <- consolidated$dv
  qualifiersApplied <- repgen:::applyQualifiers(reportObject, repgen:::fetchReportMetadataField(reportObject, 'timezone'))
  expect_equal(qualifiersApplied$upchain$min$relatedPrimary[1,]$value, "I,E 659")
  expect_equal(qualifiersApplied$upchain$min$points[1,]$value, "1.62") #not in qualifier range
  expect_equal(qualifiersApplied$upchain$max$relatedPrimary[1,]$value, "I,E 56900")
  expect_equal(qualifiersApplied$upchain$max$points[1,]$value, "21.75") #not in qualifier range
  
  expect_equal(qualifiersApplied$primary$min$relatedUpchain[1,]$value, "1.62") #not in qualifier range
  expect_equal(qualifiersApplied$primary$min$points[1,]$value, "I,E 659") 
  expect_equal(qualifiersApplied$primary$max$relatedUpchain[1,]$value, "21.75") #not in qualifier range
  expect_equal(qualifiersApplied$primary$max$points[1,]$value, "I,E 56900")
})

context("testing example of point vs. interval comparisons")
test_that("extremes report qualifiers are associated correctly",{
  library(jsonlite)
  library(dplyr)
  reportObject <- fromJSON('{
    "dv": {
         "min": {
           "points": [
             {
               "time": "2016-11-15",
               "value": 4.05
             }
           ]
          },
        "multipleMinFlag": false,
         "max": {
           "points": [
              {
               "time": "2016-11-16",
               "value": 5.7
              }
            ]
         },
        "multipleMaxFlag": false,
         "qualifiers": [
           {
            "startTime": "2015-11-01",
            "endTime": "2016-11-16",
            "identifier": "ESTIMATED"
          }
         ]
        },
       "reportMetadata": {
         "timezone": "Etc/GMT+5",
         "qualifierMetadata": {
            "ESTIMATED": {
              "identifier": "ESTIMATED",
              "code": "E",
              "displayName": "Estimated"
            }
          }
        }
      }')
  
  consolidated <- repgen:::completeQualifiers(reportObject)
  reportObject$dv <- consolidated$dv
  timezone <- "Etc/GMT+5"
  data <- repgen:::applyQualifiers(reportObject, timezone)
  expect_true(grepl("E", data$dv$min$points$value))
  expect_false(grepl("E", data$dv$max$points$value))
})

context("Testing examples of inverted vs non-inverted data")
test_that("Extremes report flips min and max labels when the provided data are inverted", {
  library("jsonlite")
  library("dplyr")
  reportObject <- fromJSON('{
    "reportMetadata": {
      "isInverted": true,
                           "timezone": "Etc/GMT+5",
                           "qualifierMetadata": {
                           "ESTIMATED": {
                           "identifier": "ESTIMATED",
                           "code": "E",
                           "displayName": "Estimated"
                           },
                           "ICE": {
                           "identifier": "ICE",
                           "code": "I",
                           "displayName": "Flow affected by Ice"
                           }
                           }
},
    "dv": {
      "min": {
        "points": [
          {
            "time": "2015-09-24",
            "value": 669
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-22",
          "endTime": "2015-04-22",
          "identifier": "ESTIMATED"
        }
      ]
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ]
      },
      "multipleMinFlag": false,
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T08:45:00.0000000Z",
            "value": 659
          }
        ]
      },
    "multipleMinFlag": false,
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T05:00:00.0000000Z",
            "value": 56900
          }
        ]
      },
      "multipleMaxFlag": false,
      "qualifiers": [
        {
          "startTime": "2015-04-17T03:46:01.0000000Z",
          "endTime": "2015-10-17T04:55:36.0000000Z",
          "identifier": "ICE"
        },
        {
          "startTime": "2015-04-22T08:29:17.0000000Z",
          "endTime": "2015-10-23T01:58:31.0000000Z",
          "identifier": "ESTIMATED"
        }
      ]
    }
  }')

  extremesInv <- repgen:::extremesTable(reportObject)
  reportObject$reportMetadata$isInverted <- FALSE
  extremes <- repgen:::extremesTable(reportObject)

  expect_is(extremesInv$toRet, 'data.frame')
  expect_is(extremes$toRet, 'data.frame')
  expect_equal(extremesInv$toRet[1,][[1]], "Min Inst  and corresponding ")
  expect_equal(extremes$toRet[1,][[1]], "Max Inst  and corresponding ")
})

test_that("testing that extremes can handle more primary than related records", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-aqcu-unmatched-upchain-min-relatedPrimary.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
  
})

test_that("testing that extremes merges series together", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-aqcu-unmatched-upchain-min-relatedPrimary.json',package = 'repgen'))
  relatedPrimary <- data[["upchain"]][["min"]][["relatedPrimary"]]
  points <- data[["upchain"]][["min"]][["points"]]
  expect_equal(nrow(relatedPrimary),20)
  expect_equal(nrow(points),21)
  merged <- repgen:::mergeAndStretch(relatedPrimary, points)
  expect_equal(nrow(merged),21)
  
})

test_that("The order function is ordering the date/times correctly in ascending order and giving the correct min inst gage height date/time for the value", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes',"extremes-example.json", package = 'repgen'))
  extremes <- repgen:::extremesTable(data)
  expect_equal(extremes$toRet[4,][[1]], "Min Inst Gage height and corresponding Discharge")
  expect_equal(extremes$toRet[4,][[2]], "2015-09-24 *")
  expect_equal(extremes$toRet[4,][[3]], "03:45:00  (UTC -05:00)")
})

test_that("The order function is ordering the date/times correctly, an example from bug ticket from Laura", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','testsnippets',"test-extremes-sorting.json", package = 'repgen'))
  extremes <- repgen:::extremesTable(data)
  expect_equal(extremes$toRet[1,][[2]], "2016-10-07 *")
  expect_equal(extremes$toRet[2,][[2]], "2016-10-21 *")
})

test_that("The order function is ordering the date/times correctly, an example from bug ticket from Chuck", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','testsnippets',"test-extremes-sortingOrder.json", package = 'repgen'))
  extremes <- repgen:::extremesTable(data)
  expect_equal(extremes$toRet[1,][[2]], "2017-02-14 *")
  expect_equal(extremes$toRet[2,][[2]], "2017-04-27 *")
})

setwd(dir = wd)
