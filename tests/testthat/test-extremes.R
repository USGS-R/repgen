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
  expect_is(extremes(data), 'character')
})

context("testing elimination of repeat inst max & min values")
test_that("example data extremes", {
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-eliminate-duplicates.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no point data")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-points-example.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no qualifiers")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-qualifiers-example.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no upchain")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-dv.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
})

context("testing example of no upchain or dv")
test_that("extremes examples work",{
  library(jsonlite)
  library(dplyr)
  data <- fromJSON(system.file('extdata','extremes','extremes-no-upchain-no-dv.json',package = 'repgen'))
  expect_is(extremes(data), 'character')
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
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-21T22:57:56.000-05:00",
          "endDate": "2015-04-21T22:57:56.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    },
    "reportMetadata": {
      "endDate": "2015-12-16T00:00:00-06:00",
      "upchainParameter": "Gage height",
      "upchainLabel": "Gage height.ft@06933500",
      "primaryParameter": "Discharge",
      "upchainId": "bd63dd6b916040d2b277204851d6ef22",
      "upchainUnit": "ft",
      "primaryLabel": "Discharge.ft^3/s@06933500",
      "dvParameter": "Discharge",
      "dvLabel": "Discharge.ft^3/s.Mean@06933500",
      "primaryUnit": "ft^3/s",
      "dvComputation": "Mean"
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ]
      },
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ]
      },
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ]
      },
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-16T22:46:01.000-05:00",
          "endDate": "2015-10-16T23:55:36.000-05:00",
          "identifier": "ICE",
          "code": "I",
          "displayName": "Flow affected by Ice",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        },
        {
          "startDate": "2015-04-22T03:29:17.000-05:00",
          "endDate": "2015-10-22T20:58:31.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    }
  }')
  extremesTable <- repgen:::extremesTable(reportObject)
  expect_equal(nrow(extremesTable), 6)
  
  expect_equal(extremesTable[1,1], "Max Inst Gage height and corresponding Discharge")
  expect_equal(extremesTable[1,2], "06-22-2015")
  expect_equal(extremesTable[1,3], "00:00:00  (UTC -05:00)")
  expect_equal(extremesTable[1,4], "I,E 56900")
  expect_equal(extremesTable[1,5], " 21.75")
  
  expect_equal(extremesTable[2,1], "Max Inst Discharge and corresponding Gage height")
  expect_equal(extremesTable[2,2], "06-22-2015")
  expect_equal(extremesTable[2,3], "00:00:00  (UTC -05:00)")
  expect_equal(extremesTable[2,4], "I,E 56900")
  expect_equal(extremesTable[2,5], " 21.75")
  
  expect_equal(extremesTable[3,1], "Max Daily Mean   Discharge")
  expect_equal(extremesTable[3,2], "06-22-2015")
  expect_equal(extremesTable[3,3], "")
  expect_equal(extremesTable[3,4], " 46100")
  expect_equal(extremesTable[3,5], "N/A")
  
  expect_equal(extremesTable[4,1], "Min Inst Gage height and corresponding Discharge")
  expect_equal(extremesTable[4,2], "09-24-2015")
  expect_equal(extremesTable[4,3], "03:45:00  (UTC -05:00)")
  expect_equal(extremesTable[4,4], "I,E 659")
  expect_equal(extremesTable[4,5], " 1.62")
  
  expect_equal(extremesTable[5,1], "Min Inst Discharge and corresponding Gage height")
  expect_equal(extremesTable[5,2], "09-24-2015")
  expect_equal(extremesTable[5,3], "03:45:00  (UTC -05:00)")
  expect_equal(extremesTable[5,4], "I,E 659")
  expect_equal(extremesTable[5,5], " 1.62")
  
  expect_equal(extremesTable[6,1], "Min Daily Mean   Discharge")
  expect_equal(extremesTable[6,2], "09-24-2015")
  expect_equal(extremesTable[6,3], "")
  expect_equal(extremesTable[6,4], " 669")
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
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-21T22:57:56.000-05:00",
          "endDate": "2015-04-21T22:57:56.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    },
    "reportMetadata": {
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ]
      },
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ]
      },
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ]
      },
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-16T22:46:01.000-05:00",
          "endDate": "2015-10-16T23:55:36.000-05:00",
          "identifier": "ICE",
          "code": "I",
          "displayName": "Flow affected by Ice",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        },
        {
          "startDate": "2015-04-22T03:29:17.000-05:00",
          "endDate": "2015-10-22T20:58:31.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    }
  }')

  extremesTable <- list(
      c("max GH and corr Q", "max q and corr GH", "max dv Q", "min GH and corr Q", "min Q and corr GH", "min dv Q"), #short handed titles
      "Date"=c("06-22-2015", "06-22-2015", "06-22-2015", "09-24-2015 *", "09-24-2015", "09-24-2015"), 
      "Time"=c("00:00:00 (UTC -05:00)", "00:00:00 (UTC -05:00)", "", "03:45:00 (UTC -05:00)", "03:45:00 (UTC -05:00)", ""), 
      "Prim"=c("I,E 56900", "I,E 56900", "46100", "I,E 659", "I,E 659", "669"),
      "Up"=c("21.75", "21.75", "N/A", "1.62", "1.62", "N/A")
  )
  qualifiersFound <- repgen:::extremesQualifiersTable(reportObject, extremesTable, "Prim", "Up")
  expect_equal(nrow(qualifiersFound), 2)
  expect_equal(qualifiersFound[1,]$Code, "I")
  expect_equal(qualifiersFound[1,]$Identifier, "ICE")
  expect_equal(qualifiersFound[1,]$Description, "Flow affected by Ice")
  expect_equal(qualifiersFound[2,]$Code, "E")
  expect_equal(qualifiersFound[2,]$Identifier, "ESTIMATED")
  expect_equal(qualifiersFound[2,]$Description, "Estimated")
})

context("testing qualifier detection in processed extremes table")
test_that("getExtremesTableQualifiers finds all qualifiers",{
  extremesTable <- list(
      c("max GH and corr Q", "max q and corr GH", "max dv Q", "min GH and corr Q", "min Q and corr GH", "min dv Q"), #short handed titles
      "Date"=c("06-22-2015", "06-22-2015", "06-22-2015", "09-24-2015 *", "09-24-2015", "09-24-2015"), 
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
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("upchain"))]], "max", "Max", TRUE)[[1]]) == 2)
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("primary"))]], "max", "Max", FALSE)[[1]]) == 2)
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("upchain"))]], "min", "min", TRUE)[[1]]) == 1)
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("primary"))]], "min", "min", FALSE)[[1]]) == 2)
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("dv"))]], "max", "Max", FALSE)[[1]]) == 1)
  expect_true(NROW(createDataRows(reportObject[[which(names(reportObject) %in% c("dv"))]], "min", "min", FALSE)[[1]]) == 1)
})

context("testing filterAndMarkDuplicates")
test_that("filterAndMarkDuplicates does removes duplicate rows and applies the given note to the date field, first of duplicates found kept",{
  data <- data.frame(
    name=c("A name repeated", "A name repeated", "A name repeated"),
    date=c("08-20-2015", "08-20-2015", "08-19-2015"),
    time=c("15:15:00 (UTC -05:00)", "15:00:00 (UTC -05:00)", "16:00:00 (UTC -05:00)"),
	primary=c(" 28.2", " 28.2", " 28.2"),
	related=c(" 28.2", " 28.2", " 28.2"),
    stringsAsFactors = FALSE)

  dateFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "date")
  expect_equal(nrow(dateFilteredData), 2)
  expect_equal(dateFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(dateFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(dateFilteredData[1,]$related, " 28.2") #related field included
  
  expect_equal(dateFilteredData[2,]$date, "08-19-2015") #not a dupe
  expect_equal(dateFilteredData[2,]$time, "16:00:00 (UTC -05:00)")
  expect_equal(dateFilteredData[2,]$related, " 28.2") #related field included
  
  primaryFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", TRUE, "primary")
  expect_equal(nrow(primaryFilteredData), 1)
  expect_equal(primaryFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(primaryFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(primaryFilteredData[1,]$related, " 28.2") #related field included
  
  noRelatedFilteredData <- repgen:::filterAndMarkDuplicates(data, "*", FALSE, "primary")
  expect_equal(nrow(noRelatedFilteredData), 1)
  expect_equal(noRelatedFilteredData[1,]$date, "08-20-2015 *")
  expect_equal(noRelatedFilteredData[1,]$time, "15:15:00 (UTC -05:00)") #verifies first dupe found is winner
  expect_equal(noRelatedFilteredData[1,]$related, NULL) #related field NOT included
})

test_that("extremes report qualifiers are associated correctly (applyQualifiers)",{
  library("jsonlite")
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
      "max": {
        "points": [
          {
            "time": "2015-06-22",
            "value": 46100
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-21T22:57:56.000-05:00",
          "endDate": "2015-04-21T22:57:56.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    },
    "reportMetadata": {
    },
    "upchain": {
      "min": {
        "relatedPrimary": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ]
      },
      "max": {
        "relatedPrimary": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ]
      },
      "qualifiers": []
    },
    "primary": {
      "min": {
        "relatedUpchain": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 1.62
          }
        ],
        "points": [
          {
            "time": "2015-09-24T03:45:00.000-05:00",
            "value": 659
          }
        ]
      },
      "max": {
        "relatedUpchain": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 21.75
          }
        ],
        "points": [
          {
            "time": "2015-06-22T00:00:00.000-05:00",
            "value": 56900
          }
        ]
      },
      "qualifiers": [
        {
          "startDate": "2015-04-16T22:46:01.000-05:00",
          "endDate": "2015-10-16T23:55:36.000-05:00",
          "identifier": "ICE",
          "code": "I",
          "displayName": "Flow affected by Ice",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        },
        {
          "startDate": "2015-04-22T03:29:17.000-05:00",
          "endDate": "2015-10-22T20:58:31.000-05:00",
          "identifier": "ESTIMATED",
          "code": "E",
          "displayName": "Estimated",
          "appliedBy": "admin",
          "dateApplied": "2015-11-27T22:35:14.957-06:00"
        }
      ]
    }
  }')
  
  qualifiersApplied <- repgen:::applyQualifiers(reportObject)
  expect_equal(qualifiersApplied$upchain$min$relatedPrimary[1,]$value, "I,E 659")
  expect_equal(qualifiersApplied$upchain$min$points[1,]$value, " 1.62") #not in qualifier range
  expect_equal(qualifiersApplied$upchain$max$relatedPrimary[1,]$value, "I,E 56900")
  expect_equal(qualifiersApplied$upchain$max$points[1,]$value, " 21.75") #not in qualifier range
  
  expect_equal(qualifiersApplied$primary$min$relatedUpchain[1,]$value, " 1.62") #not in qualifier range
  expect_equal(qualifiersApplied$primary$min$points[1,]$value, "I,E 659") 
  expect_equal(qualifiersApplied$primary$max$relatedUpchain[1,]$value, " 21.75") #not in qualifier range
  expect_equal(qualifiersApplied$primary$max$points[1,]$value, "I,E 56900")
})

context("testing example of point vs. interval comparisons")
test_that("extremes report qualifiers are associated correctly",{
  library(jsonlite)
  library(dplyr)
  
  qualifiers <-
    data.frame(
      startDate = "2015-11-01", endDate = "2016-11-16",
      identifier = "ESTIMATED", code = "E", displayName = "Estimated",
      stringsAsFactors = FALSE
    )
  
  points1 <- data.frame(
    time = c("2016-11-15"), value = c(4.05), stringsAsFactors = FALSE
  )
  points2 <- data.frame(
    time = c("2016-11-16"), value = c(5.7), stringsAsFactors = FALSE
  )
  
  q1 <- repgen:::applyQualifiersToValues(points1, qualifiers)
  expect_true(grepl("E", q1$value))
  
  q2 <- repgen:::applyQualifiersToValues(points2, qualifiers)
  expect_false(grepl("E", q2$value))
})

setwd(dir = wd)
