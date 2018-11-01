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
                       "nearestCorrectedValue": "5.02",
                       "qualifiers": [],
                       "nearestCorrectedTime": "2016-01-06T10:15:00-05:00",
                       "nearestRawTime": "2016-01-06T10:15:00-05:00",
                       "nearestRawValue": "5.02"
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
                       "nearestCorrectedValue": "5.24",
                       "qualifiers": [],
                       "nearestCorrectedTime": "2016-02-29T11:00:00-05:00",
                       "nearestRawTime": "2016-02-29T11:00:00-05:00",
                       "nearestRawValue": "5.24"
                       }
                        ] }
                       ')
  refComm <- repgen:::getComments(reportObject[["readings"]][["recorderComments"]])
  refComm <- repgen:::formatComments(refComm)
  expect_false(grepl("\\r\\n", refComm))
  expect_true(grepl("<br/>", refComm))
  
  
})  

test_that('do the simsUrls from JSON turn into a simsLink?', {
  library(jsonlite)
  reportObject <- fromJSON(' {
                           "simsUrl": "http://sims.water.usgs.gov/SIMSClassic/StationInfo.asp?site_no\u003d01014000"
                           } ')
  simsUrl <- reportObject[["simsUrl"]]
  simsLink <- repgen:::getSimsUrl(simsUrl)
  simsStub <- substr(simsLink, 0, 8)
  expect_true(grepl("<a href=", simsStub))

})

test_that('do the waterdataUrls from JSON turn into a link?', {
  library(jsonlite)
  reportObject <- fromJSON(' {
                           "waterdataUrl": "https://waterdata.usgs.gov/nwis/inventory/?site_no\u003d06893390"
                             } ')
  waterdataUrl <- reportObject[["waterdataUrl"]]
  waterdataLink <- repgen:::getWaterDataUrl(waterdataUrl)
  waterdataStub <- substr(waterdataLink, 0, 8)
  expect_true(grepl("<a href=", waterdataStub))
  
  })

test_that('does nullMask remove empty party var and return empty chars greater than length of zero?', {
  library(jsonlite)
  reportObject <- fromJSON(' {
                        "readings": [
                           {
                           "fieldVisitIdentifier": "238488A1048D1955E0530100007F6833",
                           "visitStatus": "TODO",
                           "time": "2015-03-23T17:05:00.000-05:00",
                           "estimatedTime": "2015-03-23T17:18:00.000-05:00",
                           "party": "",
                           "monitoringMethod": "Pressure Transducer",
                           "value": "11.13",
                           "parameter": "WaterLevel, BelowLSD",
                           "comments": [
                           "Comment \u003d Changed desiccant- was pink. // Wiped snow off solar panels. Still a bit of ice; should melt off in next couple of days.",
                           "Comment \u003d Held 11.99 - 0.85 - 11.14 at 1818 // Held 12.99 - 1.85 \u003d 11.14 at 1827. Both with ME-LEF-ST-1 tape."
                           ],
                           "type": "Routine"
                           },
                           {
                           "fieldVisitIdentifier": "238488A1048D1955E0530100007F6833",
                           "visitStatus": "TODO",
                           "time": "2015-03-23T17:05:00.000-05:00",
                           "estimatedTime": "2015-03-23T17:27:00.000-05:00",
                           "party": "",
                           "monitoringMethod": "Pressure Transducer",
                           "value": "11.13",
                           "parameter": "WaterLevel, BelowLSD",
                           "comments": [
                           "Comment \u003d Changed desiccant- was pink. // Wiped snow off solar panels. Still a bit of ice; should melt off in next couple of days.",
                           "Comment \u003d Held 11.99 - 0.85 - 11.14 at 1818 // Held 12.99 - 1.85 \u003d 11.14 at 1827. Both with ME-LEF-ST-1 tape."
                           ],
                           "type": "Routine"
                              }
                            ]}')
  testSeries <- reportObject[["readings"]]
  party <- repgen:::nullMask(testSeries[["party"]])
  expect_false(isTRUE(is.null(party)))
  expect_true(length(party)>0)
  
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

test_that('timeFormatting properly breaks apart a date-time string into date and time parts when specifying separator', {
	testDateTime <- "2016-05-09G09:01:35.000-06:00"
	testFormatMask <- "%m/%d/%Y"
	testDateFormat <- "05/09/2016"
	testTimeFormat <- "09:01:35  (UTC -06:00)"
	testFormattedList <-  list(date = testDateFormat, time = testTimeFormat)
	testTimeFormatting <- repgen:::timeFormatting(testDateTime, testFormatMask, "[G]")
	expect_equal(testFormattedList, testTimeFormatting)
	expect_equal(testDateFormat, testTimeFormatting[[1]])
	expect_equal(testTimeFormat, testTimeFormatting[[2]])
	
	testDateTime <- "2016-05-09 09:01:35.000-06:00"
	testFormattedList <-  list(date = testDateFormat, time = testTimeFormat)
	testTimeFormatting <- repgen:::timeFormatting(testDateTime, testFormatMask, " ")
	expect_equal(testFormattedList, testTimeFormatting)
	expect_equal(testDateFormat, testTimeFormatting[[1]])
	expect_equal(testTimeFormat, testTimeFormatting[[2]])
})

test_that('formatQualifiersTable handles no qualifiers', {
  library(jsonlite)
  
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": []
}')
  qualsDf <- repgen:::readFetchedQualifiers(inQualifiers,NULL)
  qualList <- repgen:::formatQualifiersTable(qualsDf)
  expect_true(nrow(qualList)==0)
  })

test_that('formatQualifiersTable removes dups', {
  library(jsonlite)
  reportObject <- fromJSON('{
                           "reportMetadata": {
                            "timezone": "Etc/GMT+5",
                        	  "qualifierMetadata": {
                                "EQUIP": {
                                  "identifier": "EQUIP",
                                  "code": "EQP",
                                  "displayName": "Equipment Malfunction"
                                }
                              }
                          }
                           }')
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startTime": "2015-08-26T10:00:00.0000000Z",
                           "endTime": "2015-08-26T16:00:00.0000000Z",
                           "identifier": "EQUIP"
                           },
                           {
                           "startTime": "2015-07-05T14:30:00.0000000Z",
                           "endTime": "2015-07-06T19:30:00.0000000Z",
                           "identifier": "EQUIP"
                           }
                           ]

}')
  qualsDf <- repgen:::readFetchedQualifiers(reportObject, inQualifiers,NULL)
  qualList <- repgen:::formatQualifiersTable(qualsDf)
  expect_equal(length(qualList),3L)
  expect_equal(qualList$Code, "EQP")
  expect_equal(qualList$Identifier, "EQUIP")
  expect_equal(qualList$Description, "Equipment Malfunction")
})

test_that('formatQualifiersTable handles > 1 unique qualifier', {
  library(jsonlite)
  reportObject <- fromJSON('{
                           "reportMetadata": {
                            "timezone": "Etc/GMT+5",
                        	  "qualifierMetadata": {
                                "EQUIP": {
                                  "identifier": "EQUIP",
                                  "code": "EQP",
                                  "displayName": "Equipment Malfunction"
                                },
                                "TESTQUAL": {
                                "identifier": "TESTQUAL",
                                "code": "TQL",
                                "displayName": "Test Qualifier"
                              }
                              }
                          }
                           }')
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startTime": "2015-08-26T10:00:00.0000000Z",
                           "endTime": "2015-08-26T16:00:00.0000000Z",
                           "identifier": "EQUIP"
                           },
                           {
                           "startTime": "2015-07-05T14:30:00.0000000Z",
                           "endTime": "2015-07-06T20:30:00.0000000Z",
                           "identifier": "TESTQUAL"
                           }
                           ]
}')
  qualsDf <- repgen:::readFetchedQualifiers(reportObject, inQualifiers, NULL)
  qualList <- repgen:::formatQualifiersTable(qualsDf)
  expect_equal(length(qualList),3L)
  expect_equal(qualList$Code[[1]], "EQP")
  expect_equal(qualList$Identifier[[1]], "EQUIP")
  expect_equal(qualList$Description[[1]], "Equipment Malfunction")
  expect_equal(qualList$Code[[2]], "TQL")
  expect_equal(qualList$Identifier[[2]], "TESTQUAL")
  expect_equal(qualList$Description[[2]], "Test Qualifier")
})

test_that('formatQualifiersStringList returns string of comma-separated codes (dups)', {
  library(jsonlite)
  reportObject <- fromJSON('{
                           "reportMetadata": {
                            "timezone": "Etc/GMT+5",
                        	  "qualifierMetadata": {
                                "EQUIP": {
                                  "identifier": "EQUIP",
                                  "code": "EQP",
                                  "displayName": "Equipment Malfunction"
                                }
                              }
                          }
                           }')
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startTime": "2015-08-26T10:00:00.0000000Z",
                           "endTime": "2015-08-26T16:00:00.0000000Z",
                           "identifier": "EQUIP"
                           },
                           {
                           "startTime": "2015-07-05T14:30:00.0000000Z",
                           "endTime": "2015-07-06T20:30:00.0000000Z",
                           "identifier": "EQUIP"
                           }
                           ]
}')
  qualsDf <- repgen:::readFetchedQualifiers(reportObject, inQualifiers,NULL)
  qualList <- repgen:::formatQualifiersStringList(qualsDf)
  expect_equal(qualList, "EQP,EQP")
  })

test_that('formatQualifiersStringList returns string of comma-separated codes (unique)', {
  library(jsonlite)
  reportObject <- fromJSON('{
                           "reportMetadata": {
                            "timezone": "Etc/GMT+5",
                        	  "qualifierMetadata": {
                                "EQUIP": {
                                  "identifier": "EQUIP",
                                  "code": "EQP",
                                  "displayName": "Equipment Malfunction"
                                },
                                "TESTQUAL": {
                                "identifier": "TESTQUAL",
                                "code": "TQL",
                                "displayName": "Test Qualifier"
                              }
                              }
                          }
                           }')
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": [
                           {
                           "startTime": "2015-08-26T05:00:00.000-05:00",
                           "endTime": "2015-08-26T11:00:00.000-05:00",
                           "identifier": "TESTQUAL"
                           },
                           {
                           "startDate": "2015-07-05T09:30:00.000-05:00",
                           "endDate": "2015-07-06T15:30:00.000-05:00",
                           "identifier": "EQUIP"
                           }
                           ]
}')
  qualsDf <- repgen:::readFetchedQualifiers(reportObject, inQualifiers,NULL)
  qualList <- repgen:::formatQualifiersStringList(qualsDf)
  expect_equal(qualList, "TQL,EQP")
})

test_that('formatQualifiersStringList handles null qualifiers', {
  library(jsonlite)
  reportObject <- fromJSON('{
                           "reportMetadata": {
                            "timezone": "Etc/GMT+5",
                        	  "qualifierMetadata": []
                          }
                           }')
  inQualifiers <- fromJSON('{
                           "associatedIvQualifiers": []
}')
  qualsDf <- repgen:::readFetchedQualifiers(reportObject, inQualifiers,NULL)
  qualList <- I(list(qualsDf))
  qualString <- repgen:::formatQualifiersStringList(qualList)
  expect_equal(qualString, "")
})

test_that('getSVPColumns includes comment column', {
  library(jsonlite)
  
  SVPColumns <- repgen:::getSVPColumns(TRUE)
  expect_true("Verification Comments" %in% SVPColumns)
})

test_that('getSVPColumns does not include comment column', {
  library(jsonlite)
  
  SVPColumns <- repgen:::getSVPColumns(FALSE)
  expect_false("Verification Comments" %in% SVPColumns)
})

test_that('containsOutsideUncertainty does what it is supposed to',{
    testDiff <- c("1", "3", "342 **", "kjdasd")
    expect_true(repgen:::containsOutsideUncertainty(testDiff))
    testDiff <- c("1", "3", "342", "kjdasd")
    expect_false(repgen:::containsOutsideUncertainty(testDiff))
})

setwd(dir = wd)
