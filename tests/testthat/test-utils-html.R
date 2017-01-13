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
  readings <- reportObject[["readings"]]
  party <- repgen:::nullMask(readings[["party"]])
  expect_false(isTRUE(is.null(party)))
  expect_true(length(party)>0)

})

test_that('does nullmask remove empty party var and return empty chars?', {
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
                           },
                           {
                           "fieldVisitIdentifier": "238488A1048E1955E0530100007F6833",
                           "visitStatus": "TODO",
                           "time": "2015-07-16T05:02:00.000-05:00",
                           "estimatedTime": "2015-07-16T05:06:00.000-05:00",
                           "party": "LEF/JRP",
                           "monitoringMethod": "Pressure Transducer",
                           "value": "8.53",
                           "parameter": "WaterLevel, BelowLSD",
                           "comments": [
                           "Comment \u003d Changed desiccant. Offset as found \u003d 21.140. 1052 reading after put back in well. Sensor suspended from same steel cable as before.",
                           "Comment \u003d Held 9.99 - 1.47 \u003d 8.52 at 0606 // Held 9.99 - 1.36 \u003d 8.63 AT 1052. Used to reset sensors. // Held 11.29 - 2.63 \u003d 8.66 at 1134. // All with ME-LEF-ST1 tapedown tape."
                           ],
                           "type": "Routine"
                              }
  ] ')
  
  
})

setwd(dir = wd)
