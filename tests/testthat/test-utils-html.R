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

test_that('do the URLs from JSON turn into a link?', {
  library(jsonlite)
  reportObject <- fromJSON(' {
                           "simsUrl": "http://sims.water.usgs.gov/SIMSClassic/StationInfo.asp?site_no\u003d01014000",
                           "waterdataUrl": "https://waterdata.usgs.gov/nwis/inventory/?site_no\u003d06893390"
                           } ')
  simsUrl <- reportObject[["simsUrl"]]
  waterdataUrl <- reportObject[["waterdataUrl"]]
  waterdataLink <- paste("<a href='",waterdataUrl,"' target='_blank'>","waterdata.usgs.gov URL:",waterdataUrl,"</a>")
  simsLink <- paste("<a href='",simsUrl,"' target='_blank'>","SIMS URL:",simsUrl,"</a>")
  simsStub <- substr(simsLink, 0, 8)
  waterdataStub <- substr(waterdataLink, 0, 8)
  expect_true(grepl("<a href=", waterdataStub))
  expect_true(grepl("<a href=", simsStub))

})

setwd(dir = wd)
