context("utils-html tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('does it replace the escaped characters with real html breaks?', {
  library(jsonlite)
  readings <- fromJSON('
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
  refComm <- repgen:::getComments(readings[["readings"]][["recorderComments"]])
  refComm <- repgen:::formatComments(refComm)
})


setwd(dir = wd)
