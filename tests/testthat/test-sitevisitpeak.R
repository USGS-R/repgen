context("sitevisitpeak tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing sitevisitpeak")

test_that("sitevisitpeak examples work",{
  library(jsonlite)
  
  data <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data, 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-empty-example.json', package = 'repgen'))
  expect_is(sitevisitpeak(data2, 'Author Name'), 'character')
  
})

context("testing sitevisitpeak-data")

test_that("sitevisitpeakTable returns what it's supposed to",{
  library(jsonlite)
  
  data <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-example.json', package = 'repgen'))
  siteVisitReport <- repgen:::sitevisitpeakTable(repgen:::readFieldVisitReadings(data),repgen:::fetchReportMetadataField(data,'excludeComments'))
  expect_equal(ncol(siteVisitReport),15L)
  expect_equal(nrow(siteVisitReport),4L)
  expect_true("Verification Comments" %in% colnames(siteVisitReport))
  expect_equal(siteVisitReport$Date[[1]],"12/05/2014")
  expect_equal(siteVisitReport$Date[[2]],"01/06/2015")
  expect_equal(siteVisitReport$Date[[3]],"04/03/2015")
  expect_equal(siteVisitReport$Date[[4]],"08/07/2015")
  expect_equal(siteVisitReport$Time[[1]],"09:59:00  (UTC -06:00)")
  expect_equal(siteVisitReport$Time[[2]],"08:46:00  (UTC -06:00)")
  expect_equal(siteVisitReport$Time[[3]],"09:41:00  (UTC -05:00)")
  expect_equal(siteVisitReport$Time[[4]],"09:26:00  (UTC -05:00)")
  expect_equal(siteVisitReport$Sublocation[[1]],"")
  expect_equal(siteVisitReport$Sublocation[[2]],"")
  expect_equal(siteVisitReport$Sublocation[[3]],"")
  expect_equal(siteVisitReport$Sublocation[[4]],"")
  expect_equal(siteVisitReport$Reading[[1]],"4.58")
  expect_equal(siteVisitReport$Reading[[2]],"3.31")
  expect_equal(siteVisitReport$Reading[[3]],"9.20")
  expect_equal(siteVisitReport$Reading[[4]],"21.72")
  expect_equal(siteVisitReport$`Verification Comments`[[1]],"")
  expect_equal(siteVisitReport$`Verification Comments`[[2]],"")
  expect_equal(siteVisitReport$`Verification Comments`[[3]],"")
  expect_equal(siteVisitReport$`Verification Comments`[[4]],"Comment = CSG still submerged.<br/>GageInspectedCode = NTRD<br/>IntakeHoleConditionCode = UNSP<br/>VentHoleConditionCode = UNSP<br/>")
  expect_equal(siteVisitReport$Qualifier[[1]],"")
  expect_equal(siteVisitReport$Qualifier[[2]],"")
  expect_equal(siteVisitReport$Qualifier[[3]],"TQL")
  expect_equal(siteVisitReport$Qualifier[[4]],"EQP,EQP")
  expect_equal(siteVisitReport$`Difference from Peak Verification Reading`[[1]],"NA")
  expect_equal(siteVisitReport$`Difference from Peak Verification Reading`[[2]],"0.2 **")
  expect_equal(siteVisitReport$`Difference from Peak Verification Reading`[[3]],"-0.02")
  expect_equal(siteVisitReport$`Difference from Peak Verification Reading`[[4]],"0.03")
  
  data2 <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-empty-example.json', package = 'repgen'))
  siteVisitReport2 <- repgen:::sitevisitpeakTable(repgen:::readFieldVisitReadings(data2),repgen:::fetchReportMetadataField(data2,'excludeComments'))
  expect_equal(ncol(siteVisitReport2),15L)
  expect_equal(nrow(siteVisitReport2),14L)
  expect_true("Verification Comments" %in% colnames(siteVisitReport2))
  expect_equal(siteVisitReport2$`Verification Comments`[[13]],"Comment = Disabled randoms because now sending self-timed xmits as it should. Could not clear alarm condition so set it to alarm of >100.00 // Offset as found = 21.120. Removed probe at 1710 to install new backup (B) probe. Vented to atmospheric before reinstalled in well at 1825 =0.01. Used same steel cable but sensor moved on cable. New offset = 21.35 at 1830 // Sent random at 1844 and received.<br/>Comment = Held 9.40 - 1.07 = 8.33 at 0705 // Held 9.60 - 1.28 = 8.32 at 1835 with new sensors // All done with tape ME-LEF-ST1.<br/>")
  
  data3 <- fromJSON(system.file('extdata','sitevisitpeak','sitevisitpeak-exclude-comments-example.json', package = 'repgen'))
  siteVisitReport3 <- repgen:::sitevisitpeakTable(repgen:::readFieldVisitReadings(data3),repgen:::fetchReportMetadataField(data3,'excludeComments'))
  expect_equal(ncol(siteVisitReport3),14L)
  expect_equal(nrow(siteVisitReport3),4L)
  expect_false("Verification Comments" %in% colnames(siteVisitReport3))

})

setwd(dir = wd)
