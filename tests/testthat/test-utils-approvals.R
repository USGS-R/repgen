context("utils-approvals tests")

library(gsplot)

logAxis <- FALSE

testSeries <- list(
  points=data.frame(
    time=c(as.POSIXct("2016-05-02 17:00:00"), as.POSIXct("2016-05-03 17:00:00"), as.POSIXct("2016-05-23 17:45:00")), 
    value=c(-1, 10, 20),
    month=c("1605", "1605", "1605"),
    stringsAsFactors=FALSE)
)

plot_object <- gsplot(ylog=logAxis) %>% 
  view(xlim = c(as.POSIXct("2016-05-01 00:00:00"), as.POSIXct("2016-05-31 23:59:59")),
       ylim = c(0,30)) %>% 
  lines(testSeries$time, testSeries$value, reverse = invertedFlag)

approvalBars <- list(
  appr_working_uv=list(x0=as.POSIXct("2016-05-01 00:00:00"), x1=as.POSIXct("2016-05-06 00:00:00"), 
                       legend.name="Working Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
  appr_inreview_uv=list(x0=as.POSIXct("2016-05-06 00:00:00"), x1=as.POSIXct("2016-05-20 00:00:00"), 
                        legend.name="In Review Test Series", time=as.POSIXct("2016-05-01 00:00:00")),
  appr_approved_uv=list(x0=as.POSIXct("2016-05-20 00:00:00"), x1=as.POSIXct("2016-06-30 00:00:00"), 
                        legend.name="Approved Test Series", time=as.POSIXct("2016-05-01 00:00:00"))
)

context("getApprovalBarConfig is working")

test_that("no error when approval bars are empty", {
  appr_configs <- repgen:::getApprovalBarConfig(list(), ylim(plot_object,side=2), logAxis)
  expect_true(length(appr_configs) == 0)
})


test_that("expected configs are returned", {
  appr_configs <- repgen:::getApprovalBarConfig(approvalBars[1], ylim(plot_object,side=2), logAxis)

  expected_fields <- list(xleft=as.POSIXct(character(), tz="UTC"), xright=as.POSIXct(character(), tz="UTC"),
                          ybottom=numeric(), ytop=numeric(), legend.name=character(), 
                          where=character(), col=character(), border=character())
  
  expect_true(length(setdiff(lapply(expected_fields, class), 
                             lapply(appr_configs[['rect']], class))) == 0)
  expect_true(all(names(expected_fields) %in% names(appr_configs[['rect']])))
})

test_that("approval config returned for one or more approval bars", {
  appr_configs <- repgen:::getApprovalBarConfig(approvalBars[1], ylim(plot_object,side=2), logAxis)
  expect_equal(length(appr_configs), 1)
  expect_equal(names(appr_configs), "rect")
  
  appr_configs2 <- repgen:::getApprovalBarConfig(approvalBars, ylim(plot_object,side=2), logAxis)
  expect_equal(length(appr_configs2), 3)
  expect_true(all(names(appr_configs2) == "rect"))
  
  expect_true(all(lapply(appr_configs2, '[[', 'ybottom') == -1.2))
  expect_true(all(lapply(appr_configs2, '[[', 'ytop') == -0.735))
})

test_that("ylim upper and lower works even when ylim[1] == ylim[2]", {
  appr_configs <- repgen:::getApprovalBarConfig(approvalBars, c(1,1), logAxis)
  expect_true(all(lapply(appr_configs, '[[', 'ybottom') == 0.568))
  expect_true(all(lapply(appr_configs, '[[', 'ytop') == -0.58))
})

context("getApprovalBarStyles works")

test_that("all three approval styles are returned", {
  styles <- repgen:::getApprovalBarStyles()
  expect_true(all(names(styles) %in% c("appr_approved_uv", "appr_inreview_uv", "appr_working_uv")))
})

context("approval bar y values calculated correctly")

test_that("approvalBarY works with empty ylog", {
  expect_true(repgen:::approvalBarY(c(1,10), ratio=0.2) == -0.8)
  expect_true(repgen:::approvalBarY(lims, ylog=TRUE, ratio=ratio) == 0.6309573)
})

test_that("approvalBarYTop and approvalBarYBottom both work", {
  lims <- c(1,10)
  ylog <- FALSE
  reverse <- FALSE
  expect_equal(repgen:::approvalBarYTop(lims, ylog),
               repgen:::approvalBarY(lims, ylog, ratio=0.0245))
  expect_equal(repgen:::approvalBarYBottom(lims, ylog),
               repgen:::approvalBarY(lims, ylog, ratio=0.04))
})

