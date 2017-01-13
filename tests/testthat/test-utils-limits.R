context("test-utils-limits")
wd <- getwd()
setwd(dir = tempdir())



test_that("lines to callouts", {

  g = gsplot::callouts(gsplot::gsplot(),x=3, y=2, labels=c('EVEN LONGER HOLY CATS'), angle=150)
  j = repgen:::testCallouts(g,c(2,3))
  expect_equal(g$view.1.2$callouts$angle,j$view.1.2$callouts$angle)
  
  
  g = gsplot::callouts(gsplot::gsplot(), x=3, y=2, labels=c('EVEN LONGER HOLY CATS'), angle=250)
  j = repgen:::testCallouts(g,c(2,3))
  expect_false(g$view.1.2$callouts$angle==j$view.1.2$callouts$angle)
  
})


setwd(dir = wd)
