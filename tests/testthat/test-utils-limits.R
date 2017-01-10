context("test-utils-limits")
wd <- getwd()
setwd(dir = tempdir())



test_that("lines to callouts", {
#  g = gsplot::callouts(gsplot::gsplot(),c(0,3), NULL, labels=c('REALLY REALLY LONG','EVEN LONGER HOLY CATS'))
#  expect_warning(print(g))
 
  
  g = gsplot::callouts(gsplot::gsplot(), c(0,3), y=1:2, labels=c('EVEN LONGER HOLY CATS'), angle=250)
#  callouts(gsplot(), c(0,3), y=1:2, labels=c('dogs','cats'), angle='auto')
  repgen:::testCallouts(g,c(0,1))
  
  #####COMPARE TWO PLOT OBJECTS AND THEIR CALLOUT OBJECTS TO MAKE SURE THAT TEST CALLOUTS MOVES EM. #######
})


setwd(dir = wd)
