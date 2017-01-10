context("utils-filter tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('do negative or zero values get removed from the data frame?', {
  df <- data.frame(value=c(-1, 0, 1, 2, 3, 5))
  df <- repgen:::removeZeroNegative(df)
  expect_false(df[1,1]<0)
  expect_false(df[2,1]==0)
})

setwd(dir = wd)
