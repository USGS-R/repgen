context("utils-validation tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('isEmpty returns true if value input is both null and na (test with null)',{
  val1 = NULL
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na)',{
  val1 = NA
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na and null)',{
  val1 <- c(NULL,NA)
  test_result <- isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with non-null and non-na)',{
  val1 <- c(1:3,6:4)
  test_result <- isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with null)',{
  val1 = NULL
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is  null and empty string and na (test with empty string)',{
  val1 = ""
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with na)',{
  val1 = NA
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and null)',{
  val1 <- c(NULL,"")
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and na)',{
  val1 <- c(NA,"")
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with NULL and na)',{
  val1 <- c(NA,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with empty string)',{
  val1 <- c(1:3,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and null)',{
  val1 <- c(1:3,NA,NULL)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and empty string)',{
  val1 <- c(1:3,NA,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null and empty string)',{
  val1 <- c(1:3,NULL,"")
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with non-null and non-empty string and non-na)',{
  val1 <- c(1:3,6:4,8:2)
  test_result <- isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value exists in the environment or false if it does not',{
  a <- 1
  c <- ""
  d <- NULL
  e <- NA
  expect_false(isEmptyOrBlank(listObjects = ls(), objectName = "a")) # should be FALSE
  expect_true(isEmptyOrBlank(listObjects = ls(), objectName = "b")) # should be TRUE
  expect_true(isEmptyOrBlank(val = c)) # should be TRUE
  expect_true(isEmptyOrBlank(val = d)) # should be TRUE
  expect_true(isEmptyOrBlank(val = e)) # should be TRUE
  expect_false(isEmptyOrBlank(val = a)) # should be FALSE
})

setwd(dir = wd)