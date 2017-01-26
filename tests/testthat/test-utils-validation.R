context("utils-validation tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('isEmpty returns true if value input is both null and na (test with null)',{
  val1 = NULL
  test_result <- repgen:::isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na)',{
  val1 = NA
  test_result <- repgen:::isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns true if value input is both null and na (test with na and null)',{
  val1 <- c(NULL,NA)
  test_result <- repgen:::isEmpty(val1)
  expect_true(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- repgen:::isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- repgen:::isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmpty returns false if value input is not both null and na (test with non-null and non-na)',{
  val1 <- c(1:3,6:4)
  test_result <- repgen:::isEmpty(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with null)',{
  val1 = NULL
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is  null and empty string and na (test with empty string)',{
  val1 = ""
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with na)',{
  val1 = NA
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and null)',{
  val1 <- c(NULL,"")
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with empty string and na)',{
  val1 <- c(NA,"")
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns true if value input is null and empty string and na (test with NULL and na)',{
  val1 <- c(NA,NULL)
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_true(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with empty string)',{
  val1 <- c(1:3,"")
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null)',{
  val1 <- c(1:3,NULL)
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na)',{
  val1 <- c(1:3,NA)
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and null)',{
  val1 <- c(1:3,NA,NULL)
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with na and empty string)',{
  val1 <- c(1:3,NA,"")
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with null and empty string)',{
  val1 <- c(1:3,NULL,"")
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns false if value input is not all null and empty string and na (test with non-null and non-empty string and non-na)',{
  val1 <- c(1:3,6:4,8:2)
  test_result <- repgen:::isEmptyOrBlank(val1)
  expect_false(test_result)
})

test_that('isEmptyOrBlank returns true if value exists in the environment or false if it does not',{
  a <- 1
  c <- ""
  d <- NULL
  e <- NA
  expect_false(repgen:::isEmptyOrBlank(listObjects = ls(), objectName = "a")) # should be FALSE
  expect_true(repgen:::isEmptyOrBlank(listObjects = ls(), objectName = "b")) # should be TRUE
  expect_true(repgen:::isEmptyOrBlank(val = c)) # should be TRUE
  expect_true(repgen:::isEmptyOrBlank(val = d)) # should be TRUE
  expect_true(repgen:::isEmptyOrBlank(val = e)) # should be TRUE
  expect_false(repgen:::isEmptyOrBlank(val = a)) # should be FALSE
})

test_that('validParam returns a string value if it is not NULL', {
  val1 <- "xyz"
  expect_equal(repgen:::validParam(val1, "testParam"), "xyz")
})

test_that('validParam returns a numeric value if it is not NULL', {
  val1 <- 23141
  expect_equal(repgen:::validParam(val1, "testParam"), 23141)
})

test_that('validParam returns an error if NULL and required', {
  val1 <- NULL
  expect_error(repgen:::validParam(val1, "testParam", TRUE), 'required value testParam missing.')
})

test_that('validParam returns as.numeric(NA) if NULL and not required and as.numeric', {
  val1 <- NULL
  expect_equal(repgen:::validParam(val1, "testParam", FALSE, TRUE), as.numeric(NA))
})

test_that('validParam returns empty string if NULL and not required and not as.numeric', {
  val1 <- NULL
  expect_equal(repgen:::validParam(val1, "testParam", FALSE, FALSE), "")
})

test_that('checkRequiredFields properly checks fields', {
  library(jsonlite)

  testJSON <- fromJSON('{
    "req1": "data",
    "req2": "",
    "opt1": "data",
    "opt2": ""
  }')

  testArray <- fromJSON('{
    "array": [
      {
        "req1": "1",
        "req2": "",
        "req3": "3",
        "opt1": "1",
        "opt2": ""
      },
      {
        "req1": "1",
        "req2": "",
        "opt1": "1"
      }
    ]
  }')

  requiredFields1 <- c("req1", "req2")
  requiredFields2 <- c("req1", "req2", "req3")

  valid1 <- repgen:::checkRequiredFields(testJSON, requiredFields1)
  valid2 <- repgen:::checkRequiredFields(testArray[['array']], requiredFields1)
  invalid1 <- repgen:::checkRequiredFields(testJSON, requiredFields2)
  invalid2 <- repgen:::checkRequiredFields(testArray[['array']], requiredFields2)

  expect_is(valid1, 'NULL')
  expect_is(valid2, 'NULL')
  expect_is(invalid1, 'character')
  expect_is(invalid2, 'character')

  expect_equal(valid1, NULL)
  expect_equal(valid2, NULL)
  expect_equal(invalid1, c("req3"))
  expect_equal(invalid2, c("req3"))
})

test_that('validateFetchedData properly validates data', {
  testJSON <- fromJSON('{
    "object":{
      "req1": "data",
      "req2": "",
      "opt1": "data",
      "opt2": ""
    },
    "empty": {},
    "array": [
      {
        "req1": "1",
        "req2": "",
        "req3": "3",
        "opt1": "1",
        "opt2": ""
      },
      {
        "req1": "1",
        "req2": "",
        "opt1": "1"
      }
    ]
  }')

  requiredFields1 <- c("req1", "req2")
  requiredFields2 <- c("req1", "req2", "req3")

  valid1 <- repgen:::validateFetchedData(testJSON[['object']], "object", requiredFields1)
  valid2 <- repgen:::validateFetchedData(testJSON[['array']], "array", requiredFields1)

  expect_warning(expect_false(repgen:::validateFetchedData(testJSON[['missing']], "missing", requiredFields1, stopNull=FALSE)))
  expect_error(repgen:::validateFetchedData(testJSON[['missing']], "missing", requiredFields1))

  expect_warning(expect_false(repgen:::validateFetchedData(testJSON[['array']], "array", requiredFields2, stopMissing=FALSE)))
  expect_error(repgen:::validateFetchedData(testJSON[['array']], "array", requiredFields2))

  expect_warning(expect_false(repgen:::validateFetchedData(testJSON[['empty']], "empty", requiredFields1, stopEmpty=FALSE)))
  expect_error(repgen:::validateFetchedData(testJSON[['empty']], "empty", requiredFields1))
})

setwd(dir = wd)