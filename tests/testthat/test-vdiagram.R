context("vdiagram tests")
wd <- getwd()
setwd(dir = tempdir())

context("testing vdiagram when some fields are missing and are complete")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-example.json",package = 'repgen'))
  expect_is(vdiagram(data), 'character')
})

# This test triggers the "maxStage" error found in AQCU-680
context("testing vdiagram 5yr data")
test_that("vdiagram examples work", {
 library(jsonlite)
 library(gsplot)
 data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-5yr-example.json",package = 'repgen'))
 expect_is(vdiagram(data), 'character')
})

context("testing vdiagram when gage heights are blank")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-no_gage_heights.json",package = 'repgen'))
  expect_is(vdiagram(data), 'character')
})

context("testing vdiagram when gage heights are blank for historic years")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-no_gage_heights_historic.json",package = 'repgen'))
  expect_is(vdiagram(data), 'character')
})

context("testing vdiagram when there are excluded control conditions")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-excluded-conditions.json",package = 'repgen'))
  expect_is(vdiagram(data), 'character')
})

context("testing vdiagram does not error with empty data")
test_that("vdiagram examples work", {
  library(jsonlite)
  library(gsplot)
  data <- fromJSON('{
    "reportMetadata":{
      "startDate": "2016-01-01T00:00:00",
      "endDate": "2916-01-01T00:00:00"
    }
  }')
  expect_is(vdiagram(data), 'character')
})

context("testing historyMeasurementsLabel when prior years historic is 0, null, positive, or negative.")
test_that("historyMeasurementsLabel for Vdiagram works", {
  library(jsonlite)
  data <- fromJSON(system.file('extdata','vdiagram',"vdiagram-no_gage_heights_historic.json",package = 'repgen'))
  
  #Expect there to be a label when there is historic data.
  expect_equal(repgen:::historyMeasurementsLabel(data),"Unlabeled blue points are historical measurements from the last 2 year(s).\n")
  
  #Expect there to not be a label when there isn't historic data, when it's zero, or when it's neg.
  data$reportMetadata$requestParameters$priorYearsHistoric = 0
  expect_equal(repgen:::historyMeasurementsLabel(data),NULL)
  
  data$reportMetadata$requestParameters$priorYearsHistoric = NULL
  expect_equal(repgen:::historyMeasurementsLabel(data),NULL)
  
  data$reportMetadata$requestParameters$priorYearsHistoric = -12
  expect_equal(repgen:::historyMeasurementsLabel(data),NULL)
})

context("Testing defaultHistFlags if it's empty/blank, false, if it's a string, or true.")
test_that("Testing defaultHistFlags works as expected", {
  expect_equal(repgen:::defaultHistFlags(TRUE),TRUE)
  expect_equal(repgen:::defaultHistFlags(FALSE),FALSE)
  expect_equal(repgen:::defaultHistFlags(""),FALSE)
  expect_equal(repgen:::defaultHistFlags(" "),FALSE)
  expect_equal(repgen:::defaultHistFlags(NULL),FALSE)
  expect_equal(repgen:::defaultHistFlags("I'm a string, look at me."),"I'm a string, look at me.")
  expect_equal(repgen:::defaultHistFlags("q"),"q")
  
  ### Need to pass in a parameter.
  expect_error(repgen:::defaultHistFlags())
})

context("Testing controlCondition filtering")
test_that('createControlConditionString properly constructs a comma-separated string of control conditions', {
  controlConditionJSON <- fromJSON('{
     "reportMetadata": {
        "requestParameters": {
          "excludeConditions": [
            "Clear",
            "VegetationLight",
            "VegetationModerate"
      ]
        }
}
  }')
  
  conditions <- repgen:::parseExcludedControlConditions(controlConditionJSON)
  string <- repgen:::createControlConditionsString(conditions)
  string2 <- repgen:::createControlConditionsString(NULL)
  
  expect_is(string, 'character')
  expect_is(string2, 'character')
  expect_equal(string2, '')
  expect_equal(string, 'Clear, Vegetation Light, Vegetation Moderate')
})

test_that('excludedConditionsMessage properly builds the control condition exclusion message', {
  controlConditionJSON <- fromJSON('{
     "reportMetadata": {
        "requestParameters": {
          "excludeConditions": [
            "Clear",
            "VegetationLight",
            "VegetationModerate"
      ]
        }
}
  }')
  
  string <- repgen:::excludedConditionsMessage(controlConditionJSON)
  string2 <- repgen:::createControlConditionsString(NULL)
  
  expect_is(string, 'character')
  expect_is(string2, 'character')
  expect_equal(string2, '')
  expect_equal(string, '***Measurements with the following control conditions are excluded:&nbsp;*** Clear, Vegetation Light, Vegetation Moderate')
})

## AFTER Laura tells us what parameters are necessary, write tests for parseVDiagramData using
## different parameter combinations.

setwd(dir = wd)

