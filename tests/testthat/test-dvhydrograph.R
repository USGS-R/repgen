context("dvhydrograph tests")

wd <- getwd()
setwd(dir = tempdir())

context("testing dvhydrograph")
test_that("dvhydrograph examples work",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  
  data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-example.json', package = 'repgen'))
  expect_is(dvhydrograph(data,'html', 'Author Name'), 'character')
  
  data2 <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-waterlevel-example.json', package = 'repgen'))
  expect_is(dvhydrograph(data2,'html', 'Author Name'), 'character')
  
  data3 <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-aqcu744_newData.json', package = 'repgen'))
  expect_is(dvhydrograph(data3,'html', 'Author Name'), 'character')
  
})

test_that("dvhydrograph axes flip",{
  library(jsonlite)
  library(gsplot)
  library(lubridate)
  data <- fromJSON(system.file('extdata','dvhydrograph','dvhydro-waterlevel-example.json', package = 'repgen'))
  expect_true(data$reportMetadata$isInverted)
  
  plot1 <- createDvhydrographPlot(data)
  ylims1 <- ylim(plot1)[[1]]
  expect_true(ylims1[1] > ylims1[2])

  plot2 <- createRefPlot(data, "secondary")
  # if secondary reference time series is present...
  if (!is.null(plot2)) {
    # ...check reference plot
    ylims2 <- ylim(plot2)[[1]]
    expect_true(ylims2[1] > ylims2[2])
  }
  
  plot3 <- createRefPlot(data, "tertiary")
  if (!is.null(plot3)) {
    ylims3 <- ylim(plot3)[[1]]
    expect_true(ylims3[1] > ylims3[2])
  }

  plot4 <- createRefPlot(data, "quaternary")
  if (!is.null(plot4)) {
    ylims4 <- ylim(plot4)[[1]]
    expect_true(ylims4[1] > ylims4[2])
  }
})

setwd(dir = wd)
