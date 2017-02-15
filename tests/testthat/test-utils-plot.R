context("utils-plot tests")

wd <- getwd()
setwd(dir = tempdir())

test_that('formatMinMaxLabel properly formats the min and max labels to be placed above the plot', {
    ml <- list(
        time = repgen:::flexibleTimeParse("2013-11-12T22:45:00-05:00", "Etc/GMT+5"),
        value = 1,
        legend.name = "Max. Instantaneous Discharge : 1"
    )
    units <- "ft^3/s"

    label <- repgen:::formatMinMaxLabel(ml, units)

    expect_is(label, 'character')
    expect_equal(label, 'Max. Instantaneous Discharge : 1ft^3/s Nov 12, 2013 22:45:00 (UTC -05:00)')
})

test_that('formatMinMaxLabel doesnt error for NULL data', {
    ml <- NULL
    units <- NULL

    label <- repgen:::formatMinMaxLabel(ml, units)
})

test_that('XAxisLabelStyle properly creates X Axis labels', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate1 <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)
    endDate2 <- repgen:::flexibleTimeParse("2014-11-12T23:59:00-05:00", timezone)
    plotDates1 <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )
    plotDates2 <- c(
        as.Date("2013-11-10 GMT+5"), as.Date("2013-11-17 GMT+5"), as.Date("2013-11-24 GMT+5"),
        as.Date("2013-12-01 GMT+5"), as.Date("2013-12-08 GMT+5"), as.Date("2013-12-15 GMT+5"),
        as.Date("2013-12-22 GMT+5"), as.Date("2013-12-29 GMT+5"), as.Date("2014-01-05 GMT+5"),
        as.Date("2014-01-12 GMT+5"), as.Date("2014-01-19 GMT+5"), as.Date("2014-01-26 GMT+5"),
        as.Date("2014-02-02 GMT+5"), as.Date("2014-02-09 GMT+5"), as.Date("2014-02-16 GMT+5"),
        as.Date("2014-02-23 GMT+5"), as.Date("2014-03-02 GMT+5"), as.Date("2014-03-09 GMT+5"),
        as.Date("2014-03-16 GMT+5"), as.Date("2014-03-23 GMT+5"), as.Date("2014-03-30 GMT+5"),
        as.Date("2014-04-06 GMT+5"), as.Date("2014-04-13 GMT+5"), as.Date("2014-04-20 GMT+5"),
        as.Date("2014-04-27 GMT+5"), as.Date("2014-05-04 GMT+5"), as.Date("2014-05-11 GMT+5"),
        as.Date("2014-05-18 GMT+5"), as.Date("2014-05-25 GMT+5"), as.Date("2014-06-01 GMT+5"),
        as.Date("2014-06-08 GMT+5"), as.Date("2014-06-15 GMT+5"), as.Date("2014-06-22 GMT+5"),
        as.Date("2014-06-29 GMT+5"), as.Date("2014-07-06 GMT+5"), as.Date("2014-07-13 GMT+5"),
        as.Date("2014-07-20 GMT+5"), as.Date("2014-07-27 GMT+5"), as.Date("2014-08-03 GMT+5"),
        as.Date("2014-08-10 GMT+5"), as.Date("2014-08-17 GMT+5"), as.Date("2014-08-24 GMT+5"),
        as.Date("2014-08-31 GMT+5"), as.Date("2014-09-07 GMT+5"), as.Date("2014-09-14 GMT+5"),
        as.Date("2014-09-21 GMT+5"), as.Date("2014-09-28 GMT+5"), as.Date("2014-10-05 GMT+5"),
        as.Date("2014-10-12 GMT+5"), as.Date("2014-10-19 GMT+5"), as.Date("2014-10-26 GMT+5"),
        as.Date("2014-11-02 GMT+5"), as.Date("2014-11-09 GMT+5")
    )
    plot_object1 <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates1, labels = format(plotDates1, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate1))

    plot_object2 <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates2, labels = format(plotDates2, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate2))

    newPlot1 <- repgen:::XAxisLabelStyle(plot_object1, startDate, endDate1, timezone, plotDates1)
    newPlot2 <- repgen:::XAxisLabelStyle(plot_object2, startDate, endDate2, timezone, plotDates2)

    mtext1 <- gsplot:::views(newPlot1)[[1]][which(grepl("mtext", names(gsplot:::views(newPlot1)[[1]])))]
    mtext2 <- gsplot:::views(newPlot2)[[1]][which(grepl("mtext", names(gsplot:::views(newPlot2)[[1]])))]

    expect_equal(length(mtext1), 0)
    expect_equal(length(mtext2), 2)
})

test_that('Calculate Limits properly calculates the limits of the provided data', {
    timezone <- "Etc/GMT+5"

    pts1 <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )

    pts2 <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone),
                 repgen:::flexibleTimeParse("2014-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20, 11)
    )

    lims1 <- repgen:::calculateLims(pts1)
    lims2 <- repgen:::calculateLims(pts2)

    expect_is(lims1, 'list')
    expect_is(lims2, 'list')
    expect_equal(lims1$xlim[[1]], pts1[['time']][[1]])
    expect_equal(lims1$xlim[[2]], pts1[['time']][[2]])
    expect_equal(lims1$ylim[[1]], 10)
    expect_equal(lims1$ylim[[2]], 20)    
    
    expect_equal(lims2$xlim[[1]], pts2[['time']][[1]])
    expect_equal(lims2$xlim[[2]], pts2[['time']][[3]])
    expect_equal(lims2$ylim[[1]], 10)
    expect_equal(lims2$ylim[[2]], 20)
})

test_that('plotItem properly adds an item to a GSPlot object with the proper styles', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)

    plotDates <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )

    pts <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-11T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )

    plot_object <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate))

    plot_object <- repgen:::plotItem(plot_object, pts, repgen:::getDVHydrographPlotConfig, list(pts, 'groundWaterLevels'), isDV=TRUE)
  
    expect_equal(length(plot_object$view.1.2$points$x), 2)
    expect_equal(length(plot_object$view.1.2$points$y), 2)
})

test_that('plotTimeSeries properly adds a time series to a GSPlot object with the proper styles', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)

    plotDates <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )

    pts <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-11T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )

    timeSeries <- list(
        points = pts
    )

    plot_object <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate))

    plot_object <- repgen:::plotTimeSeries(plot_object, timeSeries, 'stat1TimeSeries', timezone, repgen:::getDVHydrographPlotConfig, list(ylabel=""), isDV=TRUE)
  
    expect_equal(length(plot_object$view.1.2$lines$x), 3)
    expect_equal(length(plot_object$view.1.2$lines$y), 3)
})

test_that('formatSplitTimeSeriesForPlotting properly formats a split time series for plotting', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)
    plotDates <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )
    pts <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-11T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )
    timeSeries <- list(
        points = pts
    )
    tsList <- list(
        timeSeries = timeSeries
    )

    formatted <- repgen:::formatSplitTimeSeriesForPlotting(tsList)

    expect_is(formatted, 'list')
    expect_equal(length(formatted$timeSeries), 2)
    expect_equal(length(formatted$timeSeries$value), 2)
    expect_equal(length(formatted$timeSeries$time), 2)
    expect_equal(formatted$timeSeries$value[[1]], 10)
    expect_equal(formatted$timeSeries$value[[2]], 20)
    expect_equal(formatted$timeSeries$time[[1]], pts[[1]][[1]])
    expect_equal(formatted$timeSeries$time[[2]], pts[[1]][[2]])
})

test_that('formatSplitTimeSeriesForPlotting doesnt error with NULL time series', {
    tsList <- list()
    nullList <- NULL

    formatted1 <- repgen:::formatSplitTimeSeriesForPlotting(tsList)
    formatted2 <- repgen:::formatSplitTimeSeriesForPlotting(nullList)

    expect_equal(formatted1, NULL)
    expect_equal(formatted2, NULL)
})

test_that('formatTimeSeriesForPlotting properly formats a time series for plotting', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)
    plotDates <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )
    pts <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-11T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )
    timeSeries <- list(
        points = pts
    )

    formatted <- repgen:::formatTimeSeriesForPlotting(timeSeries)

    expect_is(formatted, 'data.frame')
    expect_equal(nrow(formatted), 2)
    expect_equal(formatted[[1]][[1]], pts[[1]][[1]])
    expect_equal(formatted[[1]][[2]], pts[[1]][[2]])
    expect_equal(formatted[[2]][[1]], 10)
    expect_equal(formatted[[2]][[2]], 20)
})

test_that('formatTimeSeriesForPlotting doesnt error with NULL time series', {
    formatted <- repgen:::formatTimeSeriesForPlotting(NULL)
    expect_equal(formatted, NULL)
})

test_that('delineateYearBoundaries properly creates lines at year boundaries', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-12-21T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2014-01-07T23:59:00-05:00", timezone)

    plotDates <- c(
         as.Date("2013-12-22T00:00:00-05:00"),
         as.Date("2013-12-29T00:00:00-05:00"),
         as.Date("2014-01-05T00:00:00-05:00")
    )

    years <- c(as.Date("2014-01-01T00:00:00-05:00"))

    plot_object <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate))

    plot_object <- repgen:::DelineateYearBoundaries(plot_object, years)

    abline <- gsplot:::views(plot_object)[[1]][which(grepl("abline", names(gsplot:::views(plot_object)[[1]])))]

    expect_equal(length(abline), 1)
})

test_that('XAxisLabels adds XAxis Labels to a GSPlot object', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-12-21T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2014-01-07T23:59:00-05:00", timezone)

    plotDates <- c(
         as.Date("2013-12-22T00:00:00-05:00"),
         as.Date("2013-12-29T00:00:00-05:00"),
         as.Date("2014-01-05T00:00:00-05:00")
    )

    months <- c(as.Date("2014-01-01T00:00:00-05:00"))
    text <- c('J')
    years <- c(as.Date("2014-01-01T00:00:00-05:00"))

    plot_object <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate))

    plot_object <- repgen:::XAxisLabels(plot_object, text, months, years)

    mtext <- gsplot:::views(plot_object)[[1]][which(grepl("mtext", names(gsplot:::views(plot_object)[[1]])))]

    expect_equal(length(mtext), 2)
    expect_equal(mtext[[1]][['text']], 'J')
    expect_equal(mtext[[2]][['text']], 2014)
})

test_that('log_tick_marks properly creates logarithmically spaced tick marks between a min and max', {
    min <- 1
    max <- 1000

    logTicks <- repgen:::log_tick_marks(min, max)

    expect_equal(length(logTicks), 12)
    expect_equal(logTicks[[1]], 1)
    expect_equal(logTicks[[2]], 2)
    expect_equal(logTicks[[3]], 5)
    expect_equal(logTicks[[5]], 20)
    expect_equal(logTicks[[7]], 100)
    expect_equal(logTicks[[9]], 500)
    expect_equal(logTicks[[11]], 2000)
    expect_equal(logTicks[[12]], 5000)
})

test_that('extendYaxisLimits properly extends the limits of the y-Axis', {
    timezone <- "Etc/GMT+5"
    startDate <- repgen:::flexibleTimeParse("2013-11-10T12:00:00-05:00", timezone)
    endDate <- repgen:::flexibleTimeParse("2013-12-02T23:59:00-05:00", timezone)

    plotDates <- c(
         as.Date("2013-11-10T00:00:00-05:00"),
         as.Date("2013-11-17T00:00:00-05:00"),
         as.Date("2013-11-24T00:00:00-05:00"),
         as.Date("2013-12-01T00:00:00-05:00")
    )

    pts <- data.frame(
        time = c(repgen:::flexibleTimeParse("2013-11-11T12:00:00-05:00", timezone), 
                 repgen:::flexibleTimeParse("2013-11-12T23:59:00-05:00", timezone)),
        value = c(10, 20)
    )

    timeSeries <- list(
        points = pts
    )

    plot_object <- gsplot(ylog = FALSE, yaxs = 'i') %>%
      grid(nx = 0, ny = NULL, equilogs = FALSE, lty = 3, col = "gray") %>%
      axis(1, at = plotDates, labels = format(plotDates, "%b\n%d"), padj = 0.5) %>%
      axis(2, reverse = FALSE, las=0) %>%
      view(xlim = c(startDate, endDate))

    plot_object <- repgen:::plotTimeSeries(plot_object, timeSeries, 'stat1TimeSeries', timezone, repgen:::getDVHydrographPlotConfig, list(ylabel=""), isDV=TRUE)

    error_bar_args <- list(
        side=2,
        y = 15,
        y.low = 6,
        y.high = 2
    )

    oldLims <- plot_object$side.2$lim

    err_lims <- repgen:::getErrorBarYLims(error_bar_args)
    plot_object <- repgen:::extendYaxisLimits(plot_object, err_lims[['limits']], err_lims[['side']])
    
    newLims <- plot_object$side.2$lim

    expect_equal(oldLims, c(10,20))
    expect_equal(newLims, c(9, 20))

})

test_that('printWithThirdYAxis properly prints a UV Hydrograph with a third Y-Axis', {
    data <- fromJSON(system.file('extdata','testsnippets','test-utils-plot.json', package = 'repgen'))
    expect_is(uvhydrograph(data[['thirdYAxis']], 'Author Name'), 'character')
})

setwd(dir = wd)
