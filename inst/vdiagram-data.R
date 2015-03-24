Metadata <- list("gageId"="15052500",
                 "gageName"="OLD TOM C NR KASAAN AK",
                 "period"=as.Date(c("2013-09-30", "2014-09-30")),
                 "ratingId"=10)


BaseRatingTable <- data.frame(stage=c(1.44, 1.46, 1.5, 1.6, 1.9, 2.3, 2.8, 3.68, 7.2), 
                              discharge=c(0.5, 1, 1.92, 4.76, 20.9, 60, 135, 328, 1670))

Ratings <- data.frame("ratingId"=rep(10, 6),
                      "datetime"=as.POSIXct(c("2013-10-29 12:37:00",
                                              "2013-11-23 04:00:00",
                                              "2014-01-14 13:15:00",
                                              "2014-06-05 06:14:00",
                                              "2014-06-22 02:30:00",
                                              "2014-08-15 10:07:00")),
                      "curveId"=c(rep(1L, 2), rep(2L, 2), rep(3L, 2)))

Curves <- list(list("x"=c(-0.04, -0.04, 0), "y"=c(1.74, 1.92, 2.7)), 
               list("x"=c(-0.07, -0.05, 0), "y"=c(1.73, 2.78, 3.6)),
               list("x"=c(-0.09, -0.05, 0), "y"=c(1.9, 2.78, 3.6)))

HistoricalFieldVisits <- data.frame(id=c(), stage=c(), discharge=c(), quality=c())
MeasuredFieldVisits <- data.frame(id=c(1,2,3,4,5), stage=c(1.8, 2.78, 1.96, 1.73, 1.9), discharge=c(7.82, 99, 16.05, 4.45, 10.2), quality=c("fair", "fair", "fair", "fair", "fair"))

shifts <- calcShifts(MeasuredFieldVisits, BaseRatingTable)

Historical <- data.frame(x=c(), y=c(), xlb=c(), xub=c())
Measured <- data.frame(x=shifts$shift, y=shifts$stage, xlb=shifts$lb, xub=shifts$ub)

xlim <- c(-.54, 0.58)
ylim <- c(1.38, 4.62)

site <- list('Metadata' = Metadata,
             'BaseRatingTable' = BaseRatingTable,
             'Ratings' = Ratings,
             'Curves' = Curves,
             'HistoricalFieldVisits' = HistoricalFieldVisits,
             'MeasuredFieldVisits' = MeasuredFieldVisits,
             'shifts' = shifts,
             'Historical' = Historical,
             'Measured' = Measured, 
             'xlim' = xlim,
             'ylim' = ylim)

save("site",file = paste0('data/',site$Metadata$gageId,'.RData'))


Metadata <- list("gageId"="8675309",
                 "gageName"="OLD TOM C NR KASAAN AK",
                 "period"=as.Date(c("2013-09-30", "2014-09-30")),
                 "ratingId"=10)


BaseRatingTable <- data.frame(stage=c(1.44, 1.46, 1.5, 1.6, 1.9, 2.3, 2.8, 3.68, 7.2), 
                              discharge=c(0.5, 1, 1.92, 4.76, 20.9, 60, 135, 328, 1670))*runif(9, min = 1.1, max = 1.2)

Ratings <- data.frame("ratingId"=rep(10, 6),
                      "datetime"=as.POSIXct(c("2013-10-29 12:37:00",
                                              "2013-11-23 04:00:00",
                                              "2014-01-14 13:15:00",
                                              "2014-06-05 06:14:00",
                                              "2014-06-22 02:30:00",
                                              "2014-08-15 10:07:00")),
                      "curveId"=c(rep(1L, 2), rep(2L, 2), rep(3L, 2)))

Curves <- list(list("x"=c(-0.05, -0.05, 0), "y"=c(1.74, 1.92, 4.7)), 
               list("x"=c(0.27, 0.25, 0), "y"=c(1.74, 2.78, 4.7)),
               list("x"=c(-0.078, -0.078, 0), "y"=c(1.74, 2.78, 4.7)),
               list("x"=c(-0.2, -0.2, 0), "y"=c(1.74, 2.78, 4.7)))

HistoricalFieldVisits <- data.frame(id=c(), stage=c(), discharge=c(), quality=c())
MeasuredFieldVisits <- data.frame(id=c(1,2,3,4,5), stage=c(1.8, 2.78, 1.96, 1.73, 1.9), discharge=c(7.82, 99, 16.05, 4.45, 10.2), quality=c("fair", "fair", "fair", "fair", "fair"))

shifts <- calcShifts(MeasuredFieldVisits, BaseRatingTable)

Historical <- data.frame(x=c(), y=c(), xlb=c(), xub=c())
Measured <- data.frame(x=shifts$shift, y=shifts$stage, xlb=shifts$lb, xub=shifts$ub)

xlim <- c(-.54, 0.58)
ylim <- c(1.49, 5.52)

site <- list('Metadata' = Metadata,
             'BaseRatingTable' = BaseRatingTable,
             'Ratings' = Ratings,
             'Curves' = Curves,
             'HistoricalFieldVisits' = HistoricalFieldVisits,
             'MeasuredFieldVisits' = MeasuredFieldVisits,
             'shifts' = shifts,
             'Historical' = Historical,
             'Measured' = Measured, 
             'xlim' = xlim,
             'ylim' = ylim)

save("site",file = paste0('data/',site$Metadata$gageId,'.RData'))
