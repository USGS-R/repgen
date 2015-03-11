# repgen
report generation in R


``R
url = 'https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/extremes/?station=06899500&dischargeIdentifier=Discharge.TS0058&stageIdentifier=Gage+height.TS0005&dailyDischargeIdentifier=Discharge.TS0058&waterYear=2012'
auth = 'Bearer aasdfa-asdfasdf-wdfe'
knit2pdf('vignettes/extremes.Rmd', output='../test.pdf')
knit2html('vignettes/extremes.Rmd', output='../test.html')
```