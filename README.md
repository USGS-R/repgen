# repgen
report generation in R


```R
library(repgen)
token = authenticateUser('arathusfreon')
url = 'https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/extremes/?station=06899500&dischargeIdentifier=Discharge.TS0058&stageIdentifier=Gage+height.TS0005&dailyDischargeIdentifier=Discharge.TS0058&waterYear=2012'
extremes(url, 'html', token)
 # or pdf:
extremes(url, 'latex', token)


data('15052500')
vdiagram(data = site, 'html')
```