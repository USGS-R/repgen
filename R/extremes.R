#'@export
extremes <- function(data, output, auth = NULL){
  rmd_file <- system.file('extdata','extremes.Rmd',package = 'repgen')
  
  url = 'https://nwissddvasvis01.cr.usgs.gov/service/timeseries/reports/extremes/?station=06899500&dischargeIdentifier=Discharge.TS0058&stageIdentifier=Gage+height.TS0005&dailyDischargeIdentifier=Discharge.TS0058&waterYear=2012'
  auth = 'Bearer aasdfa-asdfasdf-wdfe'
  knit('vignettes/extremes.Rmd')
  system("pandoc -s extremes.md -o extremes.pdf")
  system("pandoc -s extremes.md -o extremes.html")
}