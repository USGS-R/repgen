`repgen`
===========
report generation in R  

| Name       | Status           |  
| :------------ |:-------------|  
| Linux Build: | [![Build Status](https://travis-ci.org/USGS-R/repgen.svg?branch=master)](https://travis-ci.org/USGS-R/repgen) |
| Windows Build: | [![Build status](https://ci.appveyor.com/api/projects/status/gvqmwkyucwe4g59y?svg=true)](https://ci.appveyor.com/project/jread-usgs/repgen) |  
| Package tests: | [![Coverage Status](https://coveralls.io/repos/USGS-R/repgen/badge.svg)](https://coveralls.io/r/USGS-R/repgen) |  

Package installation 
----------
###Install from github (use for most current code):
```R
devtools::install_github('USGS-R/repgen')
```
###Install tagged release (use for _stable_ package releases):
```R
version <- '0.2.6'
devtools::install_url(sprintf('https://github.com/USGS-R/repgen/archive/v%s.tar.gz', version))
```

###Install production release (use for _production_ package releases):
```R
install.packages("repgen", 
    repos = c("http://owi.usgs.gov/R", "http://cran.us.r-project.org"),
    dependencies = TRUE)
```

Example usgage
----------
###Generate an 'extremes' report:
```R
library(repgen)
library(jsonlite)

data <- fromJSON(system.file('extdata',"extremes-example.json",package = 'repgen'))
extremes(data, 'pdf')
```
###Generate a 'vdiagram' report:
```R
json_file <- system.file('extdata','vdiagram-example.json', package = 'repgen')
data <-fromJSON(json_file)
vdiagram(data, 'html')
```

Disclaimer
----------
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
