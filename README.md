`repgen`
===========
report generation in R  


## Package Status

| Name       | Status           |  
| :------------ |:-------------|  
| Linux Build: | [![Build Status](https://travis-ci.org/USGS-R/repgen.svg?branch=master)](https://travis-ci.org/USGS-R/repgen) |
| Package tests: | [![Coverage Status](https://coveralls.io/repos/github/USGS-R/repgen/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/repgen?branch=master) |  

[![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

This package is considered a 'support' package. For more information, see:
[https://owi.usgs.gov/R/packages.html#support](https://owi.usgs.gov/R/packages.html#support)

## Package installation 

### Install from github (use for most current code):
```R
devtools::install_github('USGS-R/repgen')
```
### Install tagged release (use for _stable_ package releases):
```R
version <- '0.3.1'
devtools::install_url(sprintf('https://github.com/USGS-R/repgen/archive/v%s.tar.gz', version))
```

### Install production release (use for _production_ package releases):
```R
install.packages("repgen", 
    repos = c("http://owi.usgs.gov/R", "http://cran.us.r-project.org"),
    dependencies = TRUE)
```

### Server install (assumes Rserve and `pandoc` > 1.12.1 version)
```
echo $release_version
tag=$release_version
Rscript -e 'install.packages(c("devtools","httr","jsonlite","knitr", "rmarkdown","magrittr"), repos="http://cran.us.r-project.org")'
Rscript -e 'library(devtools);install_github("USGS-R/gsplot")'
Rscript -e 'library(devtools);install_url("https://github.com/USGS-R/repgen/archive/'${release_version}'.zip")'
```

### Running With Docker and Rserve
This application can also be run locally using the included docker file and docker-compose file. The included `docker-compose` file has 2 profiles to choose from when running the application locally:

1. aqcu-repgen: This is the default profile which runs the application as it would be in our cloud environment. This is not recommended for local development as it makes configuring connections to other services running locally on your machine more difficult.
2. aqcu-repgen-local-dev: This is the profile which runs the application as it would be in the aqcu-local-dev project, and is configured to make it easy to replace the aqcu-repgen instance in the local-dev project with this instance. It is run the same as the `aqcu-repgen` profile, except it uses the docker host network driver.

To build and run the application in Docker you can run: `docker-compose up --build {profile}`, replacing `{profile}` with one of the options listed above.

## Example usgage

### Generate an 'extremes' report:
```R
library(repgen)
library(jsonlite)

data <- fromJSON(system.file('extdata','extremes','extremes-example.json',package = 'repgen'))

extremesTable(data)

```
### Generate a 'vdiagram' report:
```R
library(repgen)
library(jsonlite)
json_file <- system.file('extdata','vdiagram','vdiagram-v6.json', package = 'repgen')
data <-fromJSON(json_file)

vdiagram(data)

```

### Package Support

The Water Mission Area of the USGS has supported the development and maintenance of the `repgen` R-package. Further maintenance is expected to be stable through September 2018. Resources are available primarily for maintenance. Priorities on the development of new features are determined by the `repgen` development team.

[![USGS](http://usgs-r.github.io/images/usgs.png)](https://www.usgs.gov/)

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [http://www.usgs.gov/visual-id/credit_usgs.html#copyright](http://www.usgs.gov/visual-id/credit_usgs.html#copyright)

This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."


 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
