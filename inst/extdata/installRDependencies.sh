#!/bin/bash

Rscript -e 'remove.packages("knitr")' || true
Rscript -e 'remove.packages("rmarkdown")' || true
Rscript -e 'remove.packages("jsonlite")' || true
Rscript -e 'remove.packages("httr")' || true
Rscript -e 'remove.packages("magrittr")' || true
Rscript -e 'remove.packages("htmltools")' || true
Rscript -e 'remove.packages("caTools")' || true
Rscript -e 'remove.packages("RCurl")' || true
Rscript -e 'remove.packages("memoise")' || true
Rscript -e 'remove.packages("whisker")' || true
Rscript -e 'remove.packages("rstudioapi")' || true
Rscript -e 'remove.packages("roxygen2")' || true
Rscript -e 'remove.packages("devtools")' || true
Rscript -e 'remove.packages("dplyr")' || true
Rscript -e 'remove.packages("lubridate")' || true
Rscript -e 'remove.packages("htmlTable")' || true
Rscript -e 'remove.packages("fasttime")' || true

echo '***** installing upward dependencies'
Rscript -e 'install.packages(c("memoise","whisker","rstudioapi","roxygen2","dplyr","lubridate","fasttime","rmarkdown"), repos="http://cran.us.r-project.org")'
Rscript -e 'install.packages(c("httr","jsonlite","magrittr","knitr","htmltools","caTools","RCurl"), repos="http://cran.us.r-project.org")'
Rscript -e 'install.packages(c("htmlTable", "pander"), repos="http://cran.us.r-project.org")'

echo '***** installing fixed version dependencies'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.7.0.tar.gz", repos=NULL, type="source")'
