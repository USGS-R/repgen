#!/bin/bash
Rscript -e 'remove.packages("curl")' || true
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

echo '***** installing upward dependencies'

Rscript -e 'install.packages(c("httr","jsonlite","methods","rmarkdown","knitr","dplyr","lubridate","htmlTable","pander","testthat","devtools"), repos="http://mran.revolutionanalytics.com/snapshot/2016-04-01", type="source")'

Rscript -e 'Sys.setenv(TZ="UTC")'

