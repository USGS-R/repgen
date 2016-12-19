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

Rscript -e 'install.packages(c("bitops","magrittr","stringi","stringr","yaml","evaluate","formatr","highr","R6","assertthat","lazyeval","DBI","jsonlite","brew","mime","curl","caTools","openssl","memoise","whisker","rstudioapi","git2r","withr","markdown","knitr","htmltools","rmarkdown","RCurl","htmlTable","roxygen2","dplyr","lubridate"), repos="http://mran.microsoft.com/snapshot/2016-03-31", type="source")'

Rscript -e 'install.packages("httr", repos = "http://mran.microsoft.com/snapshot/2016-01-27",type="source")'
Rscript -e 'install.packages("devtools", repos = "http://mran.microsoft.com/snapshot/2015-01-19",type="source")'
Rscript -e 'Sys.setenv(TZ="UTC")'
