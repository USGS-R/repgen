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
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/bitops_1.0-6.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/digest_0.6.9.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/stringr_1.0.0.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/brew_1.0-6.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/stringi_1.0-1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/Rcpp_0.12.4.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/assertthat_0.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/R6_2.1.2.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/magrittr_1.5.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/lazyeval_0.1.10.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/DBI_0.3.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/knitr_1.12.3.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/yaml_2.1.13.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/htmltools_0.3.5.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/caTools_1.17.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/jsonlite_0.9.19.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/mime_0.4.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/curl_0.9.6.tar.gz", repos=NULL, type="source")'
# WARNING/ERROR ON THIS INSTALL
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/openssl_0.9.2.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/Archive/httr/httr_1.0.0.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/evaluate_0.8.3.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/formatR_1.3.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/highr_0.5.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/markdown_0.7.7.tar.gz", repos=NULL, type="source")'
# WARNING/ERROR ON THIS INSTALL
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/git2r_0.14.0.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/withr_1.0.1.tar.gz", repos=NULL, type="source")'

Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/memoise_1.0.0.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/whisker_0.3-2.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/rstudioapi_0.5.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/roxygen2_5.0.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/dplyr_0.4.3.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/lubridate_1.5.0.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.us.r-project.org/src/contrib/rmarkdown_0.9.5.tar.gz", repos=NULL, type="source")'




Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/knitr_1.12.3.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/htmltools_0.3.5.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/caTools_1.17.1.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/RCurl_1.95-4.8.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/htmlTable_1.5.tar.gz", repos=NULL, type="source")'
Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/pander_0.6.0.tar.gz", repos=NULL, type="source")'

Rscript -e 'install.packages("http://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.7.0.tar.gz", repos=NULL, type="source")'

Rscript -e 'Sys.setenv(TZ="UTC")'


