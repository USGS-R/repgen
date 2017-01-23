# Install repgen development packages. These should only be necessary if you 
# intend to do repgen development. They are not needed on a production server.

source(paste0(getwd(), "/inst/extdata/installPackages.R"))

installPackages(c("devtools", "roxygen2", "testthat"))
