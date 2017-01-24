# Install repgen, production instance, prerequisite packages. Should be safe to
# run repeatedly without additional intervention.

# devtools is currently only required on a production instance to install gsplot
# from GitHub (see below). There are plans to "promote" it to CRAN (or GRAN?),
# but that hasn't happened yet. See 
# https://usgs-cida.slack.com/archives/aqcu/p1484934743005621 if it still 
# exists.
pkgs <- c(
  "devtools",
  "dplyr",
  "evaluate",
  "htmlTable",
  "jsonlite",
  "lazyeval",
  "knitr",
  "lubridate",
  "rmarkdown",
  "yaml"
)

args = commandArgs(trailingOnly = TRUE)

# if this is a production tier...
if (length(args) == 0 | args[1] == "FALSE") {
  development <- FALSE
  tryCatch({
    source("installPackages.R")
  },
  warning = function(w) {
    # No such file or directory
    print(w)
  },
  error = function(e) {
    print(e)
  })
} else if (args[1] == "TRUE") {
  development <- TRUE
  # presume the source is checked out
  source(paste0(getwd(), "/inst/extdata/installPackages.R"))
} else {
  cat(paste0("Unrecognized argument: \"", args[1], "\""))
  quit(status = 1)
}

# all packages except devtools and its prerequisites are held back to older
# versions, keyed by their repo URLs below
installPackages(pkgs, "http://mran.microsoft.com/snapshot/2016-03-31")
installPackages("httr", "http://mran.microsoft.com/snapshot/2016-01-27")

# To be able to install gsplot from GitHub using devtools (below), download
# DOI root certificate and append to openssl package's certificate bundle
# (ruthlessly, without asking). See also
# https://usgs-cida.slack.com/archives/r-activities/p1469472383000332

# locate & read the openssl certificate bundle
cert_bundle <- httr:::find_cert_bundle()
cert_bundle_lines <- readLines(cert_bundle)

# check to see if DOI root certificate is already in the certificate bundle...
if (!any(grepl("DOI Root CA", cert_bundle_lines, fixed = TRUE))) {
  # ...so that we only download & append the DOI root certificate once
  
  doiRootCA <- tempfile("DOIRootCA.")
  download.file("http://blockpage.doi.gov/images/DOIRootCA.crt", doiRootCA)
  
  cat(
    c("", "DOI Root CA", "===================="),
    file = cert_bundle,
    sep = "\n",
    append = TRUE
  )
  file.append(cert_bundle, doiRootCA)
  file.remove(doiRootCA)
}

# https://github.com/USGS-R/gsplot#installation
devtools::install_github("USGS-R/gsplot", quiet = TRUE)

# if this is a production tier...
if (development) {
  # ...devtools & these devtools prerequisites are no longer needed
  pkgs <- c("BH", "devtools", "httr")
  
  for (p in pkgs) {
    tryCatch({
      remove.packages(p, lib.loc)
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      return()
    })
  }
}
