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

# Known production tiers, listed at
# https://docs.google.com/document/d/1vBOTUPtIdeCTGe7Or2cQGMMOFS_hD545yZ1E6YBGizs/edit
tiers <-
  c(
    "nwissddvasaqcu.cr.usgs.gov",    # DEV
    "intcida-test.er.usgs.gov",      # QA
    "nwissdtrasnwisra1.cr.usgs.gov", # Train 1
    "nwissdtrasnwisra2.cr.usgs.gov", # Train 2
    "nwisdata.usgs.gov",             # PROD
    "reporting.nwis.usgs.gov"        # CHS “prod”
  )

nodename <- Sys.info()["nodename"]

# if this is a production tier...
if (nodename %in% tiers) {
  source("installPackages.R")
} else {
  # presume it's a development machine with the source checked out
  source(paste0(getwd(), "/inst/extdata/installPackages.R"))
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
if (nodename %in% tiers) {
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
