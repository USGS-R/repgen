# Install repgen, production instance, prerequisite packages. Should be safe to
# run repeatedly without additional intervention.

# set to TRUE if you intend to develop repgen on this system
developer <- FALSE

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

# convenience wrapper function around install.packages()
installPackages <- function(pkgs, lib, repos = getOption("repos")) {
  for (p in pkgs) {
    tryCatch({
      packageDescription(p) # check to see if package p is already installed
    },
    warning = function(w) {
      # if package p is not installed...
      if (any(grepl("(DESCRIPTION file of package .+ is missing or broken|no package .+ was found)", w))) {
        # ...install it
        install.packages(p, lib, repos = repos)
      } else {
        print(w)
      }
    },
    error = function(e) {
      print(e)
    })
  }
}

# if this is a production tier...
if (!developer) {
  lib <- Sys.getenv("R_LIBS")
  if (nchar(lib) == 0) {
    stop(
      paste(
        "Could not get a value for R_LIBS environment variable; is this a",
        "development system?",
        sep = "\n  "
      )
    )
  }
} else {
  libPaths <- .libPaths()
  lib <- libPaths[1]
}

# all packages except devtools and its prerequisites are held back to older
# versions, keyed by their repo URLs below
installPackages(pkgs, lib, "http://mran.microsoft.com/snapshot/2016-03-31")
installPackages("httr", lib, "http://mran.microsoft.com/snapshot/2016-01-27")

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
if (!developer) {
  # ...devtools & these devtools prerequisites are no longer needed
  pkgs <- c("BH", "devtools", "httr")
  
  for (p in pkgs) {
    tryCatch({
      remove.packages(p, lib)
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      return()
    })
  }
} else {
  installPackages(c("roxygen2", "testthat"), lib)
  
  cat(
    "If on Windows, you will now need to download Rtools from",
    "https://cran.r-project.org/bin/windows/Rtools/ and install.",
    sep = "\n"
  )
}

# restart R to avoid potential warning messages from installed.packages()
.rs.restartR()

# reference all date/time points to UTC (which is not actually a time zone)
Sys.setenv(TZ = "UTC")
