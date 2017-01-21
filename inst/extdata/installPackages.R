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
  "htmlTable",
  "jsonlite",
  "knitr",
  "lubridate",
  "rmarkdown"
)

installPackages <- function(pkgs, repos) {
  for (p in pkgs) {
    tryCatch({
      packageDescription(p) # check to see if package p is already installed
    },
    warning = function(w) {
      # if package p is not installed...
      if (any(grepl("(DESCRIPTION file of package .+ is missing or broken|no package .+ was found)", w))) {
        # ...install it
        install.packages(p, repos = repos)
      } else {
        print(w)
      }
    },
    error = function(e) {
      print(e)
    })
  }
}

# all packages are held back to older versions, which are keyed by their repo
# URLs below
installPackages(pkgs, "http://mran.microsoft.com/snapshot/2016-03-31")
installPackages(c("httr"), "http://mran.microsoft.com/snapshot/2016-01-27")

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
devtools::install_github("USGS-R/gsplot")
