# Cleans and installs prerequisite packages for repgen.

RemovePackages <- function(pkgs) {
  # Wrap R remove.packages() function for our purposes.
  # 
  # Args:
  #   pkgs: Vector of names of packages to install.
  for (p in pkgs) {
    tryCatch({
      remove.packages(p)
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      return()
    })
  }
}

#' @description Wrap R install.packages() function for our purposes.
#'
#' @param filename string to custom file 
#'
#'@examples
#'loadConfig()
#'@export
#' @importFrom graphics plot.xy
#' @importFrom grDevices dev.off
#' @importFrom graphics par
#' @importFrom yaml yaml.load_file
InstallPackages <- function(pkgs, repos) {
  # 
  # 
  # Args:
  #   pkgs: Vector of names of packages to install.
  #   repos: Same as domain of R install.packages() "repos" argument.
  for (p in pkgs) {
    tryCatch({
      install.packages(p, repos = repos)
    },
    warning = function(w) {
      print(w)
    },
    error = function(e) {
      print(e)
    })
  }
}

# list of repgen prerequisite packages
pkgs <- c(
  "DBI",
  "R6", "RCurl", "Rcpp",
  "assertthat",
  "bitops", "brew",
  "caTools", "curl",
  "devtools", "digest", "dplyr",
  "evaluate",
  "formatR",
  "git2r",
  "highr", "htmlTable", "htmltools",
  "jsonlite",
  "knitr",
  "lazyeval", "lubridate",
  "magrittr", "markdown", "memoise", "mime",
  "openssl",
  "rmarkdown", "roxygen2", "rstudioapi",
  "stringi", "stringr",
  "whisker", "withr",
  "yaml"
)

# httr and devtools packages must be held back (see below), so they are managed
# apart from packages in pkgs
RemovePackages(c(pkgs, "httr", "devtools"))

InstallPackages(pkgs, "http://mran.microsoft.com/snapshot/2016-03-31")

# packages that must be held back at older versions
InstallPackages("httr", "http://mran.microsoft.com/snapshot/2016-01-27")

# before running the next line, you may need to do:
#
#   wget http://blockpage.doi.gov/images/DOIRootCA.crt
#   httr:::find_cert_bundle()
#   [1] "C:/Users/ashalper/Documents/R/win-library/3.3/httr/cacert.pem"
#   cat DOIRootCA.crt >> C:/Users/ashalper/Documents/R/win-library/3.3/httr/cacert.pem"

# https://github.com/USGS-R/gsplot#installation
devtools::install_github("USGS-R/gsplot")

# Some copy-and-pasted code from 
# https://www.r-statistics.com/2013/02/installing-pandoc-from-r-on-windows/ 
# Don't know R well enough to know if this can/should be integrated into
# install.packages() loop above.
if (!require(installr)) {
  install.packages("installr")
  require(installr)
}
#install.pandoc()

Sys.setenv(TZ = "UTC")

