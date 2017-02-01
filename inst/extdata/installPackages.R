# Install repgen, production instance, prerequisite packages. Should be safe to
# run repeatedly without additional intervention.

pkgs <- c(
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

lib <- Sys.getenv("R_LIBS")
if (nchar(lib) == 0) {
  stop("Could not get a value for R_LIBS environment variable")
}

# all packages except devtools and its prerequisites are held back to older
# versions
installPackages(pkgs, lib, "https://mran.microsoft.com/snapshot/2016-03-31")

# devtools & these devtools prerequisites are no longer needed
pkgs <- c("BH", "devtools", "httr")

# for all gsplot installer packages
for (p in pkgs) {
  tryCatch({
    packageDescription(p)
  },
  warning = function(w) {
    # if the gsplot installer package is installed
    if (!any(grepl("(DESCRIPTION file of package .+ is missing or broken|no package .+ was found)", w))) {
      # remove it
      remove.packages(p, lib)
    }
  },
  error = function(e) {
    return()
  })
}

# reference all date/time points to UTC (which is not actually a time zone)
Sys.setenv(TZ = "UTC")
