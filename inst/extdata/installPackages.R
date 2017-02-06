# Install repgen, production instance, prerequisite packages. Should be safe to
# run repeatedly without additional intervention.

# a slight modification of utils::packageDescription(), to read "Imports"
# section of DESCRIPTION file
repgenImports <- function (lib.loc = NULL, encoding = "") {

  if (file.exists(file <- file.path(getwd(), "DESCRIPTION"))) {
    dcf <- read.dcf(file = file)
    if (NROW(dcf) < 1L) 
      stop(gettextf("DESCRIPTION file of package '%s' is corrupt", 
                    "repgen"), domain = NA)
    desc <- as.list(dcf[1, ])
  }
  else file <- ""
  
  if (nzchar(file)) {
    enc <- desc[["Encoding"]]
    if (!is.null(enc) && !is.na(encoding)) {
      if (missing(encoding) && Sys.getlocale("LC_CTYPE") == "C") 
        encoding <- "ASCII//TRANSLIT"
      newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
      if (!inherits(newdesc, "try-error")) 
        desc <- newdesc
      else warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", 
                   call. = FALSE)
    }
  }
  
  if ((file == "") || (length(desc$Imports) == 0)) {
    warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", 
                     "repgen"), domain = NA)
    return(NA)
  }

  imports <- strsplit(unlist(desc$Imports), "[\n,]+")
  return(imports[[1]][-1]) # hack to remove "" from results of strsplit() above
}

# convenience wrapper function around install.packages()
installPackages <- function(pkgs, lib, repos = getOption("repos")) {
  for (p in pkgs) {
    tryCatch({
      packageDescription(p) # check to see if package p is already installed
    },
    warning = function(w) {
      # if package p is not installed...
      if (any(grepl(
        "(DESCRIPTION file of package .+ is missing or broken|no package .+ was found)",
        w))) {
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

nodename <- Sys.info()["nodename"]
# if this is our Very Special development machine
if (nodename == "IGSWZTWWWSASHA") {
  lib <- .libPaths()[1]
} else {
  # it's the general public
  lib <- Sys.getenv("R_LIBS")
  
  if (nchar(lib) == 0) {
    stop("Could not get a value for R_LIBS environment variable")
  }
}

pkgs <-
  grep(
    "gsplot", repgenImports(lib.loc = lib),
    value = TRUE, fixed = TRUE, invert = TRUE
  )

# all packages are held back to older, MRAN versions
installPackages(c(pkgs, "devtools"), lib, "https://mran.microsoft.com/snapshot/2016-03-31")

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

# gsplot hasn't made it to an offical repository yet, so it needs to be
# installed with devtools
devtools::install_github("USGS-R/gsplot#420")

# reference all date/time points to UTC (which is not actually a time zone)
Sys.setenv(TZ = "UTC")
