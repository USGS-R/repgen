# Install repgen, production instance, prerequisite packages. Should be safe to
# run repeatedly without additional intervention.

# a slight modification of utils::packageDescription(), to read "Imports"
# section of DESCRIPTION file
repgenImports <- function (lib.loc = NULL, encoding = "") {
  pkg <- "repgen"
  fields <- "Imports"
  retval <- list()
  
  if (!is.null(fields)) {
    fields <- as.character(fields)
    retval[fields] <- NA
  }
  
  pkgpath <- if (is.null(lib.loc)) {
    if (pkg == "base") 
      file.path(.Library, "base")
    else if (isNamespaceLoaded(pkg)) 
      getNamespaceInfo(pkg, "path")
    else if ((envname <- paste0("package:", pkg)) %in% search()) {
      attr(as.environment(envname), "path")
    }
  }
  
  if (is.null(pkgpath)) 
    pkgpath <- ""
  
  if (pkgpath == "") {
    libs <- if (is.null(lib.loc))
      .libPaths()
    else
      lib.loc
    for (lib in libs)
      if (file.access(file.path(lib, pkg), 5) == 0L) {
        pkgpath <- file.path(lib, pkg)
        break
      }
  }
  
  if (pkgpath == "") {
    warning(gettextf("no package '%s' was found", pkg), 
            domain = NA)
    return(NA)
  }
  
  if (file.exists(file <- file.path(pkgpath, "DESCRIPTION"))) {
    dcf <- read.dcf(file = file)
    if (NROW(dcf) < 1L) 
      stop(gettextf("DESCRIPTION file of package '%s' is corrupt", 
                    pkg), domain = NA)
    desc <- as.list(dcf[1, ])
  }
  else file <- ""
  
  if (nzchar(file)) {
    enc <- desc[["Encoding"]]
    if (!is.null(enc) && !is.na(encoding)) {
      if (missing(encoding) && Sys.getlocale("LC_CTYPE") == 
          "C") 
        encoding <- "ASCII//TRANSLIT"
      newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
      if (!inherits(newdesc, "try-error")) 
        desc <- newdesc
      else warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", 
                   call. = FALSE)
    }
    if (!is.null(fields)) {
      ok <- names(desc) %in% fields
      retval[names(desc)[ok]] <- desc[ok]
    }
    else retval[names(desc)] <- desc
  }
  
  if ((file == "") || (length(retval) == 0)) {
    warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", 
                     pkg), domain = NA)
    return(NA)
  }
  
  class(retval) <- "repgenImports"
  
  if (!is.null(fields)) 
    attr(retval, "fields") <- fields
  
  attr(retval, "file") <- file
  retval
}

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

nodename <- Sys.info()["nodename"]
if (nodename == "IGSWZTWWWSASHA") {
  lib <- .libPaths()[1]
} else {
  lib <- Sys.getenv("R_LIBS")
  
  if (nchar(lib) == 0) {
    stop("Could not get a value for R_LIBS environment variable")
  }
}

pkgs <- repgenImports(lib.loc = lib)

# all packages except devtools and its prerequisites are held back to older
# versions
installPackages(pkgs, lib, "https://mran.microsoft.com/snapshot/2016-03-31")

# devtools is only needed to install gsplot and repgen from source on GitHub;
# it's not used by repgen in production
installPackages(c("devtools"), lib, "https://cloud.r-project.org")

# reference all date/time points to UTC (which is not actually a time zone)
Sys.setenv(TZ = "UTC")
