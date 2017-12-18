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
  imports <- lapply(imports, function(x){x[x !=""]})
  return(imports)
}

#convenience function for getting a package version from a repo URL
getVersionOnRepo <- function(pkg, repo) {
  return(available.packages(contriburl = contrib.url(repo))[pkg,]["Version"][[1]])
}
# convenience wrapper function around install.packages()
installPackages <- function(pkgs, lib, repos = getOption("repos")) {
  print(paste("Using repository", repos))
  
  print("")
  
  for (p in pkgs) {
    print(paste("Checking", p, "..."))
    v <- NULL
    tryCatch({
      v <- packageVersion(p) # check to see if package p is already installed
    }, error=function(e){})
    
    if(is.null(v)) {
      print(paste("not installed, installing from repository..."))
      install.packages(p, lib, repos = repos)
    } else {
      print(paste("Found version:", v))
      
      remoteVersion <- NULL
      tryCatch({
        remoteVersion <- getVersionOnRepo(p, repos)
      }, error=function(e){})
      
      if(is.null(remoteVersion)) {
        print("Not found in remote repo, skipping...")
      } else if(v == remoteVersion) {
        print("Up to date with repository version")
      } else {
        print(paste("Does not match version on repository:", remoteVersion))
        print(paste("Installing", p, remoteVersion, "..."))
        install.packages(p, lib, repos = repos)
      }
    }
    print("")
  }
}

lib <- Sys.getenv("R_LIBS")

if (nchar(lib) == 0) {
  stop("Could not get a value for R_LIBS environment variable")
}

pkgs <-
  grep(
    "gsplot", repgenImports(lib.loc = lib),
    value = TRUE, fixed = TRUE, invert = TRUE
  )

# all packages are held back to older, MRAN versions
installPackages(c(pkgs, "devtools"), lib, "https://mran.microsoft.com/snapshot/2017-11-15")

# reference all date/time points to UTC (which is not actually a time zone)
Sys.setenv(TZ = "UTC")
