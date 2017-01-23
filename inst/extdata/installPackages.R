installPackages <- function(pkgs, repos) {
  # passed in by Jenkins deployer
  r_libs <- Sys.getenv("R_LIBS")
  
  # if R_LIBS is defined
  if (0 < nchar(r_libs)) {
    lib <- r_libs # use it
  } else {
    # use 1st .libPaths() path
    libPaths <- .libPaths()
    lib <- libPaths[1]
  }
  
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
