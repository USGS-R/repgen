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
