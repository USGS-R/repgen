#'@aliases renderCustomFragments
#'@rdname renderCustomFragments
setMethod("renderCustomFragments", signature(reportData = "derivationchain"), 
    definition = function(reportData) {
      return(list()) #do nothing
    }
)