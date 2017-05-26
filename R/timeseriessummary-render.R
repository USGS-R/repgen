#'@aliases renderCustomFragments
#'@rdname renderCustomFragments
setMethod("renderCustomFragments", signature(reportData = "timeseriessummary"), 
    definition = function(reportData) {
      return(list()) #do nothing
    }
)