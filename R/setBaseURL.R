#'@title Set AQCU endpoint
#'
#'@param endpoint Indicate which AQCU endpoint 
#' you want to use options: \code{c('prod','qa','dev')}
#'
#'@description Sets the internal URLS used to either the production, QA, or dev server. 
#'URLS are stored internally to the package
#'
#'@author Luke Winslow, Jordan S Read
#'
#'@examples
#'\dontrun{
#'setBaseURL('prod')
#'setBaseURL('qa')
#'setBaseURL('dev')
#'}
#'@export
setBaseURL <- function(endpoint = 'prod'){
  endpoint = tolower(endpoint)
  
  if (endpoint=="prod"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/auth-webservice/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="qa"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/auth-webservice/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else if (endpoint=="dev"){
    pkg.env$url_base = "https://cida-test.er.usgs.gov/auth-webservice/"
    cat('Setting endpoint to ',pkg.env$url_base,'\n')
  } else {
    stop('Unsupported endpoint option')
  }
  
  pkg.env$auth_token = paste0(pkg.env$url_base, "auth/ad/token/")
  pkg.env$auth_check = paste0(pkg.env$url_base, "security/auth/check/")
  pkg.env$authToken = NULL
  pkg.env$username = NULL
  
}