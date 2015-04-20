#'@title authenticate a user to AQCU portal
#'@description returns a token that can be used in GET, POST or PUT methods
#'@param username an active directory username (e.g., \code{'bbadger'})
#'@param password a character password for the active directory username. 
#'Will be prompted for password if missing and in interactive mode. 
#'@param verbose boolean for verbose output. Default FALSE
#'@details if \code{authenticateUser} is called with a \code{username} argument, 
#'\code{username} is stored in the R session environment, and future calls to 
#'\code{authenticateUser} within the same R session can use the password argument 
#'only (e.g., \code{authenticateUser(password = '12345')})
#'@return a character token, or the status code if not 200
#'@importFrom httr POST accept_json content timeout verbose
#'@export 
authenticateUser <- function(username, password, verbose=FALSE){
  
  
  if (missing(username) & is.null(pkg.env$username) & interactive()) {
    username <- readPassword('Please enter your Active Directory username:')
  } else if (missing(username)) {
    username <- pkg.env$username
  } 
  if (is.null(username)) {
    stop('username required for authentication')
  }
  
  if(!interactive() & missing(password)){
    stop('No password supplied to authenticateUser in a non-interactive session.')
  }else{
    password = ifelse(missing(password), readPassword('Please enter your Active Directory password:'), password)
  }
  
  
  ## authenticate
  
  resp = POST(pkg.env$auth_token, accept_json(),
              body = list(username=username, password=password), 
              encode='form', timeout(5), 
              config = list('verbose' = verbose, ssl.verifypeer = FALSE))
  
  if (resp$status_code == 200){
    
    pkg.env$username <- username
    pkg.env$authToken <- content(resp)$tokenId
    invisible(pkg.env$authToken)
  } else {
    stop('authentication for ',username,' failed ',resp$status_code)
  }
  
}

readPassword <- function(prompt) {
  if (exists(".rs.askForPassword")) {
    .rs.askForPassword <- "_private" # spoofing variable name for package warning
    pass <- .rs.askForPassword(prompt)
  } else {
    pass <- readline(prompt)
  }
  return (pass)
}

