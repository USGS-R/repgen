#'@title authenticate a user for AQCU services
#'@param username an Active Directory user name
#'@param password an Active Directory password
#'@return an auth token from the authentication service
#'@importFrom httr POST accept_json
#'@export 
authenticateUser <- function(username, password){
  
  
  if(missing(username)){
    stop('username required for authentication')
  }
  
  if(!interactive() & missing(password)){
    stop('No password supplied to authenticateUser in a non-interactive session.')
  }else{
    password = ifelse(missing(password), readPassword('Please enter your Active Directory password:'), password)
  }
  

  ## authenticate
  resp = POST('https://cida-test.er.usgs.gov/auth-webservice/auth/ad/token/', accept_json(),
              body = list(username=username, password=password), encode='form', config=list(ssl.verifypeer = FALSE))
  return(content(resp)$tokenId)
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

