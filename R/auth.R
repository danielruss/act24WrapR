.auth = new.env()

#' Provide API Linkage Id/password
#'
#' act24_auth sets the api linkage id (\strong{not the ACT24 research id}) and password.
#' The password is cached until you close your R session
#'
#' act24_deauth removes the api linkage information.
#'
#' @param api_id The ACT24 API Linkage id
#'
#' @export
#'
act24_auth <- function(api_id){
  .auth$api_user_id <- api_id
  .auth$act_pwd <- getPass::getPass("Enter Act24 API password: ")
}

#' @rdname act24_auth
#' @export
act24_deauth <- function(){
  rm("api_user_id","act_pwd",envir = .auth)
}

get_credentials <- function(){
  invisible(.auth)
}

is_auth <- function(){
  !is.null(.auth$act_pwd) && length(.auth$act_pwd)>0
}
