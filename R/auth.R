.auth = new.env()

#' Provide API Linkage Id/password
#'
#' act24_auth sets the api linkage id (\strong{not the ACT24 research id}) and
#' password. The password is stored in a machine-specific manner based on the
#' package keyring.  You can add .ignore_keyring to avoid storing the linked
#' api password in the keychain. Even if you dont store the api password in the
#' keychain, the id and password are cached in memory for the duration of the
#' R session, unless you call act24_deauth.  The keyring can be cleared by
#' calling act24_deauth with clear_keychain = TRUE
#'
#'
#' act24_deauth removes the api linkage information.
#'
#' @param api_id The ACT24 API Linkage id
#' @param .ignore_keyring If set to true, will not use the keyring to store
#'  the api password across R sessions
#' @param clear_keyring If set to true, will clear the id/password stored in
#'  the keyring
#'
#' @export
#'
act24_auth <- function(api_id,.ignore_keyring=FALSE){
  .auth$api_user_id <- api_id
  # check if the users password is in the keyring
  if ( api_id %in% keyring::key_list("act24")$username && !.ignore_keyring){
    .auth$act_pwd <- keyring::key_get("act24",api_id)
  } else{
    .auth$act_pwd <- getPass::getPass("Enter Act24 API password: ")
    if (!.ignore_keyring){
      keyring::key_set_with_value("act24",api_id,.auth$act_pwd)
    }
  }
  invisible(.auth)
}

#' @rdname act24_auth
#' @export
act24_deauth <- function(clear_keyring=FALSE){
  if (clear_keyring){
    keyring::key_delete("act24",.auth$api_user_id)
  }
  suppressWarnings(rm("api_user_id","act_pwd",envir = .auth))
}

get_credentials <- function(){
  invisible(.auth)
}

is_auth <- function(){
  !is.null(.auth$act_pwd) && length(.auth$act_pwd)>0
}
