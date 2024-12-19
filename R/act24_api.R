base_url = "https://act24.cancer.gov/act24/"

#' Call Act24 API
#'
#' @description
#'
#' These function wrap the act24 api and return a tibble containing the results.
#'
#'
#'  \strong{getStudies()} returns a tibble containing all the studies that you can
#'   access. It contains the studyId, which is required to lookup other
#'   information about an individual staudy.
#'
#'  \strong{getParticantList(studyId)} returns a tibble of participants for a study
#'
#'  \strong{getDetailReportQC(studyId)} returns the detailed tibble of activities for a study
#'
#' @param studyId The studyId assigned by act24. Found in the getStudies results
#' @return a tibble that depends on the api called
#' @name Act24_API_Wrapper
#'
NULL

#' @rdname Act24_API_Wrapper
#' @export
getStudies <- function(){
  if (is.null(.auth) ) {
    stop("not authenticated")
  }
  api_url <- glue::glue("{base_url}studyList/")

  make_act24_api_call(api_url)
}

#' @rdname Act24_API_Wrapper
#' @export
getParticantList <-function(studyId){
  if (is.null(.auth)) {
    stop("not authenticated")
  }
  if (!is.numeric(studyId)) stop("Invalid Study Id",studyId)
  api_url <- glue::glue("{base_url}participantList/{studyId}/")

  make_act24_api_call(api_url)
}

#' @rdname Act24_API_Wrapper
#' @export
getDetailReportQC <-function(studyId){
  if (is.null(.auth)) {
    stop("not authenticated")
  }
  if (!is.numeric(studyId)) stop("Invalid Study Id",studyId)
  api_url <- glue::glue("{base_url}detailReportQC/{studyId}/")

  make_act24_api_call(api_url)
}


make_act24_api_call<-function(api_url){
  if (!is_auth()) stop("call act24_auth(userid) before making API calls")
  credentials <- get_credentials()
  httr2::request(api_url) |>
    httr2::req_auth_basic(credentials$api_user_id, password = credentials$act_pwd) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    dplyr::bind_rows()
}
