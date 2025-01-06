base_url <- "https://act24.cancer.gov/act24"

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
#' @param startDate The earliest recall date: format YYYY-MM-DD
#' @param endDate The latest recall date: format YYYY-MM-DD
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
  api_url <- glue::glue("{base_url}/studyList/")

  make_act24_api_call(api_url)
}

#' @rdname Act24_API_Wrapper
#' @export
getParticantList <-function(studyId){
  build_api_call("participantList",studyId) |>
    make_act24_api_call()
}

#' @rdname Act24_API_Wrapper
#' @export
getSummaryReportQC <- function(studyId,startDate=NULL,endDate=NULL){
  build_api_call("summaryReportQC",studyId,startDate,endDate) |>
    make_act24_api_call()
}

#' @rdname Act24_API_Wrapper
#' @export
getDetailReportQC <-function(studyId,startDate=NULL,endDate=NULL){
  build_api_call("detailReportQC",studyId,startDate,endDate) |>
    make_act24_api_call()
}

#' @rdname Act24_API_Wrapper
#' @export
getDetailReport <-function(studyId,startDate=NULL,endDate=NULL){
  build_api_call("detailReport",studyId,startDate,endDate) |>
    make_act24_api_call()
}


build_api_call<-function(api,studyId,startDate=NULL,endDate=NULL){
  if (!is_auth()) {
    stop("not authenticated")
  }
  if (!is.numeric(studyId)) stop("Invalid Study Id",studyId)
  if (length(studyId)>1){
    studyId = paste(studyId,collapse = ",")
  }
  if (is.null(startDate) && !is.null(endDate)){
    stop("cannot set endDate without setting startDate")
  }


  # reformat the start date/end date
  dplyr::case_when(
    !is.null(startDate) && !is.null(endDate) ~ paste0(base_url,"/",api,"/",studyId,"/",startDate,"/",endDate,"/"),
    !is.null(startDate) ~ paste0(base_url,"/",api,"/",studyId,"/",startDate,"/"),
    is.null(startDate) ~ paste0(base_url,"/",api,"/",studyId,"/"),
  )
}

make_act24_api_call<-function(api_url){

  if (!is_auth()) stop("call act24_auth(userid) before making API calls")
  credentials <- get_credentials()
  # TO DO: check response for:
  # 401: incorrect username/id
  # 422: Invalid study id. study id does not belong to you, general error
  httr2::request(api_url) |>
    httr2::req_auth_basic(credentials$api_user_id, password = credentials$act_pwd) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    dplyr::bind_rows()
}


