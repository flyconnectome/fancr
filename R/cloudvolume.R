fanc_cloudvolume <- function(...) {
  cv=fafbseg::flywire_cloudvolume(cloudvolume.url = fanc_cloudvolume_url(), ...)
  cv
}

fanc_cloudvolume_url <- function() {
  with_fanc(getOption("fafbseg.cloudvolume.url"))
}


#' Set the token to be used to authenticate to FANC autosegmentation resources
#'
#' @param token An optional token string. When missing you are prompted to
#'   generate a new token via your browser.
#'
#' @return The path to the token file (invisibly)
#' @export
fanc_set_token <- function(token=NULL) {
  # check we have the
  fafbseg::flywire_set_token(token=token, domain='wclee.api.zetta.ai')
}

fanc_token <- function() {
  fafbseg::chunkedgraph_token(url='wclee.api.zetta.ai')
}
