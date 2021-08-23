#' Fetch change log information for one or more neurons
#'
#' @details As of August 2021 this is a simple wrapper of
#'   \code{fabseg::\link{flywire_change_log}}. For now the old (and less
#'   convenient format) available from the zetta API can be obtained with the
#'   private \code{fancr:::fanc_change_log_zetta} function.
#'
#' @param x One or more fanc ids in any format understandable by
#'   \code{\link[fafbseg]{ngl_segments}}
#' @return a \code{data.frame} See \code{fabseg::\link{flywire_change_log}} for
#'   details
#' @export
#' @importFrom pbapply pbsapply
#' @inheritParams fafbseg::flywire_change_log
#' @examples
#' \donttest{
#' fanc_change_log("648518346473954669")
#' }

fanc_change_log <- function(x, tz="UTC", filtered=TRUE, OmitFailures=TRUE,
                            ...) {
  with_fanc(flywire_change_log(x=x, tz=tz, filtered = filtered, OmitFailures = OmitFailures, ...))
}

fanc_change_log_zetta <- function(x, ...) {
  baseurl=fanc_api_url(endpoint = "root/")
  x=fafbseg::ngl_segments(x, as_character = T)
  if(length(x)>1) {
    res=pbapply::pbsapply(x, fanc_change_log, ...)
    return(dplyr::bind_rows(res))
  }
  url=paste0(baseurl, x, "/change_log")
  res=fanc_fetch(url, ...)
  res$user_info
  userdf=data.frame(user=names(res$user_info), stringsAsFactors = FALSE)
  userdf$n_splits=sapply(res$user_info, function(x) {splits=x$n_splits; ifelse(is.null(splits), 0L, as.integer(splits))})
  userdf$n_mergers=sapply(res$user_info, function(x) {mergers=x$n_mergers; ifelse(is.null(mergers), 0L, as.integer(mergers))})
  res$operations_ids=scan(text = chartr("[]","  ", res$operations_ids),
                          what=integer(), quiet = T)
  res$user_info=userdf
  res
}
