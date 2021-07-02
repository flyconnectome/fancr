#' Fetch change log information for one or more neurons
#'
#' @param x One or more fanc ids in any format understandable by
#'   \code{\link[fafbseg]{ngl_segments}}
#' @param ... Additional argument passed to \code{\link{pbsapply}} and then on
#'   to \code{\link{flywire_fetch}} (via the private \code{fanc_fetch}).
#'
#' @return A list containing information on the changes applied to a given body
#'
#'   \itemize{
#'
#'   \item \code{n_splits},\code{n_mergers} The number of splits/mergers applied
#'   to the neuron
#'
#'   \item \code{user_info} A data.frame containing user ids or emails and the
#'   number of splits/mergers
#'
#'   \item \code{operations_ids} An integer vector of operation ids
#'
#'   \item \code{past_ids} Previous ids associated with this body
#'
#'   }
#' @export
#' @importFrom pbapply pbsapply
#' @examples
#' \donttest{
#' fanc_change_log("648518346473954669")
#' }
fanc_change_log <- function(x, ...) {
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
