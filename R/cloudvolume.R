fanc_cloudvolume <- function(...) {
  cv=fafbseg::flywire_cloudvolume(cloudvolume.url = fanc_cloudvolume_url(), ...)
  cv
}

fanc_cloudvolume_url <- function() {
  rr=with_fanc(getOption("fafbseg.cloudvolume.url"), force = FALSE)
  sub("graphene://middleauth+", "graphene://", rr, fixed = TRUE)
}

fanc_api_url <- function(endpoint="") {
  fafbseg:::flywire_api_url(endpoint=endpoint,
                            cloudvolume.url = fanc_cloudvolume_url())
}


#' Set the token to be used to authenticate to FANC autosegmentation resources
#'
#' @param token An optional token string. When missing you are prompted to
#'   generate a new token via your browser.
#'
#' @return The path to the token file (invisibly)
#' @export
fanc_set_token <- function(token=NULL) {
  # path=fafbseg::flywire_set_token(token=token, domain='cave.fanc-fly.com')
  path=fafbseg::flywire_set_token(token=token)
  # clear the token cache so the new one is immediately available
  fanc_token(cached=FALSE)
  invisible(path)
}

fanc_token <- function(cached=TRUE) {
  # fafbseg::chunkedgraph_token(url='cave.fanc-fly.com', cached = cached)
  fafbseg::chunkedgraph_token(cached = cached)
}

fanc_token_available <- function() {
  !inherits(try(fanc_token(), silent = TRUE), 'try-error')
}

#' Print information about your FANC setup including tokens and python modules
#'
#' @export
#' @seealso \code{\link{dr_fafbseg}}
#' @examples
#' \dontrun{
#' dr_fanc()
#' }
dr_fanc <- function() {
  fanc_api_report()
  cat("\n\n")
  res = fafbseg:::py_report()
  cat("\n")
  try(fafbseg:::check_cloudvolume_reticulate(min_version = "3.12"))
  invisible(res)
}

fanc_api_report <- function() {
  message("FANC Neuroglancer / CAVE API access\n----")

  token=try(fanc_token(cached = F), silent = FALSE)
  if(inherits(token, "try-error")) {
    FUN=if(requireNamespace('usethis', quietly = T)) usethis::ui_todo else message
    FUN(paste('No valid FANC API token found. Set your token by doing:\n',
                  "{ui_code('fanc_set_token()')}"))
  } else{
    cat("Valid FANC API ChunkedGraph token is set!\n")
  }
  ff=dir(fafbseg:::cv_secretdir(), pattern = '-secret\\.json$')
  if(length(ff)){
    cat(length(ff), "CloudVolume credential files available at\n",
        fafbseg:::cv_secretdir(),"\n")
    print(ff)
  }

  u=with_fanc(fafbseg:::check_cloudvolume_url(set = F), force = FALSE)
  cat("\nZetta cloudvolume URL:", u)
}
