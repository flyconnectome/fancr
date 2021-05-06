#' Return a sample Neuroglancer scene URL for FANC dataset
#'
#' @details See
#'   \href{https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900}{FANC
#'   slack} for details.
#'
#' @param ids A set of root ids to include in the scene.
#' @return A character vector containing a single Neuroglancer URL.
#' @export
#'
#' @examples
#' \donttest{
#' browseURL(fanc_scene())
#' }
fanc_scene <- function(ids=NULL) {
  url="https://neuromancer-seung-import.appspot.com/?json_url=https://api.zetta.ai/json/325890137970388411"
  url=sub("?json_url=", "?", url, fixed = T)
  parts=unlist(strsplit(url, "?", fixed = T))
  json=try(flywire_fetch(parts[2], token=fanc_token(), return = 'text', cache = TRUE))
  if(inherits(json, 'try-error')) {
    badtoken=paste0("You have a token but it doesn't seem to be authorised for FANC.\n",
                    "Have you definitely used `fanc_set_token()` to make a token for the FANC dataset?")
    if(grepl(500, json))
      stop("There seems to be a (temporary?) problem with the zetta server!")
    else if(grepl(401, json))
      stop(badtoken)

    token=try(fanc_token(), silent = T)
    if(inherits(token, 'try-error'))
      stop("It looks like you do not have a stored token. Please use `fanc_set_token()` to make one.")
    else
      stop(badtoken)
  }

  u=ngl_encode_url(json, baseurl = parts[1])
  if(length(ids)>0)
    fafbseg::ngl_segments(u) <- ids
  u
}


choose_fanc <- function(set=TRUE) {
  fafbseg::choose_segmentation(fanc_scene(), set=set)
}


with_fanc <- function(expr) {
  op <- choose_fanc(set = TRUE)
  on.exit(options(op))
  force(expr)
}

fanc_fetch <- function(url, token=fanc_token(), ...) {
  flywire_fetch(url, token=token, ...)
}
