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
  json=flywire_fetch(parts[2], token=fanc_token(), return = 'text', cache = TRUE)
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
