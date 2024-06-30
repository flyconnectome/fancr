#' Return a sample Neuroglancer scene URL for FANC dataset
#'
#' @details See
#'   \href{https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900}{FANC
#'    slack} for details.
#'
#' @param ids A set of root ids to include in the scene. Can also be a
#'   data.frame.
#' @param open Whether to open the URL in your browser (see
#'   \code{\link{browseURL}})
#' @return A character vector containing a single Neuroglancer URL (invisibly
#'   when \code{open=TRUE}).
#' @export
#' @importFrom utils browseURL
#' @examples
#' \dontrun{
#' browseURL(fanc_scene())
#' fanc_scene(open=T)
#' fanc_scene("648518346498254576", open=T)
#' }
fanc_scene <- function(ids=NULL, open=FALSE) {
  url="https://neuromancer-seung-import.appspot.com/?json_url=https://global.daf-apis.com/nglstate/api/v1/5969075557629952"
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

  if(!is.null(ids)){
    fafbseg::ngl_segments(u) <- fanc_ids(ids)
  }
  if(open) {
    browseURL(u)
    invisible(u)
  } else (u)
}


#' Choose or (temporarily) use the FANC autosegmentation
#'
#' @details \code{fancr} inherits a significant amount of infrastructure from
#'   the \code{\link{fafbseg}} package. This has the concept of the
#'   \emph{active} autosegmentation, which in turn defines one or more R options
#'   containing URIs pointing to voxel-wise segmentation, mesh etc data. These
#'   are normally contained within a single neuroglancer URL which points to
#'   multiple data layers. For FANC this is the neuroglancer scene returned by
#'   \code{\link{fanc_scene}}.
#' @param set Whether or not to permanently set the FANC autosegmentation as the
#'   default for \code{\link{fafbseg}} functions.

#'
#' @return If \code{set=TRUE} a list containing the previous values of the
#'   relevant global options (in the style of \code{\link{options}}. If
#'   \code{set=FALSE} a named list containing the option values.
#' @export
#'
#' @examples
#' \dontrun{
#' choose_fanc()
#' options()[grep("^fafbseg.*url", names(options()))]
#' }
choose_fanc <- function(set=TRUE) {
  fafbseg::choose_segmentation(fanc_scene(), set=set,
                               moreoptions=list(fafbseg.cave.datastack_name=fanc_datastack_name()))
}

banc_datastack_name <- function() 'brain_and_nerve_cord'
banc_scene <- function() "https://spelunker.cave-explorer.org/#!middleauth+https://global.daf-apis.com/nglstate/api/v1/5952656075915264"

#' @rdname choose_fanc
#' @export
choose_banc <- function(set=TRUE) {
  fafbseg::choose_segmentation(banc_scene(),
                               set=set,
                               moreoptions=list(
                                 fafbseg.cave.datastack_name=banc_datastack_name(),
                                 fancr.use_banc=TRUE
                                 ))
}


#' @param expr An expression to evaluate while FANC is the default
#'   autosegmentation
#' @param force Whether to insist on using FANC even if BANC is the active
#'   dataset. This option defaults to TRUE for the convenience of end users.
#' @rdname choose_fanc
#' @export
#' @examples
#' \donttest{
#' with_fanc(fafbseg::flywire_islatest('648518346498254576'))
#' }
#' \dontrun{
#' with_fanc(fafbseg::flywire_latestid('648518346498254576'))
#' with_fanc(fafbseg::flywire_latestid('648518346494405175'))
#' }
with_fanc <- function(expr, force=TRUE) {
  use_banc <- !force && getOption("fancr.use_banc", default = FALSE)
  if(!use_banc) {
    op <- choose_fanc(set = TRUE)
    on.exit(options(op))
  }
  force(expr)
}

fanc_fetch <- function(url, token=fanc_token(), ...) {
  flywire_fetch(url, token=token, ...)
}


#' @description \code{with_banc} provides a simple way to access the
#'   \href{https://github.com/jasper-tms/the-BANC-fly-connectome/wiki}{BANC}
#'   dataset. Just wrap \code{flywire_.*} or \code{fanc_.*} functions with this
#'   to target them at the BANC.
#'
#' @details note that \code{with_banc(fanc_xyz2id)} is not yet functional as
#'   there is no XYZ location / supervoxel id service yet available as yet.
#'
#' @rdname choose_fanc
#' @export
#' @examples
#' \dontrun{
#' # supervoxel id to root id
#' with_banc(fafbseg::flywire_rootid('76071705504180616'))
#' # is this id up to date?
#' with_banc(fafbseg::flywire_islatest('720575941472355131'))
#' # find up to date root id
#' with_banc(fafbseg::flywire_latestid('720575941472355131'))
#' }
#'
with_banc <- function(expr) {
  op <- choose_banc(set = TRUE)
  on.exit(options(op))
  force(expr)
}
