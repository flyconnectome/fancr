#' Summarise the connectivity of FANC neurons
#'
#' @details note that the rootids you pass in must be up to date. See example.
#'
#' @param rootids Character vector specifying one or more flywire rootids. As a
#'   convenience this argument is passed to \code{\link{fanc_ids}} allowing you
#'   to pass in data.frames, flywire URLs or simple ids.
#' @param datastack_name An optional CAVE \code{datastack_name}. If unset a
#'   sensible default is chosen.
#' @inheritParams fafbseg::flywire_partner_summary
#'
#' @return a data.frame
#' @seealso \code{\link{flywire_partner_summary}}, \code{\link{fanc_latestid}}
#' @export
#'
#' @examples
#' # NB id must be up to date
#' sample_id=fanc_latestid("648518346481082458")
#' head(fanc_partner_summary(sample_id))
#' head(fanc_partner_summary(sample_id, partners='inputs'))
#' \dontrun{
#' # get the latest id for an outdate
#' fanc_partner_summary(fanc_latestid("648518346473954669"))
#'
#' ## open fanc/flywire scene containing top partners
#' library(dplyr)
#' fanc_partner_summary(fanc_latestid("648518346494405175"), partners='inputs') %>%
#'   slice_max(weight, n = 20) %>%
#'   fanc_scene(open=TRUE)
#' }
fanc_partner_summary <- function(rootids, partners = c("outputs", "inputs"),
                                 threshold = 0,
                                 remove_autapses = TRUE,
                                 cleft.threshold = 0,
                                 datastack_name=NULL,
                                 ...) {
  if(is.null(datastack_name))
    datastack_name <- if(isTRUE(getOption("fancr.use_banc"))) banc_datastack_name()
    else fanc_datastack_name()
  with_fanc(force = FALSE,
    fafbseg::flywire_partner_summary(
      rootids,
      threshold = threshold,
      partners=partners,
      method = "cave",
      datastack_name = datastack_name,
      remove_autapses = remove_autapses,
      cleft.threshold = cleft.threshold,
      ...
    )
  )
}

fanc_datastack_name <- memoise::memoise(function() {
  cac=fafbseg::flywire_cave_client(NULL)
  datastacks=cac$info$get_datastacks()
  seldatastack=grep("fanc.*production", datastacks, value = T)
  if(length(seldatastack)==0)
    stop("Could not identify a FANC production datastack amongst: ",
         paste(datastacks, collapse=','),
         "\nHave you been granted access to FANC production?")
  if(length(seldatastack)>1)
    warning("Multiple FANC datastacks available; ",
            paste(seldatastack, collapse = ","),"\n",
            "choosing: ", seldatastack[1])
  seldatastack[1]
})


#' @description \code{fanc_partners} returns details of each unitary synaptic
#' connection (including its xyz location).
#'
#' @export
#'
#' @rdname fanc_partner_summary
#' @examples
#' \dontrun{
#' # plot input and output synapses of a neuron
#' nclear3d()
#' fpi=fanc_partners(fanc_latestid("648518346481082458"), partners='in')
#' points3d(fanc_raw2nm(fpi$post_pt_position), col='cyan')
#' fpo=fanc_partners(fanc_latestid("648518346481082458"), partners='out')
#' points3d(fanc_raw2nm(fpo$pre_pt_position), col='red')
#' }
fanc_partners <- function(rootids, partners=c("input", "output"), ...) {
  partners=match.arg(partners)
  rootids=fanc_ids(rootids)
  fcc=fanc_cave_client()
  pyids=fafbseg:::rids2pyint(rootids)
  res=if(partners=='input') {
    reticulate::py_call(fcc$materialize$synapse_query, post_ids=pyids, ...)
  } else {
    reticulate::py_call(fcc$materialize$synapse_query, pre_ids=pyids, ...)
  }
  fafbseg:::pandas2df(res)
}
