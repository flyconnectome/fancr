#' Read L2 skeleton or dotprops for FANC neurons using fafbseg-py
#'
#' @description \code{fanc_read_l2skel} reads one or more neurons as simplified
#'   L2 skeletons.
#'
#' @description \code{fanc_read_l2dp} reads one or more neurons as simplified
#'   dotprops format. See \code{\link[fafbseg]{read_l2skel}}.
#'
#' @param dataset An optional CAVE dataset name (expert use only, by default
#'   will choose the standard FANC dataset). See details.
#' @inheritParams fafbseg::read_l2skel
#'
#' @details \code{fanc_read_l2dp} uses a special data structure for rapid
#'   download of the dotprops version of neurons required for NBLASTing. It
#'   leverages the python navis / fafbseg-py packages and you will need to
#'   install these, typically using the \code{\link[fafbseg]{simple_python}}
#'   function.
#'
#'   \code{fanc_read_l2skel} treats the dataset argument a little differently
#'   than \code{fanc_read_l2dp} because it actually needs to identify two data sources
#'   a CAVE data
#'
#'   See \code{\link[fafbseg]{read_l2skel}} for additional details of
#'
#' @return a \code{\link{neuronlist}} containing one or more
#'   \code{\link{neuron}} or \code{\link{dotprops}} objects. Note that neurons
#'   will be calibrated in nm while dotprops will be calibrated in microns.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # one time install of necessary python packages
#' fafbseg::simple_python(pkgs="fafbseg")
#'
#' dnp42=c("648518346507131167", "648518346485772414")
#' dnp42.latest=fanc_latestid(dnp42)
#' dnp42.dps <- fanc_read_l2dp(dnp42.latest)
#'
#' # plot those
#' nclear3d()
#' plot3d(dnp42.dps, lwd=3)
#' # nb dotprops are always in microns
#' wire3d(FANC.surf/1e3, col='grey')
#'
#' nclear3d()
#' dnp42.skel <- fanc_read_l2skel(dnp42.latest)
#' plot3d(dnp42.skel, lwd=2)
#' # nb neuron skeletons are in nm
#' wire3d(FANC.surf, col='grey')
#' }
fanc_read_l2dp <- function(id, OmitFailures=TRUE, dataset=NULL, ...) {
  id=fanc_ids(id)
  if(is.null(dataset))
    dataset=with_fanc(getOption("fafbseg.cave.datastack_name"), force = FALSE)
  fafbseg::read_l2dp(id, dataset=dataset, OmitFailures=OmitFailures, ...)
}

#' @export
#' @rdname fanc_read_l2dp
fanc_read_l2skel <- function(id, OmitFailures=TRUE, dataset=NULL, ...) {
  id=fanc_ids(id)
  # partial duplication of fafbseg::read_l2skel for older versions of fafbseg-py
  fp=fafbseg:::check_fafbsegpy()

  if("set_default_dataset" %in% names(fp$flywire)) {
    # new fafbseg-py, everything is simpler
    return(with_fanc(fafbseg::read_l2skel(id), force = FALSE))
  }
  # this used to work with FANC and older fafbseg-py, but not sure if it still works ...
  # manually set the cloudvolume url / cave datastack name
  # see https://flyconnectome.slack.com/archives/C342Q3H4Y/p1694682637820519
  if(is.null(dataset)) {
    ops=choose_fanc(set=F)
    fp$flywire$utils$FLYWIRE_URLS$FANC = ops$fafbseg.cloudvolume.url
    fp$flywire$utils$CAVE_DATASETS$FANC = ops$fafbseg.cave.datastack_name
    dataset="FANC"
  }
  sk=fp$flywire$l2_skeleton(id, omit_failures = OmitFailures, dataset=dataset, ...)
  fafbseg:::navis2nat_neuronlist(sk)
}
