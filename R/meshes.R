#' Read one or more FANC neuron meshes
#'
#' @param ids One or more root ids
#' @param savedir An optional location to save downloaded meshes. This acts as a
#'   simple but effective cache since flywire neurons change id whenever they
#'   are edited.
#' @param ... Additional arguments passed to
#'   \code{fafbseg::\link{read_cloudvolume_meshes}}
#' @inheritParams fafbseg::save_cloudvolume_meshes
#'
#' @return A \code{\link[nat]{neuronlist}} containing one or more \code{mesh3d}
#'   objects. See \code{nat::\link[nat]{read.neurons}} for details.
#' @export
#' @seealso \code{fafbseg::\link{read_cloudvolume_meshes}}
#' @examples
#' \donttest{
#' read_fanc_meshes("648518346482929060")
#' }
read_fanc_meshes <- function(ids, savedir=NULL, format=c("ply", "obj"), ...) {
  format=match.arg(format)
  read_cloudvolume_meshes(ids, savedir = savedir, cloudvolume.url = fanc_cloudvolume_url(), format=format, ...)
}
