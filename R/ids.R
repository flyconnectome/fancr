#' Find the root identifier of a FANC neuron
#'
#' @inheritParams fafbseg::flywire_rootid
#' @param ... Additional arguments passed to \code{pbapply::pbsapply} and
#'   eventually to Python \code{cv$CloudVolume} object.
#' @return A vector of root ids (by default character)
#' @export
#' @family fanc-ids
#' @seealso \code{\link{flywire_rootid}}
#' @examples
#' fanc_rootid("73186243730767724")
fanc_rootid <- function(x, integer64 = FALSE, ...) {
  rid = flywire_rootid(
    x = x,
    integer64 = integer64,
    method = "cloudvolume",
    # agglomerate = T,
    cloudvolume.url = fanc_cloudvolume_url(),
    ...
  )
  rid
}

#' Find the supervoxel identifiers of a FANC neuron
#'
#' @param ... additional arguments passed to \code{\link{flywire_leaves}}
#' @inheritParams fafbseg::flywire_leaves
#'
#' @return A vector of supervoxel ids
#' @family fanc-ids
#' @seealso \code{\link{flywire_leaves}}
#' @export
#'
#' @examples
#' \dontrun{
#' svids=fanc_leaves("648518346482929060")
#' head(svids)
#' }
fanc_leaves <- function(x, integer64=TRUE, ...) {
  svids=flywire_leaves(x=x, integer64 = integer64, cloudvolume.url = fanc_cloudvolume_url(), ...)
  svids
}


#' Convert xyz location to a root or supervoxel id
#'
#' @details This is currently very slow because we do not have a supervoxel
#'   field on spine.
#'
#' @param voxdims The voxel dimensions (in nm)
#' @inheritParams fafbseg::flywire_xyz2id
#'
#' @return A character vector of segment ids, NA when lookup fails.
#' @family fanc-ids
#' @seealso \code{\link{flywire_xyz2id}}
#' @export
#'
#' @examples
#' # a point from neuroglancer, should map to 648518346479013777
#' fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE)
fanc_xyz2id <- function(xyz, rawcoords=FALSE, voxdims=c(4.25, 4.25, 45),
                        root=TRUE, ...){
  ids=flywire_xyz2id(xyz, rawcoords = rawcoords, voxdims = voxdims,
                     cloudvolume.url = fanc_cloudvolume_url(), method = 'cloudvolume')
  ids
}

