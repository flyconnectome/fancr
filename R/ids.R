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


#' Convert xyz locations to root or supervoxel ids
#'
#' @details This used to be very slow because we do not have a supervoxel
#'   field on spine.
#'
#'   I am somewhat puzzled by the voxel dimensions for FANC. Neuroglancer
#'   clearly shows voxel coordinates of 4.3x4.3x45. But in this function, the
#'   voxel coordinates must be set to 4.25 in x-y to give the correct answers.
#'
#' @param voxdims The voxel dimensions (in nm). See details.
#' @inheritParams fafbseg::flywire_xyz2id
#'
#' @return A character vector of segment ids, NA when lookup fails.
#' @family fanc-ids
#' @seealso \code{\link{flywire_xyz2id}}
#' @export
#' @importFrom nat xyzmatrix
#' @examples
#' # a point from neuroglancer, should map to 648518346479013777
#' fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE)
fanc_xyz2id <- function(xyz, rawcoords=FALSE, voxdims=c(4.3, 4.3, 45),
                        root=TRUE, ...){
  if(is.numeric(xyz) && !is.matrix(xyz) && length(xyz)==3)
    xyz=matrix(xyz, ncol=3)
  if(rawcoords)
    xyz=scale(xyzmatrix(xyz), center = F, scale = 1/voxdims)
  svids=fanc_supervoxels(xyz, voxdims=voxdims)
  if(isTRUE(root)) fanc_rootid(svids) else svids
}

# rawxyz=cbind(34496, 82782, 1954)
# nmxyz=cbind(34496, 82782, 1954)*c(4.3,4.3,45)
fanc_supervoxels <- function(x, voxdims=c(4.3,4.3,45)) {
  pts=scale(xyzmatrix(x), center = F, scale = voxdims)
  nas=rowSums(is.na(pts))>0
  if(any(nas)) {
    svids=rep("0", nrow(pts))
    svids[!nas]=fanc_supervoxels(pts[!nas,,drop=F], voxdims = c(1,1,1))
    return(svids)
  }
  u="https://services.itanna.io/app/transform-service/query/dataset/fanc_v4/s/2/values_array_string_response"
  body=jsonlite::toJSON(list(x=pts[,1], y=pts[,2], z=pts[,3]))
  res=httr::POST(u, body = body)
  httr::stop_for_status(res)
  j=httr::content(res, as='text', encoding = 'UTF-8')
  svids=unlist(jsonlite::fromJSON(j, simplifyVector = T), use.names = F)
  svids
}


fanc_supervoxels_binary <- function(x, voxdims=c(4,4,40)) {
  pts=scale(xyzmatrix(x), center = F, scale = voxdims)
  ptsb=writeBin(as.vector(pts), con = raw(), size=4)
 y
  u="https://spine.janelia.org/app/transform-service/query/dataset/flywire_190410/s/2/values_binary/format/array_float_3xN"

  res=httr::POST(u, body=ptsb, encode = "raw")
  httr::stop_for_status(res)
  arr=httr::content(res)
  bytes=readBin(arr, what = numeric(), n=length(arr)/8, size = 8, endian = 'little')
  class(bytes)="integer64"
  bit64::as.character.integer64(bytes)
}

