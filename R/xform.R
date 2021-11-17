#' Convert XYZ locations between FANC4 (autoseg) and FANC3 (CATMAID)
#'
#' @param xyz An Nx3 matrix of coordinates
#' @param ... additional arguments for \code{pbapply::pblapply} (used to chunk
#'   requests for many points) and eventually the POST command against the
#'   transform services server.
#' @inheritParams fafbseg::flywire2fafb
#' @inheritParams fafbseg::flywire_xyz2id
#'
#' @return a Nx3 matrix of coordinates.
#' @export
#'
#' @examples
#' fanc3=xyzmatrix(cbind(194569.2, 470101.3, 117630))
#' fanc4=cbind(45224, 109317, 2614)*c(4.3,4.3,45)
#' fanc4to3(fanc4)-fanc3
#' # can also pass in raw coordinates e.g. from neuroglancer
#' fanc4to3(c(45224, 109317, 2614), rawcoords = TRUE)
#'
#' # rather small error for the approx inverse in this case
#' fanc4to3(fanc3, swap=TRUE)-fanc4
#' \donttest{
#' # ... so reverse mapping works in this case, but not always
#' fanc_xyz2id(fanc4to3(fanc3, swap = TRUE))
#' }
fanc4to3 <- function(xyz, rawcoords=FALSE, swap=FALSE, chunksize=40e3,
                     method=c("mapmany", "map1"), ...) {
  method=match.arg(method)
  voxdims=c(4.3, 4.3, 45)
  if(!is.matrix(xyz) && is.numeric(xyz) && length(xyz)==3)
    xyz=matrix(xyz, ncol=3)
  if(isTRUE(rawcoords))
    xyz=scale(xyz, center = F, scale=1/voxdims)
  if(swap)
    fafbseg:::warn_hourly("Please note the FANC3->FANC4 transform is wrong but useful!")

  baseurl <- "https://spine.janelia.org/app/transform-service/dataset/fanc_v4_to_v3"
  fafbseg:::mapwrapper(xyz, baseurl=baseurl, method=method, chunksize=chunksize, swap=swap, voxdims=c(4.3, 4.3, 45), ...)
}


register_fanc3to4 <- function() {
  fanc4to3.reg <- nat::reglist(function(xyz, ...) fanc4to3(xyz, ...))
  fanc3to4.reg <- nat::reglist(function(xyz, ...) fanc4to3(xyz, swap=TRUE, ...))
  nat.templatebrains::add_reglist(fanc4to3.reg, sample = 'FANC4',
                                  reference='FANC3')
  nat.templatebrains::add_reglist(fanc3to4.reg, reference = 'FANC4',
                                  sample='FANC3')
}

fanc_to_manc_reg <- function() {
  pkg = utils::packageName()
  landmarks_path <- system.file("reg/MANC_FANC_landmarks_nm.csv",
                                package = pkg, mustWork = TRUE)
  landmarks <- read.csv(landmarks_path)
  tpsreg(
    landmarks[c("x_fanc", "y_fanc", "z_fanc")],
    landmarks[c("x_manc", "y_manc", "z_manc")]
  )
}

#' Transform FANC to MANC
#' @description transforms neurons, surfaces and other point data onto from FANC
#'   to MANC space.
#' @param inverse boolean flag that says whether to swap the registration
#' @param x an object to transform
#' @param ... additional arguments passed to (passed on to \code{\link{xform}})
#' @export
#' @examples
#' \dontrun{
#' library(nat)
#' library(malevnc)
#' FANC.in.manc <- transform_fanc2manc(FANC.surf)
#' # plot MANC and FANC mesh for comparison
#' wire3d(malevnc::MANC.surf, col='grey', add=T)
#' wire3d(FANC.in.manc/1e3, col='blue',add=T)
#' }
transform_fanc2manc <- function(x, inverse = F, ...) {
  reg = fanc_to_manc_reg()
  xform(x, reg=reg, swap=inverse,... )
}
