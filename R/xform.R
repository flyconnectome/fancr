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

  baseurl <- "https://spine.itanna.io/app/transform-service/dataset/fanc_v4_to_v3"
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

# internal function to return a CMTK mirroring registration
mirror_fanc_reglist <- function() {
  pkg = utils::packageName()
  mirror_landmarks_path <- system.file("reg/FANC_mirror_landmarks.csv",
                    package = pkg, mustWork = TRUE)
  mirror_landmarks <- read.csv(mirror_landmarks_path)
  mirror_reg_f <- tpsreg(
    mirror_landmarks[c("x_flip", "y_flip", "z_flip")],
    mirror_landmarks[c("x_mirr", "y_mirr", "z_mirr")]
  )
  mirror_reg_f
}

#' Mirror points or other 3D objects along the FANC midline
#'
#' @details These registration functions depend on an installation of the CMTK
#'   registration toolkit. See \code{\link[nat]{cmtk.bindir}} for details.
#' @param x 3D vertices (or object containing them) in FANC space Could be
#'   \code{\link{neuron}}, \code{\link{neuronlist}}, \code{\link{hxsurf}} etc.
#' @param ... additional arguments passed to \code{\link{xform}} and friends
#'
#' @return Transformed points/object
#' @export
#'
#' @examples
#' \donttest{
#' # TODO
#' }
#'
#' @importFrom nat reglist xform invert_reglist
#' @importFrom nat.templatebrains as.templatebrain mirror_brain
mirror_fanc <- function(x, ...) {
  mirror_reg_f <- mirror_fanc_reglist()
  xt <- xform(x, reg=mirror_reg_f, ... )
  xtm <- mirror_brain(xt, brain = FANCsym, mirrorAxis = 'X', transform='flip', ...)
  xtm
}


#' @rdname mirror_fanc
#' @description \code{symmetric_fanc} transforms neurons, surfaces and other
#'   point data onto a symmetrised version of the FANC template brain,
#'   optionally mirroring across the midline.
#' @param mirror Whether to mirror across the midline when using
#'   \code{symmetric_fanc}
#' @export
#' @examples
#' \dontrun{
#' FANC.surf.symm <- symmetric_fanc(FANC.surf)
#' # plot the two meshes: before and after
#' wire3d(FANC.surf, col='grey')
#' wire3d(FANC.surf.symm, col='red')
#' }
symmetric_fanc <- function(x, mirror=FALSE, ...) {
  mirror_reg_f=mirror_fanc_reglist()
  xt=xform(x, reg=mirror_reg_f, swap=T,... )
  if(isTRUE(mirror))
    xt=mirror_brain(xt, brain = FANCsym, mirrorAxis = 'X', transform='flip')
  xt
}

#' FANC symmetric template
#' @export
FANCsym = structure(
  list(
    name = "FANC-symmetric",
    regName = "FANCsym",
    type = "Synthetic average brain from synaptic predictions based on FANC SEM data",
    sex = "F",
    dims = c(672L, 830L, 1280L),
    voxdims = c(0.512,
                0.512, 0.512),
    origin = c(0, 0, 0),
    BoundingBox = structure(
      c(0,
        343.552, 0, 424.448, 0, 654.848),
      .Dim = 2:3,
      class = "boundingbox"
    ),
    units = NULL,
    description = NULL,
    doi = NULL
  ),
  class = "templatebrain"
)


#' @importFrom utils read.csv
#' @importFrom nat tpsreg
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
#' @importFrom nat xform
transform_fanc2manc <- function(x, inverse = F, ...) {
  reg = fanc_to_manc_reg()
  xform(x, reg=reg, swap=inverse,... )
}
