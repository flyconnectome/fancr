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
#' @param x an object to transform (calibrated in nm)
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


#' Mirror points, neurons in BANC space
#'
#' @details This is mirroring registration is currently not bad in central brain
#' and thoracic ganglion, not so good in abdominal ganglion, very poor in optic
#' lobes.
#'
#' @param x Points, neurons or other objects compatible with \code{xyzmatrix}
#' @param units Units for both input \emph{and} output data.
#' @param subset Optional argument to transform only a subset of a neuron list.
#'
#' @return The transformed object (calibrated according to the units argument)
#' @export
#' @importFrom nat.templatebrains mirror_brain templatebrain
#' @importFrom nat xyzmatrix<-
#' @seealso \code{\link{banc_lr_position}}
#' @examples
#' BANC.surf.m <- mirror_banc(BANC.surf)
#' \dontrun{
#' library(nat)
#' wire3d(BANC.surf)
#' # clearly not great in some places, especially optic lobe, but still useful
#' wire3d(BANC.surf.m, col='red')
#' }
mirror_banc <- function(x, units=c("nm", "microns", "raw"), subset=NULL) {
  # for thin plate splines
  check_package_available("Morpho")
  units=match.arg(units)

  if(!is.null(subset)) {
    xs=x[subset]
    xst=mirror_banc(xs, units = units)
    x[subset]=xst
    return(x)
  }
  BANCmesh=templatebrain("BANC",
                     BoundingBox = structure(c(79392.9, 966179.9, 35524.5, 1131169.6, -62.8, 315466.7
                     ), dim = 2:3, class = "boundingbox"))
  # convert to nm if necessary
  xyz=xyzmatrix(x)
  if(units=='microns')
    xyz=xyz*1e3
  else if(units=='raw')
    xyz=banc_raw2nm(xyz)

  xyzf=mirror_brain(x, brain = BANCmesh, mirrorAxis = 'X', transform = 'flip')
  xyzf2=xform(xyzf, reg = fancr::mirror_banc_lm)
  # convert from nm to original units if necessary
  if(units=='microns')
    xyzf2=xyzf2/1e3
  else if(units=='raw')
    xyzf2=banc_nm2raw(xyzf2)

  xyzmatrix(x)=xyzf2
  x
}


#' Predict whether a point is on the left or right of the BANC dataset
#'
#' @details This is not perfect as it assumes that the X displacement from the
#'   midline is a good indicator of LR displacement. This is generally true but
#'   not infallible. Furthermore it will only be as good as the registration
#'   used by \code{\link{mirror_banc}} (still variable).
#' @param x An object from which xyzmatrix can extract points, calibrated in nm.
#' @param group Whether to return the mean displacement per neuron (when
#'   \code{x} is a \code{neuronlist})
#' @return A vector of point displacements (calibrated according to
#'   \code{units}, nm is the default) where 0 is at the midline and positive
#'   values are to the fly's right.
#' @export
#' @importFrom nat is.neuronlist nvertices
#' @importFrom dplyr group_by summarise
#' @inheritParams mirror_banc
#' @seealso \code{\link{mirror_banc}}
#' @examples
#' library(nat)
#' lrdiffs=banc_lr_position(xyzmatrix(BANC.surf))
#' \dontrun{
#' points3d(xyzmatrix(BANC.surf), col=ifelse(lrdiffs>0, 'green', 'red'))
#' }
banc_lr_position <- function(x, units=c("nm", "microns", "raw"), group=FALSE) {
  xyz=xyzmatrix(x)
  xyzt=mirror_banc(xyz, units = units)
  lrdiff=xyzt[,1]-xyz[,1]
  if(group) {
    if(!is.neuronlist(x))
      stop("I only know how to group results for neuronlists")
    df=data.frame(lrdiff=lrdiff, id=rep(names(x), nvertices(x)))
    dff=summarise(group_by(df, .data$id), lrdiff=mean(lrdiff))
    # group / summarise reorders result ...
    lrdiff=dff$lrdiff[match(names(x), dff$id)]
  }
  lrdiff
}
