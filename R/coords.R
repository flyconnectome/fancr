#' Handle raw and nm calibrated FANC coordinates
#'
#' @description \code{fanc_voxdims} returns the image voxel dimensions which
#'   are normally used to scale between \bold{raw} and \bold{nm} coordinates.
#'
#' @param url Optional neuroglancer URL containing voxel size. Defaults to
#'   \code{getOption("fafbseg.sampleurl")} as set by
#'   \code{\link{choose_fanc}}.
#'
#' @return For \code{fanc_voxdims} A 3-vector
#' @export
#'
#' @examples
#' fanc_voxdims()
fanc_voxdims <- memoise::memoise(function(url=choose_fanc(set=FALSE)[['fafbseg.sampleurl']]) {
  fafbseg::flywire_voxdims(url)
})


#' @description \code{banc_voxdims} returns the image voxel dimensions for the
#'   BANC which are normally used to scale between raw and nm coordinates.
#'
#' @rdname fanc_voxdims
#' @export
banc_voxdims <- memoise::memoise(function(url=choose_banc(set=FALSE)[['fafbseg.sampleurl']]) {
  fafbseg::flywire_voxdims(url)
})


#' @param x 3D coordinates in any form compatible with \code{\link{xyzmatrix}}
#'
#' @return for \code{fanc_raw2nm} and \code{fanc_nm2raw} an Nx3 matrix of
#'   coordinates
#' @param vd The voxel dimensions in nm. Expert use only. Normally found
#'   automatically.
#' @export
#' @rdname fanc_voxdims
#' @details relies on nat >= 1.10.4
#' @examples
#' fanc_raw2nm(c(159144, 22192, 3560))
#' fanc_raw2nm('159144 22192 3560')
#' \dontrun{
#' fanc_nm2raw(clipr::read_clip())
#' }
fanc_nm2raw <- function(x, vd=fanc_voxdims()) {
  xyz=xyzmatrix(x)
  xyz[,1]=xyz[,1]/vd[1]
  xyz[,2]=xyz[,2]/vd[2]
  xyz[,3]=xyz[,3]/vd[3]
  xyz
}

#' @export
#' @rdname fanc_voxdims
fanc_raw2nm <- function(x, vd=fanc_voxdims()) {
  xyz=xyzmatrix(x)
  xyz[,1]=xyz[,1]*vd[1]
  xyz[,2]=xyz[,2]*vd[2]
  xyz[,3]=xyz[,3]*vd[3]
  xyz
}

#' @export
#' @rdname fanc_voxdims
banc_raw2nm <- function(x, vd=banc_voxdims()) fanc_raw2nm(x, vd=vd)

#' @export
#' @rdname fanc_voxdims
banc_nm2raw <- function(x, vd=banc_voxdims()) fanc_nm2raw(x, vd=vd)

