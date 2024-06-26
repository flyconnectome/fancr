#' Simplified tissue surface of FANC
#'
#' @name FANC.surf
#' @docType data
#' @description This is unsymmetrical and not normalized version of the mesh. It
#'   is calibrated in nm.
#'
#' @examples
#' \dontrun{
#' # Depends on nat
#' library(nat)
#' wire3d(FANC.surf)
#' }
"FANC.surf"


#' Simplified tissue surface of BANC
#'
#' @name BANC.surf
#' @docType data
#' @description This is unsymmetrical and not normalized version of the mesh. It
#'   is calibrated in nm. See \code{data-raw/BANC.R} for details of how it was
#'   generated.
#'
#' @examples
#' \dontrun{
#' # Depends on nat
#' library(nat)
#' wire3d(BANC.surf, col='grey')
#' }
"BANC.surf"

#' Provisional template brain object for BANC
#'
#' @name BANC
#' @docType data
#' @description This is calibrated in nm. See \code{data-raw/BANC.R} for details
#'   of how it was generated. It is still provisional pending synpase
#'   cloud-based registrations.
#'
#' @examples
#' BANC
#' # Depends on nat
#' library(nat)
#' boundingbox(BANC)
"BANC"

#' Provisional mirroring registration for BANC (lanmarks+thin plate splines)
#'
#' @name mirror_banc_lm
#' @docType data
#' @description This is calibrated in nm. See \code{data-raw/mirror_banc.R} for
#'   details of how it was generated. It is still provisional pending synpase
#'   cloud-based registrations.
#'
#' @examples
#' utils::str(mirror_banc_lm)
"mirror_banc_lm"

