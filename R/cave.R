#' Query FANC tables in the CAVE annotation system
#'
#' @param ... Additional arguments passed to
#'   \code{\link[fafbseg]{flywire_cave_query}}
#' @inheritParams fanc_partner_summary
#' @inheritParams fafbseg::flywire_cave_query
#'
#' @return A data.frame
#' @export
#' @seealso \code{\link[fafbseg]{flywire_cave_query}}
#' @examples
#' \donttest{
#' nuclei_aug2021ver2=fanc_cave_query(table = "nuclei_aug2021ver2")
#' head(nuclei_aug2021ver2)
#' hist(nuclei_aug2021ver2$volume)
#' hist(nuclei_aug2021ver2$volume^(1/3))
#' }
#' \dontrun{
#' points3d(xyzmatrix(nuclei_aug2021ver2$pt_position))
#' }
fanc_cave_query <- function(table, datastack_name = NULL, live=TRUE, ...) {
  if(is.null(datastack_name)) datastack_name=fanc_datastack_name()
  fafbseg::flywire_cave_query(table = table, datastack_name = datastack_name, live=live, ...)
}
