#' Query FANC tables in the CAVE annotation system
#'
#' @param ... Additional arguments passed to
#'   \code{\link[fafbseg]{flywire_cave_query}}
#' @inheritParams fanc_partner_summary
#' @inheritParams fafbseg::flywire_cave_query
#'
#' @return A data.frame
#'
#' @family fanc-cave
#' @export
#' @seealso \code{\link[fafbseg]{flywire_cave_query}}
#' @examples
#' \donttest{
#' nuclei_aug2021ver2=fanc_cave_query(table = "nuclei_aug2021ver2")
#' head(neuron_somas_dec2022)
#' hist(neuron_somas_dec2022$volume)
#' hist(neuron_somas_dec2022$volume^(1/3))
#' }
#' \dontrun{
#' points3d(xyzmatrix(nuclei_aug2021ver2$pt_position))
#' }
fanc_cave_query <- function(table, datastack_name = NULL, live=TRUE, ...) {
  if(is.null(datastack_name)) datastack_name=fanc_datastack_name()
  fafbseg::flywire_cave_query(table = table, datastack_name = datastack_name, live=live, ...)
}

#' Low level access to FANC's CAVE annotation infrastructure
#'
#' @return A reticulate R object wrapping the python CAVEclient.
#' @export
#'
#' @examples
#' \donttest{
#' fcc=fanc_cave_client()
#' tables=fcc$annotation$get_tables()
#' fcc$materialize$get_table_metadata(tables[1])
#' }
fanc_cave_client <- function() {
  with_fanc(flywire_cave_client(), force = FALSE)
}

#' @rdname fanc_cave_client
#' @export
banc_cave_client <- function() {
  with_banc(flywire_cave_client())
}

#' @rdname fanc_cave_query
#' @export
#' @examples
#' \dontrun{
#' library(dplyr)
#' cell_info=banc_cave_query('cell_info')
#' cell_info %>%
#'   filter(tag2=='anterior-posterior projection pattern') %>%
#'   count(tag)
#' }
banc_cave_query <- function(table, datastack_name = NULL, live=TRUE, ...) {
  if(is.null(datastack_name)) datastack_name=banc_datastack_name()
  fafbseg::flywire_cave_query(table = table, datastack_name = datastack_name, live=live, ...)
}

