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
#' #nucleus_mar2022=fanc_cave_query(table = "nucleus_mar2022")
#' neuron_somas_dec2022=fanc_cave_query(table = "neuron_somas_dec2022")
#' head(neuron_somas_dec2022)
#' hist(neuron_somas_dec2022$volume)
#' hist(neuron_somas_dec2022$volume^(1/3))
#' }
#' \dontrun{
#' points3d(fanc_raw2nm(neuron_somas_dec2022$pt_position), col='red')
#' }
fanc_cave_query <- function(table, datastack_name = NULL,
                            version=NULL,
                            timestamp=NULL,
                            live=is.null(version),
                            timetravel = FALSE,
                            filter_in_dict = NULL,
                            filter_out_dict = NULL,
                            filter_regex_dict = NULL,
                            select_columns = NULL,
                            offset = 0L,
                            limit = NULL,
                            fetch_all_rows = FALSE,
                            ...) {
  if(is.null(datastack_name)) datastack_name=fanc_datastack_name()
  fafbseg::flywire_cave_query(table = table,
                              datastack_name = datastack_name,
                              version = version,
                              timestamp = timestamp,
                              live=live,
                              timetravel = timetravel,
                              filter_in_dict = filter_in_dict,
                              filter_out_dict = filter_out_dict,
                              filter_regex_dict = filter_regex_dict,
                              select_columns = select_columns,
                              offset = offset,
                              limit = limit,
                              fetch_all_rows = fetch_all_rows,
                              ...)
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
banc_cave_query <- function(table, datastack_name = NULL,
                            version=NULL,
                            timestamp=NULL,
                            live=is.null(version),
                            timetravel = FALSE,
                            filter_in_dict = NULL,
                            filter_out_dict = NULL,
                            filter_regex_dict = NULL,
                            select_columns = NULL,
                            offset = 0L,
                            limit = NULL,
                            fetch_all_rows = FALSE,
                            ...) {
  if(is.null(datastack_name)) datastack_name=banc_datastack_name()
  fafbseg::flywire_cave_query(table = table,
                              datastack_name = datastack_name,
                              version = version,
                              timestamp = timestamp,
                              live=live,
                              timetravel = timetravel,
                              filter_in_dict = filter_in_dict,
                              filter_out_dict = filter_out_dict,
                              filter_regex_dict = filter_regex_dict,
                              select_columns = select_columns,
                              offset = offset,
                              limit = limit,
                              fetch_all_rows = fetch_all_rows,
                              ...)
}

