#' Summarise the connectivity of FANC neurons
#'
#' @inheritParams fafbseg::flywire_partner_summary
#' @param datastack_name An optional CAVE datastack_name. If unset a sensible
#'   default is chosen.
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' head(fanc_partner_summary("648518346494405175"))
fanc_partner_summary <- function(rootid, partners = c("outputs", "inputs"),
                                 threshold = 0,
                                 remove_autapses = TRUE,
                                 cleft.threshold = 0,
                                 datastack_name=NULL,
                                 ...) {
  if(is.null(datastack_name))
    datastack_name = fanc_datastack_name()
  with_fanc(
    fafbseg::flywire_partner_summary(
      rootid,
      threshold = threshold,
      method = "cave",
      datastack_name = "fanc_production_mar2021",
      remove_autapses = remove_autapses,
      cleft.threshold = cleft.threshold,
      ...
    )
  )
}

fanc_datastack_name <- memoise::memoise(function() {
  cac=fafbseg::flywire_cave_client(NULL)
  datastacks=cac$info$get_datastacks()
  seldatastack=grep("fanc.*production", datastacks, value = T)
  if(length(seldatastack)==0)
    stop("Could not identify a FANC production datastack amongst: ",
         paste(datastacks, collapse=','))
  if(length(seldatastack)>1)
    warning("Multiple FANF datastacks available; ",
            paste(seldatastack, collapse = ","),"\n",
            "choosing: ", seldatastack[1])
  seldatastack[1]
})
