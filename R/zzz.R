.onLoad <- function(libname, pkgname) {

  # make FANC4<->FANC3 bridging registrations available
  register_fanc3to4()

  invisible()
}
