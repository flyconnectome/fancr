.onLoad <- function(libname, pkgname) {

  # make FANC4<->FANC3 bridging registrations available
  register_fanc3to4()
  packageStartupMessage(
    'Do `choose_fanc()` to use many fafbseg::flywire_* functions!\n',
    'Use dr_fanc() to get a report on your installation.\n',
    'Trouble? Visit https://flyconnectome.github.io/fancr/SUPPORT.html or #code on FANC Slack')

  invisible()
}
