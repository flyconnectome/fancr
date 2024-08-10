# fancr (development version)

# fancr 0.4.0

* Update spine URL for `fanc4to3()` to cope with server move (#5, @jefferis)
* FANC surf mesh by @dokato in https://github.com/flyconnectome/fancr/pull/4
* fanc to manc registration added by @dokato in https://github.com/flyconnectome/fancr/pull/3

## New Contributors
* @dokato made their first contribution in https://github.com/flyconnectome/fancr/pull/4

**Full Changelog**: https://github.com/flyconnectome/fancr/compare/v0.3.0...v0.4.0

# fancr 0.3.0

* switch to CAVE infrastructure rather than Zetta. See https://global.daf-apis.com/info/ (#1)
* includes update return format for `fanc_change_log()`

You may find that you need to generate a new token with `fanc_set_token()`

# fancr 0.2.0

* Fast `fanc_xyz2id()` mapping using Itanna (spine) services (now thousands per second rather than 10s)
* Added `fanc_change_log()`
* Added `dr_fanc()` function
* Added a `NEWS.md` file to track changes to the package.
