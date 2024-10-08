# fancr 0.5.1

* update `fanc_cave_query()` and `banc_cave_query()` to match changes in the 
  underlying `fafbseg::flywire_cave_query()` crucially changing default from
  `live=TRUE` to `live=!is.null(version)`.
* By insisting on fafbseg >= 0.15.0 also provides support for new timetravel
  argument / and CAVE live live query.

# fancr 0.5.0

## What's Changed
* add fanc_cellid_table() and use it for cell ids by @jefferis in https://github.com/flyconnectome/fancr/pull/7
* basic support for the BANC via `with_banc()`, `banc_cave_query()`, `banc_cave_client()` by @jefferis in https://github.com/flyconnectome/fancr/pull/8
* fix `fanc_partner_summary()` for banc data
* fix `fanc_cellid_from_segid()` for when rootids=NULL

**Full Changelog**: https://github.com/flyconnectome/fancr/compare/v0.4.0...v0.5.0

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
