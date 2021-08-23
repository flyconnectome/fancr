# fancr 0.3.0

* switch to CAVE infrastructure rather than Zetta. See https://global.daf-apis.com/info/ (#1)
* includes update return format for `fanc_change_log()`

You may find that you need to generate a new token with `fanc_set_token()`

# fancr 0.2.0

* Fast `fanc_xyz2id()` mapping using Itanna (spine) services (now thousands per second rather than 10s)
* Added `fanc_change_log()`
* Added `dr_fanc()` function
* Added a `NEWS.md` file to track changes to the package.
