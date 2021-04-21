
# fanc

<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
<!-- [![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://flyconnectome.github.io/fanc/reference/) -->
[![R-CMD-check](https://github.com/flyconnectome/fanc/workflows/R-CMD-check/badge.svg)](https://github.com/flyconnectome/fanc/actions)
<!-- badges: end -->

The goal of *fanc* is to provide a package to support analysis of the Full Adult
Female Nerve Cord dataset aka (FANC), especially autosegmentation data.
Note that this package points to private resources
made available by the FANC project led by Wei-Chung Allen Lee (Harvard) and his 
collaborators including John Tuthill and Sebastian Seung/Zetta.
You must only share with people who have permissions to access the [FANC
autosegmentation dataset](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900) and have
[confirmed their agreement](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1617404290005300) with the 
[FANC proofreading and data ownership guidelines](https://docs.google.com/document/d/1cmAdQIWjZ9Ql-oTCxT6xFtPtGyckGoa9TSLwsu921lk/edit).

Broadly speaking this is designed to be a thin wrapper over the 
[fafbseg](https://github.com/natverse/fafbseg) package setting up necessary 
default paths etc.

## Installation

You can install the development version of fanc from github:

``` r
install.packages(natmanager)
natmanager::install(pkgs="flyconnectome/fanc")
```

Note that you must have a GitHub Personal Access Token (PAT) set up in order
to install the library for as long as it remains private. Do :

```
natmanager::check_pat()
```

to check and follow the instructions if necessary to create. 
See https://usethis.r-lib.org/articles/articles/git-credentials.html for the 
gory details.

