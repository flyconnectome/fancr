
# fanc

<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
<!-- [![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://flyconnectome.github.io/fanc/reference/) -->
[![R-CMD-check](https://github.com/flyconnectome/fanc/workflows/R-CMD-check/badge.svg)](https://github.com/flyconnectome/fanc/actions)
<!-- badges: end -->

The goal of *fanc* is to provide a package to support analysis of the Full Adult
Female Nerve Cord dataset aka (FANC), especially autosegmentation data. Those 
data are made available by the FANC project led by Wei-Chung Allen Lee (Harvard) and his collaborators including John Tuthill and Sebastian Seung/Zetta. 

To access FANC resources, you must have permissions to access the [FANC
autosegmentation
dataset](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900)
and have [confirmed your
acceptance](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1617404290005300)
of the FANC proofreading and data ownership guidelines. At this point you should
have a linked Google account that will be authorised (see below) for access to
FANC online resources.

Broadly speaking the *fanc* package is a thin wrapper over the 
[fafbseg](https://github.com/natverse/fafbseg) package setting up necessary 
default paths etc.

## Installation

You can install the development version of fanc from github:

```r
install.packages(natmanager)
natmanager::install(pkgs="flyconnectome/fanc")

# install required python packages esp cloudvolume
fafbseg::simple_python()
```
To prove your authorisation for programmatic access you must generate and store
a token in your web browser after logging in to an approved Google account.

This should be streamlined by running the following command in R (which will 
also set you up for Pythonin access via cloudvolume.)
```r
# set up token - will open your browser to generate a new token
fanc_set_token()
# if you already have one do 
# fanc_set_token("<my token>")
```

To check that everything is working properly, try:

```r
fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE)
svids=fanc_leaves("648518346482929060")
head(svids)
```
