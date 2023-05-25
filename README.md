# fancr

<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://flyconnectome.github.io/fancr/reference/)
[![R-CMD-check](https://github.com/flyconnectome/fanc/workflows/R-CMD-check/badge.svg)](https://github.com/flyconnectome/fanc/actions)
[![R-CMD-check](https://github.com/flyconnectome/fancr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/flyconnectome/fancr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of **fancr** is to support analysis of the Female Adult
Nerve Cord dataset aka (FANC), especially autosegmentation data. Those 
data are made available by the FANC project led by Wei-Chung Allen Lee (Harvard) and his collaborators including John Tuthill and Sebastian Seung/Zetta. 

To access FANC resources, you must have permissions to access the [FANC
autosegmentation
dataset](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1616522511001900)
and have [confirmed your
acceptance](https://fanc-reconstruction.slack.com/archives/C01RZP5JH9C/p1617404290005300)
of the FANC proofreading and data ownership guidelines. At this point you should
have a linked Google account that will be authorised (see below) for access to
FANC online resources.

Broadly speaking the **fancr** package is a thin wrapper over the 
[fafbseg](https://github.com/natverse/fafbseg) package setting up necessary 
default paths etc.

## Installation

You can install the development version of fancr from github:

```r
if(!requireNamespace('natmanager'))
  install.packages('natmanager')
natmanager::install('natverse', pkgs = 'flyconnectome/fancr')

# install required python packages esp cloudvolume/caveclient
fafbseg::simple_python()
```

To do anything useful with the fancr package, you need authorisation to access
FANC resources. To prove your authorisation for programmatic access you must
generate and store a token in your web browser after logging in to an approved
Google account. This should be streamlined by running the following command in R
(which will also set you up for Pythonic access via cloudvolume.)

```r
# set up token - will open your browser to generate a new token
fanc_set_token()
# if you already have one do 
# fanc_set_token("<my token>")
```

To check that everything is set up properly, try:

```r
dr_fanc()

fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE)
svids=fanc_leaves("648518346482929060")
head(svids)
```

### Updating

You can just repeat the install instructions, but this ensures
that all dependencies are updated:

```r
natverse::natverse_update(update = T)
natmanager::install(pkgs = 'flyconnectome/fancr')
fafbseg::simple_python()
```
