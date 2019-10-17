ss3sim
======

master: [![Build Status](https://travis-ci.org/ss3sim/ss3sim.png?branch=master)](https://travis-ci.org/ss3sim/ss3sim) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ss3sim/ss3sim?branch=master&svg=true)](https://ci.appveyor.com/project/ss3sim/ss3sim) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ss3sim)](https://cran.r-project.org/package=ss3sim)

development: [![Build Status](https://travis-ci.org/ss3sim/ss3sim.png?branch=development)](https://travis-ci.org/ss3sim/ss3sim) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/ss3sim/ss3sim?branch=development&svg=true)](https://ci.appveyor.com/project/ss3sim/ss3sim)

ss3sim is an R package that facilitates flexible, rapid, and reproducible fisheries stock assessment simulation testing with the widely-used [Stock Synthesis](https://vlab.ncep.noaa.gov/web/stock-synthesis) (SS) statistical age-structured stock assessment framework.

Contents
--------

-   [Installing the ss3sim R package](#installing-the-ss3sim-r-package)
-   [The ss3sim simulation setup](#the-ss3sim-simulation-setup)
-   [How ss3sim works](#how-ss3sim-works)
-   [Example output from an ss3sim simulation](#example-output-from-an-ss3sim-simulation)
-   [Papers published using ss3sim](#papers-published-using-ss3sim)
-   [Citing ss3sim](#citing-ss3sim)

<!-- end toc -->
Installing the ss3sim R package
-------------------------------

Install the [CRAN version](https://cran.r-project.org/package=ss3sim) of ss3sim with:

``` r
install.packages("ss3sim")
```

Or, install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("ss3sim/ss3sim", 
  ref = "development", build_vignettes = TRUE, dependencies = TRUE)
library(ss3sim)
```

We suggest using the GitHub version because it comes with the SS executable/binary. If you are using the CRAN version, you'll need to install the binary and place it in your system path. See the Introduction vignette with `vignette("introduction", "ss3sim")` for more details on how to get the latest version of SS and place it in your path.

You can read the help files and access the vignettes for reproducible examples of ss3sim simulations with

``` r
?ss3sim
browseVignettes("ss3sim")
```

The ss3sim simulation setup
---------------------------

An ss3sim simulation requires three types of input:

1.  a base model of the underlying truth (an SS operating model)
2.  a base model of how you will assess that truth (an SS estimation model),
3.  a set of cases that deviate from these base models that you want to compare (configuration arguments provided as plain-text control files).

You can find examples of these SS operating and estimation models [within the package data](https://github.com/ss3sim/ss3sim/tree/master/inst/extdata/models). Plain-text case files for some current simulation projects run by the developers of the package are [also available](https://github.com/ss3sim/ss3sim/tree/master/inst/extdata/eg-cases) along with the [case files for the examples](https://github.com/ss3sim/ss3sim/tree/master/inst/extdata/eg-cases) used in the paper and vignette.

![An illustration of the input and output file and folder structure.](https://raw.githubusercontent.com/ss3sim/ss3sim/f763cfb462a9e68db670155070cd554812a65160/man/figures/filestructure.png)

An illustration of the input and output file and folder structure.

How ss3sim works
----------------

ss3sim works by converting simulation arguments (e.g., a given natural mortality trajectory) into manipulations of SS configuration files. It takes care of running the operating and estimation models as well as making these manipulations at the appropriate stage in the simulation.

ss3sim functions are divided into three types:

1.  `change` and `sample` functions that manipulate SS configuration files. These manipulations generate an underlying "truth" (operating models) and control our assessment of those models (estimation models).

2.  `run` functions that conduct simulations. These functions generate a folder structure, call manipulation functions, run SS3 as needed, and save the output.

3.  `get` functions for synthesizing the output.

Example output from an ss3sim simulation
----------------------------------------

![An example of ss3sim output](https://raw.github.com/seananderson/ss3sim/master/inst/ms/fig2-20131109.png)

Example output from an ss3sim simulation. This example shows a crossed simulation in which we considered (1) the effect of fixing natural mortality (*M*) at its true value (0.2; case E0) or estimating *M* (case E1) and (2) the effect of high survey effort (sigma\_survey = 0.1; case D0) or low survey effort (sigma\_survey = 0.4; case D1). Upper panels (blue) show time series of relative error in spawning stock biomass (SSB). Lower panels (grey) show the distribution of relative error across four scalar variables: depletion, *M*, SSB at maximum sustainable yield (SSB\_MSY), and fishing mortality (*F*) in the terminal year. We show the values across simulation iterations with dots and the distributions with beanplots (kernel density smoothers).

Citing ss3sim
-------------

If you use ss3sim in a publication, please cite ss3sim as shown by

``` r
citation("ss3sim")
toBibtex(citation("ss3sim"))
```
