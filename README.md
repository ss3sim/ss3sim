# ss3sim

[![Build Status](https://travis-ci.org/ss3sim/ss3sim.png?branch=master)](https://travis-ci.org/ss3sim/ss3sim)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ss3sim)](http://cran.r-project.org/package=ss3sim)

ss3sim is an R package that facilitates flexible, rapid, and reproducible fisheries stock assessment simulation testing with the widely-used [Stock Synthesis 3][SS3] (SS3) statistical age-structured stock assessment framework.

## Contents

- [Installing the ss3sim R package](#installing-the-ss3sim-r-package)
- [The ss3sim simulation setup](#the-ss3sim-simulation-setup)
- [How ss3sim works](#how-ss3sim-works)
- [Example output from an ss3sim simulation](#example-output-from-an-ss3sim-simulation)
- [Papers published using ss3sim](#papers-published-using-ss3sim)
- [Citing ss3sim](#citing-ss3sim)

<!-- end toc -->

## Installing the ss3sim R package

Install the [CRAN version](http://cran.r-project.org/package=ss3sim) of ss3sim with:

```R
install.packages("ss3sim")
```

Or, install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("ss3sim/ss3sim") # without vignettes (faster)
devtools::install_github("ss3sim/ss3sim", build_vignettes = TRUE) # with vignettes
```

There has been substantial work on the GitHub version since the latest CRAN release, so we'd suggest using the GitHub version.

If you would like to run simulations in parallel, then also run:

```R
install.packages(c("doParallel", "foreach"))
```

You can then load ss3sim with:

```R
library("ss3sim")
```

You can read the help files with:

```R
?ss3sim
```

and access the vignettes for reproducible examples of ss3sim simulations with:

```R
browseVignettes("ss3sim")
```

If you're using the GitHub version of ss3sim then the SS3 executables/binaries are included in the package and your installation is complete. If you're using the CRAN version, you'll need to install these binaries and place them in your system path. ss3sim requires a specific version of the SS3 binary/executable. With the permission of Rick Methot, we have hosted those files [here](https://github.com/ss3sim/ss3sim/tree/master/inst/bin). See the Introduction vignette with `vignette("introduction", "ss3sim")`.

In addition to the vignette, we published a [paper][paper] in PLOS ONE, which describes the package.

## The ss3sim simulation setup

An ss3sim simulation requires three types of input:

1. a base model of the underlying truth (an SS3 operating model)
2. a base model of how you will assess that truth (an SS3 estimation model),
3. a set of cases that deviate from these base models that you want to compare (configuration arguments provided as plain-text control files).

You can find examples of these SS3 operating and estimation models [within the
package data][models]. Plain-text case files for some current simulation
projects run by the developers of the package are [also available][cases]
along with the [case files for the examples][eg-cases] used in the paper and
vignette.

![An illustration of the input and output file and folder structure.](https://raw.githubusercontent.com/ss3sim/ss3sim/f763cfb462a9e68db670155070cd554812a65160/man/figures/filestructure.png)

An illustration of the input and output file and folder structure.

## How ss3sim works

ss3sim works by converting simulation arguments (e.g. a given natural
mortality trajectory) into manipulations of SS3 configuration files. It
takes care of running the operating and estimation models as well as making
these manipulations at the appropriate stage in the simulation.

ss3sim functions are divided into three types:

1. `change` and `sample` functions that manipulate SS configuration files. These
   manipulations generate an underlying "truth" (operating models) and control
   our assessment of those models (estimation models).

2. `run` functions that conduct simulations. These functions generate a folder
   structure, call manipulation functions, run SS3 as needed, and save the
   output.

3. `get` functions for synthesizing the output.

## Example output from an ss3sim simulation

![An example of ss3sim output](https://raw.github.com/seananderson/ss3sim/master/inst/ms/fig2-20131109.png)

Example output from an ss3sim simulation. This example shows a crossed simulation in which we considered (1) the effect of fixing natural mortality (*M*) at its true value (0.2; case E0) or estimating *M* (case E1) and (2) the effect of high survey effort (sigma_survey = 0.1; case D0) or low survey effort (sigma_survey = 0.4; case D1). Upper panels (blue) show time series of relative error in spawning stock biomass (SSB). Lower panels (grey) show the distribution of relative error across four scalar variables: depletion, *M*, SSB at maximum sustainable yield (SSB_MSY), and fishing mortality (*F*) in the terminal year. We show the values across simulation iterations with dots and the distributions with beanplots (kernel density smoothers).

## Papers published using ss3sim

Kuriyama, P. T., K. Ono, F. Hurtado-Ferro, A. C. Hicks, I. G. Taylor, R. R.
Licandeo, K. F. Johnson, S. C. Anderson, C. C. Monnahan, M. B. Rudd, C. C.
Stawitz, and J. L. Valero. (2016). An empirical weight-at-age approach reduces
estimation bias compared to modeling parametric growth in integrated,
statistical stock assessment models when growth is time varying. Fisheries
Research. In press. <http://doi.org/10.1016/j.fishres.2015.11.002>.
([code repository](https://github.com/ss3sim/Empirical)).

Monnahan, C. C., K. Ono, S. C. Anderson, M. B. Rudd, A. C. Hicks, F.
Hurtado-Ferro, K. F. Johnson, P. T. Kuriyama, R. R. Licandeo, C. C. Stawitz, I.
G. Taylor, and J. L. Valero. (2016). The effect of length bin width on growth
estimation in integrated age-structured stock assessments. Fisheries Research.
In press. <http://doi.org/10.1016%2Fj.fishres.2015.11.002>. 
([code repository](https://github.com/ss3sim/binning)).

Johnson, K.F., C.C. Monnahan, C.R. McGilliard, K.A. Vert-pre, S.C. Anderson, C.J. Cunningham, F. Hurtado-Ferro, R.R. Licandeo, M.L. Muradian, K. Ono, C.S. Szuwalski, J.L. Valero, A.R. Whitten, A.E. Punt. 2015. Time-varying natural mortality in fisheries stock assessment models: identifying a default approach.  2015. ICES Journal of Marine Science 72 (1): 137-150 <http://doi.org/10.1093/icesjms/fsu055>.
([PDF](http://icesjms.oxfordjournals.org/content/early/2014/04/09/icesjms.fsu055.full.pdf?keytype=ref&ijkey=NEXmZIkz3289u3z); [code repository](https://github.com/ss3sim/natural-mortality "R code to recreate the simulation")).

Hurtado-Ferro, F., C.S. Szuwalski, J.L. Valero, S.C. Anderson, C.J. Cunningham, K.F. Johnson, R.R. Licandeo, C.R. McGilliard, C.C. Monahan, M.L. Muradian, K. Ono, K.A. Vert-Pre, A.R. Whitten, A.E. Punt. 2015. Looking in the rear-view mirror: bias and retrospective patterns in integrated, age-structured stock assessment models. ICES Journal of Marine Science. 72 (1): 99-110 <http://doi.org/10.1093/icesjms/fsu198>.

Ono, K., R. Licandeo, M.L. Muradian, C.J. Cunningham, S.C. Anderson, F. Hurtado-Ferro, K.F. Johnson, C.R. McGilliard, C.C. Monnahan, C.S. Szuwalski, J.L. Valero, K.A. Vert-pre, A.R. Whitten, A.E. Punt. 2015. The importance of length and age composition data in statistical catch-at-age models for marine species. ICES Journal of Marine Science. 72 (1): 31-43. <http://doi.org/10.1093/icesjms/fsu007> ([PDF](https://dl.dropboxusercontent.com/u/254940/papers/Ono_etal_2014_importance_of_length_and_age_composition_data.pdf)).

Anderson, S.C., C.C. Monnahan, K.F. Johnson, K. Ono, J.L. Valero. ss3sim: An R package for fisheries stock assessment simulation with Stock Synthesis. 2014. PLOS ONE. 9(4): e92725. <http://doi.org/10.1371/journal.pone.0092725> ([PDF](http://www.plosone.org/article/fetchObject.action?uri=info%3Adoi%2F10.1371%2Fjournal.pone.0092725&representation=PDF)).

## Citing ss3sim

If you use ss3sim in a publication, please cite ss3sim as shown by `citation("ss3sim")`:

Anderson, SC, Monnahan, CC, Johnson, KF, Ono, K, Valero, JL,
  Cunningham, CJ, Hurtado-Ferro, F, Kuriyama, P, Licandeo, R,
  McGilliard, CR, Rudd, M, Szuwalski, CS, Taylor, IG, Vert-pre, KA, and
  Whitten, AR (2015). ss3sim: Fisheries Stock Assessment Simulation
  Testing with Stock Synthesis. R package version 0.9.0.

Anderson, SC, Monnahan, CC, Johnson, KF, Ono, K, and Valero, JL (2014). ss3sim: An R package for fisheries stock assessment simulation with Stock Synthesis. PLOS ONE. 9(4): e92725. DOI: 10.1371/journal.pone.0092725.


<!-- toBibtex(citation("ss3sim")) -->

```tex
@Manual{,
  title = {ss3sim: Fisheries Stock Assessment Simulation Testing with Stock Synthesis},
  author = {Sean C. Anderson and Cole C. Monnahan and Kelli F. Johnson and Kotaro Ono and Juan L. Valero and Curry J. Cunningham and Felipe Hurtado-Ferro and Peter Kuriyama and Roberto Licandeo and Carey R. McGilliard and Merrill Rudd and Cody S. Szuwalski and Ian G. Taylor and Katyana A. Vert-pre and Athol R. Whitten},
  year = {2016},
  note = {R package version 0.9.1},
}

@Article{,
  title = {ss3sim: An {R} package for fisheries stock assessment simulation with {Stock Synthesis}},
  author = {Sean C. Anderson and Cole C. Monnahan and Kelli F. Johnson and Kotaro Ono and Juan L. Valero},
  year = {2014},
  journal = {PLOS ONE},
  volume = {9},
  number = {4},
  pages = {e92725},
  doi = {10.1371/journal.pone.0092725},
}
```

[DESCRIPTION]: https://github.com/seananderson/ss3sim/blob/master/DESCRIPTION
[models]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/models
[cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/cases
[eg-cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/eg-cases
[vignette]: https://dl.dropboxusercontent.com/u/254940/ss3sim-vignette.pdf
[paper]: http://www.plosone.org/article/info%3Adoi%2F10.1371%2Fjournal.pone.0092725
[SS3]: http://nft.nefsc.noaa.gov/Stock_Synthesis_3.htm
[r-project]: http://www.r-project.org/
[SAFS]: http://fish.washington.edu/
