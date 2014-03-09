# ss3sim: An R package for stock assessment simulation with Stock Synthesis

ss3sim facilitates flexible, rapid, and reproducible fisheries stock assessment simulation testing with the widely-used [Stock Synthesis 3][SS3] (SS3) statistical age-structured stock assessment framework.

## Installing the ss3sim R package

Install the [CRAN version](http://cran.r-project.org/web/packages/ss3sim/index.html) of ss3sim with:

```S
install.packages("ss3sim")
```

Or, install the development version from GitHub:

```S
install.packages("devtools")
devtools::install_github("ss3sim/ss3sim", dependencies = TRUE)
```

If you would like to run simulations in parallel, then also run:

```S
install.packages(c("doParallel", "foreach"))
```

You can then load ss3sim with:

```S
library(ss3sim)
```

You can read the help files and access the vignette with:

```S
?ss3sim
vignette("ss3sim-vignette", package = "ss3sim")
```

For reproducible examples of ss3sim simulations as well as details on installing
the correct version of SS3, and adding SS3 to your operating system's path so
ss3sim can find the software, see the [vignette][vignette] [PDF]. Also, see the
[paper on arXiv][paper], which in press at PLOS ONE.

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

![An illustration of the input and output file and folder structure.](https://raw2.github.com/ss3sim/ss3sim/master/man/figures/filestructure.png)

An illustration of the input and output file and folder structure.

## How ss3sim works

ss3sim works by converting simulation arguments (e.g. a given natural
mortality trajectory) into manipulations of SS3 configuration files. It
takes care of running the operating and estimation models as well as making
these manipulations at the appropriate stage in the simulation.

ss3sim functions are divided into three types:

1. `change` functions that manipulate SS configuration files. These
   manipulations generate an underlying "truth" (operating models) and control
   our assessment of those models (estimation models).

2. `run` functions that conduct simulations. These functions generate a folder
   structure, call manipulation functions, run SS3 as needed, and save the
   output.

3. `get` functions for synthesizing the output.

The main simulation function, `run_ss3sim()` runs the following steps:

![An illustration of the ss3sim simulation structure and relevant R functions](https://raw.github.com/seananderson/ss3sim/master/inst/ms/sim-steps.png)

An illustration of the ss3sim simulation structure and relevant R functions

## Example output from an ss3sim simulation

![An example of ss3sim output](https://raw.github.com/seananderson/ss3sim/master/inst/ms/fig2-20131109.png)

Example output from an ss3sim simulation. This example shows a crossed simulation in which we considered (1) the effect of fixing natural mortality (*M*) at its true value (0.2; case E0) or estimating *M* (case E1) and (2) the effect of high survey effort (sigma_survey = 0.1; case D0) or low survey effort (sigma_survey = 0.4; case D1). Upper panels (blue) show time series of relative error in spawning stock biomass (SSB). Lower panels (grey) show the distribution of relative error across four scalar variables: depletion, *M*, SSB at maximum sustainable yield (SSB_MSY), and fishing mortality (*F*) in the terminal year. We show the values across simulation iterations with dots and the distributions with beanplots (kernel density smoothers).

## Citing ss3sim

If you use ss3sim in a publication, please cite ss3sim as shown by `citation("ss3sim")`:

Anderson, SC, Monnahan, CC, Johnson, KF, Ono, K, Valero, JL, Cunningham, CJ, Hurtado-Ferro, F, Licandeo, R, McGilliard, CR, Szuwalski, CS, Vert-pre, KA, and Whitten, AR (2014). ss3sim: Fisheries stock assessment simulation testing with Stock Synthesis. R package version 0.8.2.

Anderson, SC, Monnahan, CC, Johnson, KF, Ono, K, and Valero, JL (2014). ss3sim: An R package for fisheries stock assessment simulation with Stock Synthesis. PLOS ONE. In press. DOI: 10.1371/journal.pone.0092725.

```tex
@Manual{,
  title = {ss3sim: Fisheries stock assessment simulation with Stock Synthesis},
  author = {Sean C. Anderson and Cole C. Monnahan and Kelli F. Johnson and Kotaro Ono and Juan L. Valero and Curry J. Cunningham and Felipe Hurtado-Ferro and Roberto Licandeo and Carey R. McGilliard and Cody S. Szuwalski and Katyana A. Vert-pre and Athol R. Whitten},
  year = {2014},
  note = {R package version 0.8.2},
}

@Article{,
  title = {ss3sim: An R package for fisheries stock assessment simulation with Stock Synthesis},
  author = {Sean C. Anderson and Cole C. Monnahan and Kelli F. Johnson and Kotaro Ono and Juan L. Valero},
  year = {2014},
  journal = {PLOS ONE},
  doi = {10.1371/journal.pone.0092725},
}
```

[DESCRIPTION]: https://github.com/seananderson/ss3sim/blob/master/DESCRIPTION
[models]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/models
[cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/cases
[eg-cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/eg-cases
[vignette]: https://dl.dropboxusercontent.com/u/254940/ss3sim-vignette.pdf
[paper]: http://arxiv.org/abs/1312.6450
[SS3]: http://nft.nefsc.noaa.gov/Stock_Synthesis_3.htm
[r-project]: http://www.r-project.org/
[SAFS]: http://fish.washington.edu/
