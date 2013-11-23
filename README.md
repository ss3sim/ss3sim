## ss3sim: An R package for stock-assessment simulation with Stock Synthesis

`ss3sim` is a software package for the statistical programming language
[R][r-project] that facilitates large-scale, rapid, and reproducible
stock-assessment simulation with the widely-used [Stock Synthesis 3][SS3]
(SS3) statistical catch-at-age stock-assessment framework.

`ss3sim` was developed by graduate students and post doctoral researchers at
the University of Washington in the [School of Aquatic and Fishery
Sciences][SAFS]. The authors of individual functions are listed within the
documentation and all contributors are listed in the [DESCRIPTION
file][DESCRIPTION].

An `ss3sim` simulation requires three types of input: (1) a base model of the
underlying truth (an SS3 operating model), (2) a base model of how you will
assess that truth (an SS3 estimation model), (3) and a set of cases that
deviate from these base models that you want to compare (configuration
arguments provided as plain-text control files).

You can find examples of these SS3 operating and estimation models [within the
package data][models]. Plain-text case files for current simulation
projects run by the developers of the package are [also available][cases] and
the case files for the examples used in the paper and vignette 
[available][eg-cases].

`ss3sim` works by converting simulation arguments (e.g. a given natural
mortality trajectory) into manipulations of `SS3` configuration files. It
takes care of running the operating and estimation models as well as making
these manipulations at the appropriate stage in the simulation.

`ss3sim` functions are divided into three types:

1. `change` functions that manipulate SS configuration files. These
   manipulations generate an underlying "truth" (operating models) and control
   our assessment of those models (estimation models).

2. `run` functions that conduct simulations. These functions generate a folder
   structure, call manipulation functions, run `SS3` as needed, and save the
   output.

3. `get` functions for synthesizing the output.

![An illustration of the `ss3sim` simulation structure and relevant R functions](https://raw.github.com/seananderson/ss3sim/master/inst/ms/sim-steps.png)

An illustration of the `ss3sim` simulation structure and relevant R functions

## Installing the ss3sim R package

The package can be installed and loaded with:

```S
# Dependencies if needed:
install.packages(c("r4ss", "plyr", "gtools", "ggplot2", "lubridate", "reshape2"))

# If you would like to run simulations in parallel:
install.packages("doParallel", "foreach")

# Install devtools to install ss3sim directly from GitHub:
install.packages("devtools")

# Install ss3sim:
devtools::install_github("ss3sim", username = "seananderson")

# Load ss3sim:
library(ss3sim)
```

You can read the help files with:

```S
?ss3sim
help(package = "ss3sim")
```

For details on using `ss3sim` for stock-assessment simulation, see the
[in-progress vignette][vignette] [PDF] and the [in-progress paper][paper] [PDF].

[DESCRIPTION]: https://github.com/seananderson/ss3sim/blob/master/DESCRIPTION
[models]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/models
[cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/cases
[eg-cases]: https://github.com/seananderson/ss3sim/tree/master/inst/extdata/eg-cases
[vignette]: https://dl.dropboxusercontent.com/u/254940/ss3sim-vignette.pdf
[paper]: https://dl.dropboxusercontent.com/u/254940/ss3sim-ms.pdf
[SS3]: http://nft.nefsc.noaa.gov/Stock_Synthesis_3.htm
[r-project]: http://www.r-project.org/
[SAFS]: http://fish.washington.edu/
