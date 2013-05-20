This repository holds code for the 'fish-600 project' by students at the School of Aquatic and Fishery Sciences, University of Washington. The project will develop assessment models, operating models, and associated R functions for stock assessment simulation studies using Stock Synthesis. The project aims to complete several studies, the results of which will be presented at the World Conference on Stock Assessment Methods in Boston, July 2013.

This package can be installed and loaded with:
```r
# install.packages("r4ss", "MCMCpack") # dependencies, if you don't have them
# install.packages("devtools") # for install_github()
devtools::install_github("ss3sim", username="seananderson")
library(ss3sim)
```
You can read the vignette and see the help pages with:
```r
vignette("ss3sim") # still under development and slightly out of date
help(package = "ss3sim")
```

Authors of functions in this package are listed within functions and will be collated here.
