^1^Department of Biological Sciences, Simon Fraser University, Burnaby
BC, V5A 1S6, Canada\
^2^School of Aquatic and Fishery Sciences, University of Washington, Box
355020, Seattle, WA 98195-5020, USA\
^3^NOAA\
^4^\
^5^UBC?\
^6^?

Introduction
============

Paragraph 1: What is stock assessment simulation? Why is it increasingly
critical?

-   stock assessment simulation is...
-   stock-assessment simulation is a critical component to evaluating
    stock assessment methods and understanding their strengths and
    weaknesses. ...
-   important because it lets us test our assessments on known truths
-   further, it lets us explore truths we are interested in and match
    (or mismatch) truths and assessments
-   refs: Hilborn and Walters (1992) among others; recent papers on
    stock-assessment simulation

2.  Paragraph 2: What is SS3, why is it important, why simulate with it?

-   Stock synthesis is a modelling framework... Integrated analysis ---
    models population dynamics using a wide range of data (Maunder and
    Punt, 2012)
-   SS3 is the 3rd version of the software using this framework
-   SS software ref: Methot and Wetzel (2012)
-   ADMB software ref: Fournier et al. (2012)
-   Importance of integrated analysis with SS as an example: Maunder and
    Punt (2012)
-   most widely used now world wide (?) and especially on West Coast of
    United States
-   facilitates rapid, reproducible analyses... focus on peer-review of
    the science not the modelling code
-   allows a separation of research from stock assessment that informs
    management (Methot and Wetzel, 2012)
-   been instrumental to investigating new stock assessment concepts:
    e.g. Piner et al. (2011), Methot and Taylor (2011)
-   been used in XX stock assessments world wide (\~60 as of 2012 - ask
    Rick) and involved in many more currently
-   Piner et al. (2011) example of stock-assessment simulation research
    with SS3
-   Methot and Taylor (2011) example of stock-assessment research with
    SS

Methot and Wetzel (2012):

> A comprehensive modeling framework such as SS enhances communication,
> efficiency, and education in the fishery assessment community (Methot,
> 2009). Communication is enhanced by creating a familiarity among
> users, reviewers, and clients regarding terminology and approach.
> Reviewers who are already familiar with SS can quickly focus on key
> issues for the assessment being reviewed, rather than spend time
> learning the features of a novel assessment model.

Therefore there are two benefits to simulating with SS: (1) much of the
model has already been built (research can then progress rapidly and
with less chance of errors) and checked and (2) the results are directly
applicable to the tools used by stock assessment scientists --- in fact,
used by all Western US assessments.

There are, however, many complications to conducting large-scale, rapid,
and reproducible stock-assessment simulations. Complications include how
to manage data and file structure, how to avoiding coding errors, how to
repeatedly manipulate simulation models to ask specific questions, and
how to translate models and questions across stocks and species. [Maybe
delete this or go into how most solutions are GUI right now]Further,
while the statistical software R has become the standard for data
analysis and visualization, and the stock-assessment framework Stock
Synthesis is increasingly the standard for fisheries stock assessment,
we lack a generalized framework to link the two in a simulation context.

In this paper we introduce ss3sim, a software package for the popular
statistical programming language R that facilitates large-scale, rapid,
and reproducible stock-assessment simulation with the widely-used SS
framework. We begin by outlining the general philosophy of ss3sim, and
describing its functions. Then, to demonstrate how a researcher might
conduct a stock-assessment simulation with ss3sim, we work through an
example starting at a research question and ending with plots and
interpretation of the output. Our example includes considerations for
setting up operating and estimation models, choosing a folder structure,
model testing, and output manipulation and plotting. We conclude by
discussing how ss3sim complements other stock assessment simulation
software and outlining research questions our accessible and general SS
simulation framework could address.

The ss3sim framework
====================

Terminology
-----------

Throughout this paper we refer to a number of terms which we defined
here. We use the term *operating model* (OM) to refer to the model that
represents the underlying true dynamics of the system. We use the term
*estimation model* (EM) refer to the model used to estimate quantities
of interest. Whereasa the OM refers to the underlying truth, the EM
generates our perception of that truth. We use the term *scenario* to
refer to a combination of operating and estimation model *cases*. An OM
case might be natural mortality that follows a random walk, an EM case
might be estimating a fixed parameter for natural mortality, and the
combination of these two cases along with all other specified conditions
creates a scenario. We refer to *iterations* as replicates of a scenario
with potentially new process and observation error added with each
replicate. A simulation therefore refers to the combination of all
scenarios and iterations.

General philosophy
------------------

We designed ss3sim to be reproducible, flexible, and rapid. To be
reproducible, ss3sim allows for the simulation to be documented in code
and plaintext control files. Further, the plaintext control files refer
to individual cases, which allows researchers to reuse control files as
much as possible across scenarios to make some relation code easier to
understand and less error-prone. ss3sim than keeps all SS3 output files
as well as generating its own log files along the way for documentation.

To be flexible, ss3sim allows the user to specify their own OM and EM
using all the possibilities of SS3. ss3sim can take input in a number of
forms (in R list format or through control files), and return output in
a standard, separated value (CSV) format allowing researchers to work
with the output either using the package provided functions or their own
tools.

To be rapid, ss3sim relies on SS3, which uses ADMB as a backend
optimization platform --- the most rapid and robust optimization
software available today. Further, we built ss3sim so that it is easy to
deploy across multiple computers or multiple researchers and re-combine
the output. The package provides a number of functions to make
visualization easy so that users are more likely to visualize their
models and therefore more likely to detect errors quickly and understand
their models. Finally, ss3sim minimizes the amount of bookkeeping
simulation code that researchers have to write so that they can
concentrate on the science itself.

General structure
-----------------

An ss3sim simulation requires three types of input: (1) a base model of
the underlying truth (an SS3 OM), (2) a base model of how you will
assess that truth (an SS3 EM), (3) and a set of cases that deviate from
these base models that you want to compare (configuration arguments
either as R lists or plaintext control files). ss3sim works, in general,
by converting simulation arguments (e.g. a given natural mortality
trajectory) into manipulations of SS3 configuration files at the
appropriate stage along with running the OM and EM as needed.

Low-level generic ss3sim functions
----------------------------------

See Table 1 for description of functions. See Figure 1 for the functions
fit into the general structure. ss3sim functions are divided into three
types of functions:

1.  Functions that manipulate SS configuration files. These
    manipulations generate an underlying "truth" (OM) and control our
    assessment of those models (EM).

2.  Functions that conduct simulations. These functions generate a
    folder structure, call manipulation functions, run `SS3` as needed,
    and save the output.

3.  Functions for analyzing and plotting simulation output.

High-level tailored ss3sim functions
------------------------------------

-   an example framework
-   because it relies on manipulation of these configuration files, it's
    important the config files match a specific format
-   general framework, because you start with your own OM and EM, and a
    wide variety of questions are then available through manipulations
    of ..., ...

An example simulation with ss3sim
=================================

Setting up the SS models
------------------------

-   the (simple) research question
-   setting up the OM and EM SS models
-   things to keep in mind
-   running through SS to format as `.ss_new` files and renaming

File and folder setup
---------------------

-   required files
-   Why we chose a flat-file structure
-   see vignette

Translating research questions into configuration files
-------------------------------------------------------

-   E.g. time-varying M

Deterministic model testing
---------------------------

-   reduce recdevs, reduce sigma R, bias correction
-   what to plot, what to look for, how good is OK?

Output analysis and visualization
---------------------------------

-   examples using the included functions
-   brief take home of what we'd conclude

Discussion
==========

Other sections? how we validated it; benefit of using one well tested
and well-understood model (but disadvantages too) --- benefit to playing
with all the switches and understanding one framework (SS) well versus
having many tools that we superficially understand

How ss3sim complements other generic stock-assessment simulation software
-------------------------------------------------------------------------

-   focus on "generic" software, e.g. not software the just works for
    salmon simulation

### r4ss

-   Taylor et al. (2013)
-   r4ss has functions to facilitate aspects of simulations, mostly
    focused on reading and plotting output for stock assessment
-   ss3sim uses r4ss functions for some reading, writing, and bias
    adjustment

### FLR

-   Kell et al. (2007) for FLR and Hillary (2009) for simulation in FLR
-   statistical catch-at-age only?
-   not integrated analysis, not SS
-   but particularly relevant to Europe

### "Hooilator"

-   http://fisherysimulation.codeplex.com, Windows only, GUI..., works
    on bootstrapped data only, therefore isn't as flexible as ss3sim.
    Used in:
    1.  Lee et al. (2012)
    2.  Piner et al. (2011)
    3.  Lee et al. (2011)

### Others?

The need for balance between generalizing and tailoring in simulation software
------------------------------------------------------------------------------

-   maybe?
-   why we developed generic low-level functions and higher level
    functions
-   but researchers are free to develop their own higher level functions
-   because in an open-source MIT(?) licensed R package, users are free
    to modify functions as needed

Maybe lessons learned? From Athol's work
----------------------------------------

-   importance of version control
-   benefits to developing analysis within an R package
-   importance of model testing
-   importance of rapid visualization of output, example `shiny` or
    `manipulator`

Research opportunities with ss3sim
----------------------------------

Acknowledgements
================

-   funding: Fulbright Canada, NSERC, Simon Fraser University, ...
-   discussions and advice: André Punt, Richard Methot, Ian Taylor,
    James Thorson, ...

Figure captions
===============

Figure 1: Flow diagram of ss3sim

Figure 2: Panels with output from the example

Tables
======

Table 1: User-facing ss3sim functions and a description of their
purpose.

  ---------------------------------------------------------------
  Function name          Description
  ---------------------- ----------------------------------------
  `change_f`             Changes the fishing mortality

  `change_m`             Adds time-varying natural mortality
                         features

  `change_growth`        Adds time-varying growth features

  `change_sel`           Adds time-varying selectivity

  `change_e`             Controls what and how parameters are
                         estimated

  `change_lcomp`         Controls how length composition data are
                         sampled

  `change_agecomp`       Controls how age composition data are
                         sampled

  `change_index`         Controls how the fishery and survey
                         indices operate

  `change_rec_devs`      Substitutes recruitment deviations

  `change_retro`         Controls the number of years to discard
                         for a retrospective analysis

  `run_ss3sim`           Master function that runs an ss3sim
                         simulation

  `run_fish600`          Wrapper function that facilitates one
                         particular simulation setup

  `get_results_all`      Extract results from a series of
                         scenarios

  `get_results_scenario` Extract the results for a single
                         scenario

  `plotting functions!!` Plot the output...
  ---------------------------------------------------------------

Table X: Comparison with related software? - maybe a table with the
possible columns: software, reference, platform (e.g. R, GUI...), Short
description/comparison, examples of papers using it

References
==========

Fournier, D. A., Skaug, H. J., Ancheta, J., Ianelli, J., Magnusson, A.,
Maunder, M. N., and Nielsen, A.*et al.* 2012. AD Model Builder: using
automatic differentiation for statistical inference of highly
parameterized complex nonlinear models. Optimization Methods and
Software, 27: 233–249.

Hilborn, R. W., and Walters, C. 1992. Quantitative Fisheries Stock
Assessment: Choice, Dynamics, and Uncertainty. Chapman and Hall, London.

Hillary, R. 2009. An introduction to FLR fisheries simulation tools.
Aquatic Living Resources, 22: 225–232.

Kell, L. T., Mosqueira, I., Grosjean, P., Fromentin, J.-M., Garcia, D.,
Hillary, R., and Jardim, E.*et al.* 2007. FLR: an open-source framework
for the evaluation and development of management strategies. ICES
Journal of Marine Science, 64: 640–646.

Lee, H.-H., Maunder, M. N., Piner, K. R., and Methot, R. D. 2011.
Estimating natural mortality within a fisheries stock assessment model:
An evaluation using simulation analysis based on twelve stock
assessments. Fisheries Research, 109: 89–94.

Lee, H.-H., Maunder, M. N., Piner, K. R., and Methot, R. D. 2012. Can
steepness of the stock-recruitment relationship be estimated in fishery
stock assessment models?. Fisheries Research, 125–126: 254–261.

Maunder, M. N., and Punt, A. E. 2012. A review of integrated analysis in
fisheries stock assessment. Fisheries Research, 142: 61–74.

Methot, R. D., and Taylor, I. G. 2011. Adjusting for bias due to
variability of estimated recruitments in fishery assessment models.
Canadian Journal of Fisheries and Aquatic Sciences, 68: 1744–1760.

Methot, R. D., and Wetzel, C. R. 2012. Stock Synthesis: A biological and
statistical framework for fish stock assessment and fishery management.
Fisheries Research, 142: 86–99.

Piner, K. R., Lee, H.-H., Maunder, M. N., and Methot, R. D. 2011. A
simulation-based method to determine model misspecification: examples
using natural mortality and population dynamics models. Marine and
Coastal Fisheries, 3: 336–343.

Taylor, I., Stewart, I., Hicks, A., Garrison, T., Punt, A., Wallace, J.,
and Wetzel, C. 2013. r4ss: R code for Stock Synthesis.
<http://code.google.com/p/r4ss/>.
