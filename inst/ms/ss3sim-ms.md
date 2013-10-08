# ss3sim: An R package for fish stock-assessment simulation with Stock Synthesis

Sean C. Anderson^1*^, Kelli F. Johnson^2^, Cole C. Monnahan^3^, Kotaro Ono^2^, additional authors to be added as contributions are added...

^1^Department of Biological Sciences,
Simon Fraser University,
Burnaby BC, V5A 1S6, Canada

^2^School of Aquatic and Fishery Sciences,
University of Washington, Box 355020,
Seattle, WA 98195-5020, USA

^3^Quantitative Ecology and Resource Management,
University of Washington, Box 352182,
Seattle, WA 98195-5020, USA

^*^Corresponding author: sean@seananderson.ca

Short title: ss3sim: Simulation with Stock Synthesis
<!--< 50 characters-->

\clearpage

# Abstract

Simulation is critical to testing fishery stock-assessment methods.
In the last decade, the statistical stock-assessment framework
Stock Synthesis (SS) has become widely-used around the world.
However, there lacks a generalized and scriptable framework to run SS simulations.
Here, we introduce ss3sim, an R package that facilitates
large-scale, rapid, and reproducible simulation
with the third version of SS, SS3.
ss3sim requires only minor modifications to an existing SS3 model
along with a set of plaintext control files describing
alternative states of nature, sampling scenarios,
and assessment scenarios.
ss3sim then generates an underlying truth,
samples from that truth,
modifies and runs an estimation model,
and synthesizes the results.
The simulations can be run in parallel to speed computation,
and the source code is open source
and free to be modified under a GPL-3 license.
We demonstrate the software with a simple example,
discuss how ss3sim complements other simulation software,
and outline research questions that ss3sim could address.

<!--ss3sim can be easily used to answer questions about time-varying model misspecification, retrospective patterns-->

\clearpage

# Introduction

Simulation is a critical component to testing fishery stock-assessment methods
[@hilborn1987; @hilborn1992; @rosenberg1994; @peterman2004].
With simulation, we can evaluate the precision and bias of complex assessment methods
in a controlled environment where we know the true state of nature [@hilborn1992].
For example, recent simulation studies have been key to improving strategies for dealing with
time-varying natural mortality [@lee2011; @jiao2012; @deroba2013]
and uncertainty in steepness of the stock-recruit relationship [@lee2012],
as well as determining what makes fisheries data informative [@magnusson2007; @wetzel2011a].

Stock Synthesis [SS; @methot2013] is a widely-used stock-assessment framework.
It implements statistical age-structured population dynamics modeling
using a wide range of minimally-processed data [@maunder2013; @methot2013].
By using this framework,
individuals conducting stock assessments and peer reviewers
can focus on the underlying science, instead of the model code [@methot2013].
Owing to these advantages, SS3 (the third version of the software)
is one of the world's most commonly-used stock-assessment tools,
particularly in the United States and Australia,
where it has been used in 35 and 12 stock assessments as of 2012,
respectively [@methot2013].

Although SS is increasingly a standard for fisheries stock assessment,
and the programming language R [@rcoreteam2013] has become the standard
for statistical computing and visualization,
there lacks a generalized framework
to link these components in a simulation context.
Here, we introduce ss3sim,
an R package that facilitates
large-scale, rapid, and reproducible simulation
with the widely-used SS framework.
We begin by outlining the general structure of ss3sim
and describing its functions.
We then demonstrate the software by developing a simple example.
We conclude by discussing how ss3sim complements other simulation software
and outlining research questions
our accessible and general SS simulation framework could address.

# The ss3sim framework

## Design goals of ss3sim

We designed ss3sim to be reproducible, flexible, and rapid.
*Reproducible*: ss3sim allows for the simulation to be documented
in code and plain-text control files
and allows for random seeds to be set observation and process error.
Simulations are therefore repeatable.
*Flexible*: ss3sim can implement all possible configurations of SS3.
Further, ss3sim summarizes the entire simulation into comma-separated-value (`.csv`) files
allowing for the output to be easily processed
in nearly any statistical software, including R.
Finally, the ss3sim code is written under a GPL-3 license and can be freely modified.
*Rapid*: ss3sim relies on SS3,
which uses ADMB as a backend optimization platform ---
the most rapid and robust optimization software available [@fournier2012].
ss3sim also facilitates the deployment of simulations
across multiple computers or computer cores, thereby accelerating computation.
<!--Finally, the package provides a number of visualization functions-->
<!--to quickly explore simulation output.-->

## The general stucture of an ss3sim simulation

An ss3sim simulation requires three types of input:
(1) a base model of the underlying truth, or operating model (OM);
(2) a base model used to assess that truth,
also known as the estimation model or method (EM);
and (3) a set of plain-text files (case files)
describing deviations from these base models.
Each unique combination of OM, EM, and case files are referred to as scenarios.
These scenarios are usually run for multiple iterations,
possibly adding unique process and observation error each time.
A simulation therefore refers to the combination of all scenarios and iterations.

ss3sim works, in general, by converting case file arguments
(e.g. arguments specifying a given natural mortality trajectory)
into manipulations of SS3 configuration files (`change` functions);
running the OM;
sampling the time-series of population dynamics with fishery dependent and independent surveys (`sample` functions);
running the EM;
and synthesizing the output
for easy data manipulation and visualization
(`get` functions) (Figure 1).

# An example simulation with ss3sim

To demonstrate ss3sim, we show a simple example 
in which we test the effect of 
high vs.\ low research survey effort 
and the effect of fixing vs.\ estimating natural mortality (*M*).
We have included all files to run this example in the package data 
and describe the example in greater detail in the accompanying vignette file.

## Setting up the SS models

Any existing SS model can be used with ss3sim
with minimal modifications (Text S1).
Further, ss3sim comes with built-in SS models
that represent three life histories:
cod-like (slow-growing and long-lived),
flatfish-like (fast-growing and intermediate-lived),
and sardine-like (fast-growing and short-lived).
These models are based on
North Sea cod (*Gadus morhua*) (R. Methot, pers.\ comm.),
Yellowtail flounder (*Limanda ferruginea*) (R. Methot, pers.\ comm.),
and California sardine (*Sardinops caeruleus*) [@hill2012].
Further details on these models are available in @johnson2013 and @ono2013. 
We will base our example around the cod-like model.

## Setting up the case files

<!--TODO what happens if there are no time varying parameters specified?-->

ss3sim simulations are controlled by a set of semicolon-delimited plain-text files that describe alternative cases. 
These files contain the argument values that will be passed 
to the low-level ss3sim R functions (e.g. `change_e`) during the simulation.
To use the high-level function `run_ss3sim`, the naming of the case files is important. 
All case files are named according to the the type of case 
(e.g. `E` for estimation, `D` for data, or `F` for fishing mortality), 
a number representing the case number, and a three letter code representing the species or stock (e.g. `cod`).

To investigate the effect of research survey effort,
we will manipulate the argument `sd_obs_surv`
that gets passed to `change_index`.
In case 1, we'll specify the standard deviation at `0.1`
and in case 2 we'll increase the standard deviation to `0.4`.
We can do this by including the line: `sd_obs_surv; 0.1`
in the file `D1-cod.txt` and the line: `sd_obs_surv; 0.4`
in the file `D2-cod.txt`.
We will set up a base-case file describing fishing mortality (`F0-cod.txt`) 
and we will specify that we don't want to run a retrospective analysis in the file `R0-cod.txt`.

To start, we'll load the ss3sim package and locate three sets of folders:
(1) the folder with the OM,
(2) the folder with the EM,
(3) and the folder with the plain-text case files,

```
library(ss3sim)
d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")
```

## Deterministic model testing

It is important to validate a simulation model first with minimal or no observation and process error to ensure unbiased and consistent recovery of parameters [@hilborn1992].
We can test our model by passing a set of deterministic recruitment deviations to our simulation. 
We'll set up a matrix of zeros with 100 rows for 100 years of data and 20 columns for 20 iterations:

```
recdevs_det <- matrix(0, nrow = 100, ncol = 20)
```

We've also set up deterministic case files (`E100` and `E101`) in which we set recruitment deviation standard deviation (`SR_sigmaR` in SS3) to the nominal level of 0.001.


```
run_ss3sim(iterations = 1:20, scenarios =
  c("D1-E100-F0-R0-cod", "D2-E100-F0-R0-cod",
    "D1-E101-F0-R0-cod", "D2-E101-F0-R0-cod"),
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, user_recdevs = recdevs_det)
```

- reduce recdevs, reduce sigma R, bias correction
- what to plot, what to look for, how good is OK?

*Output analysis and visualization*:

- examples using the included functions
- brief take home of what we'd conclude

# How ss3sim complements other simulation software

Probably turn this into a small table:

*r4ss*

- @r4ss2013
- r4ss has functions to facilitate aspects of simulations, mostly focused on
  reading and plotting output for stock assessment
- ss3sim uses r4ss functions for some reading, writing, and bias adjustment

*FLR*

- @kell2007 for FLR and @hillary2009 for simulation in FLR
- statistical catch-at-age only?
- not integrated analysis, not SS
- but particularly relevant to Europe

*"Hooalator"*

- http://fisherysimulation.codeplex.com, Windows only, GUI..., works on
  bootstrapped data only, therefore isn't as flexible as ss3sim. Used in:
    1. @lee2012
    2. @piner2011
    3. @lee2011

# Research opportunities with ss3sim

ss3sim makes many research opportunities easily accessible.
Below we outline some key examples.

<!--Although the low-level functions (`change` and `sample`) functions can be combined or modified in a user's own wrapper function to address nearly any simulation-based research question, there are-->

<!--*Domed selectivity*:-->
<!--*The importance of contrast in index series*:-->

*Time-varying model misspecification*:

Ecological processes can vary through time in response to, for example,
changes to fishing behaviour [@hilborn1992],
regime shifts [@vert-pre2013], or
changes to climate [@walther2002]
However, parameters such as natural mortality, catchability, and selectivity
are commonly assumed to be time invariant (REFS).
What are the consequences for stock assessment?
Further, although many studies have tried to isolate the effects
of single time-varying parameters [@lee2011; @jiao2012; @deroba2013],
few have considered the effect of multiple time-varying parameters.
and multiple time-varying parameters could interact in unexpected ways (REFS).
ss3sim can easily incorporate
the effect of single or multiple time-varying parameters.

*Patterns in recruitment deviations*:
Typically, estimation methods assume
independent normally-distributed recruitment deviations (REF).
However, recruitment deviations are frequently auto-correlated and their variability can change through time (REF).
ss3sim makes it simple
to incorporate different recruitment deviation structures
and consider how they affect model performance.

*The impact of bias adjustment*:
Bias adjustment helps assure
that the estimated recruitment deviations
are mean-unbiased leading to unbiased estimates
of biomass [@methot2011].
However, bias adjustment requires extra model runs,
which can be computationally intensive and time consuming.
As a result, bias adjustment is routinely not used in practice (REF).
ss3sim can turn bias adjustment on or off
with a single argument and so could be easily used to test when
and how bias adjustment makes an important difference in stock assessment.

*Retrospective patterns*:
Retrospective patterns,
in which model estimates are systematically biased
with each additional year of data,
are a major problem in stock-assessment science [@mohn1999; @legault2008].
Key questions are: what causes retrospective patterns and what assessment strategies reduce retrospective patterns [@legault2008]?
ss3sim can run retrospective analyses as part of any simulation by adding a single argument --- the number of retrospective years to investigate.

# Conclusions

- benefit of using one well tested and well-understood modeling framework (SS) i.e. benefit to playing with all the switches and understanding one framework well versus having many tools that we superficially understand (based on Rick's comments at the conference)
- why we developed generic low-level functions and high-level functions
- researchers are free to develop their own low- and high-level functions because in an open-source MIT(?) licensed R package, users are free to modify functions as needed
- (these points are somewhat random at the moment)

# Acknowledgements
We thank Ian Taylor, André Punt, and Richard Methot, AND INSERT ALL FISH 600 PARTICIPANTS NOT LISTED AS AUTHORS for helpful discussions during the development of ss3sim.
KFJ and KO were partially supported by NOAA grant 423 NA10OAR4320148.
SCA was supported by Fulbright Canada, NSERC, and a Garfield Weston Foundation/B.C. Packers Ltd.\ Graduate Fellowship in Marine Sciences.

# Tables

Table X: Comparison with related software? Possible columns: software, reference, platform (e.g. R, GUI...), short description/comparison, examples of papers using it

\clearpage

Table 1: Main ss3sim functions and a description of their purpose. 
Simulations can be run through the `run_ss3sim` function. 
`run_ss3sim` then calls the `change` functions. 
You can control what the `change` functions do through a series of plain-text case files. 
For example, the case ID `D1` corresponds to the case files `lcomp1`, `agecomp1`, and `index1`, as described in the table. 
You could also use the `change` functions directly as part of your own simulation structure.

----------------------------------------------------------------
Function name          Description
---------------------- -----------------------------------------
`run_ss3sim`           Main function to run ss3sim simulations.

`ss3sim_base`          Underlying base simulation function. Can also be called directly.

`change_f`             Controls fishing mortality. (Case file and ID `F`)

`change_tv`            Adds time-varying features. For example, time-varying natural mortality, growth, or selectivity. (Any case file, e.g. `M`, `G`, or `S`, starting with "`function_type; change_tv`")

`change_lcomp`         Controls how length composition data are sampled. (Case file `lcomp`, case ID `D`)

`change_agecomp`       Controls how age composition data are sampled. (Case file `agecomp`, case ID `D`)

`change_index`         Controls how the fishery and survey indices are sampled. (Case file `index`, case ID `D`)

`change_e`             Controls which and how parameters are estimated. (Case file and ID `E`)

`change_retro`         Controls the number of years to discard for a retrospective analysis. (Case file and ID `R`)

`change_rec_devs`      Substitutes recruitment deviations.

`get_results_scenario` Extract the results for a single scenario.

`get_results_all`      Extract results from a series of scenarios.
---------------------- -----------------------------------------



# Figures

\begin{center}
\includegraphics[width=3.9in]{sim-steps.pdf}
\end{center}

Figure 1: Flow diagram of the main steps
in an ss3sim simulation carried out using `run_ss3sim`.
Functions that are called internally are shown in a monospaced font.

\clearpage

\begin{center}
\includegraphics[width=5.0in]{fig2.pdf}
\end{center}

Figure 2: Example output from ss3sim simulations.
We ran a crossed simulation in which we considered
(1) the effect of fixing natural mortality (*M*)
at its historical value (0.2; case E0) or estimating *M* (case E1) and
(2) the effect of high survey effort
($\sigma_\mathrm{survey} = 0.1$; case D1)
or low survey effort ($\sigma_\mathrm{survey} = 0.4$; case D2).
Upper panels (blue) show time series of relative error
in spawning stock biomass (SSB).
The shaded regions indicate 50% and 90%
of the relative errors and the line indicates the median.
Lower panels (grey) show the distribution
of relative error across four scalar variables:
depletion, *M*, SSB at maximum sustainable yield (MSY),
and fishing mortality (F) in the last year.
We show the values across simulation iterations with dots
and the distributions with beanplots (kernel density smoothers).

\clearpage

# References
