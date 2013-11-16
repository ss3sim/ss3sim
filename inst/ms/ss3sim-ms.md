# Abstract

Simulation testing is an important approach to evaluating fishery stock-assessment methods.
In the last decade, the fisheries stock-assessment modeling framework
Stock Synthesis (SS) has become widely-used around the world.
However, there lacks a generalized
and scriptable framework for running SS simulations.
Here, we introduce `ss3sim`, an `R` package that facilitates
large-scale, rapid, and reproducible simulation
with the third version of SS, SS3.
`ss3sim` requires only minor modifications to an existing SS3 model
along with a set of plain-text control files describing
alternative states of nature, sampling scenarios,
and assessment scenarios.
`ss3sim` then generates an underlying truth,
samples from that truth,
modifies and runs an estimation model,
and synthesizes the results.
The simulations can be run in parallel, speeding computation,
and the source code is open source
and free to be modified under an MIT license.
We demonstrate the software with a simple example,
discuss how `ss3sim` complements other simulation software,
and outline specific research questions that `ss3sim` could address.
For example, `ss3sim` can be easily used to answer questions about
time-varying model misspecification,
retrospective patterns,
and the relative importance of various types of fisheries data.

# Introduction

Fisheries stock-assessment models are crucial
to providing scientific advice and to evaluating the impact
of alternative management actions on fishery resources [@hilborn1992].
Although a variety of stock-assessment methods and models
are currently available,
it is often not straightforward to choose among competing approaches
that often lead to different modeling outcomes
and associated scientific advice to management (REF).

Simulation testing is a critical component
to testing fishery stock-assessment methods,
particularly given the potential for model misspecification
[@hilborn1987; @hilborn1992; @rosenberg1994; @peterman2004; @deroba2013a].
With simulation testing,
we can evaluate the precision and bias
of alternative complex assessment methods
in a controlled environment
where we know the true state of nature [@hilborn1992].
Recent simulation studies
have been key to improving strategies for dealing with, for example,
time-varying natural mortality [@lee2011; @jiao2012; @deroba2013; @johnson2013]
and uncertainty in steepness of the stock-recruit relationship [@lee2012],
as well as determining what makes fisheries data informative
[@magnusson2007; @wetzel2011a; @ono2013].

Stock Synthesis (SS) [@methot2013]
is a widely-used fisheries stock-assessment modeling framework.
It implements statistical age-structured population dynamics modeling
using a wide range of minimally-processed data [@maunder2013; @methot2013].
By using this generalized framework,
individuals conducting fisheries stock assessments and peer reviewers
can focus on the underlying science, instead of the model code [@methot2013].
Owing to these advantages, SS3 (the third version of the software)
is one of the world's most commonly-used stock-assessment tools,
particularly in the United States and Australia,
where it has been used in 35 and 12 stock assessments as of 2012,
respectively [@methot2013].

Although SS is increasingly becoming a standard for fisheries stock assessment
and has commonly been used in simulation testing
[@lee2011; @lee2012; @piner2011; @crone2013a],
and the programming language `R` [@rcoreteam2013] has become the standard
for statistical computing and visualization,
there lacks a generalized framework
to link these components in a simulation context.
Most simulation testing work to date has used custom frameworks tailored to the particular needs of each study.
The only available `R` package to interface with SS is `r4ss` [@r4ss2013],
which primarily allows reading, processing, and plotting of SS output,
but not a simulation framework.

Here we introduce `ss3sim`,
an `R` package that facilitates
large-scale, rapid, and reproducible simulation testing
with the widely-used SS framework.
We begin by outlining the general structure of `ss3sim`
and by describing its functions.
We then demonstrate the software by developing a simple example.
We conclude by discussing how `ss3sim` complements other currently available simulation software
and by outlining some research questions that
our freely accessible and general SS simulation framework could address.

# The ss3sim framework

## Design goals of ss3sim

We designed `ss3sim` to be reproducible, flexible, and rapid.

*Reproducible*: `ss3sim` allows for the simulation testing structure to be documented
in `R` code and plain-text control files.
It also allows for random seeds to be set
when generating observation and process error,
making simulations repeatable across computers
and operating systems (Windows, OS X, and Linux).

*Flexible*: `ss3sim` inherits the flexibility of SS3
and can therefore implement many
available stock-assessment configurations by
either modifying existing SS3 models (Text S1)
or by modifying built-in generic life-history models.
Furthermore, `ss3sim` summarizes the simulation output
into plain-text comma-separated-value (`.csv`) files
allowing for the output to be easily processed
in nearly any statistical software, including `R`.
Finally, the `ss3sim` code is written
under an open-source MIT license and can be freely modified.

*Rapid*: `ss3sim` relies on SS3,
which uses AD Model Builder as a backend optimization platform ---
the most rapid and robust optimization software available [@fournier2012].
`ss3sim` also facilitates the deployment of simulations
across multiple computers or computer cores, thereby accelerating computation.
Perhaps most importantly, `ss3sim` can substantially reduce the time
to develop a large-scale simulation study,
allowing users to focus on the research questions themselves.

## The general structure of an ss3sim simulation

`ss3sim` comes with both low-level functions
that modify SS3 configuration files
and high-level functions that combine these low-level functions
into a complete simulation experiment (Figure 1, Table 1).
In this paper we will focus on the structure
of the high-level function `run_ss3sim`;
however, the low-level functions
can be used on their own
as part of a more customized simulation wrapper function (Text S1).

An `ss3sim` simulation requires three types of input:
(1) a base SS3 model describing the underlying truth,
or operating model (OM);
(2) a base SS3 model to assess that truth,
also known as the estimation model or method (EM);
and (3) a set of plain-text files (case files)
describing alternative model configurations and deviations from these base models.
We refer to each unique combination of OM, EM, and case files as a scenario.
Scenarios are usually run for multiple iterations,
possibly adding unique process and observation error each time.
An `ss3sim` simulation therefore refers to the combination of all scenarios and iterations.

The `run_ss3sim` function works by reading case-file arguments
(e.g. arguments specifying a given natural mortality trajectory over time)
to modify SS3 configuration files (`change` functions);
running the OM;
sampling the time-series of true population dynamics
with fishery dependent and independent surveys (`sample` functions);
running the EM;
and synthesizing the output
for easy data manipulation and visualization
(`get` functions) (Figure 1).

# An example simulation with ss3sim

To demonstrate `ss3sim`, we will work through a simple example
in which we examine the effect of
high vs.\ low precision on a research survey index of abundance
and fixing vs.\ estimating natural mortality ($M$).
All files to run this example are available in the package data,
and a more detailed description
is available in the accompanying vignette (Text S1).

## Setting up the SS models

<!--TODO model vs. model setup?-->

Many existing SS model setups can be used with `ss3sim`
following minimal modifications (Text S1).
`ss3sim` also comes with built-in pre-modified SS3 model setups
that represent three general life histories:
cod-like (slow-growing and long-lived),
flatfish-like (fast-growing and long-lived),
and sardine-like (fast-growing and short-lived).
These model setups are based on
North Sea cod (*Gadus morhua*) (R. Methot, pers.\ comm.),
yellowtail flounder (*Limanda ferruginea*) (R. Methot, pers.\ comm.),
and Pacific sardine (*Sardinops sagax caeruleus*) [@hill2012] (Text S1).
We will base our example around the cod-like model.

## Setting up the case files

The high-level function `run_ss3sim`
can run all simulation steps
based on a specified scenario ID
and a set of semicolon-delimited plain-text files
that describe alternative cases (Figure 1).
These files contain the argument values that will be passed
to the low-level `ss3sim` `R` functions
(e.g. `change_e`, a function controlling which and how parameters are estimated;
Table 1).

To use `run_ss3sim`, the naming of the case files is important.
All case files are named according to the type of case
(e.g. `E` for estimation or `F` for fishing mortality),
a numeric value representing the case number,
and an alphanumeric identifier
representing the species or stock (e.g. `cod`) (Table 1, Text S1).
We combine these case IDs with hyphens to create scenario IDs.
For example, one of our scenarios will
have the scenario ID ``D1-E0-F0-M0-R0-cod``.
This scenario ID tells `run_ss3sim`
to read the case files corresponding
to the first data (`D`) case
(i.e. `index1-cod.txt`, `lcomp1-cod.txt`, `agecomp1-cod.txt`),
the zero case for estimation (E) (i.e. `E0-cod.txt`), and so on.

To investigate the effect of different levels of precision
of a research survey index of abundance,
we will manipulate the argument `sds_obs`
that gets passed to `change_index`.
In case 0, we will specify the standard deviation of the index of abundance at `0.1`
and in case 1 we will increase the standard deviation to `0.4`.
We can do this by including the line: `sds_obs; list(0.1)`
in the file `index0-cod.txt` and the line: `sds_obs; list(0.4)`
in the file `index1-cod.txt`.
We will also set up a base-case file describing fishing mortality (`F0-cod.txt`), a file describing a stationary $M$ trajectory (`M0-cod.txt`),
and we will specify that we do not want to run a retrospective analysis
in the file `R0-cod.txt`.
We will set up the file `E0-cod.txt`
to fix the estimation of $M$ at the true value
and case `E1-cod.txt` to estimate a stationary $M$ (Text S1).

All of these text files are available in the package data
in the folder `inst/extdata/eg-cases/`.
As an example, here is what the complete `index0-cod.txt` file looks like:

    fleets; 2
    years; list(seq(1974, 2012, by = 2))
    sds_obs; list(0.1)

\noindent
`fleets`, `years`, and `sds_obs`
refer to the arguments in the function `change_index`
and users can read the help
for this function with `?change_index` in `R`.

To start, we will load the `ss3sim` package into an `R` session
and locate three sets of folders within the package data:
the folder with the OM,
the folder with the EM,
and the folder with the plain-text case files:

    library(ss3sim)
    d <- system.file("extdata", package = "ss3sim")
    om <- paste0(d, "/models/cod-om")
    em <- paste0(d, "/models/cod-em")
    case_folder <- paste0(d, "/eg-cases")

## Running the simulations

It is important to validate a simulation testing framework
with minimal or no process and observation error
to ensure unbiased and consistent recovery of parameters
under ideal conditions [@hilborn1992].
`ss3sim` makes model validation simple by allowing users
to specify process error (i.e. recruitment deviations)
and control sampling error (Text S1).

The cod-like model setup has already been validated (Text S1), therefore
we can now run our simulation scenario.
We will set `bias_adjust = TRUE`
to enable a procedure that aims to produce mean-unbiased estimates
of recruitment and biomass despite log-normal recruitment deviations [@methot2011].
Although not used in this example,
we could have run the simulations with parallel processing
to substantially reduce computing time (Text S1).
We can run this simulation scenario with the following code:

    run_ss3sim(iterations = 1:100, scenarios =
      c("D0-E0-F0-M0-R0-cod", "D1-E0-F0-M0-R0-cod",
        "D0-E1-F0-M0-R0-cod", "D1-E1-F0-M0-R0-cod"),
      case_folder = case_folder, om_model_dir = om,
      em_model_dir = em, bias_adjust = TRUE)

We can then collect the output from all the simulations
in our current directory with one function call:

    get_results_all()

\noindent
This command creates two files in our working directory:
`ss3sim_scalars.csv` and `ss3sim_ts.csv`,
which contain scalar output estimates (e.g. maximum sustainable yield)
and time-series estimates (e.g. biomass each year).
These estimates come from the `Report.sso` files produced from each run of SS
and are read by the `r4ss` `R` package.
The `.csv` files contain separate columns for OM and EM values,
making it simple to calculate error metrics,
such as relative or absolute error.
In addition to parameter estimates,
the `.csv` files contain performance metrics,
such as the maximum gradient,
whether the covariance matrix was successfully calculated,
and model run-time, which can be used to gauge model convergence.
These results are organized into "long" data format,
with columns for scenario and iteration,
for quick analysis and plotting using
common `R` packages such as ggplot2 [@wickham2009].

For our example simulation, the relative error in spawning stock biomass (SSB) over time is,
as expected, lower when the true value of $M$ is specified rather than estimated
(Figure 2, top panels E0 vs. E1).
Furthermore, lower precision on the research survey index of abundance
results in greater relative error in SSB in recent years
(Figure 2, top panels D0 vs. D1),
and greater relative error of terminal-year depletion and F, but not
SSB at maximum sustainable yield, or $M$ (Figure 2, lower panels).

# How ss3sim complements other simulation software

The general purpose of `ss3sim`
is to explore the behaviour and performance
of alternative sampling scenarios
and EM configurations across alternative states of nature
specified by OMs.
In particular, `ss3sim` provides a suite of functions
for dynamically creating structural differences in OMs and EMs.
This expedites testing the properties of alternative stock-assessment model configurations,
whether the differences are between the OMs and EMs [@johnson2013],
or between multiple versions of the EMs [@ono2013].
<!--ss3sim is thus ideal for answering questions-->
<!--about mismatches between OMs and EMs-->
<!--or different structures of EMs for a given OM.-->
<!--However, because ss3sim relies on manipulating SS configuration files,-->
<!--the package relies on certain SS model configurations.-->
However, `ss3sim` is less well-suited
for quickly exploring arbitrary SS model setups,
which may rely on SS configurations not yet programmed into the `ss3sim` package functions.
Therefore, depending on the simulation study goal,
other software frameworks may provide better alternatives.

One alternative framework is *Fisheries libraries in R* (`FLR`) [@kell2007] ---
an open-source `R` package developed specifically
for evaluating fisheries management strategies through simulation.
<!--`FLR` aims to incorporate a variety of disciplines-->
<!--beyond fisheries science and ecology (TODO SUCH AS)-->
<!--into the development of new methods-->
<!--for evaluating alternative management strategies and procedures.-->
Compared to `ss3sim`, `FLR` is designed to explore broader questions
regarding management strategies
with flexible biological, economic, and management components [@hillary2009].
Thus, it is not specifically designed to explore the impact
of structural differences within OMs and EMs.

Another alternative stock-assessment simulation testing framework is *Fishery Simulation*
(FS, <http://fisherysimulation.codeplex.com>).
FS is primarily a file management tool
adapted to aid in simulation testing.
FS can work with stock-assessment models besides SS,
make simple changes to input text files,
generate random process (using a built-in random number generator)
and observation errors
(using the SS3 bootstrap option),
run simulations in parallel
and collect results from output files.
Thus, FS is closer to `ss3sim` in its scope than `FLR`
in that it specifically focuses on the performance of stock-assessment models.

FS differs from `ss3sim` mainly in that
it uses user-specified text manipulation commands
(e.g. "change line 50 from 0 to 1")
to alter model setups rather than the approach of `ss3sim`,
which uses modular functions tailored to specific purposes.
Since FS does not rely on pre-built manipulation functions,
FS works well for testing arbitrary assessment models
[@lee2012; @piner2011; @lee2011].
In contrast, FS cannot make complicated structural changes
to a model (e.g. adding time-varying parameters or changing the survey years),
making it more difficult to induce and test
structural differences between OMs and EMs.
In addition, the current version of FS
is not an end-to-end package ---
additional code is necessary to
to incorporate process and observation error in simulation testing.
Finally, FS is also open-source but requires the Microsoft .NET framework
and is therefore only available on the Windows operating system.

# Research opportunities with ss3sim

The `ss3sim` package has been used so far
to evaluate alternative approaches
to deal with time-varying natural mortality [@johnson2013],
the importance of length and age composition data [@ono2013],
and the causes of retrospective patterns in stock-assessment models.
Along with those studies,
`ss3sim` makes many important research opportunities easily approachable.
Below we outline some examples.

<!--Although the low-level functions (`change` and `sample`) functions can be combined or modified in a user's own wrapper function to address nearly any simulation-based research question, there are-->
<!--*Domed selectivity*:-->
<!--*The importance of contrast in index series*:-->

*Time-varying model misspecification*:
Ecological processes can vary through time in response to,
for example,
changes to fishing behaviour [@hilborn1992],
regime shifts [@vert-pre2013], or
climate change [@walther2002].
However, parameters such as natural mortality, catchability, and selectivity
are commonly assumed to be time invariant and
the consequences of assuming time invariance
of such parameters when facing true temporal changes
has been a long-standing discussion
in fisheries science [@royama1992; @wilberg2006; @fu2001].
Furthermore, although many studies have tried
to isolate the effects
of single time-varying parameters,
such as natural mortality [@lee2011; @jiao2012; @deroba2013; @johnson2013],
few have considered the effect of multiple time-varying parameters and their potential interaction.

*Patterns in recruitment deviations*:
Typically, estimation methods assume
independent log-normally-distributed recruitment deviations
around a spawning stock recruitment function (@hilborn1992).
However, recruitment deviations are frequently auto-correlated
and their variability can change through time [@beamish1995; @pyper1998].
`ss3sim` makes it simple
to incorporate different recruitment deviation structures
and consider how they affect model performance.

*The impact of bias adjustment*:
Bias adjustment helps assure
that the estimated recruitment deviations
are mean-unbiased leading to unbiased estimates
of biomass [@methot2011].
However, bias adjustment requires extra model runs
to iteratively calculate the proper adjustment level,
which can be computationally intensive and time consuming.
As a result, bias adjustment is routinely
not used in stock assessment simulation testing (REF) (TODO OR STOCK ASSESSMENT ITSELF?).
`ss3sim` can turn bias adjustment on or off
with a single argument and so could be easily used to test when
and how bias adjustment affects model performance.

*Retrospective patterns*:
Retrospective patterns,
in which model estimates are systematically biased
with each additional year of data,
are a major problem in stock-assessment science [@mohn1999; @legault2008].
Key questions are: what causes retrospective patterns
and what assessment approaches reduce retrospective patterns [@legault2008]?
`ss3sim` can run retrospective analyses as part of any simulation
by adding a single argument --- the number of retrospective years to investigate.

# Conclusions

TODO IN PROGRESS AND TO BE CUT DOWN:

The increasing complexity of modern statistical integrated models and expanding computing power allows for the inclusion of a multitude of processes in fisheries stock-assessment methods.
However, with added complexity comes the potential for model misspecification and model misspecification is often not accounted for in the evaluation of alternative modeling approaches.

Simulation testing allows for the formal evaluation of the ability of assessment models to accurately and precisely estimate parameters of interest under different conditions and levels of misspecification [@deroba2013a; @wilberg2006; @crone2013].

However, most simulation testing work to date has used custom frameworks
tailored to the particular needs of each study.


A recent large scale study [@deroba2013a] focused on evaluating the robustness of several stock-assessment models to observation error.
That study encountered challenges due to fundamental differences in the structure, data requirements, and limitations of different stock-assessment models.
An alternative approach is to focus on a single generalized modeling platform such as SS, where alternative model setups reflect only alternative assumptions about the true underlying dynamics of stocks or estimation approaches and assumptions.

The initial release of `ss3sim` describes the basic structure used in recent studies [@johnson2013; @ono2013]. However, the current version of `ss3sim` could be used to address many other important questions in stock assessment science. Furthermore, the open-source package format allows users to extend the functionality for their own needs, potentially contributing back to the main code base.



<!--For intro: Although several simulation testing studies have used the widely
    used fisheries stock-assessment platform SS [@lee2011; @lee2012;
    @piner2011; @crone2013a] a common end-to-end open access simulation testing
    framework is still lacking. `ss3sim`, allows for large-scale, rapid, and
    reproducible simulation with SS.-->

# Acknowledgements

We thank the participants and mentors of the University of Washington School of Aquatic and Fishery Sciences 2013 FISH 600 course.
Discussions with these individuals were instrumental to developing `ss3sim`.
Many participants also contributed code and are listed within specific `ss3sim` `R` functions.
Participants:
Curry Cunningham,
Felipe Hurtado-Ferro,
Ricardo Licandeo,
Carey McGilliard,
Mellisa Muradian,
Cody Szuwalski,
Katyana Vert-pre, and
Athol Whitten.
Mentors:
Richard Methot,
Andre Punt, and
Ian Taylor.

SCA was supported by Fulbright Canada, NSERC, and a Garfield Weston Foundation/B.C. Packers Ltd.\ Graduate Fellowship in Marine Sciences.
KFJ and KO were partially supported by NOAA grant 423 NA10OAR4320148 and
CCM was partially supported by a Washington Sea Grant.
TODO Juan: any funding to acknowledge?

\bibliography{refs}
\clearpage

\clearpage

# Tables

**Table 1. Main `ss3sim` functions and a description of their purpose.**
Simulations can be run through the `run_ss3sim` function.
`run_ss3sim` then calls the `change` functions.
Users can control what the `change` functions do through a series of plain-text case files.
For example, the case ID `D1` corresponds to the case files `lcomp1`, `agecomp1`, and `index1`, as described in the table.
Users can also use the `change` functions directly as part of their own simulation structure.

----------------------------------------------------------------
Function name          Description
---------------------- -----------------------------------------
`run_ss3sim`           Main high-level function to run `ss3sim` simulations.

`ss3sim_base`          Underlying base simulation function. Can also be called directly.

`change_rec_devs`      Substitutes recruitment deviations.

`change_f`             Controls fishing mortality. (Case file and ID `F`)

`change_tv`            Adds time-varying features. For example, time-varying natural mortality, growth, or selectivity. (Any case file and ID, e.g. `M`, starting with "`function_type; change_tv`")

`change_index`         Controls how the fishery and survey indices are sampled. (Case file `index`, case ID `D`)

`change_agecomp`       Controls how age composition data are sampled. (Case file `agecomp`, case ID `D`)

`change_lcomp`         Controls how length composition data are sampled. (Case file `lcomp`, case ID `D`)

`change_retro`         Controls the number of years to discard for a retrospective analysis. (Case file and ID `R`)

`change_e`             Controls which and how parameters are estimated. (Case file and ID `E`)

`run_bias_ss3`         Determines the level of adjustment to ensure mean-unbiased estimates of recruitment and biomass.

`get_results_scenario` Extracts results for a single scenario.

`get_results_all`      Extracts results for a series of scenarios.
---------------------- -----------------------------------------

\clearpage

# Figure Legends

\begin{figure}[!ht]
\begin{center}
\includegraphics[width=4in]{sim-steps.pdf}
\end{center}
\caption{
{\bf Flow diagram of the main steps
in an \texttt{ss3sim} simulation carried out using \texttt{run\_ss3sim}.}
Functions that are called internally are shown in a monospaced font.
}
\label{fig:sim-steps}
\end{figure}

\begin{figure}[!ht]
\begin{center}
\includegraphics[width=5in]{fig2-20131109.pdf}
\end{center}
\caption{
{\bf Example output from an \texttt{ss3sim} simulation.}
We ran a crossed simulation in which we considered
(1) the effect of fixing natural mortality ($M$)
at its historical value (0.2; case E0) or estimating $M$ (case E1) and
(2) the effect of high survey effort
($\sigma_\mathrm{survey} = 0.1$; case D1)
or low survey effort ($\sigma_\mathrm{survey} = 0.4$; case D2).
Upper panels (blue) show time series of relative error
in spawning stock biomass (SSB).
The shaded regions indicate 50\% and 90\%
of the relative errors and the line indicates the median.
Lower panels (grey) show the distribution
of relative error across four scalar variables:
depletion, $M$, SSB at maximum sustainable yield (MSY),
and fishing mortality ($F$) in the terminal year.
We show the values across simulation iterations with dots
and the distributions with beanplots (kernel density smoothers).
}
\label{fig:results}
\end{figure}
