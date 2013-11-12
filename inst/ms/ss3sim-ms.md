# ss3sim: An R package for fisheries stock assessment simulation with Stock Synthesis

Sean C. Anderson^1*^, Kelli F. Johnson^2^, Cole C. Monnahan^3^, Kotaro Ono^2^, Juan L. Valero^4^

^1^Department of Biological Sciences,
Simon Fraser University,
Burnaby BC, V5A 1S6, Canada

^2^School of Aquatic and Fishery Sciences,
University of Washington, Box 355020,
Seattle, WA 98195-5020, USA

^3^Quantitative Ecology and Resource Management,
University of Washington, Box 352182,
Seattle, WA 98195-5020, USA

^4^Center for the Advancement of Population Assessment Methodology, 
(CAPAM), 8901 La Jolla Shores Drive, La Jolla, CA 92937, USA

^*^Corresponding author: sean@seananderson.ca

Short title: ss3sim: Simulation with Stock Synthesis
<!--< 50 characters-->

\clearpage

# Abstract

Simulation testing is an important approach to evaluating fishery stock assessment methods.
In the last decade, the statistical fisheries stock assessment modeling framework
Stock Synthesis (SS) has become widely-used around the world.
However, there lacks a generalized 
and scriptable framework for running SS simulations.
Here, we introduce ss3sim, an R package that facilitates
large-scale, rapid, and reproducible simulation
with the third version of SS, SS3.
ss3sim requires only minor modifications to an existing SS3 model
along with a set of plain-text control files describing
alternative states of nature, sampling scenarios,
and assessment scenarios.
ss3sim then generates an underlying truth,
samples from that truth,
modifies and runs an estimation model,
and synthesizes the results.
The simulations can be run in parallel, speeding computation,
and the source code is open source
and free to be modified under an MIT license.
ss3sim can be easily used to answer questions about, for example, 
time-varying model misspecification, 
retrospective patterns, 
and the relative importance of various types of fisheries data.
We demonstrate the software with a simple example,
discuss how ss3sim complements other simulation software,
and outline specific research questions that ss3sim could address.

\clearpage

# Introduction

Fisheries stock assessment models are crucial 
to provide scientific advice and to evaluate the impact 
of alternative management actions on fishery resources.
Although a variety of stock assessment methods and models 
are currently available to fishery scientists, 
it is often not straightforward to choose among competing approaches 
that often lead to different modeling outcomes 
and associated scientific advice to management. 

Simulation testing is a critical component to testing fishery stock assessment methods,
particularly given the potential for misspecification
[@hilborn1987; @hilborn1992; @rosenberg1994; @peterman2004].
With simulation testing, we can evaluate the precision and bias of alternative complex assessment methods
in a controlled environment where we know the true state of nature [@hilborn1992].
Recent simulation studies 
have been key to improving strategies for dealing with
time-varying natural mortality [@lee2011; @jiao2012; @deroba2013; @johnson2013]
and uncertainty in steepness of the stock-recruit relationship [@lee2012],
as well as determining what makes fisheries data informative 
[@magnusson2007; @wetzel2011a; @ono2013].

Stock Synthesis (SS) [@methot2013] is a widely-used fisheries stock assessment modeling framework.
It implements statistical age-structured population dynamics modeling
using a wide range of minimally-processed data [@maunder2013; @methot2013].
By using this generalized framework,
individuals conducting fisheries stock assessments and peer reviewers
can focus on the underlying science, instead of the model code [@methot2013].
Owing to these advantages, SS3 (the third version of the software)
is one of the world's most commonly-used stock assessment tools,
particularly in the United States and Australia,
where it has been used in 35 and 12 stock assessments as of 2012,
respectively [@methot2013].

Although SS is increasingly becoming a standard for fisheries stock assessment,
and the programming language R [@rcoreteam2013] has become the standard
for statistical computing and visualization,
there lacks a generalized framework
to link these components in a simulation context.
The only R package available to date to interface with SS is r4ss [@r4ss2013],
allowing reading and plotting SS output, 
manipulating SS input files, and computing likelihood profiles.
Here, we introduce ss3sim,
an R package that facilitates
large-scale, rapid, and reproducible simulation testing
with the widely-used SS framework. 
We begin by outlining the general structure of ss3sim
and by describing its functions.
We then demonstrate the software by developing a simple example.
We conclude by discussing how ss3sim complements other currently available simulation software
and by outlining some research questions that
our freely accessible and general SS simulation framework could address.

# The ss3sim framework

## Design goals of ss3sim

We designed ss3sim to be reproducible, flexible, and rapid.

*Reproducible*: ss3sim allows for the simulation testing structure to be documented
in R code and plain-text control files.
It allows for random seeds to be set 
prior to generating observation and process error making
simulations are therefore repeatable across computers and operating systems (Windows, OS X, and Linux)
using freely-available, open-source software.

*Flexible*: ss3sim inherits the flexibility of SS3 and can therefore implement many
available stock assessment configurations by modifying built-in 
generic life-history models.
Further, ss3sim summarizes the simulation output 
into comma-separated-value (`.csv`) files
allowing for the output to be easily processed
in nearly any statistical software, including R.
Finally, the ss3sim code is written under an open-source MIT license and can be freely modified.

*Rapid*: ss3sim relies on SS3,
which uses AD Model Builder as a backend optimization platform ---
the most rapid and robust optimization software available [@fournier2012].
ss3sim also facilitates the deployment of simulations
across multiple computers or computer cores, thereby accelerating computation.
<!--TODO add somewhere: Lastly, ss3sim uses the r4ss R package [@r4ss2013] suite of tools-->
<!--for reading and writing SS3 files and processing result data for the user.-->

## The general structure of an ss3sim simulation

An ss3sim simulation requires three types of input:
(1) a base SS3 model describing the underlying truth, 
or operating model (OM) in fisheries simulation terminology;
(2) a base SS3 model to assess that truth,
also known as the estimation model or method (EM);
and (3) a set of plain-text files (case files)
describing alternative model configurations and deviations from these base models.
Each unique combination of OM, EM, and case files are referred to here as a scenario.
Scenarios are usually run for multiple iterations,
possibly adding unique process and observation errors each time.
A simulation therefore refers to the combination of all scenarios and iterations.

ss3sim works, in general, by using case file arguments
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

To demonstrate ss3sim, we will work through a simple example 
in which we examine the effect on TODO XX of
high vs.\ low precision on a research survey index of abundance
and fixing vs.\ estimating natural mortality (*M*).
All files needed to run this example are available in the package data,
and a more detailed description of this example 
is available in the accompanying vignette (Text S1).

## Setting up the SS models

<!--TODO model vs. model setup?-->

Any existing SS model setup can be used with ss3sim
following minimal modifications (Text S1).
ss3sim also comes with built-in pre-modified SS3 model setups
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

ss3sim comes with a high-level function `run_ss3sim`, 
which can run all simulation steps 
based on a specified scenario ID 
and a set of semicolon-delimited plain-text files 
that describe alternative cases (Figure 1). 
These files contain the argument values that will be passed 
to the low-level ss3sim R functions 
(e.g. `change_e`, a function controlling which and how parameters are estimated;
Table 1) during the simulation.
Alternatively, the low-level functions can be used on their own 
as part of a more customized simulation wrapper function.

To use the high-level function `run_ss3sim`, the naming of the case files is important. 
All case files are named according to the type of case 
(e.g. `E` for estimation, `D` for data, or `F` for fishing mortality), 
a numeric value representing the case number, 
and a three letter code representing the species or stock (e.g. `cod`) (Table 1, Text S1).
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
we will manipulate the argument `sd_obs_surv`
that gets passed to `change_index`.
In case 1, we will specify the standard deviation of the index of abundance at `0.1`
and in case 2 we will increase the standard deviation to `0.4`.
We can do this by including the line: `sd_obs_surv; 0.1`
in the file `D1-cod.txt` and the line: `sd_obs_surv; 0.4`
in the file `D2-cod.txt`.
We will set up a base-case file describing fishing mortality (`F0-cod.txt`) 
and we will specify that we do not want to run a retrospective analysis 
in the file `R0-cod.txt`.

To start, we will load the ss3sim package into an R session
and locate three sets of folders within the package data:
(1) the folder with the OM,
(2) the folder with the EM,
(3) and the folder with the plain-text case files:

```
library(ss3sim)
d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
case_folder <- paste0(d, "/eg-cases")
```

## Running the simulations

It is important to validate a simulation testing framework
with minimal or no process and observation error 
to ensure unbiased and consistent recovery of parameters 
under ideal conditions [@hilborn1992].
ss3sim makes model validation simple by allowing users 
to specify process error (i.e. recruitment deviations) 
and control sampling error (Text S1).

The cod-like model setup has already been validated (Text S1), therefore
we can now run our simulation scenario.
We will set `bias_adjust = TRUE`, 
to enable a procedure that aims to produce mean-unbiased estimates 
of recruitment and biomass despite log-normal recruitment deviations [@methot2011].
Although not used in this example,
we could have run the simulations with parallel processing 
to substantially reduce computing time (Text S1).
We can run this simulation scenario with the following code:

```
run_ss3sim(iterations = 1:100, scenarios =
  c("D0-E0-F0-M0-R0-cod", "D1-E0-F0-M0-R0-cod",
    "D0-E1-F0-M0-R0-cod", "D1-E1-F0-M0-R0-cod"),
  case_folder = case_folder, om_model_dir = om, 
  em_model_dir = em, bias_adjust = TRUE)
```

We can then collect the output from all the simulations 
in our current directory with one function call.


```
get_results_all()
```

This creates two plain-text files in our working directory: 
`ss3sim_scalars.csv` and `ss3sim_ts.csv` 
containing scalar output estimates (e.g. maximum sustainable yield) 
and time-series estimates (e.g. biomass each year), respectively. 
There are separate columns for OM and EM values, making it simple to calculate 
error metrics, such as relative (Figure 2) or absolute error.
TODO add a couple sentences about the results.

# How ss3sim complements other simulation software

The general purpose of ss3sim 
is to explore the behaviour and performance
of alternative sampling scenarios 
and EM configurations across alternative states of nature 
specified by the OMs.
In particular, ss3sim provides a suite of functions 
for dynamically creating structural differences in OMs and EMs. 
This expedites testing the properties of alternative stock assessment model configurations,
whether the differences are between the OMs and EMs [@johnson2013],
or between multiple versions of the EMs [@ono2013]. 
ss3sim is thus ideal for answering questions 
about mismatches between OMs and EMs 
or different structures of EMs for a given OM. 

TODO fix this paragraph!
The downside this approach is the functions only work 
for a subset of combinations of options available for SS3 models. 
Due to this fact, and also due to the initial goal of ss3sim 
we emphasize generic models that can be broadly applied 
to a variety of simulation studies. 
Therefore, it is difficult 
to quickly adapt a new SS model to work within the framework. 
However, testing the properties of very specific assessment model setups 
is not the goal of ss3sim, 
and thus more generic models should be sufficient 
for most simulation studies conducted in this framework. 
Depending on the goal of the simulation study, 
there are other software frameworks available,
which may be a better alternative to ss3sim.

Fisheries libraries in R (FLR) [@kell2007] 
is an open-source framework developed specifically 
for evaluating fisheries management strategies using simulation. 
FLR aims to broadly incorporate a variety of disciplines
beyond fisheries science and ecology 
into the development of new methods and software 
for evaluating alternative management strategies and procedures. 
Compared to ss3sim, FLR is designed to explore broader questions
regarding management strategies 
with flexible biological, economic, and management components [@hillary2009].
Thus, it is not designed to explore the impact 
of structural differences within OMs and EMs.

The Fishery Simulation program (FS, <http://fisherysimulation.codeplex.com>)
is primarily a file system management tool 
adapted to aid in stock assessment simulation testing. 
FS can work with stock assessment models besides SS,
make simple changes to input text files, 
generate random process (using a built-in random number generator)
and observation errors
(using the SS3 bootstrap option),
run the models in parallel
and collect desired results from output files. 
Thus, FS is closer to ss3sim in its scope than FLR 
in that it specifically focuses on the performance of stock assessment models.

FS differs from ss3sim mainly in that it uses simple text manipulation
(e.g. change line 50 from 0 to 1)
to alter models rather than the approach of ss3sim, 
which uses flexible external functions. 
Since it does not rely on these external functions, 
there are few restrictions on which types of models can be used
and it works well for testing arbitrary assessment models 
[e.g. @lee2012; @piner2011; @lee2011]. 
In contrast, FS cannot make complicated structural changes
to a model (e.g. add time-varying parameters, change years of surveys),
making it difficult to induce and test 
structural differences between OMs and EMs. 
In addition, the version of FS available
at the time of publication of this paper 
is not an end-to-end package. 
That is, additional code is necessary to be able 
to incorporate process and observation error in simulation testing.
FS is also open-source but requires the Microsoft .NET framework
and is therefore only available on the Windows operating system.

# Research opportunities with ss3sim

The ss3sim package has been used so far 
to evaluate alternative approaches 
to deal with potentially varying natural mortality [@johnson2013],
the importance of composition data [@ono2013], 
and retrospective patterns in stock assessment models. 
Along with those studies, 
ss3sim makes many relevant research opportunities easily approachable.
Below we outline some examples.

<!--Although the low-level functions (`change` and `sample`) functions can be combined or modified in a user's own wrapper function to address nearly any simulation-based research question, there are-->
<!--*Domed selectivity*:-->
<!--*The importance of contrast in index series*:-->

*Time-varying model misspecification*:
Ecological processes can vary through time in response to, for example,
changes to fishing behaviour [@hilborn1992],
regime shifts [@vert-pre2013], or
climate change [@walther2002].
However, parameters such as natural mortality, catchability, and selectivity
are commonly assumed to be time invariant [@wilberg2006].
The consequences of assuming time invariance of such parameters when facing temporal changes have not been formally evaluated in a systematic way. TODO TRUE?
Further, although many studies have tried to isolate the effects
of single time-varying parameters, 
such natural mortality [@lee2011; @jiao2012; @deroba2013; @johnson2013],
few have considered the effect of multiple time-varying parameters and their potential interaction (REFS).

*Patterns in recruitment deviations*:
Typically, estimation methods assume
independent normally-distributed recruitment deviations
around a spawning stock recruitment function (REF).
However, recruitment deviations are frequently auto-correlated 
and their variability can change through time (REF).
ss3sim makes it simple
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
As a result, bias adjustment is routinely not used in stock assessment OR SIMULATION STUDIES? TODO (REF).
ss3sim can turn bias adjustment on or off
with a single argument and so could be easily used to test when
and how bias adjustment affects model performance.

*Retrospective patterns*:
Retrospective patterns,
in which model estimates are systematically biased
with each additional year of data,
are a major problem in stock assessment science [@mohn1999; @legault2008].
Key questions are: what causes retrospective patterns 
and what assessment approaches reduce retrospective patterns [@legault2008]?
ss3sim can run retrospective analyses as part of any simulation 
by adding a single argument --- the number of retrospective years to investigate.

# Conclusions

TODO re-work this:
The increasing complexity of modern statistical integrated models and expanding computing power allows for the inclusion of a multitude of processes in fisheries stock assessment methods. 
However, with added complexity comes the potential for model misspecification and model misspecification is often not accounted for in the evaluation of alternative modeling approaches. 
Simulation testing allows for the formal evaluation of the ability of assessment models to accurately and precisely estimate parameters of interest under different conditions and levels of misspecification [@deroba2013a; @wilberg2006; @crone2013]. 
However, most simulation testing work to date has been done using custom programmed frameworks tailored to the particular needs of each study. 
A recent large scale study [@deroba2013a] focused on evaluating the robustness of several stock assessment models to observation error. 
Some of the challenges found steamed from the use of models that were very different, both in their fundamental structure and in their data requirements and use limitations. 
An alternative approach is to use a generalized modeling platform such as SS, where alternative model setups reflect only alternative assumptions about the true underlying dynamics of stocks or estimation approaches and assumptions, but always using the same modeling plat-form. 
Although several simulation testing studies have used the widely used fisheries stock assessment platform SS [@lee2011; @lee2012; @piner2011; @crone2013a] a common end-to-end open access simulation testing framework is still lacking. 
ss3sim, allows for large-scale, rapid, and reproducible simulation with SS. 
The initial release of ss3sim describes the basic structure used in recent studies [@johnson2013; @ono2013]; however, future users are free to develop their own low- and high-level functions as needed, taken advantage of the open-source MIT-licensed R package structure, why we developed generic low-level functions and high-level functions

# Acknowledgements

We thank the participants and mentors of the University of Washington School of Aquatic and Fishery Sciences 2013 FISH 600 course. Participants:
Curry Cunningham
Felipe Hurtado-Ferro, 
Ricardo Licandeo, 
Carey McGilliard, 
Mellisa Muradian, 
Cody Szuwalski,
Katyana Vert-pre, and
Athol Whitten.
Mentors:
Ian Taylor, André Punt, and Richard Methot.
Discussions with these individuals were instrumental to developing ss3sim.
Many participants also contributed code and are listed as authors within specific ss3sim R functions.

# Funding
SCA was supported by Fulbright Canada, NSERC, and a Garfield Weston Foundation/B.C. Packers Ltd.\ Graduate Fellowship in Marine Sciences.
KFJ and KO were partially supported by NOAA grant 423 NA10OAR4320148 and 
CCM was partially supported by a Washington Sea Grant.
TODO Juan: any funding to acknowledge?

# Tables
\clearpage

Table 1: Main ss3sim functions and a description of their purpose. 
Simulations can be run through the `run_ss3sim` function. 
`run_ss3sim` then calls the `change` functions. 
Users can control what the `change` functions do through a series of plain-text case files. 
For example, the case ID `D1` corresponds to the case files `lcomp1`, `agecomp1`, and `index1`, as described in the table. 
Users can also use the `change` functions directly as part of their own simulation structure.

----------------------------------------------------------------
Function name          Description
---------------------- -----------------------------------------
`run_ss3sim`           Main high-level function to run ss3sim simulations.

`ss3sim_base`          Underlying base simulation function. Can also be called directly.

`change_f`             Controls fishing mortality. (Case file and ID `F`)

`change_tv`            Adds time-varying features. For example, time-varying natural mortality, growth, or selectivity. (Any case file, e.g. `M`, starting with "`function_type; change_tv`")

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
\includegraphics[width=5.0in]{fig2-20131109.pdf}
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
and fishing mortality (F) in the terminal year.
We show the values across simulation iterations with dots
and the distributions with beanplots (kernel density smoothers).

\clearpage

# References
