# ss3sim: An R package for fisheries stock-assessment simulation with Stock Synthesis

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
Our package can simultaneously investigate multiple scenarios
and be run across multiple computers in parallel.
We demonstrate the software with a simple example,
discuss how ss3sim complements other simulation software,
and outline research questions that ss3sim could address.


# Introduction

Simulation is a critical component to testing fishery stock-assessment methods
[@hilborn1987; @hilborn1992; @rosenberg1994; @peterman2004].
With simulation, we can evaluate the precision and bias of complex assessment methods
in a controlled environment where we know the true state of nature [@hilborn1992].
Recently, simulation studies have been key to improving strategies for dealing with,
for example, time-varying natural mortality [@lee2011; @jiao2012; @deroba2013],
uncertainty in steepness of the stock-recruit relationship [@lee2012],
and uncertainty in stock productivity [@ianelli2002].
[not necessarily the best examples]

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
in code and plain-text control files.
*Flexible*: ss3sim can implement all possible configurations of SS3.
Further, ss3sim summarizes the entire simulation into comma-separated-value (`.csv`) files
allowing for the output to be easily processed
with the package-provided functions or with other tools.
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
and (3) a set of plain text files (case files)
describing deviations from these base models.
Each unique combination of OM, EM, and case files are referred to as scenarios.
These scenarios are usually run for multiple iterations,
possibly adding unique process and observation error each time.
A simulation therefore refers to the combination of all scenarios and iterations.

ss3sim works, in general, by converting case file arguments
(e.g. arguments specifying a given natural mortality trajectory)
into manipulations of SS3 configuration files (`change` functions);
running the OM, sampling the time-series of population dynamics (pseudo data),
and running the EM (`run` functions);
and facilitating the manipulation and visualization of output
(`get` and `plot` functions) (Figure 1).

# An example simulation with ss3sim

(unsure how much of this will go in the main paper and how much will just be in the appendix...
probably many of these details should go in an appendix with just enough elements
to give a flavour for what can be done in the main paper)

*Setting up the SS models*:

- choosing a specific conditioning model or generic conditioning type
- setting up the OM and EM SS models
- things to keep in mind
- running through SS to format as `.ss_new` files and renaming
- required files

*Setting up the configuration files*:

- the (simple) research question (increasing or decreasing survey effort crossed with estimating M or fixing M)
- indicate which arguments to adjust

*Deterministic model testing*:

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

- there are lots, we should brainstorm some key ones

- KFJ: Effects of multiple time-varying parameters: Depending on the choice and
  trends of parameters the effects could be entirely different. Most
  simulations try to isolate the affects of a single parameter and very few
  look at two parameters. The package now has the ability to simulate data with
  multiple time-varying parameters and minimal coding on the users part. I
  think this would be a great interesting question to stress.

- CM: The effect of different structures for the recdevs. Most people
  (including us) generate independent normal recdevs, but that is probably not
  likely considering the changes in environment we see. With our user.recdevs
  capability it would be easy to simulate different structures such as
  autocorrelation or time varying sigma_r and see how that affects the EM.

- CM: The impact of bias adjustment. It is so easy to do duplicate runs with
  and without this option turned out, it would probably make sense to explore
  when and how it makes it a difference.

- KFJ: Additional info recdevs: The SS3 manual mentions that time-varying
  recdevs should be implemented with a recruitment survey by transforming the
  environmental variable into an age 0 pre-recruit survey. The environmental
  link can then be specified for the selectivity parameter. Using environmental
  deviations for the recdevs is discouraged because, "it  interacts  with  the
  level of  residual  recruitment variability and  there  is  no implementation
  of  a  bias  correction  for  the variability in recruitment caused by the
  environmental variable."... It would be interesting to see the difference
  between having a single set of recdevs with time-varying properties versus
  using a recruitment survey.

- KO: The impact of model mis-specification in general. This rejoins some of
  the questions that have been mentioned above and also some of the simulation
  we have already run. One example is: fishery changes every year in response
  to regulation, fish movement, and gear changes among others, causing changes
  in catchability and selectivity over time. However, selectivity and/or
  catchability are commonly assumed to be time invariant (for simplicity and
  data availability). What is the consequence? This is done very easily with
  this package.

# Conclusions

- benefit of using one well tested and well-understood modeling framework (SS) i.e. benefit to playing with all the switches and understanding one framework well versus having many tools that we superficially understand (based on Rick's comments at the conference)
- why we developed generic low-level functions and high-level functions
- researchers are free to develop their own low- and high-level functions because in an open-source MIT(?) licensed R package, users are free to modify functions as needed
- (these points are somewhat random at the moment)

# Acknowledgements
We thank Ian Taylor, André Punt, and Richard Methot for helpful discussions during the development of ss3sim.
KFJ and KO were partially supported by NOAA grant 423 NA10OAR4320148.
SCA was supported by Fulbright Canada, NSERC, and a Garfield Weston Foundation/B.C. Packers Ltd. Graduate Fellowship in Marine Sciences.

# Tables

Table X: Comparison with related software? Possible columns: software, reference, platform (e.g. R, GUI...), short description/comparison, examples of papers using it

# Figures legends

Figure 1: Flow diagram of `run_ss3sim()` stock-assessment simulation steps.
Function names are shown in a monospaced font.

Figure 2: Panels with output from the example

# References
