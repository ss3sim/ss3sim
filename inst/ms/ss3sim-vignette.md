% ss3sim vignette
% Sean C. Anderson and lots of others...
%

First, we'll locate three sets of folders that are located within the package data: (1) the folder with the plaintext case files, (2) the folder with the operating model (OM), and (3) the folder with the estimating model (EM).


```r
library(ss3sim)
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
```


First, we'll run some "deterministic" runs to check our model for bias when we don't have any process error.
To do this, we'll start by setting up a matrix of recruitment deviations with 0 deviations. 
We need 100 rows (for 100 year simulations) and 20 columns (for 20 deterministic iterations).


```r
recdevs_det <- matrix(0, nrow = 100, ncol = 20)
```


Then we'll set up case "estimation" files in which the recruitment deviations are set to the nominal level of `0.001`. 
We'll name these files `E100-cod.txt` and `E101-cod.txt`. 
In the control files, the key element is setting `par_name = SR_sigmaR` and `par_int = 0.001`.

When we run the simulations, we'll pass our deterministic recruitment deviations to the function `run_fish600`. 
Running 20 replicates should be enough to identify whether our models are performing as we expect.


```r
run_fish600(iterations = 1:20, scenarios = 
  c("D0-E100-F0-G0-R0-S0-M0-cod",
    "D1-E100-F0-G0-R0-S0-M0-cod", 
    "D0-E101-F0-G0-R0-S0-M0-cod",
    "D1-E101-F0-G0-R0-S0-M0-cod"), 
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, user_recdevs = recdevs_det)
```


Now we can run the stochastic simulations.


```r
run_fish600(iterations = 1:100, scenarios = 
  c("D0-E0-F0-G0-R0-S0-M0-cod",
    "D1-E0-F0-G0-R0-S0-M0-cod", 
    "D0-E1-F0-G0-R0-S0-M0-cod",
    "D1-E1-F0-G0-R0-S0-M0-cod"), 
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE)
```


The function `get_results_all` reads in a set of scenarios and combines the output into two `.csv` files: `final_results_scalar.csv` and `final_results_ts.csv`.


```r
get_results_all(user.scenarios = 
  c("D0-E100-F0-G0-R0-S0-M0-cod",
    "D1-E100-F0-G0-R0-S0-M0-cod", 
    "D0-E101-F0-G0-R0-S0-M0-cod",
    "D1-E101-F0-G0-R0-S0-M0-cod", 
    "D0-E0-F0-G0-R0-S0-M0-cod",
    "D1-E0-F0-G0-R0-S0-M0-cod",
    "D0-E1-F0-G0-R0-S0-M0-cod",
    "D1-E1-F0-G0-R0-S0-M0-cod"))
```


Let's read in the `.csv` files and calculate some useful values in new columns. 



```r
scalar_dat <- read.csv("final_results_scalar.csv")
ts_dat <- read.csv("final_results_ts.csv")

scalar_dat <- transform(scalar_dat,
  SSB_MSY=(SSB_MSY_em-SSB_MSY_om)/SSB_MSY_om,
  log_max_grad = log(max_grad))

ts_dat <- transform(ts_dat, SpawnBio=(SpawnBio_em-SpawnBio_om)/SpawnBio_om)
ts_dat <- merge(ts_dat, scalar_dat[,c("scenario", "replicate",
    "max_grad")])

scalar_dat_det <- subset(scalar_dat, E %in% c("E100", "E101"))
scalar_dat_sto <- subset(scalar_dat, E %in% c("E0", "E1"))
ts_dat_det <- subset(ts_dat, E %in% c("E100", "E101"))
ts_dat_sto <- subset(ts_dat, E %in% c("E0", "E1") & replicate %in% 1:50)

# add more here
```


Now let's look at boxplots of the deterministic model runs. 


```r

plot_scalar_boxplot(scalar_dat_det, x = "SR_LN_R0_om", y = "SSB_MSY",
  vert = "D", relative_error = TRUE)
```

![Boxplot of relative error for SSB MSY. We see relatively little bias.](figure/plot-deterministic.pdf) 

```r

# add more here
```


Let's look at the relative error in estimates of spawning biomass. 
We'll colour the time series according to the 


```r
plot_ts_points(ts_dat_sto, y = "SpawnBio", vert = "D", 
  color = "max_grad", relative_error = TRUE)
```

![Time series of relative error in spawning stock biomass.](figure/plot-sto-ts.pdf) 


