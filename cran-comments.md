Adds functionality and fixes compatibility of vignette with ggplot2 2.0.0.

---

## Test environments
* local OS X install, R 3.2.3
* Ubuntu 12.04 (on travis-ci), R 3.2.3
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, or WARNINGs. There were two NOTES:

1.
```
Possibly mis-spelled words in DESCRIPTION:
  sim (26:21)
  ss (26:18)
```

These are part of 'ss3sim', the package name, and are not mis-spelled.

2.
```
change_e: possible error in r4ss::SS_changepars(
...
unused arguments (newlos = NULL, newhis = NULL,
newphs = par_phase[phasenochange])
```

These new arguments are contained in r4ss 1.24.0, which has
been simultaneously submitted.

## Downstream dependencies

There are currently no downstream dependencies.
