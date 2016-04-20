Fixes compatibility with pending release of dplyr (development version 
0.4.3.9001).

---

## Test environments
* local OS X install, R 3.2.4
* Ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, or WARNINGs. There was one NOTE:

1.
```
Possibly mis-spelled words in DESCRIPTION:
  sim (26:21)
  ss (26:18)
```

These are part of 'ss3sim', the package name, and are not mis-spelled.

## Downstream dependencies

There are currently no downstream dependencies.
