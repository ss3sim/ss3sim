# ss3sim 0.8.2.99

## New features

* Add Cole Monnahan's ss3sim plotting functions (#134).

## Bug fixes and changes

* Bump required version of r4ss to 1.22.1 since 1.22.0 did not export functions
  that ss3sim needs.

* Vignette no longer uses LaTeX `appendix.sty`, which causes build errors on CRAN
  Mavericks build.

* CITATION file now points to complete PLOS ONE citation.

* ss3sim now requires R 3.1.0 since we cannot easily test on earlier versions.

# ss3sim 0.8.2

## New features

* Added ability to pass seed value to `get_recdevs()` (#125).

* Added dimension check of `user_recdevs` to ensure recruitment deviations are
  supplied for all iterations (#130).

* Vignette now uses a custom Solarized colour theme.

## Bug fixes

* Moved vignette data files to the `data` folder so they are user-accessible
  after installing the package (#126).

* Updated out-dated paper citations. All papers are now accepted.

* Removed multi-core code in vignette for CRAN checks.
