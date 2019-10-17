## Resubmission
This is a resubmission ss3sim, where the version has increased to 1.0.1.
In this version I have taken care of 
1 Note, 1 Error, and fixed two broken links (see below).

### Note
The package maintainer was changed from Sean Anderson <sean@seananderson.ca>
to Kelli F. Johnson <kelli.johnson@noaa.gov>. Sean Anderson sent an email
regarding the change on October 16, 2019 to forewarn CRAN.

### Error
r4ss is a required packages of ss3sim. The error, 
`Package required and available but unsuitable version: ‘r4ss’`,
will be taken care of once the newly submitted r4ss version 1.36.1 is approved.

### Broken links

* fixed the invalid URL in Readme.md
* fixed the invalid URI in inst/doc/introduction.html
* confirmed the success of a potentially invalid URI in inst/doc/making-models.html

## Test environments
* local Windows, R 3.6.1
* local macOS install 10.14.6, R 3.6.1
* Ubuntu Trusty 14.04 (on travis-ci), R devel
* win-builder (R devel)

## R CMD check results
The 1 Note and 1 Error are addressed above in the resubmission comments.
Locally on Windows (R 3.6.1) there are no Notes, Warnings, or Errors.

## Downstream dependencies
There are currently no downstream dependencies for this package.
