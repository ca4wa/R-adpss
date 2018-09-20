## Resubmission
This is a resubmission. In this version I have resolved the issues identified
by clang-UBSAN check of CRAN.

[Details]
The package has passed the clang-UBSAN check in my Debian 9.5.0.  Some of the
issues indicated by CRAN check have been resolved.  The rest problems did not
appear in my Debian environment.  Nonetheless, I identified that the problem
was the use of a generic pointer and hence replaced it with a function
template.  Thus, the all problems have been expected to be removed.

## Test environment
* Ubuntu 14.04.5 LTS, R.3.5.0
* Debian 9.5.0, R.3.5.1 Patched (2018-9-14 r75320)
* win_builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were no NOTES in Ubuntu and Debian.

There were 1 NOTEs in win_builder:

* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Kosuke Kashiwabara <kashiwabara-tky@umin.ac.jp>'

We further note that, because this packages requires R >= 3.5.0, errors in CRAN
check by older versions of R are not the problem of this package.

## Downstream dependencies
No dependencies.