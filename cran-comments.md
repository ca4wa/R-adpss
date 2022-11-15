## Test environment
* Ubuntu 20.04.1 LTS, R-release(4.2.2), GCC, by devtools::check_rhub
* Debian, R-devel, GCC ASAN/UBSAN, by devtools::check_rhub
* Windows (devel, release and oldrelease), by devtools::check_win

## R CMD check results
There were no ERRORs or WARNINGs.

There were no NOTES in Debian.

There was 1 NOTE in Windows:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kosuke Kashiwabara <kashiwabara-tky@umin.ac.jp>'

There were 2 NOTEs in Ubuntu:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Kosuke Kashiwabara <kashiwabara-tky@umin.ac.jp>’

* checking installed package size ... NOTE
  installed size is  8.4Mb
  sub-directories of 1Mb or more:
    libs   8.2Mb

## Downstream dependencies
No dependencies.