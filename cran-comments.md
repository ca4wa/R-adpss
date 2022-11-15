## Test environment
* Ubuntu 20.04.1 LTS, R-release(4.2.2), GCC, by devtools::check_rhub
* Debian, R-devel, GCC ASAN/UBSAN, by devtools::check_rhub
* Windows (devel, release and oldrelease), by devtools::check_win_...

## R CMD check results
There were no ERRORs or WARNINGs in all environments.

There were no NOTES in Windows and Debian.

There was 1 NOTE in Ubuntu:

* checking installed package size ... NOTE
  installed size is  8.4Mb
  sub-directories of 1Mb or more:
    libs   8.2Mb

## Downstream dependencies
No dependencies.