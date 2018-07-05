## Test environment
* Ubuntu 14.04.5 LTS, R.3.5.0
* win_builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were no NOTES in Ubuntu.

There were 3 NOTEs in win_builder:

* checking CRAN incoming feasibility ... NOTE

Maintainer: 'Kosuke Kashiwabara <kashiwabara-tky@umin.ac.jp>'

* running examples for arch 'i386' ... [21s] NOTE
* running examples for arch 'x64' ... [20s] NOTE

I considered that these two NOTEs were not significant:
because the example contains 7 functions we newly developed
but each function works within a few seconds.

## Downstream dependencies
No dependencies.