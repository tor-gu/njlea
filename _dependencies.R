# Not sourced by anything. This file exists so that renv's implicit snapshot
# records the development tooling in renv.lock, which it would otherwise skip:
# nothing in R/, tests/ or data-raw/ refers to these packages, but the workflow
# in CLAUDE.md depends on them being present after renv::restore().
library(devtools)
library(testthat)
library(readxl)   # rebuilding the Major Discipline agency extract (data-raw/sources/major_discipline/README.md)
